library(shiny)
setwd("~/Exam_porject")
library(edgeR)
library(ggplot2)


ui <- fluidPage(sidebarLayout(
  
  sidebarPanel(
    
    selectInput(inputId = "data",
                label = "Counts table",
                choices = c("p.vanderplankii",
                            "p.vanderplankii(without 3K replicate)",
                            "yeast"),
                selected = "yeast"),

    selectInput(inputId = "method",
                label = "Correlation Method",
                choices = c("spearman",
                            "kendall",
                            "pearson"),
                selected = "spearman"),
    
    selectInput(inputId = "p_correction",
                label = "Multiple correction method",
                choices = c("Holm"="holm", "Hochberg"="hochberg",
                            "Hommel"="hommel", "Bonferroni" = "bonferroni",
                             "Benjamini & Hochberg"="BH",
                            "Benjamini & Yekutieli"="BY",
                            "False discovery rate"="fdr",
                            "Without multiple correction"="none"),
                selected = "BH"),
    
    sliderInput(inputId = "pval",
                label = "P-value trashold",
                value = 0.05,
                min = 0.001,
                max = 0.05),
    
    numericInput(inputId = "size",
                 label = "Dot size:",
                 value = 1,
                 min = 0.5,
                 max = 2),
    
    sliderInput(inputId = "alpha",
                label = "Transparancy",
                value = 0.5,
                min = 0,
                max = 1),
    
    selectInput(inputId = "direction",
                label = "logFC change restriction",
                choices = c("Upperbound","Lowerbound"),
                selected = "Upperbound" 
                ),
    
    sliderInput(inputId = "logFC",
                label = "logFC trashold",
                value = 0,
                min = -5,
                max = 5),
    
    sliderInput(inputId = "logCPM",
                label = "logCPM trashold",
                value = 0,
                min = 2.5,
                max = 10),
    
    submitButton("Update View", icon("refresh"))
    
  ),
  
  mainPanel(
    
    plotOutput(outputId = "heatmap"),
    plotOutput(outputId = "BCV"),
    plotOutput(outputId = "scatter"),
    plotOutput(outputId = "logCPM"),
    plotOutput(outputId = "logFC"),
    tableOutput(outputId = "table")
    
  )))
  

server <- function(input, output) {
 
  my_project <- function(filename="yeast", hm_method = "spearman",
                         p_correction = "BH",
                         table_save = "Total",
                         p_trashold = 0.05) {
    
    if (filename == "yeast") {
      group <- c(1,1,2,2)
    } else if (filename == "p.vanderplankii(without 3K replicate)") {
      group <- c(1,1,2,2,2)
    } else {
      group <- c(1,1,1,2,2,2)
    }
    
    data <- read.csv(filename)
    count <- data[,-1]
    rownames(count) <- data[,1] 
    edger <- DGEList(counts=count,group=group)
    keep <- rowSums(cpm(edger)>1) >= 2
    edger <- edger[keep, , keep.lib.sizes=FALSE]
    # Нормализация
    edger <- calcNormFactors(edger)
    mat_cor <- cor(edger$counts,method = hm_method)
    
    
    design <- model.matrix(~group)
    # Оцениваем дисперсии
    edger <- estimateGLMCommonDisp(edger,design)
    edger <- estimateGLMTrendedDisp(edger,design)
    edger <- estimateGLMTagwiseDisp(edger,design)
   
    
    # likehood ratio test
    strict.disp <- pmax(edger$tagwise.dispersion,
                        edger$trended.dispersion,
                        edger$common.dispersion)
    results <- list()
    results$edger <- edger
    
    fit <- glmFit(edger,design)
    pv.st <- glmLRT(fit,coef=2)
    pv.st$table$PValue <- p.adjust(pv.st$table$PValue, method = p_correction)
    geneid <- as.data.frame(rownames(pv.st$table))
    colnames(geneid) <- "geneid"
    pv.st$table<- cbind(geneid,
    pv.st$table) 
    results$mat_cor <- mat_cor
    results$pv.st <- pv.st
    return(results)
  }
  

res <-  reactive({my_project(filename = input$data,
             hm_method = input$method,
             p_correction = input$p_correction,
             p_trashold = input$pval,
             table_save = input$table)})
  
output$heatmap <- renderPlot({
  
  
  heatmap(res()$mat_cor)
  
  })

output$BCV <- renderPlot({
  
  
  plotBCV(res()$edger)
  
})


output$scatter <- renderPlot({
  dfeg <- res()$pv.st
  
  if(input$direction == "Upperbound"){
      condition <- dfeg$table$PValue < input$pval & dfeg$table$logFC > input$logFC & dfeg$table$logFC > input$logCPM
      plotSmear(dfeg, 
                              de.tags = rownames(dfeg$table)
                             [condition],
                            cex=input$size)
  } else {
    condition <- dfeg$table$PValue < input$pval & dfeg$table$logFC < input$logFC & dfeg$table$logFC > input$logCPM  
    plotSmear(dfeg, 
              de.tags = rownames(dfeg$table)
              [condition],
              cex=input$size)
    
    
  }
})

output$logCPM <- renderPlot({
  res <- res()$pv.st$table
  
  ggplot(res, aes(logCPM)) + geom_density(fill = "green",
                                          color = "green",
                                          alpha = input$alpha)
  
})

output$logFC <- renderPlot({
  res <- res()$pv.st$table
  
  ggplot(res, aes(logFC)) + geom_density(fill = "red",
                                         color = "red",
                                         alpha = input$alpha)
  

})

output$table <- renderTable({
  res <- res()$pv.st$table
  
  if(input$direction == "Upperbound"){
    filter(res,
         PValue < input$pval & logFC > input$logFC & logCPM > input$logCPM)}
  else {
         filter(res,
                PValue < input$pval & logFC < input$logFC & logCPM > input$logCPM) 
  }
  
})


}

shinyApp(ui = ui, server = server)