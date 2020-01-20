library(shiny)
library(dplyr)
library(DT)
library(tidyr)
library(tibble)
library(shinyWidgets)
library(ggplot2)
library(data.table)
library(magrittr)


setwd('~/master/R')
path <- '/scratch/mfiruleva/winter/initial_kallisto/'
m_path <- '/scratch/mfiruleva/winter/kallisto_new/'
kallisto <- fread('table_for_pipeline_running_kallisto.tsv') %>% 
  select(GSE, GSM, secondary_sample_accession, run_accession, scientific_name, technology) %>% 
  filter(!duplicated(secondary_sample_accession)) %>% select(-run_accession)
multiple <- fread('multiple_runs.csv', header=F) %>% set_colnames("SRS")

stats <- read.csv('stats.csv', header=F) %>% set_colnames(c("cells.before", "cells.after", "run")) %>% 
  mutate(copy_run = run) %>% 
  separate(run, c("GSE", "SRS", "SRR")) %>%
  select(-SRR) %>% 
  mutate(count_matrix = ifelse(SRS %in% multiple$SRS, paste0(m_path, copy_run), paste0(path, copy_run))) %>% 
  select(-copy_run)

def_chr <- fread('10x_define_tech.csv') %>% select(-c(1, 3, 6, 7, 9)) %>% 
  set_colnames(c('GSE', 'GSM', 'secondary_sample_accession', 'run_accession', 'scientific_name', 'R1_length','technology'))
uniq_chr <- fread('uniq_runs_only_task.csv', header=F) %>% set_colnames("SRS")
chromium <- fread('only_10x_data.tsv') %>%
  select(GSE, GSM, secondary_sample_accession, run_accession, scientific_name, technology) %>% 
  mutate(technology = def_chr$technology[match(run_accession, def_chr$run_accession)]) %>% 
  filter(secondary_sample_accession %in% uniq_chr$SRS) %>% select(-run_accession)
kallisto <- rbind(kallisto, chromium)

matched_srs <- filter(stats, SRS %in% kallisto$secondary_sample_accession)

kallisto <- kallisto %>% 
  mutate(counts_path = matched_srs$count_matrix[match(secondary_sample_accession, matched_srs$SRS)]) %>% 
  mutate(counts_path = ifelse(!is.na(counts_path), paste0(counts_path, "/counts.RData"), NA)) %>% 
  mutate(counts = ifelse(!is.na(counts_path), "yes", "no")) %>% 
  mutate(cells.before = matched_srs$cells.before[match(secondary_sample_accession, matched_srs$SRS)]) %>% 
  mutate(cells.after = matched_srs$cells.after[match(secondary_sample_accession, matched_srs$SRS)])

all_techs <- sort(unique(kallisto$technology))
processed_counts <- sort(unique(kallisto$counts))
organisms <- sort(unique(kallisto$scientific_name))
chr_techs <- sort(unique(def_chr$technology))
# UI
ui <- fluidPage(
  titlePanel("scRNA-seq processing, GEO database"),
  sidebarLayout(
    # Input(s)
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "All"',
        pickerInput("technology","Select technology", choices=all_techs, options = list(`actions-box` = TRUE),multiple = T, selected = all_techs),
        pickerInput("scientific_name","Select organism", choices=organisms, options = list(`actions-box` = TRUE),multiple = T, selected = organisms),
        pickerInput("counts","Select objects with or without count matrix", choices=processed_counts, options = list(`actions-box` = TRUE),multiple = T, selected=processed_counts),
        downloadButton("downloadData", "Download"),
        plotOutput('Hist')
      ),
      conditionalPanel(
        'input.dataset === "10x_R1"',
        plotOutput('Hist_R1')
      )
    ),
    # Output(s)
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("All", DT::dataTableOutput(outputId = "techtable")),
        tabPanel("10x_R1", DT::dataTableOutput(outputId = "tenx_R1"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Create data table
  datasetInput <- reactive({
    req(input$technology)
    samples_with_tech <- kallisto %>%
      filter(technology %in% input$technology)
    samples_with_counts <- samples_with_tech %>%
      filter(counts %in% input$counts) %>% select(-counts)
    samples_with_org <- samples_with_counts %>%
      filter(scientific_name %in% input$scientific_name)
    samples_with_org
  })
  output$techtable <- renderDataTable({
    datatable(datasetInput(), rownames = F, options = list(
      autoWidth = TRUE,
      scrollX=TRUE,
      columnDefs = list(list(width = '200px', targets = "_all"))
    ))
  },
  )
  output$tenx_R1 <- DT::renderDataTable({
    datatable(def_chr, rownames = F, options = list(
      autoWidth = TRUE,
      scrollX=TRUE,
      columnDefs = list(list(width = '200px', targets = "_all"))
    ))
  })
  output$Hist <- renderPlot({
    ggplot(filter(kallisto, counts == "yes"), aes(x = log10(cells.after)))+
      geom_histogram(col = "black", fill="white", bins=30)+
      theme_bw()+
      theme(aspect.ratio = 1)+
      ggtitle("cells after emptyDrops filtration\nfor all count matrixes")
  }) 
  output$Hist_R1 <- renderPlot({
    ggplot()+aes(def_chr$R1_length)+
      geom_histogram(col = "black", fill="white", bins=30)+
      theme_bw()+
      theme(aspect.ratio = 1)+
      xlab("R1 length")+
      ggtitle("R1 length distribution, 10x")
  }) 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("geo_filtered_data.csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE, quote = F)
    }
  )
  
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)