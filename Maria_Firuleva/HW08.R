library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
data <- mtcars


data$cyl <- as.factor(data$cyl)
data$vs <- as.factor(data$vs)
data$am <- as.factor(data$am)
data$gear <- as.factor(data$gear)
data$carb <- as.factor(data$carb)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "title",
                label = "Plot title",
                placeholder = "Enter the title"),
      pickerInput("x","Select x-lab variable", choices=colnames(mtcars), options = list(`actions-box` = TRUE),multiple = F, selected = "mpg"),
      pickerInput("y","Select y-lab variable", choices=colnames(mtcars), options = list(`actions-box` = TRUE),multiple = F, selected="hp"),
      pickerInput("color","Select color variable", choices=colnames(mtcars), options = list(`actions-box` = TRUE),multiple = F,selected="gear"),
      sliderInput(inputId = "alpha",
                  label = "Alpha",
                  min = 0, max = 1,
                  value = 0.5),
      numericInput(inputId = "size",
                   label = "Dot size",
                   value = 3,
                   min = 1, max = 9),
      checkboxInput(inputId = "show_data",
                    label = "Data",
                    value = FALSE),
      checkboxInput(inputId = "show_summary",
                    label = "Summary",
                    value = FALSE),
      submitButton("Submit")
      
    ),
    mainPanel(
      plotOutput(outputId = "scatter"),
      fluidRow(column(width = 6, tableOutput(outputId = "data_selected")
      ),
      column(width = 6, tableOutput(outputId = "summary"))
      )
    )
  )
)

server <- function(input, output) {
  output$scatter <- renderPlot({
    req(input$size)
    ggplot(data, aes_string(x = input$x, y = input$y, color = input$color)) +
      geom_point(alpha = input$alpha, size = input$size) + 
      theme_bw(base_size = 8)+
      ggtitle(tools::toTitleCase(isolate(input$title)))+
      theme(aspect.ratio = 1, plot.title = element_text(hjust = 0.5, size = 12))
  })
  
  final_data <- reactive({
    data %>% select_(input$x, input$y,input$color)
  })
  output$data_selected <- renderTable({
    if (input$show_data) {
      final_data()
    }
  })
  output$summary <- renderTable({
    if (input$show_summary) {
      final_data() %>% 
        group_by_(input$color) %>% 
        summarise_all(funs(mean, sd))
      
    }
  })
  
}


shinyApp(ui = ui, server = server)