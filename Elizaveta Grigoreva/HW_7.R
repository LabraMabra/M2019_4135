library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

#Load data
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
      selectInput(inputId = "x",
                  label = "X-axis",
                  choices = c("Miles per Gallon" = "mpg", "Displacement" = "disp", "Horsepower" = "hp", "Rear axle ratio" = "drat", "Weight" = "wt", "1/4 Mile" = "qsec"),
                  selected = "mpg"),
      
      selectInput(inputId = "y", 
                  label = "Y-axis",
                  choices =  c("Miles per Gallon" = "mpg", "Displacement" = "disp", "Horsepower" = "hp", "Rear axle ratio" = "drat", "Weight" = "wt", "1/4 Mile" = "qsec"),
                  selected = "disp"),
      
      selectInput(inputId = "color",
                  label = "Color",
                  choices = c("Cylinders" = "cyl", "Engine" = "vs", "Transmission" = "am", "Number of gears" = "gears", "Number of carburators" = "carb"),
                  selected = "cyl"),
      
      sliderInput(inputId = "alpha",
                  label = "Alpha",
                  min = 0, max = 1,
                  value = 0.5),
      
      numericInput(inputId = "size",
                   label = "Dot size",
                   value = 3,
                   min = 1, max = 9),
      checkboxInput(inputId = "show_d",
                    label = "Show data?",
                    value = FALSE),
      checkboxInput(inputId = "show_s",
                    label = "Show summary?",
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

#Define server logic
server <- function(input, output) {
  output$scatter <- renderPlot({
    req(input$size)
    ggplot(data, aes_string(x = input$x, y = input$y, color = input$color)) +
      geom_point(alpha = input$alpha, size = input$size) + 
      ggtitle(tools::toTitleCase(isolate(input$title)))
  })
 
 #Create  a vector with reactive values
  final_data <- reactive({
    data %>% select_(input$x, input$y,input$color)
  })
  output$data_selected <- renderTable({
    if (input$show_d) {
      final_data()
    }
  })
  
  output$summary <- renderTable({
    if (input$show_s) {
      final_data() %>% 
        group_by_(input$color) %>% 
        summarise_all(funs(mean, sd))
      
    }
  })
  
}

#Run the application
shinyApp(ui = ui, server = server)
