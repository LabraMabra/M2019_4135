library(shiny)
library(ggplot2)
library(dplyr)

data <- mtcars
data$cyl <- as.factor(mtcars$cyl)
data$vs <- as.factor(mtcars$vs)
data$am <- as.factor(mtcars$am)
data$gear <- as.factor(mtcars$gear)
data$carb <- as.factor(mtcars$carb)

ui <-fluidPage (
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = 'title',
                label = 'Plot title:',
                placeholder = 'Enter the title'),
      selectInput(inputId = 'x',
                  label = 'X-axis',
                  choices = c('Miles' = 'mpg', 'Displacement' = 'disp', 'Gross horsepower' = 'hp', 'Rear axle ratio' = 'drat', 'Weight ' = 'wt', '1/4 mile time' = 'qsec'),
                  selected = 'mpg'),
      selectInput(inputId = 'y',
                  label = 'Y-axis',
                  choices = c('Miles' = 'mpg', 'Displacement' = 'disp', 'Gross horsepower' = 'hp', 'Rear axle ratio' = 'drat', 'Weight ' = 'wt', '1/4 mile time' = 'qsec'),
                  selected = 'disp'),
      selectInput(inputId = 'color',
                  label = 'Color by',
                  choices = c('Number of cylinders' = 'cyl', 'Engine type' = 'vs',
                              'Transmission' = 'am', 'Number of forward gears' = 'gear',
                              'Number of carburetors' = 'carb'),
                  selected = 'cyl'),
      sliderInput(inputId = 'alpha',
                  label = 'Alpha:',
                  min = 0, max = 1,
                  value = 0.5),
      numericInput(inputId = 'size',
                   label = 'Dot size:',
                   min = 1, max = 9,
                   value = 3),
      checkboxInput(inputId = 'show_data',
                    label = 'Show data?',
                    value = FALSE),
      checkboxInput(inputId = 'show_s',
                    label = 'Show summary?',
                    value = FALSE),
      submitButton("Submit")
    ),
    mainPanel(
      plotOutput(outputId = 'scatter'),
      fluidRow(column(width = 6,
                      tableOutput(outputId = "data_selected")),
               column(width = 6,
                      tableOutput(outputId = "summary"))
      )
    )
  )
)

server <- function(input, output){
  output$scatter <- renderPlot({
    req(input$size)
    ggplot(data, aes_string(x = input$x, y = input$y, color = input$color)) +
      geom_point(alpha = input$alpha, size = input$size) +
      ggtitle(tools::toTitleCase(isolate(input$title)))
  })
  
  new_data <- reactive ({
    data %>% select_(input$x, input$y, input$color)
  })
  
  output$data_selected <- renderTable({
    if(input$show_data) {
      new_data()
    }
  })
  output$summary <-renderTable({
    if (input$show_s) {
      new_data() %>% 
        group_by_(input$color) %>%
        summarise_all(funs(mean, sd))
    }
  })
}

shinyApp(ui, server)

