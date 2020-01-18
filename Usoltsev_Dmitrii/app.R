#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/  
#

library(shiny)
library(ggplot2)

data <- mtcars
data$cyl <- as.factor(data$cyl)
data$vs <- as.factor(data$vs)
data$am <- as.factor(data$am)
data$gear <- as.factor(data$gear)
data$carb <- as.factor(data$carb)


# Define UI for application 
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "x",
                        label ='X-axis',
                        choices = c('Miles per gallon'='mpg','Displacement'='disp','Horsepower'='hp',
                                    'Rear axle ratio'='drat', 'Weight' ='wt','1/4 mile time '='qsec'),
                        selected = 'mpg'),
            selectInput(inputId = "y",
                        label ='Y-axis',
                        choices = c('Miles per gallon'='mpg','Displacement'='disp','Horsepower'='hp',
                                    'Rear axle ratio'='drat', 'Weight' ='wt','1/4 mile time '='qsec'),
                        selected = 'disp'),
            selectInput(inputId = "col",
                        label ='color_by',
                        choices = c('Number of cylinders' ='cyl','Engine'='vs','Transmission'='am',
                                    'Number of forward gears' ='gear','Number of carburetors' = 'carb'),
                        selected = 'cyl'),
            sliderInput(inputId = "alpha",
                       label ='Transperancy:',
                       value = 0.8,
                       min =0, max =1),
            numericInput(inputId = "size",
                         label ="Dot size:",
                         value = 3,
                         min =1, max =9),
            textInput(inputId ="title",
                      label = "Plot title:",
                      placeholder = "Enter the title"),
            checkboxInput(inputId = "show_data",
                          label = "Show data?",
                          value = FALSE),
            checkboxInput(inputId = "show_sum",
                          label = "Show summary?",
                          value = FALSE),
            submitButton("Apply")
            ),
      
        mainPanel(
           plotOutput(outputId = 'scatter'),
           fluidRow (
               column(width = 6,
                      tableOutput(outputId = "data_selected")),
               column(width = 6,
                      tableOutput(outputId = "summary")
               )
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatter <- renderPlot({
        req(input$size) #necessarily
        ggplot(data, aes_string(x =input$x, y=input$y, col=input$col))+
            geom_point(alpha = input$alpha, size = input$size)+
            ggtitle(tools::toTitleCase(isolate(input$title)))
    })
    new_data <- reactive({
        data %>% 
            select_(input$x, input$y, input$col)
    })
    
    output$data_selected <- renderTable({
        if (input$show_data) {
            new_data()
        }
    })
    
    output$summary <- renderTable({
        if (input$show_sum) {
            new_data() %>% 
                group_by_(input$col) %>%
                summarise_all(funs(mean,sd))
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
