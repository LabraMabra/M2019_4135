library(shiny)
library(ggplot2)
data <- mtcars
mtcars$gear <- as.factor(mtcars$gear)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$carb <- as.factor(mtcars$carb)
mtcars$am <- as.factor(mtcars$am)
mtcars$vs <- as.factor(mtcars$vs)

ui <- fluidPage(
  
     sidebarLayout(
       
       sidebarPanel(
        
         textInput(inputId = "mtcars", label = "Plot title"),
         selectInput(inputId = "x",
                     label = "X-axis",
                     choices = c("mpg","disp","hp","drat","wt","qsec"),
                     selected = "mpg"),
         
         
         selectInput(inputId = "y",
                     label = "Y-axis",
                     choices = c("mpg","disp","hp","drat","wt","qsec"),
                     selected = "disp"),
         
         
         
  
       selectInput(inputId = "color",
                   label = "color by",
                   choices = c("Cylinders" = "cyl", "Engines" = "vs","Transmission"= "am","Gears" = "gear","Carburetors" = "carb"),
                   selected = "cyl"),
       
       
       sliderInput(inputId = "alpha",
                   label = "Transparancy",
                   value = 0.5,
                   min = 0,
                   max = 1),
       
       numericInput(inputId = "size",
                    label = "Dot size:",
                    value = 3,
                    min = 1,
                    max = 9),
       checkboxInput(inputId = "data", label = "Show data", value = T),
       
       submitButton("Update View", icon("refresh"))
       
       
     ),
       
       mainPanel(
         
         plotOutput(outputId = "scatter"),
         tableOutput(outputId = "table")
       )
       
     )
  
)

server <- function(input, output) {
  
  output$scatter <- renderPlot({
    ggplot(data, aes_string(x = input$x, y = input$y, color = input$color)) +
      geom_point(size = input$size,alpha = input$alpha)
  })
  
  output$table <- renderTable({
    if (input$data) {
       mtcars
    }
  })
}


shinyApp(ui = ui, server = server)