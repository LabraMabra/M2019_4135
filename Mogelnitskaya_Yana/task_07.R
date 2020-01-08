library(shiny) 
library(ggplot2)
library(dplyr)


data <- mtcars
data$cyl<- as.factor(data$cyl)
data$vs <- as.factor(data$vs)
data$am <- as.factor(data$am)
data$gear <- as.factor(data$gear)
data$carb <- as.factor(data$carb)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "x",
                  label = "X-axis",
                  choices = c("Miles per gallon" = "mpg","Displacement" = "disp","Gross horsepower" = "hp", "Rear axle ratio" ="drat", "Weight (1000 lbs)"= "wt","1/2 mile time" = "qsec"),
                  selected = "mpg"),
      selectInput(inputId = "y",
                  label = "Y-axis",
                  choices = c("Miles per gallon" = "mpg","Displacement" = "disp","Gross horsepower" = "hp", "Rear axle ratio" ="drat", "Weight (1000 lbs)"= "wt","1/2 mile time" = "qsec"), #give them reasonable names
                  selected = "disp"),
      selectInput(inputId = "col",
                  label = "Colour",
                  choices = c("Number of cylinders" = "cyl", "Engine" = "vs", "Transmission"="am", "Number of forward gears" = "gear", "Number of carburators" = "carb"),
                  selected = "cyl"),
      sliderInput(inputId = "alpha",
                  label = "Alpha:",
                  min = 0, max = 1,
                  value = 0.5),
      numericInput(inputId = "size",
                   label = "Dot size:",
                   value = 3,
                   min = 1, max = 9),
      textInput(inputId ="title",
                label = "Plot title:",
                placeholder = "Enter the title"),
      checkboxInput(inputId = "show_d",
                    label = "Show data?",
                    value = FALSE),
      checkboxInput(inputId = "show_s",
                    label = "Show summary?",
                    value = FALSE),
      submitButton("Apply")
    ),
    mainPanel(
      
      plotOutput(outputId = "scatter"),
      
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


server <- function(input, output){
  output$scatter <- renderPlot({
    req(input$size) #app won't work without this argument
    ggplot(data, aes_string(x = input$x, y = input$y, color=input$col))+
        geom_point(alpha = input$alpha, size = input$size)+
        ggtitle(tools::toTitleCase(isolate(input$title))) # we added isolate so the whole data won't render if we only change title
  })

  new_data <- reactive({
    data %>% 
      select_(input$x, input$y, input$col)
  })
  
  output$data_selected <- renderTable({
    if (input$show_d) {
      new_data()
    }
  })
  
  output$summary <- renderTable({
    if (input$show_s) {
        new_data() %>% 
          group_by_(input$col) %>%
          summarise_all(funs(mean,sd))
    }
  })
}

shinyApp(ui = ui, server = server)


