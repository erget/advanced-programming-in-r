library(shiny)
library(ggplot2)
library(shinydashboard)

# Easy control over the layout (in ui) with shinydashboard
ui <- dashboardPage(
        dashboardHeader(title = "My Dashboard"),
        dashboardSidebar(
          sliderInput(inputId = "SSize",
                      label = "Sample size",
                      value = 100, min = 1, max = 10000),
          radioButtons(inputId = "DistChoice",
                       label = "Choose sampling distribution",
                       choices = c("Uniform", "Normal"),
                       selected = "Uniform", inline = T)
        ),
        dashboardBody(
          plotOutput("SampleHist")
        )
      )

server <- function(input, output){
  output$SampleHist <- renderPlot({
    if(input$DistChoice == "Normal"){
      data <- input$SSize %>% rnorm 
    }else{
      data <- input$SSize %>% runif(-1, 1)
    }
    qplot(data, geom = "histogram"
          , main = paste(input$DistChoice,"data"))
  })
}

shinyApp(ui = ui, server = server)