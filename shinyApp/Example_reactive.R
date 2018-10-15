#######################
## WITHOUT REACTIVE() #
#######################

server <- function(input, output){
  output$SampleHist <- renderPlot({
    hist(rnorm(input$SSize))
  })
  output$MaxVal <- renderText({
    max(rnorm(input$SSize))
  })
}

ui <- fluidPage(
  sliderInput(inputId = "SSize",
              label = "Sample size",
              value = 100, min = 1, max = 1000),
  plotOutput("SampleHist"),
  h4("Maximum value equals:"),
  h1(textOutput("MaxVal"))
  
)

shinyApp(ui = ui, server = server)



#######################
## WITH REACTIVE() ####
#######################

# COMMENT ABOVE CODE OUT AND UNCOMMENT THE BELOW CODE... 
# server <- function(input, output){
#   
#   MySample <- reactive(rnorm(input$SSize))
#   
#   output$SampleHist <- renderPlot({
#     hist(MySample())
#   })
#   output$MaxVal <- renderText({
#     max(MySample())
#   })
# }
# 
# ui <- fluidPage(
#   sliderInput(inputId = "SSize",
#               label = "Sample size",
#               value = 100, min = 1, max = 1000),
#   plotOutput("SampleHist"),
#   h4("Maximum value equals:"),
#   h1(textOutput("MaxVal"))
#   
# )
# 
# shinyApp(ui = ui, server = server)