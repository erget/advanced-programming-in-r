# function(input, output){
#   output$SampleHist <- renderPlot({
#     if(input$DistChoice == "Normal"){
#       data <- input$SSize %>% rnorm 
#     }else{
#       data <- input$SSize %>% runif(-1, 1)
#     }
#     qplot(data, geom = "histogram"
#           , main = paste(input$DistChoice,"data"))
#   })
#   output$HistSmry <- renderPrint({
#     if(input$DistChoice == "Normal"){
#       input$SSize %>% rnorm %>% summary
#     }else{
#       input$SSize %>% runif(-1,1) %>% summary
#     }
#   })
# }

## Problem: in above Code: data used for graph and summary are not the same!
## Reason: renderPlot and renderPrint, being reactive, rerun what is in {}
##         independent of each other
## Solution: Produce data once, make it reactive() and pass this data to the render fcns
##           If data is called by the second render fcn it will not update again as input hasn't changed
##           --> Modularization

function(input, output){
  
  data <- reactive({
      if(input$DistChoice == "Normal"){
        input$SSize %>% rnorm
      }else{
        input$SSize %>% runif(-1, 1)
      }
  })
  
  output$SampleHist <- renderPlot({
    qplot(data(), geom = "histogram"
          , main = paste(input$DistChoice,"data"))
  })
  output$HistSmry <- renderPrint({
    summary(data())
  })
}

# data is actually a (reactive) fcn. Therefore must be called with (), data()