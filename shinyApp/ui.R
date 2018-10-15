dashboardPage(
  dashboardHeader(title = "My analysis dashboard"),
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
    plotOutput("SampleHist"),
    verbatimTextOutput("HistSmry")
  )
)