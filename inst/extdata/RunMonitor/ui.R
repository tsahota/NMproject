library(shiny)

fluidPage(
  titlePanel("Run Monitor"),   # Application title
  checkboxInput("trans","Transform parameters",TRUE),
  plotOutput("distPlot")
)
