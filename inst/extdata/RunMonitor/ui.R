library(shiny)

shinyUI(fluidPage(
  titlePanel("Run Monitor"),   # Application title
  plotOutput("distPlot")
))
