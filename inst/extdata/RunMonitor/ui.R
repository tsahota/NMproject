library(shiny)

fluidPage(
  titlePanel("Run Monitor"),   # Application title
  h3("status"),
  tableOutput("status"),
  h3("trace"),
  checkboxInput("trans","Transform parameters",TRUE),
  plotOutput("distPlot")
)
