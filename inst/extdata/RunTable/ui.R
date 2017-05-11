library(shiny)

shinyUI(fluidPage(
  titlePanel("Run Table"),   # Application title
  tableOutput("run_table")
))
