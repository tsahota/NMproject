library(shiny)

fluidPage(
  titlePanel("Run database"),   # Application title
  actionButton("refresh_button","refresh"),
  br(),br(),
  dataTableOutput("run_table")
)
