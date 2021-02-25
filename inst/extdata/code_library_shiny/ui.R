library(shiny)
library(DT)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)

setwd(.currentwd)

fluidPage(
  
  titlePanel("code library"), 
  mainPanel(DT::dataTableOutput("code_library_table"))
  
  
)
