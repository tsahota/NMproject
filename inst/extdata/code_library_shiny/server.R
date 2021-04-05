library(shiny)
library(DT)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)

setwd(.currentwd)

function(input, output, session) {
  
  output$code_library_table <- DT::renderDataTable(
    code_library(return_info = TRUE, viewer = FALSE)
  )
  
}

