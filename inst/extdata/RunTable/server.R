library(shiny)

gen_run_table <- function(){
  orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
  run_table()
}

function(input, output, session) {
  session$onSessionEnded(function() stopApp())
  new_table <- eventReactive(input$refresh_button,gen_run_table(),ignoreNULL = FALSE)
  output$run_table <- renderDataTable({
    new_table()
  })
}

