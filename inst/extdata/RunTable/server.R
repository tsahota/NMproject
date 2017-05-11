library(shiny)

function(input, output, session) {
  session$onSessionEnded(function() stopApp())
  #autoInvalidate <- reactiveTimer(20000, session)
  output$run_table <- renderTable({
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
  #  autoInvalidate()
    run_table()
  })
}

