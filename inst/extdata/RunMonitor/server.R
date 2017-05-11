library(shiny)

function(input, output, session) {
  session$onSessionEnded(function() stopApp())
  #autoInvalidate <- reactiveTimer(20000, session)
  output$status <- renderTable({
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    status(.object)
  })
  output$distPlot <- renderPlot({
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    #autoInvalidate()
    plot_iter(.object,trans = input$trans)
  })
}

