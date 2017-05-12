library(shiny)

function(input, output, session) {
  session$onSessionEnded(function() stopApp())

  status_ob <- eventReactive(input$refresh_button,{
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    print("doing status")
    status(.object)
  },ignoreNULL = FALSE)

  output$status <- renderTable({
    status_ob()
  })

  plot_iter_ob <- eventReactive(input$refresh_plot,{
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    print("doing plot")
    plot_iter(.object,trans = input$trans, skip = input$skip)
  },ignoreNULL = FALSE)

  output$distPlot <- renderPlot({
    plot_iter_ob()
  })
}

