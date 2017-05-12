library(shiny)

gen_run_table <- function(){
  orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
  run_table()
}

function(input, output, session) {
  session$onSessionEnded(function() stopApp())

  ## run table
  new_table <- eventReactive(input$refresh_button,gen_run_table(),ignoreNULL = FALSE)
  output$run_table <- DT::renderDataTable({
    new_table()
  },selection = "single", server = TRUE,
  options = list(paging=FALSE,
                 searching=FALSE,
                 filtering=FALSE,
                 ordering=FALSE))

  object <- reactive({
    input$run_table_rows_selected
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    row <- input$run_table_rows_selected
    print(row)
    print("er executing")
    nmdb_extract(new_table()$entry[row])
  })

  observeEvent(input$run_table_rows_selected, {
    print("oe executing")
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    row <- input$run_table_rows_selected
    print(row)
    print("er executing")
    object <- nmdb_extract(new_table()$entry[row])
    output$status <- renderTable({
      orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
      print("doing status in oe")
      status(object)
    })
    output$distPlot <- renderPlot({
      orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
      print("doing plot in oe")
      plot_iter(object,trans = input$trans, skip = input$skip)
    })
    updateTabsetPanel(session, "mainPanel", selected = "run monitor")
  })

  ### run monitor
  status_ob <- eventReactive(input$refresh_button,{
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    print("doing status")
    status(object())
  },ignoreNULL = FALSE)

  output$status <- renderTable({
    status_ob()
  })

  plot_iter_ob <- eventReactive(input$refresh_plot,{
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    print("doing plot")
    plot_iter(object(),trans = input$trans, skip = input$skip)
  },ignoreNULL = FALSE)

  output$distPlot <- renderPlot({
    plot_iter_ob()
  })
}

