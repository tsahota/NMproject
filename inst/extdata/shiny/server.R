library(shiny)

gen_run_table <- function(){
  orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
  run_table()
}

function(input, output, session) {
  session$onSessionEnded(function() stopApp())

  ## run table
  new_table <- eventReactive(input$refresh_db,gen_run_table(),ignoreNULL = FALSE)
  output$run_table <- DT::renderDataTable({
    new_table()
  },selection = "single", server = FALSE,
  options = list(paging=FALSE,
                 searching=FALSE,
                 filtering=FALSE,
                 ordering=FALSE))

  object <- eventReactive(
    list(input$run_table_rows_selected,
         input$refresh_status,
         input$refresh_plot),{
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    row <- input$run_table_rows_selected
    print("refreshing object from db")
    nmdb_extract(new_table()$entry[row])
  })

  observeEvent(input$run_table_rows_selected, {
    updateTabsetPanel(session, "mainPanel", selected = "run monitor")
  })

  ## run monitor
  output$status <- renderTable({
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    status(object())
  })

  output$distPlot <- renderPlot({
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    plot_iter(object(),trans = input$trans, skip = input$skip)
  })
}

