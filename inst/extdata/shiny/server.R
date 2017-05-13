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
  }, server = FALSE, rownames = FALSE,
  options = list(paging=TRUE,
                 searching=TRUE,
                 filtering=TRUE,
                 ordering=TRUE))

  objects <- eventReactive(
    list(input$go_to_monitor,
         input$go_to_results),{
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    row <- input$run_table_rows_selected
    lapply(new_table()$entry[row],extract_nm)
  })

  observe({
    output$selected_runs <- renderText({
      pretext <- "Select run(s):"
      if(length(input$run_table_rows_selected)==0) entries <- "None" else
        entries <- isolate(new_table()$entry[input$run_table_rows_selected])
      paste(pretext,paste(entries,collapse = ","))
    })
  })

  observeEvent(input$go_to_monitor, {
    updateNavbarPage(session, "mainPanel", selected = "monitor")
  })

  observeEvent(input$go_to_results, {
    updateNavbarPage(session, "mainPanel", selected = "results")
  })

  ## run monitor

  status_ob <- eventReactive(
    list(input$refresh_status,
         input$go_to_monitor),{
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    object <- objects()[[1]]
    status(object)
  })

  output$status <- renderTable({
    status_ob()
  })

  plot_iter_ob <- eventReactive(
    list(input$refresh_plot,
         input$go_to_monitor),{
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    object <- objects()[[1]]
    if(file.exists(object$output$psn.ext))
      plot_iter(object,trans = input$trans, skip = input$skip)
  })

  output$distPlot <- renderPlot({
    plot_iter_ob()
  })

  run_record_ob <- eventReactive(input$go_to_results,{
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    do.call(run_record,objects())
  })

  output$run_record <- renderTable({
    data.frame(A=1,B=1)
    run_record_ob()
  })

}

