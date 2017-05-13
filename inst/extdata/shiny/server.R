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

  object <- eventReactive(input$go_to_monitor,{
    if(length(input$run_table_rows_selected)==1){
      orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
      row <- input$run_table_rows_selected
      extract_nm(new_table()$entry[row])
    } else object()
  })

  observeEvent(input$go_to_monitor, {
    if(length(input$run_table_rows_selected)==0) showModal(modalDialog(
      title = "Important message",
      "Select a run first"
    ))
    if(length(input$run_table_rows_selected)>0) showModal(modalDialog(
      title = "Important message",
      "Select only one run"
    ))
    if(length(input$run_table_rows_selected)==1)
      updateNavbarPage(session, "mainPanel", selected = "run monitor")
  })

  observeEvent(input$go_to_results, {
    updateNavbarPage(session, "mainPanel", selected = "run results")
  })

  ## run monitor

  status_ob <- eventReactive(
    list(input$refresh_status,
         input$run_table_rows_selected),{
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    status(object())
  })

  output$status <- renderTable({
    status_ob()
  })

  plot_iter_ob <- eventReactive(
    list(input$refresh_plot,
         input$run_table_rows_selected),{
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    if(file.exists(object()$output$psn.ext))
      plot_iter(object(),trans = input$trans, skip = input$skip)
  })

  output$distPlot <- renderPlot({
    plot_iter_ob()
  })
}

