library(shiny)
library(plotly)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)

gen_run_table <- function(){
  orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
  run_table()
}


function(input, output, session) {
  session$onSessionEnded(stopApp)

  ## run table
  new_table <- eventReactive(input$refresh_db,gen_run_table(),ignoreNULL = FALSE)
  output$run_table <- DT::renderDataTable({
    new_table()
  }, server = FALSE, rownames = FALSE,
  options = list(paging=TRUE,
                 searching=TRUE,
                 filtering=TRUE,
                 ordering=TRUE))

  objects <- eventReactive(input$run_table_rows_selected,{
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    row <- input$run_table_rows_selected
    lapply(new_table()$entry[row],extract_nm)
  })

  output$runs_selected_info <- renderTable({
    if(length(input$run_table_rows_selected)==0) return(data.frame())
    new_table()[input$run_table_rows_selected,c("entry","type","ctl")]
  })
  output$runs_selected_info2 <- renderTable({
    if(length(input$run_table_rows_selected)==0) return(data.frame())
    new_table()[input$run_table_rows_selected,c("entry","type","ctl")]
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

  observeEvent(input$back_to_db, {
    updateNavbarPage(session, "mainPanel", selected = "database")
  })

  observeEvent(input$back_to_db2, {
    updateNavbarPage(session, "mainPanel", selected = "database")
  })

  ## run monitor

  output$monitor_select_warning <- renderText({
    if(length(input$run_table_rows_selected)>1) return("Warning: will only monitor first")
    if(length(input$run_table_rows_selected)==0) return("Warning: no runs selected")
  })

  output$results_select_warning <- renderText({
    if(length(input$run_table_rows_selected)==0) return("Warning: no runs selected")
  })

  status_ob <- eventReactive(
    list(input$refresh_status,
         input$run_table_rows_selected,
         plot_iter_ob),{
           orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
           object <- objects()[[1]]
           st <- status(object)
           st[,c("TEST","RESULT","COMMENT")]
         })

  output$status <- renderTable({
    status_ob()
  })

  plot_iter_ob <- eventReactive(
    list(input$refresh_plot,
         input$run_table_rows_selected),{
           orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
           object <- objects()[[1]]
           if(file.exists(object$output$psn.ext)){
             ggplotly(plot_iter(object,trans = input$trans, skip = input$skip))
             #plot_iter(object,trans = input$trans, skip = input$skip)
           }
         })

  #output$distPlot <- renderPlot({
  output$distPlot <- renderPlotly({
    plot_iter_ob()
  })

  ## run record
  run_record_ob <- eventReactive(
    list(input$refresh_run_record,
         input$run_table_rows_selected),{
           orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
           do.call(run_record,objects())
         })

  output$run_record <- DT::renderDataTable({
    run_record_ob()
  },rownames = FALSE,options = list(paging=FALSE,
                                    searching=FALSE,
                                    filtering=FALSE,
                                    ordering=TRUE))

}

