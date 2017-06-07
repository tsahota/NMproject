library(shiny)
library(plotly)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)

options(warn =-1)

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
    if(length(input$run_table_rows_selected)==0)
      return(showModal(modalDialog(
        title = "Error",
        "Select a run first"
      )))
    updateNavbarPage(session, "mainPanel", selected = "monitor")
  })

  observeEvent(input$go_to_results, {
    if(length(input$run_table_rows_selected)==0)
      return(showModal(modalDialog(
        title = "Error",
        "Select run(s) first"
      )))
    updateNavbarPage(session, "mainPanel", selected = "results")
  })

  observeEvent(input$back_to_db, {
    updateNavbarPage(session, "mainPanel", selected = "database")
  })

  observeEvent(input$back_to_db2, {
    updateNavbarPage(session, "mainPanel", selected = "database")
  })
  
  observeEvent(input$run_job, {
    if(length(input$run_table_rows_selected)==0)
      return(showModal(modalDialog(
        title = "Error",
        "Select run(s) first"
      )))
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    res <- try(do.call(run,c(objects(),overwrite=TRUE,wait=FALSE)),silent = TRUE)
    if(inherits(res,"try-error"))
      showModal(modalDialog(
        title = "Error from run()",
        res
      ))
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
           status(object)
         })

  output$status <- renderText({
    status_ob()
  })
  
  # tail_ob <- eventReactive(
  #   list(input$refresh_status,
  #        input$run_table_rows_selected,
  #        plot_iter_ob),{
  #          orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
  #          object <- objects()[[1]]
  #          tail_lst(object)
  #        })
  
  tail_ob <- reactivePoll(1000, session,
                          # This function returns the time that log_file was last modified
                          checkFunc = function(){
                            if(length(input$run_table_rows_selected)==0)
                              return("")
                            orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
                            object <- isolate(objects())[[1]]
                            tail_lst(object)
                          },
                          # This function returns the content of log_file
                          valueFunc = function() {
                            orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
                            object <- isolate(objects())[[1]]
                            tail_lst(object)
                          }
  )
  
  output$tail_lst <- renderUI({
    tail_ob <- tail_ob()
    tail_ob <- do.call(paste,c(as.list(tail_ob),sep='<br/>'))
    HTML(tail_ob)
  })

  plot_iter_ob <- eventReactive(
    list(input$refresh_plot,
         input$run_table_rows_selected),{
           orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
           object <- objects()[[1]]
           if(is.null(object$output$psn.ext)) return(plotly_empty())
           if(!file.exists(object$output$psn.ext)) return(plotly_empty())
           if(file.exists(object$output$psn.ext)){
             tryCatch({
               gg <- plot_iter(object,trans = input$trans, skip = input$skip)
             }, error = function(e){
               return(plotly_empty())
             })
             l <- ggplotly(gg)
             l$x$layout$width <- NULL
             l$x$layout$height <- NULL
             l$width <- NULL
             l$height <- NULL
             l
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
           tryCatch(do.call(run_record,objects()),
                    error=function(e){
                      data.frame()
                    })
         })

  output$run_record <- DT::renderDataTable({
    run_record_ob()
  },rownames = FALSE,options = list(paging=FALSE,
                                    searching=FALSE,
                                    filtering=FALSE,
                                    ordering=TRUE))

}

