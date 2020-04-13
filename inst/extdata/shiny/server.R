library(shiny)
library(dygraphs)

.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)
.m <- get(".m", envir = NMproject:::.sso_env)

setwd(.currentwd)

options(warn =-1)

gen_run_table <- function(){
  d <- nm_row(.m)
  d$target <- NULL
  d$run_dir <- NULL
  d$results_dir <- NULL
  d$unique_id <- NULL
  d$output_location <- NULL
  d
}

get_plot_bootstrapjs_div <- function(plot_object_list) {
  #### local_function
  get_col_div <- function(plot_object_list, index, css_class = 'col-xs-12 col-sm-4')  {
    col_div <- div(class = css_class)

    if(length(plot_object_list) >= index) {
      plotname <- paste("plot", index, sep="")
      plot_output_object <- dygraphOutput(plotname)
      col_div <- tagAppendChild(col_div, plot_output_object)
    }
    return(col_div)
  }
  #
  get_plot_div <- function(plot_object_list) {
    result_div <- div(class = 'container-fluid')

    suppressWarnings(for(i in seq(1,length(plot_object_list),3)) {
      row_div <- div(class = 'row')
      row_div <- tagAppendChild(row_div, get_col_div(plot_object_list, i))
      row_div <- tagAppendChild(row_div, get_col_div(plot_object_list, i+1))
      row_div <- tagAppendChild(row_div, get_col_div(plot_object_list, i+2))
      result_div <- tagAppendChild(result_div, row_div)
    })
    return(result_div)
  }
  ####
  plot_output_list_div <- get_plot_div(plot_object_list)

  return(plot_output_list_div)
}

function(input, output, session) {
  session$onSessionEnded(stopApp)
  ## run table
  new_table <- eventReactive(input$refresh_db,
                             gen_run_table(),
                             ignoreNULL = FALSE)
  
  output$run_table <- DT::renderDataTable({
    new_table()
  }, server = FALSE, rownames = FALSE, options = list(paging=TRUE,
                                                      searching=TRUE,
                                                      filtering=TRUE,
                                                      ordering=TRUE)
  )
  
  objects <- eventReactive(input$run_table_rows_selected,{
    #row <- input$run_table_rows_selected
    .m[input$run_table_rows_selected]
    #lapply(new_table()$entry[row],function(...)extract_nm(...,.db_name))
  })
  # 
  output$runs_selected_info <- renderTable({
    if(length(input$run_table_rows_selected)==0) return(data.frame())
    new_table()[input$run_table_rows_selected,c("run_id","type","ctl_name")]
  })
  output$runs_selected_info2 <- renderTable({
    if(length(input$run_table_rows_selected)==0) return(data.frame())
    new_table()[input$run_table_rows_selected,c("run_id","type","ctl_name")]
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

  # ## run monitor
  # 
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
         plot_iter_ob
         ),
    {
      object <- objects()[[1]]
      status(object)
    }
  )
  
  output$status <- renderText({
    status_ob()
  })

  tail_ob <- reactivePoll(1000, session,
                          # This function returns the time that log_file was last modified
                          checkFunc = function(){
                            if(length(input$run_table_rows_selected)==0) return("")
                            object <- isolate(objects())[[1]]
                            tail_lst(object)
                          },
                          # This function returns the content of log_file
                          valueFunc = function() {
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
           object <- objects()[[1]]
           psn_ext_path <- object %>% nm_output_path("ext")
           #if(is.null(object$output$psn.ext)) return(dygraph(data.frame(x=0,y=0)))
           if(!file.exists(psn_ext_path)) return(dygraph(data.frame(x=0,y=0)))
           d <- try(plot_iter_data(object,trans = input$trans, skip = 0),silent=TRUE)
           if(inherits(d,"try-error")) return(dygraph(data.frame(x=0,y=0)))
           p <- list()
           for(i in seq_along(unique(d$variable))){
             var_name <- unique(d$variable)[i]
             dt <- d[d$variable %in% var_name,c("ITERATION","value")]
             p[[i]] <- dygraph(dt,main=var_name,xlab="Iteration",group = "hi") %>%
               dyOptions(drawPoints = TRUE, pointSize = 2, sigFigs=3) %>%
               dyRangeSelector()
           }
           p
         }
  )
  # 
  output$distPlot <- renderUI({
    get_plot_bootstrapjs_div(plot_iter_ob())
  })

  observe(
    for (i in 1:50) {
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        output[[plotname]] <- renderDygraph({
          plot_iter_ob()[[my_i]]
        })
      })
    }
  )
  # 
  # ## run record
  run_record_ob <- eventReactive(
    list(input$refresh_run_record,
         input$run_table_rows_selected),{
           #orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
           tryCatch(#do.call(run_record,objects()),
             rr(objects()),
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

