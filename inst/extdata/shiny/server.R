library(shiny)
library(dygraphs)
library(ggplot2)
.currentwd <- get(".currentwd", envir = NMproject:::.sso_env)

options(warn = -1)

gen_run_table <- function(){
  orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
  run_table()
}

get_plot_bootstrapjs_div <- function(plot_object_list) {
  #### local_function
  get_col_div <- function(plot_object_list, index, css_class = 'col-xs-12 col-sm-4')  {
    col_div <- div(class = css_class)

    if (length(plot_object_list) >= index) {
      plotname <- paste("plot", index, sep = "")
      plot_output_object <- dygraphOutput(plotname)
      col_div <- tagAppendChild(col_div, plot_output_object)
    }
    return(col_div)
  }
  #
  get_plot_div <- function(plot_object_list) {
    result_div <- div(class = 'container-fluid')#

    suppressWarnings(for (i in seq(1,length(plot_object_list),3)) {
      row_div <- div(class = 'row')
      row_div <- tagAppendChild(row_div, get_col_div(plot_object_list, i))
      row_div <- tagAppendChild(row_div, get_col_div(plot_object_list, i + 1))
      row_div <- tagAppendChild(row_div, get_col_div(plot_object_list, i + 2))
      result_div <- tagAppendChild(result_div, row_div)
    })
    return(result_div)
  }
  ####
  plot_output_list_div <- get_plot_div(plot_object_list)

  return(plot_output_list_div)
}

#get_plot_bootstrapjs_div2 <- function(plot_object_list) {
  #### local_function
#  get_col_div2 <- function(plot_object_list, index2, css_class = 'col-xs-12 col-sm-4')  {
#    col_div2 <- div(class = css_class)#
#
#    if (length(plot_object_list) >= index2) {
#      plotname2 <- paste("plot2", index2, sep = "")
#      plot_output_object2 <- plotOutput(plotname2)
#      col_div2 <- tagAppendChild(col_div2, plot_output_object2)
#    }
#    return(col_div2)
#  }
#
#  get_plot_div2 <- function(plot_object_list) {
#    result_div2 <- div(class = 'container-fluid')##
#
#    suppressWarnings(for (j in seq(1,length(plot_object_list),2)) {
#      row_div2 <- div(class = 'row')
#      row_div2 <- tagAppendChild(row_div2, get_col_div2(plot_object_list, j))
#      row_div2 <- tagAppendChild(row_div2, get_col_div2(plot_object_list, j + 1))
#    
#      result_div2 <- tagAppendChild(result_div2, row_div2)
##    })
#    return(result_div2)
#  }
#  ####
#  plot_output_list_div2 <- get_plot_div2(plot_object_list)#
#
#  return(plot_output_list_div2)
#}


multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols)    # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i - 1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}
plot_gof_obsvspred <- function(r, trans = TRUE, skip = 0, yvar = "OBJ"){
  # Checking if package present in project
  if (!requireNamespace("reshape2", quietly = TRUE))
    stop("reshape2 needed for this function to work. Please install it.", call. = FALSE)
  if (!requireNamespace("grid", quietly = TRUE))
    stop("grid needed for this function to work. Please install it.", call. = FALSE)

    ###function for gof
  sdtab <- read.table(r$output$ctl_out_files[1],skip = 1, header = TRUE)  # Read table wita data
  max1 <- max(sdtab$IPRED,sdtab$PRED,sdtab$DV)                            #Set up equal axes for all plots DV, PRED, IPRED
  
  ### DV vs IPRED 
  sdtab$TITLE <- c("IPRED vs. DV")                                        # Setup title
  DV_IPRED <-  ggplot(data = sdtab,aes(x = DV, y = IPRED)) +
    geom_point(alpha = 3/8) +
    theme_minimal() +
    facet_wrap(~TITLE) +
    geom_abline(col = "blue") +     
    ylab("") +
    ylim(0,max1) +
    xlim(0,max1) +
    theme(plot.title = element_text(size = 22, face = "bold"), strip.background = element_rect( fill = "gray"),
          axis.line = element_line(size = 0.5, colour = "black"), axis.ticks = element_line(size = 1),
          axis.text = element_text(colour = "black")) +
    theme_bw(base_size = 11) 
  
  ### DV vs PRED  
  sdtab$TITLE <- c("PRED vs. DV")                                         # Setup title  
  DV_PRED <- ggplot(data = sdtab,aes(x = DV, y = PRED)) +
    geom_point(alpha = 3/8) +
    theme_minimal() +
    geom_abline(col = "blue") +
    ylab("") +
    ylim(0,max1) +
    xlim(0,max1) +
    facet_wrap(~TITLE) +
    theme(plot.title = element_text(size = 22, face = "bold"), strip.background = element_rect( fill = "gray"),
          axis.line = element_line(size = 0.5, colour = "black"), axis.ticks = element_line(size = 1),
          axis.text = element_text(colour = "black")) +
    theme_bw(base_size = 11) 
    #Plot both   
    multiplot(DV_IPRED,DV_PRED, cols = 2)
}

plot_gof_resplot <- function(r,output,trans=TRUE, skip = 0){
  # Checking if package present in project
  if (!requireNamespace("reshape2", quietly = TRUE))
    stop("reshape2 needed for this function to work. Please install it.", call. = FALSE)
  if (!requireNamespace("grid", quietly = TRUE))
    stop("grid needed for this function to work. Please install it.", call. = FALSE)
  sdtab <- read.table(r$output$ctl_out_files[1], skip = 1, header = TRUE)  # Read table wita data
  for (i in 1:4) {
    local({
      ii <- i  # need i evaluated here
      ## These would be your 10 plots instead
      output[[sprintf('%s_%g', "plot", ii)]] <- renderPlot({
        rmdplot(ii, sdtab)  
      })
    })
  }
}
  



  ###function for gof
  rmdplot <- function(ii, sdtab) {
    if ( ii == 1) {
  ### WRES vs TIME and PRED
#  maximal     <- )                                      # Set up equal axes
  sdtab$TITLE <- c("WRES vs. TIME")                                        # Setup title  
  
    ggplot() + geom_point(data = sdtab, aes(x = TIME, y = WRES), alpha = 3/8) +
    theme_minimal() +
    geom_hline( yintercept = 0, col = "blue")  +
    ylim(-max(abs(sdtab$WRES)),max(abs(sdtab$WRES))) +
    ylab("") +
    facet_wrap(~TITLE) +
    theme(plot.title = element_text(size = 22, face = "bold"), strip.background = element_rect( fill = "gray") ,
          axis.line = element_line(size = 0.5, colour = "black"),axis.ticks = element_line(size = 1) ,
          axis.text = element_text(colour = "black")) +
    theme_bw(base_size = 11) 
  
  }else
   if( ii == 2){
     maximal     <- max(abs(sdtab$WRES))                                      # Set up equal axes
  sdtab$TITLE <- c("WRES vs. PRED")                                        # Setup title  
  ggplot() + geom_point(data = sdtab,aes(x = PRED, y = WRES),alpha = 3/8) +
    
    theme_minimal() +
    geom_hline( yintercept = 0, col = "blue") +
    ylim(-maximal,maximal) +
    ylab("") +
    facet_wrap(~TITLE)+
    theme(plot.title = element_text(size = 22, face = "bold"), strip.background = element_rect( fill = "gray"),
          axis.line = element_line(size = 0.5, colour = "black"),axis.ticks = element_line(size = 1),
          axis.text = element_text(colour = "black")) +
    theme_bw(base_size = 11) 
  
   }else
  
  
  if(ii == 3){
   ### CWRES vs TIME and PRED   
  maximal     <- max(abs(sdtab$CWRES))                                      # Set up equal axes
  sdtab$TITLE <- c("CWRES vs. TIME")                                        # Setup title  
  
  ggplot() + geom_point(data = sdtab,aes(x = TIME, y = CWRES),alpha = 3/8) +
    
    theme_minimal() +
    geom_hline( yintercept = 0, col = "blue")  +
    ylim(-maximal,maximal) +
    ylab("") +
    facet_wrap(~TITLE) +
    theme(plot.title = element_text(size = 22, face = "bold"),strip.background = element_rect( fill = "gray"),
          axis.line = element_line(size = 0.5, colour = "black"),axis.ticks = element_line(size = 1),
          axis.text = element_text(colour = "black")) +
    theme_bw(base_size = 11) 
 
  }else
  
  if( ii == 4) {
    maximal     <- max(abs(sdtab$CWRES)) 
  sdtab$TITLE <- c("CWRES vs. PRED")                                        # Setup title  
  ggplot() + geom_point(data = sdtab, aes(x = PRED, y = CWRES),alpha = 3/8)+
   
    theme_minimal() +
    geom_hline( yintercept = 0, col = "blue") +
    ylim(-maximal,maximal) +
    ylab("") +
    facet_wrap(~TITLE) +
    theme(plot.title = element_text(size = 22,face = "bold"),strip.background = element_rect( fill = "gray"),
          axis.line = element_line(size = 0.5, colour = "black"),axis.ticks = element_line(size = 1),
          axis.text = element_text(colour = "black")) +
    theme_bw(base_size = 11) 
  #multiplot(WRES_TIME,WRES_PRED,CWRES_TIME,CWRES_PRED, cols=2)
  }
  }
  #p <- list(WRES_TIME,WRES_PRED,CWRES_TIME,CWRES_PRED)

makePlotContainers <- function(n=4,ncol=2, prefix="plot", height=200, width="100%", ...) {
  ## Validate inputs
  validateCssUnit(width)
  validateCssUnit(height)
  
  ## Construct plotOutputs
  lst <- lapply(seq.int(n), function(i)
    plotOutput(sprintf('%s_%g', prefix, i), height=height, width=width))
  
  ## Make columns
  lst <- lapply(split(lst, (seq.int(n)-1)%/%ncol), function(x) column(12/ncol, x))
  do.call(tagList, lst)
}


### Calculate individual numbers
ind_number_fun <- function(r,...){
  sdtab <- read.table(r$output$ctl_out_files[1], skip = 1, header = TRUE)  # Read table wita data
  ind_n <- length(unique(sdtab$ID))
  return(ind_n)
}


ind_number <- eventReactive(
  list(input$refresh_plot,
       input$run_table_rows_selected),{
         orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
         object <- objects()[[1]]
         print(object$output$ctl_out_files[1])
         print(!file.exists(object$output$ctl_out_files[1]))
         return(ind_number_fun(object))
      })


makePlotContainers_ind <- function(n=ind_number,ncol = 5, prefix="plot2", height=100, width="100%", ...) {
  ## Validate inputs
  validateCssUnit(width)
  validateCssUnit(height)
  
  ## Construct plotOutputs
  lst <- lapply(seq.int(n), function(i)
    plotOutput(sprintf('%s_%g', prefix, i), height=height, width=width))
  
  ## Make columns
  lst <- lapply(split(lst, (seq.int(n)-1)%/%ncol), function(x) column(12/ncol, x))
  do.call(tagList, lst)
}

plot_gof_indplot <- function(r,output,trans=TRUE, skip = 0){
  # Checking if package present in project
  if (!requireNamespace("reshape2", quietly = TRUE))
    stop("reshape2 needed for this function to work. Please install it.", call. = FALSE)
  if (!requireNamespace("grid", quietly = TRUE))
    stop("grid needed for this function to work. Please install it.", call. = FALSE)
  sdtab <- read.table(r$output$ctl_out_files[1], skip = 1, header = TRUE)  # Read table wita data
  ID_list <- unique(sdtab$ID)
  for (i in 1:length(unique(sdtab$ID))) {
    local({
      ii <- i  # need i evaluated here
      ## These would be your 10 plots instead
      
      output[[sprintf('%s_%g', "plot2", ii)]] <- renderPlot({
        indplot(ii,sdtab,ID_list)  
      })
    })
  }
}

indplot <- function (ii, sdtab,ID_list) {
  data_plot <-  subset(sdtab, ID == ID_list[ii])
  data_plot$TITLE <- c(paste("ID",ID_list[ii]))
  ggplot(data = data_plot ) + geom_point(aes(x = TIME, y = DV),alpha = 3/8, color="blue")+
    geom_line(aes(x = TIME, y = PRED), color = "red")+
    geom_line(aes(x = TIME, y = IPRED), color="green")+
    ylab("Concentration")+
    theme(plot.title = element_text(size=22,face="bold"),strip.background = element_rect( fill = "gray"),
          axis.line = element_line(size = 0.5, colour = "black"),axis.ticks = element_line(size = 1),
          axis.text = element_text(colour = "black"))+
    theme_minimal()+
    facet_wrap(~TITLE)+
    xlab("Time")
  
}

#### Main part ####
function(input, output, session) {
  session$onSessionEnded(stopApp)
  ## run table
  new_table <- eventReactive(input$refresh_db,gen_run_table(),ignoreNULL = FALSE)
  output$run_table <- DT::renderDataTable({
    new_table()
  }, server = FALSE, rownames = FALSE,
                       options = list(paging    = TRUE,
                          searching = TRUE,
                          filtering = TRUE,
                          ordering  = TRUE))
  # Select model
  objects <- eventReactive(input$run_table_rows_selected,{
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir)) # Check and setup directory
    row <- input$run_table_rows_selected
    lapply(new_table()$entry[row], extract_nm)                         # Extract list with files in directory
  })

  
  output$runs_selected_info  <- renderTable({
    if (length(input$run_table_rows_selected) == 0) return(data.frame())
    new_table()[input$run_table_rows_selected,c("entry","type","ctl")]
  })
  output$runs_selected_info2 <- renderTable({
    if (length(input$run_table_rows_selected) == 0) return(data.frame())
    new_table()[input$run_table_rows_selected,c("entry","type","ctl")]
  })
  output$runs_selected_info3 <- renderTable({
    if (length(input$run_table_rows_selected) == 0) return(data.frame())
    new_table()[input$run_table_rows_selected,c("entry","type","ctl")]
  })

  observe({
    output$selected_runs <- renderText({
      pretext <- "Select run(s):"
      if (length(input$run_table_rows_selected) == 0) entries <- "None" else
        entries <- isolate(new_table()$entry[input$run_table_rows_selected])
      paste(pretext,paste(entries,collapse = ","))
    })
  })

  observeEvent(input$go_to_monitor, {
    if (length(input$run_table_rows_selected) == 0)
      return(showModal(modalDialog(
        title = "Error",
        "Select a run first"
      )))
    updateTabItems(session, "tabs", "SideB4")
  })
  observeEvent(input$go_to_results, {
    if (length(input$run_table_rows_selected) == 0)
      return(showModal(modalDialog(
        title = "Error",
        "Select run(s) first"
      )))
    updateTabItems(session, "tabs", "SideB5")
  })
  observeEvent(input$back_to_db, {
    updateTabItems(session, "tabs", "SideB3")
  })
  observeEvent(input$back_to_db2, {
    updateTabItems(session, "tabs", "SideB3")
  })
  observeEvent(input$back_to_db3, {
    updateTabItems(session, "tabs", "SideB3")
  })
  observeEvent(input$run_job, {
    if (length(input$run_table_rows_selected) == 0)
      return(showModal(modalDialog(
        title = "Error",
        "Select run(s) first"
      )))
    orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
    res <- try(do.call(run_nm, c(objects(), overwrite = TRUE, wait = FALSE)), silent = TRUE)
    if (inherits(res,"try-error"))
      showModal(modalDialog(
        title = "Error from run()",
        res
      ))
    withProgress(message = 'running', value = 0, {
      n <- 10
      for (i in 1:n) {
        incProgress(1/n)
        Sys.sleep(0.1)
      }
    })
  })

 
  ### Select model warnings
  output$monitor_select_warning <- renderText({
    if (length(input$run_table_rows_selected) > 1) return("Warning: will only monitor first")
    if (length(input$run_table_rows_selected) == 0) return("Warning: no runs selected")
  })
  output$results_select_warning <- renderText({
    if(length(input$run_table_rows_selected)==0) return("Warning: no runs selected")
  })
  output$gof_select_warning <- renderText({
    if(length(input$run_table_rows_selected)==0) return("Warning: no runs selected")
  })
  
  status_ob <- eventReactive(
    list(input$refresh_status,
         input$run_table_rows_selected,
         plot_iter_ob),{
           orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
           object <- objects()[[1]]
           status(object)$status
         })

  output$status <- renderText({
    status_ob()
  })

  tail_ob <- reactivePoll(1000, session,
                          # This function returns the time that log_file was last modified
                          checkFunc = function(){
                            if (length(input$run_table_rows_selected) == 0)
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
    tail_ob <- do.call(paste,c(as.list(tail_ob),sep = '<br/>'))
    HTML(tail_ob)
  })

  plot_iter_ob <- eventReactive(
    list(input$refresh_plot,
         input$run_table_rows_selected),{
           orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
           object <- objects()[[1]]
           if(is.null(object$output$psn.ext)) return(dygraph(data.frame(x=0,y=0)))
           if(!file.exists(object$output$psn.ext)) return(dygraph(data.frame(x=0,y=0)))
           d <- try(plot_iter_data(object,trans = input$trans, skip = 0),silent=TRUE)
           if(inherits(d,"try-error")) return(dygraph(data.frame(x=0,y=0)))
           p <- list()
           for(i in seq_along(unique(d$variable))){
             var_name <- unique(d$variable)[i]
             dt <- d[d$variable %in% var_name,c("ITERATION","value")]
             p[[i]] <- dygraph(dt,main=var_name,xlab="Iteration",group = "hi") %>%
               dyOptions(drawPoints = TRUE, pointSize = 2) %>%
               dyRangeSelector()
           }
           p
         }
  )
  
  plot_gof_obsvspred_ob <- eventReactive(
    list(input$refresh_plot,
         input$run_table_rows_selected),{
           orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
           object <- objects()[[1]]

           print(object$output$ctl_out_files[1])
           print(!file.exists(object$output$ctl_out_files[1]))
           #return(ggplot2::qplot(1,1))
           return(plot_gof_obsvspred(object,output))
           if(is.null(object$output$ctl_out_files[1])) return(dygraph(data.frame(x=0,y=0)))
           if(!file.exists(object$output$ctl_out_files[1])) return(dygraph(data.frame(x=0,y=0)))
          
         })
  
  plot_gof_resplot_ob <- eventReactive(
    list(input$refresh_plot,
         input$run_table_rows_selected),{
           orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
           object <- objects()[[1]]
           
           print(object$output$ctl_out_files[1])
           print(!file.exists(object$output$ctl_out_files[1]))
           #return(ggplot2::qplot(1,1))
           return(plot_gof_resplot(object,trans = input$trans, skip = 0, output))
           #if(is.null(object$output$ctl_out_files[1])) return(dygraph(data.frame(x=0,y=0)))
           #if(!file.exists(object$output$ctl_out_files[1])) return(dygraph(data.frame(x=0,y=0)))
          
         })
  
 
  plot_gof_indplot_ob <- eventReactive(
    list(input$refresh_plot,
         input$run_table_rows_selected),{
           orig.dir <- getwd();  setwd(.currentwd) ; on.exit(setwd(orig.dir))
           object <- objects()[[1]]
           
           print(object$output$ctl_out_files[1])
           print(!file.exists(object$output$ctl_out_files[1]))
           
           return(plot_gof_indplot(object,trans = input$trans, skip = 0, output))
           
           
         })
  

  output$distPlot <- renderUI({
    get_plot_bootstrapjs_div(plot_iter_ob())
  })

 output$obsvspred <- renderPlot({
   plot_gof_obsvspred_ob()
 })
  
  output$resplot <- renderUI({
    makePlotContainers(4, height=400)

  })
  
  observe(plot_gof_resplot_ob())
  
  
  output$indplot <- renderUI({
      makePlotContainers_ind( )
   })
  
  observe( plot_gof_indplot_ob())
  

  
  
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
  #### Part for data table
  output$ui1 = renderUI({
    inFile = input$ds_ex
    if (is.null(inFile))
      return(NULL)
    ds_ex = read.csv(inFile$datapath)
    tagList(
      div(style="font-size:90%;", selectizeInput("sets", "Select column with ID:", choices = as.list(names(ds_ex)), multiple = T, width = "250px")),
      div(style="font-size:90%;", selectizeInput("DV", "Select dependent variable:", choices = as.list(names(ds_ex)), multiple = T, width = "250px")),
      div(style="font-size:90%;", selectizeInput("stratification", "Select groups for stratification:", choices = as.list(names(ds_ex)), multiple = T, width = "250px")),
      div(style="font-size:90%;", selectizeInput("time_column", "Select time columns:", choices = as.list(names(ds_ex)), multiple = T, width = "250px")),
      div(style="font-size:90%;", selectizeInput("concov", "Select continious covariates:", choices = as.list(names(ds_ex)), multiple = T, width = "250px")),
      div(style="font-size:90%;", selectizeInput("catcov", "Select categorical covariates:", choices = as.list(names(ds_ex)), multiple = T, width = "250px")),
      div(style="font-size:90%;", selectizeInput("concov", "Select Dose column:", choices = as.list(names(ds_ex)), multiple = T, width = "250px"))
    )
  })

 # output$ex_out <- renderPrint({
#    str(sapply(sprintf('e%d', 0:input$Parameters), function(id) {
#      input[[id]]
#    }, simplify = FALSE))
#  })
# parameters_list <- reactive ({
#   
# })
  
}


