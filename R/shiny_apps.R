#' Run monitor & summary app
#' 
#' Interactively monitor NONMEM runs.  This interface is intentionally limited to monitoring
#'  activities only.  Running and post processing should be scripted
#' 
#' @param m either nm_list object, or data.frame or list or environment contain nm_lists
#' @param envir if missing, the environment to search
#' @examples
#' \dontrun{
#'
#' shiny_nm()        ## use all objects in global workspace
#' shiny_nm(m1)      ## only m1
#' shiny_nm(d$m)     ## only d$m
#' shiny_nm(d)       ## all nm_lists in d (data.frame/list/environment)
#'
#' }
#' @export
shiny_nm <- function(m, envir = .GlobalEnv){
  if(missing(m)) {
    m <- nm_list_gather(envir)
  } else {
    if(is_nm_generic(m)) m <- as_nm_list(m)
    if(!is_nm_list(m)){
      m <- nm_list_gather(m)
      if(!is_nm_list(m))
        stop("couldn't find any nm_list objects in m")
    }
  }
  if(!requireNamespace("DT", quietly = TRUE))
    stop("DT needed for this function to work. Please install it.",
         call. = FALSE)
  if(!requireNamespace("dygraphs", quietly = TRUE))
    stop("dygraphs needed for this function to work. Please install it.",
         call. = FALSE)
  dygraphs::dygraph  ## check main function exists
  DT::datatable      ## check main function exists
  shiny_dir <- system.file("extdata/shiny",package="NMproject")
  .sso_env$.currentwd <- getwd()  # see zzz.R for .sso_env
  .sso_env$.m <- m  # see zzz.R for .sso_env
  on.exit({
    .sso_env$.currentwd <- NULL
    .sso_env$.m <- NULL
  }, add = TRUE)
  shiny::runApp(shiny_dir,launch.browser = TRUE)
}

code_library_app <- function(){
  shiny_dir <- system.file("extdata/code_library_shiny", package="NMproject")
  .sso_env$.currentwd <- getwd()  # see zzz.R for .sso_env
  on.exit({
    .sso_env$.currentwd <- NULL
  }, add = TRUE)
  shiny::runApp(shiny_dir,launch.browser = TRUE)
}


overwrite_behaviour_app <- function() {
  
  if(!requireNamespace("miniUI", quietly = TRUE))
    stop("miniUI needed for this function to work. Please install it.",
         call. = FALSE)
  
  miniUI::miniPage  ## main function needed
  
  shiny_dir <- system.file("extdata/overwrite_behaviour", package="NMproject")
  .sso_env$.currentwd <- getwd()  # see zzz.R for .sso_env
  on.exit({
    .sso_env$.currentwd <- NULL
  }, add = TRUE)
  #viewer <- shiny::paneViewer(300)
  viewer <- shiny::dialogViewer(dialogName = "overwrite behaviour")
  shiny::runGadget(shiny::shinyAppDir(shiny_dir), viewer = viewer)
  
}

#' Plot iterations vs parameters/OBJ with dygraphs
#'
#' @param m object of class nmexecute
#' @param trans logical. should parameter transformations be performed
#' @export
plot_iter_dygraph <- function(m, trans = TRUE){
  if(!requireNamespace("dygraphs")) stop("install dygraphs")
  psn_ext_path <- m %>% nm_output_path("ext")
  if(!file.exists(psn_ext_path)) return(dygraphs::dygraph(data.frame(x=0,y=0)))
  d <- try(plot_iter_data(m,trans = trans, skip = 0),silent=TRUE)
  if(inherits(d,"try-error")) return(dygraphs::dygraph(data.frame(x=0,y=0)))
  p <- list()
  for(i in seq_along(unique(d$variable))){
    var_name <- unique(d$variable)[i]
    dt <- d[d$variable %in% var_name,c("ITERATION","value")]
    p[[i]] <- dygraphs::dygraph(dt,main=var_name,xlab="Iteration",group = "hi", width = 400) %>%
      dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2, sigFigs=3) %>%
      dygraphs::dyRangeSelector()
  }
  #htmltools::browsable(htmltools::tagList(p))
  #get_plot_bootstrapjs_div(p)
  htmltools::browsable(get_plot_bootstrapjs_div(p))
}

get_plot_bootstrapjs_div <- function(plot_object_list) {
  
  get_col_div <- function(plot_object_list, index, css_class = 'col-xs-12 col-sm-4'){
    col_div <- htmltools::div(class = css_class)
    
    if(length(plot_object_list) >= index) {
      plotname <- paste("plot", index, sep="")
      plot_output_object <- htmltools::tagList(plot_object_list[[index]]) #dygraphOutput(plotname)
      col_div <- htmltools::tagAppendChild(col_div, plot_output_object)
    }
    return(col_div)
  }
  #
  get_plot_div <- function(plot_object_list) {
    result_div <- htmltools::div(class = 'container-fluid', dep)
    
    suppressWarnings(for(i in seq(1,length(plot_object_list),3)) {
      row_div <- htmltools::div(class = 'row')
      row_div <- htmltools::tagAppendChild(row_div, get_col_div(plot_object_list, i))
      row_div <- htmltools::tagAppendChild(row_div, get_col_div(plot_object_list, i+1))
      row_div <- htmltools::tagAppendChild(row_div, get_col_div(plot_object_list, i+2))
      result_div <- htmltools::tagAppendChild(result_div, row_div)
    })
    return(result_div)
  }
  ####
  plot_output_list_div <- get_plot_div(plot_object_list)
  
  #browser()
  
  dep <- htmltools::HTML('<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css">')
  
  plot_output_list_div <- htmltools::tagList(dep, plot_output_list_div)
  
  return(plot_output_list_div)
}


#' Tail of lst file
#' 
#' @param r nm object
#' @export
tail_lst <- function(r){
  if(type(r) == "execute"){
    lst_name <- r %>% nm_output_path("ext") #r$output$psn.lst
    out_name <- file.path(dirname(lst_name),"OUTPUT")
    if(file.exists(out_name)) lst_name <- out_name
    lst <- try(readLines(lst_name),silent = TRUE)
    if(inherits(lst,"try-error")) return("no output")
    if(length(lst)==0) return("no output")
    lst <- c(rep("",5),lst)
    len_lst <- length(lst)
    lst[(len_lst-4):len_lst]
  } else return("no output to display")
}

