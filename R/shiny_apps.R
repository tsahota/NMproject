#' Shiny view of NMproject
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
  dygraphs::dygraph
  DT::datatable
  shiny_dir <- system.file("extdata/shiny",package="NMproject")
  .sso_env$.currentwd <- getwd()  # see zzz.R for .sso_env
  .sso_env$.m <- m  # see zzz.R for .sso_env
  on.exit({
    .sso_env$.currentwd <- NULL
    .sso_env$.m <- NULL
  }, add = TRUE)
  shiny::runApp(shiny_dir,launch.browser = TRUE)
}

#' Code library
#' 
#' Shiny app to search code library and stage code into R package
#' 
#' @export
code_library_app <- function(){
  shiny_dir <- system.file("extdata/code_library_shiny", package="NMproject")
  .sso_env$.currentwd <- getwd()  # see zzz.R for .sso_env
  on.exit({
    .sso_env$.currentwd <- NULL
  }, add = TRUE)
  shiny::runApp(shiny_dir,launch.browser = TRUE)
}