#' Run status
#'
#' Get status of NONMEM job
#'
#' @param x object to get status of
#' @param simple logical default TRUE. Should result be simple one liner
#' @export
status <- function(x, simple = TRUE) UseMethod("status",x)

#' @export
status.default <- function(x, simple = TRUE) {
  if(length(x) == 1) if(is.na(x)) return(NA)
  stop("don't know how to handle this")
}

#' @export
status.data.frame <- function(x, simple = TRUE) {
  if(ncol(x) == 1) {
    return(status(as.list(x)[[1]]))
  }
}

#' @export
status.list <- function(x, simple = TRUE) {
  if(simple){
    sapply(x, status, simple = TRUE)    
  } else {
    lapply(x, status, simple = FALSE)    
  }
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
