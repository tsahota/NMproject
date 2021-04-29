#' Run status
#'
#' Get status of NONMEM job
#'
#' @param x object to get status of
#' @param simple logical default TRUE. Should result be simple one liner
#' @export
status <- function(x, simple = TRUE) UseMethod("status",x)


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
