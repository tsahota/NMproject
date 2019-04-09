#' Run status
#'
#' Get status of NONMEM job
#'
#' @param x object to get status of
#' @export
status <- function(x) UseMethod("status",x)

# run_type <- function(run.dir){  ## internal function: get type of run from directory
#   if(!file.exists(file.path(run.dir,"command.txt"))) {
#     warning("can't find command.txt, assuming run is type \"execute\"")
#     return("execute")
#   }
#   command <- readLines(file.path(run.dir,"command.txt"))
#   command <- gsub("^(\\S+)\\s.*","\\1",command,perl=TRUE)
#   gsub("^.*[/\\]+([^/\\]+)$","\\1",command)
# }


# status_execute <- function(ctl_name,run_in,run_dir,sub.dir="NM_run1"){
# 
#   r <- data.frame(RUN=ctl_name,DIR=run_in,TYPE="execute",TEST=c(
#     "dir created",
#     "*.ext file exists",
#     "*.ext file written to",
#     "*.lst file exists",
#     "*.lst file written to",
#     "*.lst data check",
#     "*.lst stop time reached"
#   ),
#   RESULT=FALSE,COMMENT=NA)
# 
#   r$RESULT[r$TEST %in% "dir created"] <- file.exists(run_dir)
#   r$COMMENT[r$TEST %in% "dir created"] <- NA
# 
#   r$RESULT[r$TEST %in% "*.ext file exists"] <- file.exists(file.path(run_dir,sub.dir,"psn.ext"))
#   if(r$RESULT[r$TEST %in% "*.ext file exists"]) r$COMMENT[r$TEST %in% "*.ext file exists"] <- paste("Created:",file.info(file.path(run_dir,sub.dir,"psn.ext"))$ctime)
# 
#   if(r$RESULT[r$TEST %in% "*.ext file exists"]){
#     r$RESULT[r$TEST %in% "*.ext file written to"] <- length(readLines(file.path(run_dir,sub.dir,"psn.ext")))>0
#     if(r$RESULT[r$TEST %in% "*.ext file written to"]) r$COMMENT[r$TEST %in% "*.ext file written to"] <- paste("Last modified:",file.info(file.path(run_dir,sub.dir,"psn.ext"))$mtime)
#   }
# 
#   r$RESULT[r$TEST %in% "*.lst file exists"] <- file.exists(file.path(run_dir,sub.dir,"psn.lst"))
#   if(r$RESULT[r$TEST %in% "*.lst file exists"]) r$COMMENT[r$TEST %in% "*.lst file exists"] <- paste("Created:",file.info(file.path(run_dir,sub.dir,"psn.lst"))$ctime)
# 
#   if(r$RESULT[r$TEST %in% "*.lst file exists"]){
#     s <- readLines(file.path(run_dir,sub.dir,"psn.lst")) ## TODO: try to avoid reading lst file. too slow
#     r$RESULT[r$TEST %in% "*.lst file written to"] <- length(s)>0
#     if(r$RESULT[r$TEST %in% "*.lst file written to"]) r$COMMENT[r$TEST %in% "*.lst file written to"] <- paste("Last modified:",file.info(file.path(run_dir,sub.dir,"psn.lst"))$mtime)
# 
#     data.line <- s[grepl("0FORMAT FOR DATA:",s)]
#     r$RESULT[r$TEST %in% "*.lst data check"] <- length(data.line)!=0
# 
#     r$RESULT[r$TEST %in% "*.lst stop time reached"] <- s[length(s)-1] == "Stop Time:"
#     r$COMMENT[r$TEST %in% "*.lst stop time reached"] <- paste(s[length(s)])
#   }
# 
#   r
# }
# 
# status_scm <- function(ctl_name,run_in,run_dir){
# 
#   r <- data.frame(RUN=ctl_name,DIR=run_in,TYPE="scm",TEST=c(
#     "dir created",
#     "data_preprocessing_dir created",
#     "base_modelfit_dir1 created",
#     "modelfit_dir1 created",
#     "short_scmlog.txt"
#   ),
#   RESULT=NA,COMMENT=NA)
# 
#   r$RESULT[r$TEST %in% "dir created"] <- file.exists(run_dir)
#   r$COMMENT[r$TEST %in% "dir created"] <- NA
# 
#   r$RESULT[r$TEST %in% "data_preprocessing_dir created"] <- file.exists(file.path(run_dir,"data_preprocessing_dir"))
#   if(r$RESULT[r$TEST %in% "data_preprocessing_dir created"]) r$COMMENT[r$TEST %in% "data_preprocessing_dir created"] <- paste("Created:",file.info(file.path(run_dir,"data_preprocessing_dir"))$ctime)
# 
#   r$RESULT[r$TEST %in% "base_modelfit_dir1 created"] <- file.exists(file.path(run_dir,"base_modelfit_dir1"))
#   if(r$RESULT[r$TEST %in% "base_modelfit_dir1 created"]) r$COMMENT[r$TEST %in% "base_modelfit_dir1 created"] <- paste("Created:",file.info(file.path(run_dir,"base_modelfit_dir1"))$ctime)
# 
#   r$RESULT[r$TEST %in% "modelfit_dir1 created"] <- file.exists(file.path(run_dir,"modelfit_dir1"))
#   if(r$RESULT[r$TEST %in% "modelfit_dir1 created"]) r$COMMENT[r$TEST %in% "modelfit_dir1 created"] <- paste("Created:",file.info(file.path(run_dir,"modelfit_dir1"))$ctime)
# 
#   r$RESULT[r$TEST %in% "short_scmlog.txt"] <- file.exists(file.path(run_dir,"short_scmlog.txt"))
#   if(r$RESULT[r$TEST %in% "short_scmlog.txt"]){
#     s <- readLines(file.path(run_dir,"short_scmlog.txt"))
#     r$COMMENT[r$TEST %in% "short_scmlog.txt"] <- paste("nlines =",length(s))
#   }
#   r
# }
# 
# status_unknown <- function(ctl_name,run_in,run_dir){
# 
#   r <- data.frame(RUN=ctl_name,DIR=run_in,TYPE=run_type(run_dir),TEST=c(
#     "dir created"
#     ),
#   RESULT=NA,COMMENT=NA)
# 
#   r$RESULT[r$TEST %in% "dir created"] <- file.exists(run_dir)
#   r$COMMENT[r$TEST %in% "dir created"] <- NA
# 
#   r
# }
# 

#status.nmexecute <- function(x) status_execute(ctl_name=basename(x$ctl),
#                                               run_in = x$run_in,
#                                               run_dir=x$run_dir)

#status.nmscm <- function(x) status_scm(ctl_name=basename(x$ctl),run_in = x$run_in,run_dir=x$run_dir)

#status.nm <- function(x) status_unknown(ctl_name=basename(x$ctl),run_in = x$run_in,run_dir=x$run_dir)

#' @export
status.nm <- function(x) run_status(x)

#' @export
status.default <- function(x) {
  if(is.na(x)) return(NA)
  stop("don't know how to handle this")
}

#' @export
status.data.frame <- function(x) {
  if(ncol(x) == 1) {
    return(status(as.list(x)[[1]]))
  }
}

#' @export
status.list <- function(x) {
  lapply(x, status)
}

#' Tail of lst file
#' 
#' @param r nm object
#' @export
tail_lst <- function(r){
  if(r$type == "execute"){
    lst_name <- r$output$psn.lst
    out_name <- file.path(dirname(r$output$psn.lst),"OUTPUT")
    if(file.exists(out_name)) lst_name <- out_name
    lst <- try(readLines(lst_name),silent = TRUE)
    if(inherits(lst,"try-error")) return("no output")
    if(length(lst)==0) return("no output")
    lst <- c(rep("",5),lst)
    len_lst <- length(lst)
    lst[(len_lst-4):len_lst]
  } else return("no output to display")
}
