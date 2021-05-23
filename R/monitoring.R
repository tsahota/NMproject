#' Get status of a run
#' 
#' @param x nm object
#' @export

status <- function(x) {
  UseMethod("status")
}

#' @export
status.nm_generic <- function(x){
  m <- x
  get_sub_dirs <- function(path){
    ## only checks 1 level deep - good enough for execute/boostrap/sse
    contents <- dir(path,full.names = TRUE)
    contents[file.info(contents)$isdir]
  }
  
  execution_dirs <- get_sub_dirs(run_dir_path(m))
  
  ##############################
  ## for bootstraps, etc. go to modelfit_dir1
  modelfit_dir <- execution_dirs[grepl("modelfit_dir1", execution_dirs)]
  if(length(modelfit_dir) > 0){
    execution_dirs <- get_sub_dirs(modelfit_dir)
  }
  ##############################
  if(length(execution_dirs) == 0) return("not_started")
  
  ## for each execution dir get vector of status'
  
  statuses <- sapply(execution_dirs, function(execution_dir){
    if(file.exists(file.path(execution_dir,"psn_nonmem_error_messages.txt"))) return("error")
    lst_name <- file.path(execution_dir,"psn.lst")
    if(!file.exists(lst_name)) return("not_started")
    lst <- try(readLines(lst_name),silent = TRUE)
    if(inherits(lst,"try-error")) return("running")
    #lst <- lst[max(1,(length(lst)-5)):length(lst)]
    stopped <- any(grepl("Stop Time:",lst))
    if(stopped) return("finished")
    stopped <- any(grepl("No nonmem execution",lst))
    if(stopped) return("error")
    return("running")
  })
  
  if("execute" %in% type(m)) {
    if(any(statuses == "finished")) {
      ## NOTE: removed the following because don't want status and reverse dependencies to care about 
      ##   exported outputs - only the raw outputs in the nonmem run directory NM_run1.
      # psn_exports <- psn_exported_files(m, minimal = TRUE)
      # psn_exports_exist <- file.exists(psn_exports)
      # if(!all(psn_exports_exist)){
      #   #warning("incomplete_tables in runs\n ", paste(psn_exports[!psn_exports_exist], collapse = "\n"))
      #   statuses[statuses %in% "finished"] <- "running"#"stopped-incomplete_tables"
      # }
    }
  }
  
  statuses
  
}
#' @export
status.nm_list <- Vectorize_nm_list(status.nm_generic)

#' Get status of multiple runs in form of table
#' 
#' @param m nm objects
#' 
#' @export
status_table <- function(m){
  tab <- m %>% status %>% 
    factor(levels = c("finished", "error", "running", "not started")) %>%
    table
  dplyr::as_tibble(tab)
}


#' Tests if job is finished
#'
#' @param r object class nm
#' @param initial_timeout numeric. time in seconds.
#' time period to give up on a run if directory hasn't been created.
#' @export
is_finished <- function(r, initial_timeout=NA){
  UseMethod("is_finished")
}

#' @export
is_finished.nm_generic <- function(r, initial_timeout=NA){
  
  ## first check if meta.yaml is there, if so, just use that
  ## otherwise do a basic check.
  
  meta_yaml <- file.path(run_dir_path(r), "meta.yaml")
  
  if(file.exists(meta_yaml)){
    yaml_contents <- readLines(meta_yaml)
    yaml_finish_match <- grep("finish_time", yaml_contents)
    if(length(yaml_finish_match) == 0) return(FALSE)
    if(length(yaml_finish_match) > 1) stop("more than one finish tag in meta.yaml. Debug needed")
    ## can now assume a unique match
    return(TRUE)
  } else {
    ## the backup way doesn't care about psn exports
    return(all(status(r) %in% c("finished", "error") | is.na(r)) )
  }
  
}

#' @export
is_finished.nm_list <- Vectorize_nm_list(is_finished.nm_generic)

#' Test if NONMEM ran without errors
#' 
#' @param r nm object
#' 
#' @export
is_successful <- function(r) {
  res <- all(status(r) %in% "finished")
  res[is.na(res)] <- FALSE
  res
}


#' Wait for runs to finish
#' 
#' blocks subsequent r execution until run(s) are finished
#' 
#' @param r nm object
#' @param timeout numeric seconds to wait before timeout
#' @export
wait_finish <- function(r, timeout=NA){
  UseMethod("wait_finish")
}
#' @export
wait_finish.nm_list <- function(r, timeout=NA){
  r_orig <- r
  r <- r[!is_finished(r)]

  if(is.na(timeout)) wait_for(all(is_finished(r))) else
    wait_for(all(is_finished(r)), timeout = timeout)
  invisible(r_orig)
}

#' @export
wait_finish.nm_generic <- function(r, timeout=NA){
  wait_finish.nm_list(as_nm_list(r), timeout = timeout)
}
