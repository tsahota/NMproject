#' @include utilsExtra.R

NULL

#' Run NONMEM jobs
#' 
#' Run nm objects.  Uses \code{system_nm()} to submit the \code{cmd()} value of object
#' 
#' @param m nm objects
#' @param wait logical (default=FALSE). Should R wait for run to finish.
#' Default can be changed with  wait_by_default() function
#' @param ignore.stdout logical (default=TRUE). Parameter passed to system()
#' @param ignore.stderr logical (default=TRUE). Parameter passed to system()
#' @param quiet logical (default=FALSE). should system_nm output be piped to screen
#' @param intern logical. intern arg to be passed to system
#' @param force logical (default = FALSE).  Force run even results unchanged
#' @param cache_ignore_cmd logical (default = FALSE). Should check \code{cmd} field with cache?
#' @param cache_ignore_ctl logical (default = FALSE). Should check control file contents with cache?
#' @param cache_ignore_data logical (default = FALSE). Should check dataset with cache? 
#'
#' @return m with \code{job_info} fields populated.
#'
#' @export
run_nm <- function(m, wait=getOption("wait"),
                   ignore.stdout = TRUE, ignore.stderr = TRUE,
                   quiet = getOption("quiet_run"),intern=getOption("intern"),
                   force = FALSE, 
                   cache_ignore_cmd = FALSE, cache_ignore_ctl = FALSE, cache_ignore_data = FALSE){
  UseMethod("run_nm")
}

#' @export
run_nm.nm_generic <- function(m, wait=getOption("wait"),
                              ignore.stdout = TRUE, ignore.stderr = TRUE,
                              quiet = getOption("quiet_run"),intern=getOption("intern"),
                              force = FALSE, 
                              cache_ignore_cmd = FALSE, cache_ignore_ctl = FALSE, cache_ignore_data = FALSE){
  
  if(is.na(m)) return(m)
  
  ## write control stream
  ctl <- ctl_contents(m)
  if(length(ctl) == 1){
    if(is.na(ctl)){
      warning("no ctl_contents defined.
              Use ctl_contents() e.g. m <- m %>% ctl_contents(\"/path/to/ctl/file\")")
      return(m)
    }
  }
  
  m %>% write_ctl()
  
  ## caching
  if(!force){
    ## pull existing checksum info
    #run_cache_disk <- lapply(run_cache_paths(m), readRDS)
    if(length(run_cache_paths(m)) > 0){
      run_cache_disk <- readRDS(run_cache_paths(m))
      ## get current checksum
      current_checksums <- run_checksums(m)
      ## determine matches
      
      if(cache_ignore_cmd){  ## remove cmd check
        keep <- !names(current_checksums) %in% "cmd"
        run_cache_disk$checksums <- run_cache_disk$checksums[keep]
        current_checksums <- current_checksums[keep]
      }
      
      if(cache_ignore_ctl){  ## remove cmd check
        keep <- !names(current_checksums) %in% "ctl"
        run_cache_disk$checksums <- run_cache_disk$checksums[keep]
        current_checksums <- current_checksums[keep]
      }        
      
      if(cache_ignore_data){  ## remove cmd check
        keep <- !names(current_checksums) %in% "data"
        run_cache_disk$checksums <- run_cache_disk$checksums[keep]
        current_checksums <- current_checksums[keep]
      }
      
      ## ignore names
      names(current_checksums) <- NULL
      names(run_cache_disk$checksums) <- NULL
      
      matched <- identical(run_cache_disk$checksums, current_checksums)
      if(matched){
        message("rebuilding run from cache... use run_nm(force = TRUE) to override")
        ## update object and return
        m <- m %>% executed(TRUE)
        m <- m %>% job_info(run_cache_disk$job_info)
        m <- m %>% save_run_cache()
        return(invisible(m))    ## if up to date, skip
      }
    }
  }
  
  ## NONMEM will run from this point on
  ## check new_jobs() behaviour 
  behaviour <- new_jobs()
  if(behaviour == "stop") stop("new_jobs detected: stopping... \n change new_job behaviour with new_jobs()", call. = FALSE)
  if(behaviour == "skip") {
    message("new job detected: skip \n change new_job behaviour with new_jobs()")
    return(invisible(m))
  }
  
  wipe_run(m)
  kill_job(m)
  
  message(paste0("Running: ",type(m),":",ctl_path(m)))
  stdout0 <- system_nm(cmd = cmd(m),
                       dir = run_in(m), 
                       wait = FALSE,
                       ignore.stdout = FALSE,
                       ignore.stderr = FALSE,
                       intern=intern)
  
  if(intern) {
    cat(stdout0,sep = "\n")
    job_info <- getOption("get_job_info")(stdout0)
    if(is.null(job_info)) job_info <- NA
  } else job_info <- NA
  
  m <- m %>% executed(TRUE)
  m <- m %>% job_info(job_info)
  
  ## The object is now ready to be saved in cache
  m <- m %>% save_run_cache()

  invisible(m)
}

#' @export
run_nm.nm_list <- Vectorize_nm_list(run_nm.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)


#' Run NONMEM
#' 
#' Use run_nm instead. run was deprecated due to naming conflict with future package's run() function
#' 
#' @param ... objects
#' @export

run <- function(...){
  .Deprecated("run_nm", msg = "run() will soon be deprecated, use run_nm() instead")
  run_nm(...)
}