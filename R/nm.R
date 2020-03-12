Vectorize_nm <- function (FUN, vectorize.args = arg.names, SIMPLIFY = FALSE, USE.NAMES = TRUE) 
{
  arg.names <- as.list(formals(FUN))
  arg.names[["..."]] <- NULL
  arg.names <- names(arg.names)
  vectorize.args <- as.character(vectorize.args)
  if (!length(vectorize.args)) 
    return(FUN)
  if (!all(vectorize.args %in% arg.names)) 
    stop("must specify names of formal arguments for 'vectorize'")
  collisions <- arg.names %in% c("FUN", "SIMPLIFY", "USE.NAMES", 
                                 "vectorize.args")
  if (any(collisions)) 
    stop(sQuote("FUN"), " may not have argument(s) named ", 
         paste(sQuote(arg.names[collisions]), collapse = ", "))
  FUNV <- function() {
    args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
    ###############################
    ## MODIFIED CODE FOR NMPROJECT
    args <- lapply(args, function(arg){
      if(inherits(arg, "nm")) return(list(arg)) else return(arg)
    })
    ################################
    names <- if (is.null(names(args))) 
      character(length(args))
    else names(args)
    dovec <- names %in% vectorize.args
    ans <- do.call("mapply", c(FUN = FUN, args[dovec], MoreArgs = list(args[!dovec]), 
                               SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES))
    ###############################
    ## MODIFIED CODE FOR NMPROJECT
    if(is.list(ans) & length(ans) == 1) ans[[1]] else {
      names(ans) <- NULL
      ans
    }
    ###############################
  }
  formals(FUNV) <- formals(FUN)
  FUNV
}


#' Get job information (if it exists)
#'
#' Requires 'get_job_info' to be defined as an option - function that takes stdout console output
#' from a job and returns a character
#'
#' @param m object class nm
#' @param text optional character to set job_info
#' @export
job_info <- function(m, text){
  UseMethod("job_info")  
}
#' @export
job_info.default <- function(m, text){
  if("job_info" %in% names(m)) return(m$job_info)
  if(is.null(m$db_name)) return(NA)
  matched_entry <- nmdb_match_entry(m)
  get_char_field(m$db_name,matched_entry,"job_info")
}

nmdb_match_info <- function(r,db=NULL){
  if(!file.exists(r$db_name))
    return(data.frame(entry=numeric(),
                      match_type=logical(),
                      match_run_in=logical(),
                      match_run_dir=logical(),
                      match_ctl = logical(),
                      overlap_outputs=logical(),
                      overlap_outputs_char=character()))

  if(is.null(db)) d <- nmdb_get(r$db_name) else d <- db

  #dproposal <- nmdb_make_db_row(r)

  ans <- list()
  ans$entry <- d$entry
  ans$match_type <- d$type %in% r$type
  ans$match_run_dir <- d$run_dir %in% r$run_dir
  ans$match_run_in <- d$run_in %in% r$run_in
  ans$match_ctl <- d$ctl %in% r$ctl

  dependent_run_dir <- sapply(d$run_dir,function(i)grepl(paste0("^",i,"$"),r$run_dir))
  ans$dependent_run_dir <- as.logical(dependent_run_dir) & !ans$match_run_dir

  overlap_outputs <- sapply(strsplit(d$output_files,","),function(out_filesi){
    length(intersect(out_filesi,unlist(r$output)))>0
  })

  overlap_outputs_char <- sapply(strsplit(d$output_files,","),function(out_filesi){
    paste(intersect(out_filesi,unlist(r$output)),collapse = ", ")
  })

  ans$overlap_outputs <- as.logical(overlap_outputs)
  ans$overlap_outputs_char <- overlap_outputs_char

  ans <- as.data.frame(ans)
  return(ans)

}

nmdb_make_db_row <- function(r,add_cols=list()){
  if(r$type %in% "execute") r$input$data <- NULL
  d <- data.frame(entry=as.numeric(NA),
                  object=I(list(serialize(r,NULL))),
                  ctl = r$ctl,
                  description=r$description,
                  type=r$type,
                  input_files=paste(unlist(r$input),collapse=","),
                  output_files=paste(unlist(r$output),collapse=","),
                  run_in=r$run_in,
                  run_dir=r$run_dir,
                  cmd = r$cmd,
                  job_info = as.character(NA))
  if(file.exists(r$run_dir))
    d$run_status <- "dir exists" else
      d$run_status <- "not run"
  status_ob <- list()
  status_ob$status <- d$run_status
  if(d$run_status %in% "dir exists") status_ob$seen_at <- Sys.time()
  d$run_status_ob <- I(list(serialize(status_ob,NULL)))
  name_order <- names(d)
  if(length(add_cols)>0){
    d <- merge(d[,names(d)[!names(d) %in% names(add_cols)]],as.data.frame(add_cols))
    d <- d[,name_order]
  }
  d
}

nmdb_printable_db <- function(d){
  d$object <- NULL
  d$output_files <- NULL
  d$lst_exists <- NULL
  d$stop_time_reached <- NULL
  d$run_status <- NULL
  d$run_status_ob <- NULL
  d$input_files <- lapply(strsplit(d$input_files,","),function(char_vec){
    paste(unique(basename(char_vec)),collapse=" ")
  })
  d$run_in <- NULL
  ## include if statements for columns no in the db
  if("outputs_present" %in% names(d))
    d$outputs_present <- signif(d$outputs_present,2)
  for(i in names(d)[!names(d) %in% "entry"]) d[,i] <- as.character(d[,i]) ## convert everything to characters
  names(d) <- gsub("_"," ",names(d))
  d
}

nmdb_add_entry <- function(r,entry=NULL,silent=FALSE,...){
  new_db_flag <- !file.exists(r$db_name)
  dnew <- nmdb_make_db_row(r,...)

  my_db <- DBI::dbConnect(RSQLite::SQLite(), r$db_name)
  RSQLite::dbClearResult(RSQLite::dbSendQuery(my_db, "PRAGMA busy_timeout=5000;"));
  RSQLite::dbClearResult(RSQLite::dbSendQuery(my_db, "PRAGMA journal_mode=WAL;"));
  tryCatch({
    if(new_db_flag) {
      dempty <- dnew[c(),]
      #dempty <- cbind(data.frame(entry=numeric()),dempty)
      DBI::dbWriteTable(my_db, "runs", dempty)
    }
    d <- DBI::dbGetQuery(my_db, 'SELECT * FROM runs')

    if(nrow(d)==0) new_entry <- 1 else
      if(!is.null(entry)) new_entry <- entry else
        new_entry <- max(d$entry) + 1

    #dnew <- cbind(data.frame(entry=new_entry),dnew)
    dnew$entry <- new_entry

    if(!silent) message("Creating database entry: ",new_entry)
    #res <- DBI::dbSendQuery(my_db, "PRAGMA busy_timeout=5000;")
    #res <- DBI::dbSendQuery(my_db, "PRAGMA journal_mode=WAL;")
    DBI::dbWriteTable(my_db, "runs", dnew, append=TRUE)
    DBI::dbDisconnect(my_db)

    invisible(new_entry)
  },
  error=function(e){
    if(DBI::dbIsValid(my_db)) DBI::dbDisconnect(my_db)
    if(new_db_flag) {
      unlink(r$db_name,force = TRUE)
      error_msg <- e
    } else {
      error_msg <- paste0(e,"\n",r$db_name," may be corrupted.
This may be due to a backwards incompatible change in NMproject.
Consider rebuilding. Instructions to rebuild:
1. Delete the file: ",normalizePath(r$db_name,mustWork = FALSE),"
2. Rebuild database by running nm() statements again")
    }
    stop(error_msg)
  })
}

nmdb_get <- function(db_name="runs.sqlite",readable=FALSE){
  my_db <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  tryCatch({
    d <- DBI::dbGetQuery(my_db, 'SELECT * FROM runs')
    if(readable) d <- nmdb_printable_db(d)
    DBI::dbDisconnect(my_db)
    d <- d[order(d$entry),]
    return(d)
  },
  error=function(e) {
    if(DBI::dbIsValid(my_db)) DBI::dbDisconnect(my_db)
    stop(paste0(e,"\n",db_name," may be corrupted.
This may be due to a backwards incompatible change in NMproject.
Consider rebuilding. Instructions to rebuild:
1. Delete the file: ",normalizePath(db_name,mustWork = FALSE),"
2. Rebuild database by running nm() statements again"),call. = FALSE)
  })
}

#' Show table of runs in database
#'
#' @param db_name character. Name of db
#' @param ... arguments to pass to nmdb_get
#' @export
show_runs <- function(db_name="runs.sqlite",...) nmdb_get(db_name=db_name,readable = TRUE,...)

#' Extract nm object from data.base
#' @param entry numeric. Database ID of nm object
#' @param db_name character. Name of db
#' @export

extract_nm <- function(entry,db_name="runs.sqlite"){
  my_db <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  d <- DBI::dbGetQuery(my_db, paste('SELECT * FROM runs WHERE entry ==',entry))
  DBI::dbDisconnect(my_db)
  if(nrow(d)>1) stop("More than one entry found",call. = FALSE)
  if(nrow(d)==0) stop("No entry found",call. = FALSE)
  r <- unserialize(d$object[[1]])
  return(r)
}

#' @export
print.nm <- function(x,...) utils::str(x)

#' Show lst file
#'
#' @param r object of class nm
#' @export
out <- function(r) {
  .Deprecated("show_out")
}

show_file <- function(file_name){
  if(.Platform$OS.type=="windows")
    file.show(file_name) else
      if(exists("file.show")) file.show(file_name) else
        utils::file.edit(file_name)
}

edit_file <- function(file_name){
  if(.Platform$OS.type=="windows")
    file.show(file_name) else
      get("file.edit")(file_name)
}

#' Edit control file
#'
#' @param r object of class nm, a file name, or a run_id
#' @export
show_ctl <- function(r) {
  UseMethod("show_ctl")
}
#' @export
show_ctl.default <- function(r) {
  edit_file(search_ctl_name(r))
}

#' Show lst file
#'
#' @param r object of class nm
#' @export
show_out <- function(r){
  UseMethod("show_out")
}

#' @export
show_out.nm <- function(r) {
  show_file(r$output$psn.lst)
}



search_ctl_name <- function(r, models_dir=getOption("models.dir")){
  if(inherits(r,"nm")) ctl_name <- r$ctl
  if(inherits(r,"numeric") | inherits(r,"character")) {
    r <- as.character(r)
    rtemp <- normalizePath(r,mustWork = FALSE)
    if(file.exists2(rtemp)) ctl_name <- rtemp else {
      rtemp <- from_models(normalizePath(r,mustWork = FALSE), models_dir = models_dir)
      if(file.exists2(rtemp)) ctl_name <- rtemp else {
        ctl_name <- from_models(paste0(getOption("model_file_stub"),r,".",getOption("model_file_extn")), models_dir = models_dir)
      }
    }
  }
  ctl_name
}

file.exists2 <- function(x){ ## only true is file exists and is not a directory
  if(!file.exists(x)) return(FALSE)
  !file.info(x)$isdir
}



gsub2 <- function(pattern,replacement,x,...){
  match <- grepl(pattern,x,...)
  if(!match) return(character())
  gsub(pattern,replacement,x,...)
}

get_stub_name <- function(file_name) gsub("(.*)\\..*","\\1",file_name)

get_run_id <- function(ctl_name){
  stub_name <- get_stub_name(ctl_name)
  match <- paste0("^.*",getOption("model_file_stub"),"(.*).*$")
  ans <- gsub2(match,"\\1",stub_name)
  if(length(ans)==0) ans <- NA
  ans
}

call_fun_on_nm_data_frame <- function(x, fun){
  is_nm <- sapply(x, is_list_nm)
  if(length(which(is_nm)) == 1) return(fun(x[is_nm])) else {
    if(length(which(is_nm)) == 0) stop("can't find column of nm objects")
    if(length(which(is_nm)) > 1) stop("can't find unique column of nm objects")
  }
}

#' Run NONMEM
#' 
#' Run nm objects.  Uses system_nm() to submit the "cmd" value of object 
#' 
#' @param r objects of class nm/or list of objects class nm/or data.frame with column of nm objects
#' @param overwrite logical. Should run directory be overwritten (default=FALSE)
#' @param delete_dir logical. NA (default - directory will be deleted if no dependencies exists)
#' TRUE or FALSE. Should run_dir be deleted.
#' @param wait logical (default=FALSE). Should R wait for run to finish.
#' Default can be changed with  wait_by_default() function
#' @param update_db logical (default=TRUE). Should run_status be updated
#' @param ignore.stdout logical (default=TRUE). Parameter passed to system()
#' @param ignore.stderr logical (default=TRUE). Parameter passed to system()
#' @param initial_timeout numeric. time in seconds.
#' time period to give up on a run if directory hasn't been created.
#' @param quiet logical (default=FALSE). should system_nm output be piped to screen
#' @param intern logical. intern arg to be passed to system
#' @param force logical (default = FALSE).  Force run even results unchanged
#'
#' @return If only one object of class nm was specified, silently returns object.
#' Otherwise returns nothing.
#'
#' @export
run_nm <- function(r, overwrite=getOption("run_overwrite"),delete_dir=c(NA,TRUE,FALSE),wait=getOption("wait"),
                   update_db=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE,
                   initial_timeout=NA, quiet = getOption("quiet_run"),intern=getOption("intern"),
                   force = FALSE)
  UseMethod("run_nm")

#' @export
run_nm.default <- function(r, overwrite=getOption("run_overwrite"),delete_dir=c(NA,TRUE,FALSE),wait=getOption("wait"),
                           update_db=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE,
                           initial_timeout=NA, quiet = getOption("quiet_run"),intern=getOption("intern"),
                           force = FALSE){
  invisible(r)
}

#' @export
run_nm.nm <- function(r, overwrite=getOption("run_overwrite"),delete_dir=c(NA,TRUE,FALSE),wait=getOption("wait"),
                      update_db=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE,
                      initial_timeout=NA, quiet = getOption("quiet_run"),intern=getOption("intern"),
                      force = FALSE){
  
  tidyproject::check_if_tidyproject()
  #if(!quiet & !wait) stop("quiet=FALSE requires wait=TRUE")
  
  if(is.null(r$db_name)) update_db <- FALSE
  ## if directory exists, and if it's definately a directory stop
  if(file.exists(r$run_dir) & !overwrite)if(file.info(r$run_dir)$isdir %in% TRUE) stop("run already exists. To rerun select overwrite=TRUE\n  or use the switch overwrite_default(TRUE)",call.=FALSE)
  if(!is.null(getOption("kill_run"))) getOption("kill_run")(r)
  clean_run(r,delete_dir=delete_dir[1],update_db = update_db)
  wait_arg <- FALSE
  if(!quiet) {
    #if(.Platform$OS.type == "windows") wait_arg <- TRUE
    ignore.stdout = FALSE
    ignore.stderr = FALSE
  }
  message(paste0("Running: ",r$type,":",r$ctl))
  stdout0 <- system_nm(cmd = r$cmd, dir = r$run_in, wait = wait_arg,
                       ignore.stdout = ignore.stdout, ignore.stderr = ignore.stderr, intern=intern)
  if(intern) {
    cat(stdout0,sep = "\n")
    job_info <- getOption("get_job_info")(stdout0)
    if(is.null(job_info)) job_info <- NA
  } else job_info <- NA
  
  if(update_db){
    matched_entry <- nmdb_match_entry(r)
    status_ob <- list(status="submitted",run_at=Sys.time(),job_info=job_info)
    update_object_field(r$db_name,matched_entry,run_status_ob=status_ob)
    update_char_field(r$db_name,matched_entry,run_status=status_ob$status)
    update_char_field(r$db_name,matched_entry,job_info=job_info)
  }
  r$job_info <- job_info
  
  if(wait) wait_for_finished(r, timeout=initial_timeout)
  invisible(r)
}

#' @export
run_nm.list <- function(r, overwrite=getOption("run_overwrite"),delete_dir=c(NA,TRUE,FALSE),wait=getOption("wait"),
                        update_db=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE,
                        initial_timeout=NA, quiet = getOption("quiet_run"),intern=getOption("intern"),
                        force = FALSE){
  
  args <- as.list(match.call()[-1])
  
  call_f <- function(r, args){
    args[["r"]] <- r
    args[["wait"]] <- FALSE
    do.call(run_nm, args)
  }
  
  r <- lapply(r, call_f, args = args)

  if(wait) wait_for_finished(r, timeout=initial_timeout)
  invisible(r)
  
}

#' experimental fast version of run (only tested in linux)
#' @param r objects of class nm/or list of objects class nm/or data.frame with column of nm objects
#' @param wait logical (default=FALSE). Should R wait for run to finish.
#' Default can be changed with  wait_by_default() function
#' @param ignore.stdout logical (default=TRUE). Parameter passed to system()
#' @param ignore.stderr logical (default=TRUE). Parameter passed to system()
#' @param intern logical. intern arg to be passed to system
#'
#' @return If only one object of class nm was specified, silently returns object.
#' Otherwise returns nothing.
#'
#' @export

run_nm_fast_linux <- function(r, wait=getOption("wait"),
                              ignore.stdout = TRUE,
                              ignore.stderr = TRUE,
                              intern=getOption("intern")){
  
  if(inherits(r, "nm")) r <- list(r)
  
  cmd <- sapply(r, function(r) {
    if(is_single_na(r)) return(NA)
    r$cmd
  })
  
  run_in <- run_in(r)
  
  cmd <- paste0("cd $(pwd); cd ", run_in, "; echo ",seq_along(run_in)," ; ", cmd)
  
  cmd <- cmd[!is.na(r)]
  
  cmd <- paste(cmd, collapse = "; ")
  
  for(ri in r[!is.na(r)]){
    message(paste0("Running: ",ri$type,":",ri$ctl))
    clean_run(ri, delete_dir=NA,update_db = FALSE)
  }

  stdout0 <- system_nm(cmd = cmd, dir = ".", wait = FALSE,
                       ignore.stdout = FALSE, 
                       ignore.stderr = FALSE, 
                       intern=TRUE)
  cat(stdout0,sep = "\n")
  
  index <- cumsum(grepl("^[0-9]+$", stdout0))
  
  job_info <- sapply(seq_along(run_in), function(i){
    getOption("get_job_info")(stdout0[index %in% i])
  })
  
  for (i in seq_along(r)){
    if(!is_single_na(r[[i]])) r[[i]]$job_info <- job_info[i] 
  }
  
  return(r)
}
  
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

#' wait for a run to finish
#'
#' @param r objects of class nm/or list of objects class nm
#' @param timeout numeric. time in seconds.
#' time period to give up on a run if directory hasn't been created.
#' @export

wait_for_finished <- function(r, timeout=NA){
  UseMethod("wait_for_finished")
}

#' @export
wait_for_finished.default <- function(r, timeout=NA){
  rl <- r
  if(inherits(rl, "nm")) rl <- list(rl)
  
  rl <- rl[!sapply(rl, is.null)] ## remove nulls
  
  is_single_na <- function(x) if(length(x) == 1) is.na(x) else FALSE
  rl <- rl[!sapply(rl, is_single_na)] ## remove nas
  
  message("Waiting for jobs:\n",paste(sapply(rl,function(i)paste0(" ",i$type,":",i$ctl)),collapse = "\n"))

  i <- 0
  while(length(rl)>0){
    j <- (i %% length(rl))+1
    r <- rl[[j]]
    status_ob <- run_status(r,initial_timeout=timeout)

    ######################################
    ## apply finishing test(s) here

    finished <- is_status_finished(status_ob)

    if(finished){ ## check again in 5 seconds
      if(r$type != "execute") Sys.sleep(5) else Sys.sleep(1)

      status_ob_new <- run_status(r,initial_timeout=)

      finished <- identical(status_ob,status_ob_new)
    }

    ######################################
    if(finished){
      rl[[j]] <- NULL
      message(paste0("Finished: ",r$type,":",r$ctl))
    }
    i <- i + 1
    Sys.sleep(1)
  }
  
  return(invisible(r))
}

#' run status
#'
#' @param r object class nm
#' @param db data.frame. Output of nmdb_get() (optional)
#' @param entry numeric. db entry number(optional)
#' @param initial_timeout numeric (default = NA).
#' Time to wait for directory creation before concluding error
#' @export
run_status <- function(r, db, entry, initial_timeout=NA){
  ## logic:
  ## if file.mtime(r$run_dir) ==  same use status_prev
  ## if file.mtime(r$run_dir) !=  same
  ##    and if lst_names %in% prev & existing_lsts %in% prev
  ##       check new lsts add them to db
  ##    else
  ##       check all directories
  
  ## sometimes status_prev needs to be completely up to date.
  ##  e.g. in wait_for.
  ##  other times it doesn't matter and will update itself
  
  status_ob <- list()
  status_ob_prev <- list()
  if(!is.null(r$db_name)) {
    missing_db <- missing(db)
    if(missing_db) db <- nmdb_get(r$db_name)
    if(missing(entry)) matched_entry <- nmdb_match_entry(r,db = db) else
      matched_entry <- entry
      status_ob_prev <- get_object_field(db_name = r$db_name, db = db, entry = matched_entry, field = "run_status_ob") 


    if(!file.exists(r$run_dir)) {
      if(status_ob_prev$status %in% c("not run","error")) {
        return(status_ob_prev)
      }
      if(status_ob_prev$status %in% c("submitted")) {
        test_timeout <- FALSE
        if(!is.na(initial_timeout)){
          timediff <- difftime(Sys.time(),status_ob_prev$run_at,units = "secs")
          if(timediff > initial_timeout)
            test_timeout <- TRUE
        }
        if(test_timeout) {
          status_ob <- status_ob_prev
          status_ob$status <- "error"
          update_object_field(r$db_name,matched_entry,run_status_ob=status_ob)
          update_char_field(r$db_name,matched_entry,run_status=status_ob$status)
          return(status_ob)
        } else {
          return(status_ob_prev)
        }
      }
      status_ob$status <- "not run"
      update_object_field(r$db_name,matched_entry,run_status_ob=status_ob)
      update_char_field(r$db_name,matched_entry,run_status=status_ob$status)
      return(status_ob)
    }

  }

  #####
  ## assume directory exists and there is something new
  ## get sub run info

  get_sub_dirs <- function(path){
    ## only checks 1 level deep - good enough for boostrap/sse
    contents <- dir(path,full.names = TRUE)
    contents[file.info(contents)$isdir]
  }

  if(r$type %in% c("execute")){
    execution_dirs <- get_sub_dirs(r$run_dir)
  } else if(r$type %in% c("bootstrap","sse","vpc")){
    ## assume relevant directories end in _dir[0-9]+
    first_level <- get_sub_dirs(r$run_dir)
    first_level <- first_level[grepl("_dir[0-9]+",first_level)]
    execution_dirs <- sapply(first_level,get_sub_dirs)
  } else { ## warning: this is slow, but will work for all psn run types
    execution_dirs <- dirname(dir(r$run_dir,
                                  pattern = "psn\\.mod$",
                                  recursive = TRUE,
                                  full.names = TRUE))
  }

  execution_dirs <- unique(execution_dirs)

  if(length(execution_dirs) == 0) {
    if(is.null(r$db_name)) {
      status_ob$status <- "not run"
      return(status_ob)
    }
    if(status_ob_prev$status %in% c("error")) {
      return(status_ob_prev)
    }
    if(status_ob_prev$status %in% c("dir exists")) {
      test_timeout <- FALSE
      if(!is.na(initial_timeout)){
        timediff <- difftime(Sys.time(),status_ob_prev$seen_at,units = "secs")
        if(timediff > initial_timeout)
          test_timeout <- TRUE
      }
      if(test_timeout) {
        status_ob <- status_ob_prev
        status_ob$status <- "error"
        update_object_field(r$db_name,matched_entry,run_status_ob=status_ob)
        update_char_field(r$db_name,matched_entry,run_status=status_ob$status)
        return(status_ob)
      } else {
        return(status_ob_prev)
      }
    }
    status_ob$status <- "dir exists"
    status_ob$seen_at <- Sys.time()
    update_object_field(r$db_name,matched_entry,run_status_ob=status_ob)
    update_char_field(r$db_name,matched_entry,run_status=status_ob$status)
    return(status_ob)
  }

  lst_names <- file.path(execution_dirs,"psn.lst")
  lst_mtimes <- file.mtime(lst_names)
  names(lst_mtimes) <- lst_names
  lst_mtimes <- sort(lst_mtimes)
  lst_names <- names(lst_mtimes)

  if("sub_run_mtimes" %in% names(status_ob_prev)){
    sub_run_mtimes_prev <- status_ob_prev$sub_run_mtimes
  } else sub_run_mtimes_prev <- character()
  sub_run_mtimes_prev <- sort(sub_run_mtimes_prev)
  sub_run_name_order <- names(sub_run_mtimes_prev)

  if("sub_run_status" %in% names(status_ob_prev)){
    sub_run_status_prev <- status_ob_prev$sub_run_status
  } else sub_run_status_prev <- character()
  sub_run_status_prev <- status_ob_prev$sub_run_status
  sub_run_status_prev <- sub_run_status_prev[sub_run_name_order]

  ## go through lst_mtimes, and check if there needs to be an update
  name_checked_before <- lst_names %in% names(sub_run_mtimes_prev)
  #mtime_updated <- !lst_mtimes %in% sub_run_mtimes_prev[]

  recheck_lst <- sapply(seq_along(lst_mtimes),function(i){
    if(!lst_names[i] %in% names(sub_run_mtimes_prev)) return(TRUE)
    if(lst_mtimes[i]!=sub_run_mtimes_prev[lst_names[i]]) return(TRUE)
    return(FALSE)
  })
  names(recheck_lst) <- lst_names

  if(!any(recheck_lst)) {
    return(status_ob_prev)
  }

  ## can assume some lst_mtimes need checking
  lst_status <- rep(NA,length=length(recheck_lst))
  names(lst_status) <- lst_names

  for(i in seq_along(lst_mtimes)){
    lst_name <- lst_names[i]
    if(!recheck_lst[i]) lst_status[i] <- sub_run_status_prev[lst_name] else
    {
      #print(paste("checking",lst_name))
      ## can assume lst_name needs checking now
      psn_error <- file.exists(file.path(dirname(lst_name),"psn_nonmem_error_messages.txt"))
      lst_mtimes[i] <- file.mtime(lst_name)
      if(psn_error){
        lst_status[i] <- "error"
      } else { ## no psn error, read lst
        lst <- try(readLines(lst_name),silent = TRUE)
        if(inherits(lst,"try-error")) {
          lst_status[i] <- "running"
        } else { ## no error reading, see if stop time
          lst <- lst[max(1,(length(lst)-5)):length(lst)]
          stopped <- any(grepl("Stop Time:",lst))
          if(stopped) lst_status[i] <- "finished" else lst_status[i] <- "running"
        }
      }
    }
  }

  ## lst_status & lst_mtimes are up to date.
  running <- length(which(lst_status %in% "running"))
  finished <- length(which(lst_status %in% "finished"))
  error <- length(which(lst_status %in% "error"))
  status <- paste0("running:",running," finished:",finished," errors:",error)

  status_ob$status <- status
  status_ob$sub_run_mtimes <- lst_mtimes
  status_ob$sub_run_status <- lst_status
  status_ob$run_dir_mtime <- file.mtime(r$run_dir)
  if(!is.null(r$db_name)){
    update_object_field(r$db_name,matched_entry,run_status_ob=status_ob)
    update_char_field(r$db_name,matched_entry,run_status=status_ob$status)
  }
  return(status_ob)
}

#' tests if job is finished
#'
#' @param status_ob output from run_status
is_status_finished <- function(status_ob){

  finished <- FALSE
  #if("status" %in% names(status_ob))
  #  if(status_ob$status %in% c("error","finished")) {
  #    finished <- TRUE
  #  }
  if("sub_run_status" %in% names(status_ob)) {
    sub_finished <- status_ob$sub_run_status %in% c("finished","error")
    finished <- length(sub_finished)>0 & all(sub_finished)
  }
  finished
}



#' tests if job is finished
#'
#' @param r object class nm
#' @param initial_timeout numeric. time in seconds.
#' time period to give up on a run if directory hasn't been created.
#' @export
is_finished <- function(r,initial_timeout=NA){
  UseMethod("is_finished")
}
#' @export
is_finished.default <- function(r,initial_timeout=NA){
  if(is_single_na(r)) return(FALSE)
  status_ob <- run_status(r,initial_timeout=initial_timeout)
  is_status_finished(status_ob)
}
is_finished.default <- Vectorize_nm(is_finished.default, vectorize.args = "r", SIMPLIFY = TRUE)

#' Condition number of run
#' 
#' @param r object of class nm
#' @export

cond_num <- function(r){
  UseMethod("cond_num")
}

#' @export
cond_num.default <- function(r){
  if(is_single_na(r)) return(as.numeric(NA))
  stop("don't know how to get cond_num of this")
}

#' @export
cond_num.nm <- function(r){
  dc <- try(coef_nm(r, trans = FALSE), silent = TRUE)
  if(inherits(dc, "try-error")) return(as.numeric(NA))
  cond_num(dc)
}

#' @export
cond_num.nmcoef <- function(r){
  if(is_empty_nmcoef(r)) return(as.numeric(NA))
  dc <- r
  ans <- as.numeric(dc$FINAL[dc$parameter %in% "CONDNUM"])
  if(length(ans) == 0) as.numeric(NA) else ans
}

#' @export
cond_num.list <- function(r){
  sapply(r, cond_num)
}

#cond_num <- Vectorize_nm(cond_num, vectorize.args = "r", SIMPLIFY = TRUE, USE.NAMES = FALSE)


nm_steps_finished <- function(r){ # for waiting
  execution_dirs <- dirname(dir(r$run_dir,
                                pattern = "psn\\.mod$",
                                recursive = TRUE,
                                full.names = TRUE))
  execution_dirs <- unique(execution_dirs)
  lst_names <- file.path(execution_dirs,"psn.lst")

  if(length(execution_dirs) > 0){
    ## check these all exist
    finished <- sapply(lst_names,function(lst_name){
      if(!file.exists(lst_name)) return(FALSE)
      lst <- try(readLines(lst_name),silent = TRUE)
      if(inherits(lst,"try-error")) return(FALSE)
      lst <- lst[max(1,(length(lst)-5)):length(lst)]
      stopped <- any(grepl("Stop Time:",lst))
      psn_error <- file.exists(file.path(dirname(lst_name),"psn_nonmem_error_messages.txt"))
      return(stopped | psn_error)
    })
    finished <- all(finished)
  } else finished <- FALSE ## if no execution_dirs
  return(finished)
}

last_modified <- function(r){
  directory <- r$run_dir
  d <- file.info(directory)
  if(nrow(d)==0) return("")
  d <- data.frame(file=rownames(d),mtime=d$mtime)
  if(nrow(d)==0) return(as.character(NA))
  d <- d[d$mtime %in% max(d$mtime), ]
  as.character(unique(d$mtime))
}

#' Should run() wait for job to finish
#'
#' @param x logical. TRUE means run() will wait, FALSE = asynchronous execution
#' @export

wait_default <- function(x) options("wait"=x)

#' Should run() overwrite previously run jobs
#'
#' @param x logical. TRUE means run() will overwrite previous runs by default,
#' @export

overwrite_default <- function(x) options("run_overwrite"=x)

#' Should run() work in non interactive mode
#'
#' Depreciated
#'
#' @export
non_interactive_mode <- function() {
  .Deprecated("interactive_mode")
}

#' Should run() work in interactive mode
#'
#' @param value logical. TRUE = use interactive mode. FALSE = use non-interactive mode
#' @export
interactive_mode <- function(value) {
  if(missing(value)) stop("expecting TRUE/FALSE argument")
  if(!is.logical(value)) stop("expecting TRUE/FALSE argument")
  if(value){
    overwrite_default(FALSE)
    wait_default(FALSE)
  } else {
    overwrite_default(TRUE)
    wait_default(TRUE)
  }
}

#' @export
nm_tran.nm <- function(x){
  tidyproject::check_if_tidyproject()
  nm_tran.default(x$ctl)
}

#' List files to be cleaned up
#' 
#' @param r object class nm
#' @export
#' @examples 
#' \dontrun{
#' mod1 %>% extra_files
#' mod1 %>% extra_files %>% cleanup
#' }

extra_files <- function(r){
  tidyproject::check_if_tidyproject()
  if(r$type %in% "execute"){
    table_files <- ctl_table_files(r)
    files <- file.path(r$run_dir, "NM_run1", table_files)
  }
  if(r$type %in% "vpc"){
    print("TBD")
    files <- character()
  }
  return(files)
}

#' clean_run files
#'
#' @param r object class nm
#' @param delete_dir logical. NA (default), TRUE or FALSE. Should run_dir be deleted.
#' @param update_db logical (default=TRUE). Should run_status be updated
#' @export
clean_run <- function(r,delete_dir=c(NA,TRUE,FALSE),update_db=!is.null(r$db_name)){
  UseMethod("clean_run")
}

clean_run.default <- function(r,delete_dir=c(NA,TRUE,FALSE),update_db=!is.null(r$db_name)){
  ## assumes ctrl file is run[run_id].mod and -dir=[run_id] was used
  tidyproject::check_if_tidyproject()
  unlink(r$output$ctl_out_files)
  delete_dir <- delete_dir[1]
  if(is.na(delete_dir)){
    if(update_db){
      match_info <- nmdb_match_info(r)
      matched_entry <- match_info$entry[match_info$dependent_run_dir]
      if(length(matched_entry)>0) {
        stop("Dependent run(s) found: ",paste(matched_entry,collapse=","),
             "\nRerun with delete_dir=TRUE or delete_dir=FALSE or remove dependent run")
      }
    }
    unlink(r$run_dir,recursive=TRUE,force = TRUE)
  }
  if(TRUE %in% delete_dir) unlink(r$run_dir,recursive=TRUE,force = TRUE) ## delete run directory
  if(update_db){
    matched_entry <- nmdb_match_entry(r)
    status_ob <- list(status="not run")
    update_object_field(r$db_name,matched_entry,run_status_ob=status_ob)
    update_char_field(r$db_name,matched_entry,run_status=status_ob$status)
  }
  invisible()
}

ctl_table_files <- function(ctl){
  UseMethod("ctl_table_files") 
}

ctl_table_files.default <- function(ctl){ 
  ctl <- ctl_character(ctl)
  s0 <- rem_comment(ctl)
  s <- grep("FILE\\s*=\\s*(\\S+)",s0,value=TRUE)
  table_files <- gsub(".*FILE\\s*=\\s*(\\S+)\\s*.*$","\\1", s)
  table_files
}

ctl_out_files <- function(ctl_file){
  UseMethod("ctl_out_files")  
}

ctl_out_files.default <- function(ctl_file){ ## will get vector of $TABLE file names from control file.
  if(!file.exists(ctl_file)) stop(paste(ctl_file, "doesn't exist"))
  dir0 <- dir(dirname(ctl_file))
  
  ctl <- readLines(ctl_file,warn = FALSE)
  
  table.files <- ctl_table_files(ctl)

  stub <- basename(ctl_file)
  stub <- gsub("(.+)\\.\\w+$","\\1",stub)

  out.files <- dir0[grepl(paste(stub,"\\.",sep=""),dir0)]
  out.files <- out.files[!grepl("scm",out.files)]
  out.files <- out.files[!out.files%in%basename(ctl_file)]

  out.files <- c(table.files,out.files)
  out.files
}

input_files <- function(run_in,run_type,ctl_name){
  r <- list()
  r$ctl <- ctl_name
  if(run_type %in% "execute"){
    r$data_name <- data_name(ctl_name)
    r$data_loc <- file.path(run_in,r$data_name)
  }
  return(r)
}

output_files <- function(run_in,run_type,run_dir,ctl_name){
  r <- list()
  if(run_type %in% "execute"){
    extn <- tools::file_ext(ctl_name)
    r$lst <- paste0(get_stub_name(ctl_name),".lst")
    r$psn.mod <- file.path(run_dir,"NM_run1","psn.mod")
    r$psn.lst <- file.path(run_dir,"NM_run1","psn.lst")
    r$psn.ext <- file.path(run_dir,"NM_run1","psn.ext")
    r$psn.cov <- file.path(run_dir,"NM_run1","psn.cov")
    r$psn.cor <- file.path(run_dir,"NM_run1","psn.cor")
    r$psn.xml <- file.path(run_dir,"NM_run1","psn.xml")
    r$ctl_out_files <- file.path(run_in,ctl_out_files(ctl_name))
  }
  return(r)
}

#' write csv for NONMEM control files
#' @param ... arguments for write.csv
#' @param na character. Default changed to ".".
#' @param row.names logical. Default changed to FALSE.
#' @param quote logical. Fefault changed to FALSE
#' @export

write.csv.nm <- function(d, ...,na=".",
                         row.names=FALSE,
                         quote=FALSE){
  
  utils::write.csv(d, ...,na=na,row.names=row.names,quote=quote)
}

#' Setup demo files
#'
#' @param demo_name character. Name of demo. Default = "theopp"
#' @param new_project character. To set up demo in a separate (new) project
#' @param file_stub character. Default = "run1". Stub to some file names
#' @param overwrite logical. Default changed to FALSE.
#' @param exclude character. Name of extension to exclude from copying
#' @param additional_demo_locations character vector. default = NULL.
#'   locations for demo directories
#' @export

setup_nm_demo <- function(demo_name="theopp",
                          new_project = NA,
                          file_stub = paste0(getOption("model_file_stub"),1),
                          overwrite=FALSE,
                          exclude=NULL,
                          additional_demo_locations = NULL){
  
  if(!is.na(new_project)){
    cwd <- getwd()
    on.exit(setwd(cwd))
    tidyproject::make_project(proj_name = new_project)
    setwd(new_project)
  }
  
  tidyproject::check_if_tidyproject()
  
  examples_dir <- character()
  examples_dirs <- character()

  if(length(additional_demo_locations) > 0) {
    examples_dir <- normalizePath(additional_demo_locations, mustWork = FALSE)
    examples_dirs <- list.files(examples_dir, full.names = TRUE, recursive = FALSE)
    #examples_dir <- examples_dirs[grepl(paste0(.Platform$file.sep, demo_name,"$"), examples_dirs)]
  }
  ## TODO: rename examples to demos
  examples_dirs <- append(examples_dirs, 
                          list.files(system.file("extdata","examples",package = "NMproject"),
                                     full.names = TRUE, recursive = FALSE))
  matched_examples_dirs <- examples_dirs[grepl(paste0(.Platform$file.sep, demo_name,"$"), examples_dirs)]
  
  if(length(matched_examples_dirs) == 0)
    stop("demo not found.\nAvailable demos:\n ",
         paste(unique(basename(examples_dirs)), collapse = "\n "), call. = FALSE)
  
  #examples_dir <- append(examples_dir, system.file("extdata","examples",demo_name,package = "NMproject"))
  examples_dir <- matched_examples_dirs[1]

  files_to_copy <- dir(examples_dir, all.files = TRUE, full.names = TRUE, recursive = TRUE)
  
  stage_info <- tidyproject::stage(files_to_copy, overwrite = overwrite, silent = TRUE)
  
  tidyproject::import(stage_info, overwrite = overwrite)

}

#' Get NONMEM output tables
#'
#' This combines $TABLE output with the input data, allowing text columns to be retained for plotting/summaries.
#'
#' @param r data.frame.  object of class nmexecute
#' @param dorig data.frame. optional NONMEM input dataset.
#' @param ... additional arguments to pass on to read.csv
#' @export

nm_output <- function(r,dorig,...){
  UseMethod("nm_output")  
}

nm_output.default <- function(r,dorig,...){
  
  if(requireNamespace("xpose4")) {
    xpdb <- xpose4::xpose.data(run_id(r), directory=paste0(run_in(r),"/"))
    d <- xpdb@Data
  } else d <- data.frame()
  
  if(nrow(d) == 0){
    ctl_out_files <- r$output$ctl_out_files
    ctl_out_files <- ctl_out_files[grepl("tab", ctl_out_files)]
    
    d <- lapply(ctl_out_files, function(out_file){
      d <- utils::read.table(out_file, skip = 1, header = TRUE)
    })
    
    d <- do.call(cbind,d)
    d <- d[,!duplicated(names(d))]
  }
  
  if(missing(dorig)) dorig <- get_data(r,...)
  
  filter_statements <- data_filter_char(r)
  if(identical(filter_statements, "TRUE")){
    dORD <- seq_len(nrow(dorig))
  } else {
    expre <- parse(text=filter_statements)
    dORD <- which(with(dorig,eval(expre)))    
  }

  if(nrow(d) %% length(dORD) != 0) {
    stop("something wrong... when R reads in original dataset
and applies filter ",filter_statements,",
there's ",length(dORD),"rows, but NONMEM output has ", nrow(d), " rows")
  }    

  ctl_contents <- ctl_character(r)
  sim_ctl <- any(grepl("^\\s*\\$SIM",rem_comment(ctl_contents)))
  
  nreps <- nrow(d) / length(dORD)
  
  if("PRKEY" %in% names(d)) stop("name conflict with PRKEY in xpose table. aborting...")
  if("PRKEY" %in% names(dorig)) stop("name conflict with PRKEY in original data. aborting...")

  d$PRKEY <- dORD
  dorig$PRKEY <- 1:nrow(dorig)
  if(sim_ctl){
    if("SIM" %in% names(d)) stop("name conflict with SIM in xpose table. aborting...")
    if("SIM" %in% names(dorig)) stop("name conflict with SIM in original data. aborting...")
    d$SIM <- rep(1:nreps,each=length(dORD))
    message("Adding column: SIM")
  }

  d$INNONMEM <- TRUE

  ## want a DV_OUT columsn
  if("DV_OUT" %in% names(d)) warning("name conflict with DV_OUT in xpose table. replacing...")
  d$DV_OUT <- d$DV
  d$DV <- NULL
  d <- d[,c(setdiff(names(d),names(dorig)[!names(dorig) %in% c("PRKEY")]))]
  #dorig <- dorig[,names(dorig)[!names(dorig) %in% c("DV")]]

  d2 <- merge(dorig, d, all.x = TRUE, by = "PRKEY")

  d2$INNONMEM <- d2$INNONMEM %in% TRUE
  if(nreps > 1) d2$SIM[is.na(d2$SIM)] <- 0

  ## row number check
  if(nrow(d2) != nrow(d)*(nreps-1)/nreps + nrow(dorig)) stop("merge went wrong. debug")

  message("Adding column: PRKEY")

  return(d2)
}



process_output <- function(r, ...){
  #if(!inherits(r, "nmexecute")) stop("can only currently process outputs for execute runs")
  do <- nm_output(r, ...)
  save(do, file = file.path(run_dir(r, full_path = TRUE), "NMout.RData"))
  invisible(do)
}

#' Get processed output table
#' 
#' @param r object of class nm
#' @param ... optional additional arguments to pass on to read.csv of orig data
#' @export

output_table <- function(r, ...){
  UseMethod("output_table") 
}

#' @export
output_table.default <- function(r, ...){
  out_path <- file.path(run_dir(r, full_path = TRUE), "NMout.RData")
  if(!file.exists(out_path)) {
    do <- process_output(r, ...)
  } else {
    load(out_path)
  }
  return(do)
}

#' Get ignore statement
#' @param r object coercible into ctl_list
#' @export
data_ignore_char <- function(r){
  UseMethod("data_ignore_char")
}
#' @export
data_ignore_char.default <- function(r){
  dol_data <- ctl_list(r)$DATA
  dol_data <- dol_data[!dol_data %in% ""]
  dol_data <- rem_comment(dol_data)
  
  ignore_present <- any(grepl(".*IGNORE\\s*=\\s*\\(",dol_data))
  accept_present <- any(grepl(".*ACCEPT\\s*=\\s*\\(",dol_data))
  
  type <- NA
  if(ignore_present & accept_present) stop("cannot identify ignore columns")
  if(ignore_present) type <- "IGNORE"
  if(accept_present) type <- "ACCEPT"
  no_filter <- is.na(type)
  
  if(!no_filter){
    filter_statements <- paste0(".*",type,"\\s*=\\s*\\((\\S[^\\)]+)\\)*.*")
    dol_data <- dol_data[grepl(filter_statements, dol_data)]
    filter_statements <- gsub(filter_statements,"\\1",dol_data)
    filter_statements <- unlist(strsplit(filter_statements,","))
    filter_statements <- gsub("\\.EQ\\.","==",filter_statements)
    filter_statements <- gsub("\\.NE\\.","!=",filter_statements)
    filter_statements <- gsub("\\.EQN\\.","==",filter_statements)
    filter_statements <- gsub("\\.NEN\\.","!=",filter_statements)
    filter_statements <- gsub("\\./E\\.","!=",filter_statements)
    filter_statements <- gsub("\\.GT\\.",">",filter_statements)
    filter_statements <- gsub("\\.LT\\.","<",filter_statements)
    filter_statements <- gsub("\\.GE\\.",">=",filter_statements)
    filter_statements <- gsub("\\.LE\\.","<=",filter_statements)
    filter_statements <- paste(filter_statements, collapse= " | ")
    if("ACCEPT" %in% type) filter_statements <- paste0("!(",filter_statements,")")
  } else {
    filter_statements <- "FALSE"
  }
  filter_statements
}

#' Get filter statement
#' 
#' Opposite of data_ignore_char 
#' 
#' @param r object coercible into ctl_list
#' @export
data_filter_char <- function(r){
  ignore_char <- data_ignore_char(r)
  if(ignore_char == "FALSE") return("TRUE")
  ignored <- !grepl("^!\\((.*)\\)", ignore_char)
  accepted <- !ignored
  if(accepted){
    return(gsub("^!\\((.*)\\)", "\\1", ignore_char) )
  } else {
    return(paste0("!(",ignore_char,")"))
  }
}

#' replace ignore statement
#' @param ctl object coercible into ctl_list
#' @param ignore_char character. replacement statement
#' @export
update_ignore <- function(ctl, ignore_char){
  UseMethod("update_ignore")
}

#' @export
update_ignore.default <- function(ctl, ignore_char){
  ctl <- ctl_list(ctl)
  
  ignore_present <- any(grepl(".*IGNORE\\s*=\\s*\\(",ctl$DATA))
  if(ignore_present){
    ## remove any row that matches exactly
    ctl$DATA <- ctl$DATA[!grepl("^(\\s*)IGNORE\\s*=\\s*\\(*\\S[^\\)]+\\)*(\\s*)$",ctl$DATA)]
    ## remove only bracketed IGNORE statement if other things are on the line.
    ctl$DATA <- gsub("(.*)IGNORE\\s*=\\s*\\(+\\S[^\\)]+\\)+(.*)",
         "\\1\\2", ctl$DATA)
  }
  
  ignore_char <- gsub("\\s*\\|\\s*", ", ", ignore_char)
  
  ignore_char <- gsub("==",".EQ.",ignore_char)
  ignore_char <- gsub("!=",".NE.",ignore_char)
  ignore_char <- gsub(">",".GT.",ignore_char)
  ignore_char <- gsub("<",".LT.",ignore_char)
  ignore_char <- gsub(">=",".GE.",ignore_char)
  ignore_char <- gsub("<=",".LE.",ignore_char)
  ignore_char <- gsub("\\s+(\\.\\S+\\.)\\s+", "\\1", ignore_char)
  
  ignore_char <- paste0("IGNORE=(",ignore_char,")")
  
  last_line <- ctl$DATA[length(ctl$DATA)]
  
  if(grepl("^\\s*$", last_line)){
    ctl$DATA[length(ctl$DATA)] <- ignore_char
  } else {
    ctl$DATA <- append(ctl$DATA, ignore_char) 
  }
  ctl$DATA <- append(ctl$DATA, "")
  ctl

}

#' update sizes statement
#' @param ctl object coercible into ctl_list
#' @param sizes_char character. replacement statement
#' @export

update_sizes <- function(ctl, sizes_char){
  ctl <- ctl_character(ctl)
  if("SIZES" %in% names(ctl_list(ctl))){
    stop("can't modifying existing sizes yet")
  } else {
    dol_matches <- grep("\\s*\\$", ctl)
    if(length(dol_matches) == 0) dol_matches <- 1 else {
      dol_matches <- dol_matches[1]
    }
    before <- c()
    after <- ctl
    if(dol_matches > 1){
      before <- ctl[1:(dol_matches-1)]
      after <- ctl[dol_matches:length(ctl)]
    }
    save_attr <- attributes(ctl)
    ctl <- c(before,
      paste("$SIZES", sizes_char),
      after)
    attributes(ctl) <- save_attr
  }
  ctl_list(ctl)
}


#' Exclude rows of NONMEM dataset
#' 
#' @param d data.frame for NONMEM dataset
#' @param dexcl data.frame consisting of rows to be ignored
#' @param exclude_col character.  Name of exclude column in d
#' @examples 
#' \dontrun{
#' ## use with dplyr
#' dexcl <- d %>% filter(ID == 23, TIME > 18, TIME < 24) %>% select(ID, TIME, DV, EXCL)
#' dexcl  ## view rows to be excluded
#' d <- d %>% exclude_rows(dexcl)
#' }
#' @export

exclude_rows <- function(d, dexcl, exclude_col = "EXCL"){
  excluded <- do.call(paste, d[, names(d) %in% names(dexcl)]) %in% 
    do.call(paste, dexcl)
  excluded <- which(excluded)
  if(nrow(dexcl) != length(excluded)) stop("couldn't find all rows")
  d[[exclude_col]][excluded] <- 1
  d
}

