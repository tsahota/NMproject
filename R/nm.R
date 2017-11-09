#' Make object of class nm
#'
#' @param cmd character. system command to launch NONMEM (PsN)
#' @param psn_command character. Name of PsN command (optional)
#' @param shell_script_name character. Name of shell script for grid submission (optional)
#' @param shell_script_extn character. File extension shell script for grid submission (optional)
#' @param ctl_name character. Name of control file (optional)
#' @param run_id character or numeric. Run identifier (optional)
#' @param run_dir character or numeric. Run directory (optional)
#' @param run_in character. Directory to execute. Default = getOption("models.dir")
#' @param db_name character. Name of db
#' @return object of class nm
#' @export
nm <- function(cmd,psn_command,
               shell_script_name,shell_script_extn="sh",
               ctl_name,run_id,run_dir,
               run_in=getOption("models.dir"),
               db_name = "runs.sqlite"){
  tidyproject::check_if_tidyproject()
  r <- list()
  class(r) <- "nm"

  r$db_name <- db_name
  if(is.null(run_in)) run_in <- "."
  r$run_in <- run_in
  r$cmd <- cmd

  if(!missing(shell_script_name)){
    if(!file.exists(file.path(r$run_in,shell_script_name)))
      stop("cannot find shell script in ",r$run_in," directory",call. = FALSE)
    ss_contents <- readLines(file.path(r$run_in,shell_script_name))
    message("reading shell script: ",shell_script_name)
    if(length(ss_contents)==0) stop(shell_script_name," is empty",call. = FALSE)
    cmd <- c(cmd,ss_contents)
  } else {
    match_string <- paste0("^.*\\b(.+\\.",shell_script_extn,")\\b.*")
    ss_file_name_detected <- grepl(match_string,cmd)
    if(ss_file_name_detected){
      shell_script_name <- gsub(match_string,"\\1",cmd)
      if(file.exists(file.path(r$run_in,shell_script_name))){
        message("reading shell script: ",shell_script_name)
        ss_contents <- readLines(file.path(r$run_in,shell_script_name))
        if(length(ss_contents)==0) stop(shell_script_name," is empty",call. = FALSE)
        cmd <- c(cmd,ss_contents)
      }
    }
  }
  ## cmd is now set
  cmd <- paste(cmd,collapse = " ")

  if(!missing(psn_command)) r$type <- psn_command
  if(!missing(ctl_name)) {
    if(any(!file.exists(file.path(r$run_in,ctl_name))))
      stop(paste(ctl_name,collapse = ",")," do(es) not exist",call. = FALSE)
    r$ctl <- file.path(r$run_in,ctl_name)
  }

  if(is.null(r$type)){
    matched_psn_commands <- unlist(sapply(getOption("psn.commands"),
                                          function(i) gsub2(paste0("^.*(",i,")\\b\\s.+$"),"\\1",cmd)))
    matched_psn_commands <- as.character(matched_psn_commands)
    if(length(matched_psn_commands)==0)
      stop("couldn't infer psn command type.\nRerun with psn_command argument",call. = FALSE)
    if(length(matched_psn_commands)>1)
      stop("couldn't infer unique psn command type.\nRerun with psn_command argument",call. = FALSE)
    message(paste("inferring run type :",matched_psn_commands))
    r$type <- matched_psn_commands
  }

  if(is.null(r$ctl)){
    subcmd <- gsub(paste0("^.*(\\s|^)(",r$type,".+)$"),"\\2",cmd)
    matched_ctl <- gsub2(paste0("^.*(",getOption("model_file_stub"),"\\S+",getOption("model_file_extn"),")\\s*.*$"),"\\1",subcmd)
    message(paste("inferring ctl file :",matched_ctl))
    r$ctl <- file.path(r$run_in,matched_ctl)
  }

  if(missing(run_dir)){
    matched_run_dir <-
      gsub2(paste0("^.*-dir[a-z]*=\\s*(\\S*)\\s*.*$"),"\\1",cmd)
    if(length(matched_run_dir)==0)
      stop("couldn't infer run directory type.\nRerun with run_dir argument",call. = FALSE)
    if(length(matched_run_dir)>1)
      stop("couldn't infer unique run directory type.\nRerun with run_dir argument",call. = FALSE)
    message(paste("inferring run directory :",matched_run_dir))
    r$run_dir <- file.path(r$run_in,matched_run_dir)
  } else r$run_dir <- run_dir

  if(missing(run_id)){
    matched_run_id <- get_run_id(r$ctl[1])
    r$run_id <- matched_run_id
  } else r$run_id <- run_id

  if(length(r$ctl)==0) stop("cannot infer model file.\nRerun with ctl_name argument",call. = FALSE)
  if(!file.exists(r$ctl)) stop("cannot find model file",call. = FALSE)

  ## class for execute runs
  class(r) <- c(paste0("nm",r$type),class(r))

  r$input <- input_files(run_in = r$run_in,
                         run_type = r$type,
                         ctl_name = r$ctl)
  r$output <- output_files(run_in = r$run_in,
                           run_type = r$type,
                           run_dir = r$run_dir,
                           ctl_name = r$ctl)

  if(r$type %in% "execute") r$param_info <- param_info(r$ctl)

  r$description <- tidyproject::get_script_field(r$ctl,"Description")

  ####

  match_info <- nmdb_match_info(r)
  matched_entry <- match_info$entry[match_info$match_type &
                                      match_info$match_run_in &
                                      match_info$match_ctl]
  overlapped_output_entries <-
    match_info$entry[match_info$overlap_outputs &
                       !(match_info$entry %in% matched_entry)]

  overlapped_output_files <-
    match_info$overlap_outputs_char[match_info$overlap_outputs &
                                      !(match_info$entry %in% matched_entry)]

  overlapped_run_dir_entries <-
    match_info$entry[match_info$match_run_dir & match_info$match_run_in &
                       !(match_info$entry %in% matched_entry)]

  if(length(overlapped_run_dir_entries)>0)
    stop("\nThis run has the same run_dir as entry: ",overlapped_run_dir_entries,
         "\nFix command and try again",call. = FALSE)

  if(length(overlapped_output_entries)>0){
    stop("\nThe following outputs would overwrite those from entry ",overlapped_output_entries,
         ":\n    ",overlapped_output_files,
         "\nFix control stream and try again",
         "\n--View runs with: show_runs()",
         "\n--Delete old runs with: delete_nm(entry)",call. = FALSE)
  }

  if(length(matched_entry)==0) {
    nmdb_add_entry(r)
  } else if(length(matched_entry)==1) {
    delete_nm(db_name,matched_entry)
    message("Rewriting database entry: ",matched_entry)
    nmdb_add_entry(r,matched_entry,silent=TRUE)
  } else stop("Matched more than one database entry. Debug")
  union_write(".gitignore",getOption("nmproj_gitignore"))
  ## last step: update run_status in db
  run_status(r)
  return(r)
}

nmdb_match_entry <- function(r,db=NULL){

  match_info <- nmdb_match_info(r,db=db)
  matched_entry <- match_info$entry[match_info$match_type &
                                      match_info$match_run_in &
                                      match_info$match_ctl]
  if(length(matched_entry)>1) stop("Can't identify unique database entry. Something wrong. Debug")
  matched_entry
}

union_write <- function (path, new_lines){
  if (file.exists(path))
    lines <- readLines(path, warn = FALSE) else
      lines <- character()
    all <- union(lines, new_lines)
    writeLines(all, path)
}

#' Delete run from database
#'
#' @param db_name character. Name of db
#' @param entry numeric. entry name to delete
#'
#' @export
delete_nm <- function(db_name = "runs.sqlite",entry){
  query <- paste('DELETE FROM runs WHERE entry ==',entry)
  my_db <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  DBI::dbExecute(my_db, query)
  DBI::dbDisconnect(my_db)
}

update_object_field <- function(db_name = "runs.sqlite",entry,...){
  args <- list(...)
  my_db <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  d <- DBI::dbGetQuery(my_db, 'SELECT * FROM runs')

  for(i in seq_along(args)){
    arg_name <- names(args)[i]
    arg_val <- args[[i]]
    d[d$entry %in% entry,][[arg_name]] <- I(list(serialize(arg_val,NULL)))
  }

  DBI::dbWriteTable(my_db, "runs", d,overwrite=TRUE)
  DBI::dbDisconnect(my_db)
}

get_object_field <- function(db_name = "runs.sqlite",entry,field){
  my_db <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  d <- DBI::dbGetQuery(my_db, paste('SELECT * FROM runs WHERE entry ==',entry))
  DBI::dbDisconnect(my_db)
  unserialize(d[d$entry %in% entry,][[field]][[1]])
}

update_char_field <- function(db_name="runs.sqlite",entry,...){
  args <- list(...)
  repl <- paste(paste0(names(args)," = '",args,"'"),collapse = ", ")
  query <- paste("UPDATE runs SET",repl,"WHERE entry ==",entry)
  my_db <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  DBI::dbExecute(my_db, query)
  DBI::dbDisconnect(my_db)
}

get_char_field <- function(db_name="runs.sqlite",entry,field){
  my_db <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  d <- DBI::dbGetQuery(my_db, paste('SELECT * FROM runs WHERE entry ==',entry))
  DBI::dbDisconnect(my_db)
  d[d$entry %in% entry,][[field]]
}

nmdb_match_info <- function(r,db=NULL){
  if(!file.exists(get_db_name(r)))
    return(data.frame(entry=numeric(),
                      match_type=logical(),
                      match_run_in=logical(),
                      match_run_dir=logical(),
                      match_ctl = logical(),
                      overlap_outputs=logical(),
                      overlap_outputs_char=character()))

  if(is.null(db)) d <- nmdb_get(get_db_name(r)) else d <- db

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
  d <- data.frame(object=I(list(serialize(r,NULL))),
                  ctl = r$ctl,
                  description=r$description,
                  type=r$type,
                  input_files=paste(unlist(r$input),collapse=","),
                  output_files=paste(unlist(r$output),collapse=","),
                  run_in=r$run_in,
                  run_dir=r$run_dir,
                  cmd = r$cmd,
                  job_id = as.character(NA))
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
  new_db_flag <- !file.exists(get_db_name(r))
  dnew <- nmdb_make_db_row(r,...)
  my_db <- DBI::dbConnect(RSQLite::SQLite(), get_db_name(r))
  tryCatch({
    if(new_db_flag) {
      dempty <- dnew[c(),]
      dempty <- cbind(data.frame(entry=numeric()),dempty)
      DBI::dbWriteTable(my_db, "runs", dempty)
    }
    d <- DBI::dbGetQuery(my_db, 'SELECT * FROM runs')

    if(nrow(d)==0) new_entry <- 1 else
      if(!is.null(entry)) new_entry <- entry else
        new_entry <- max(d$entry) + 1

    dnew <- cbind(data.frame(entry=new_entry),dnew)

    if(!silent) message("Creating database entry: ",new_entry)
    DBI::dbWriteTable(my_db, "runs", dnew, append=TRUE)
    DBI::dbDisconnect(my_db)
    invisible(new_entry)
  },
  error=function(e){
    if(DBI::dbIsValid(my_db)) DBI::dbDisconnect(my_db)
    if(new_db_flag) unlink(get_db_name(r),force = TRUE)
    stop(e)
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
    stop(paste0(e,"\n",db_name," may be corrupted.\nConsider deleting and rebuilding"),call. = FALSE)
  })
}

get_db_name <- function(r){
  if(is.null(r$db_name)) return("runs.sqlite") else return(r$db_name)
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

#' Edit control file
#'
#' @param r object of class nm, a file name, or a run_id
#' @export
ctl <- function(r) {
  if(inherits(r,"nm")) ctl_name <- r$ctl
  if(inherits(r,"numeric") | inherits(r,"character")) {
    r <- as.character(r)
    rtemp <- normalizePath(r,mustWork = FALSE)
    if(file.exists2(rtemp)) ctl_name <- rtemp else {
      rtemp <- from_models(normalizePath(r,mustWork = FALSE))
      if(file.exists2(rtemp)) ctl_name <- rtemp else {
        rtemp <- from_models(paste0(getOption("model_file_stub"),r,".",getOption("model_file_extn")))
        if(file.exists2(rtemp)) ctl_name <- rtemp
      }
    }
  }
  if(.Platform$OS.type=="windows")
    file.show(ctl_name) else
      get("file.edit")(ctl_name)
}

file.exists2 <- function(x){ ## only true is file exists and is not a directory
  if(!file.exists(x)) return(FALSE)
  !file.info(x)$isdir
}

#' Show lst file
#'
#' @param r object of class nm
#' @export
lst <- function(r) {
  if(.Platform$OS.type=="windows")
    file.show(r$output$psn.lst) else
      if(exists("file.show")) file.show(r$output$psn.lst) else
        utils::file.edit(r$output$psn.lst)
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


#' Run NONMEM
#' @param ... objects of class nm
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
#' @export
run <- function(...,overwrite=getOption("run_overwrite"),delete_dir=c(NA,TRUE,FALSE),wait=getOption("wait"),
                update_db=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE,
                initial_timeout=NA, quiet = getOption("quiet_run"),intern=getOption("intern")){
  UseMethod("run")
}

#' @export
run.nm <- function(...,overwrite=getOption("run_overwrite"),delete_dir=c(NA,TRUE,FALSE),wait=getOption("wait"),
                   update_db=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE,
                   initial_timeout=NA, quiet = getOption("quiet_run"),intern=getOption("intern")){
  tidyproject::check_if_tidyproject()
  #if(!quiet & !wait) stop("quiet=FALSE requires wait=TRUE")
  rl <- list(...)
  lapply(rl,function(r){
    ## if directory exists, and if it's definately a directory stop
    if(file.exists(r$run_dir) & !overwrite)if(file.info(r$run_dir)$isdir %in% TRUE) stop("run already exists. To rerun select overwrite=TRUE\n  or use the switch overwrite_default(TRUE)",call.=FALSE)
    if(!is.null(getOption("kill_run"))) getOption("kill_run")(r)
    clean_run(r,delete_dir=delete_dir[1])
    wait_arg <- FALSE
    if(!quiet) {
      #if(.Platform$OS.type == "windows") wait_arg <- TRUE
      ignore.stdout = FALSE
      ignore.stderr = FALSE
    }
    message(paste0("Running: ",r$type,":",r$ctl))
    stdout0 <- system_nm(cmd = r$cmd, dir = r$run_in, wait = wait_arg,
                         ignore.stdout = ignore.stdout, ignore.stderr = ignore.stderr, intern=intern)
    if(intern) cat(stdout0,sep = "\n")

    if(update_db){
      matched_entry <- nmdb_match_entry(r)
      status_ob <- list(status="submitted",run_at=Sys.time())
      update_object_field(get_db_name(r),matched_entry,run_status_ob=status_ob)
      update_char_field(get_db_name(r),matched_entry,run_status=status_ob$status)
    }
  })
  if(wait) wait_for_finished(...,initial_timeout=initial_timeout)
  invisible()
}

#' Run run.nm method
#' @param ... objects of class nm and other args
#' @export
run_nm <- function(...){
  run.nm(...)
}

#' wait for a run to finish
#'
#' @param ... objects of class nm
#' @param initial_timeout numeric. time in seconds.
#' time period to give up on a run if directory hasn't been created.
#' @export
wait_for_finished <- function(...,initial_timeout=NA){
  rl <- list(...)
  message("Waiting for jobs:\n",paste(sapply(rl,function(i)paste0(" ",i$type,":",i$ctl)),collapse = "\n"))

  i <- 0
  while(length(rl)>0){
    j <- (i %% length(rl))+1
    r <- rl[[j]]
    status_ob <- run_status(r,initial_timeout=initial_timeout)

    ######################################
    ## apply finishing test(s) here

    finished <- is_status_finished(status_ob)

    if(finished){ ## check again in 5 seconds
      if(r$type != "execute") Sys.sleep(5) else Sys.sleep(1)

      status_ob_new <- run_status(r,initial_timeout=initial_timeout)

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
  invisible()
}

#' run status
#'
#' @param r object class nm
#' @param db data.frame. Output of nmdb_get() (optional)
#' @param entry numeric. db entry number(optional)
#' @param initial_timeout numeric (default = NA).
#' Time to wait for directory creation before concluding error
#' @export
run_status <- function(r,db,entry,initial_timeout=NA){
  ## logic:
  ## if file.mtime(r$run_dir) ==  same use status_prev
  ## if file.mtime(r$run_dir) !=  same
  ##    and if lst_names %in% prev & existing_lsts %in% prev
  ##       check new lsts add them to db
  ##    else
  ##       check all directories

  if(missing(db)) db <- nmdb_get(get_db_name(r))
  if(missing(entry)) matched_entry <- nmdb_match_entry(r,db = db) else
    matched_entry <- entry
  status_ob_prev <- get_object_field(get_db_name(r),matched_entry,"run_status_ob")
  status_ob <- list()

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
        update_object_field(get_db_name(r),matched_entry,run_status_ob=status_ob)
        update_char_field(get_db_name(r),matched_entry,run_status=status_ob$status)
        return(status_ob)
      } else {
        return(status_ob_prev)
      }
    }
    status_ob$status <- "not run"
    update_object_field(get_db_name(r),matched_entry,run_status_ob=status_ob)
    update_char_field(get_db_name(r),matched_entry,run_status=status_ob$status)
    return(status_ob)
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
        update_object_field(get_db_name(r),matched_entry,run_status_ob=status_ob)
        update_char_field(get_db_name(r),matched_entry,run_status=status_ob$status)
        return(status_ob)
      } else {
        return(status_ob_prev)
      }
    }
    status_ob$status <- "dir exists"
    status_ob$seen_at <- Sys.time()
    update_object_field(get_db_name(r),matched_entry,run_status_ob=status_ob)
    update_char_field(get_db_name(r),matched_entry,run_status=status_ob$status)
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
  update_object_field(get_db_name(r),matched_entry,run_status_ob=status_ob)
  update_char_field(get_db_name(r),matched_entry,run_status=status_ob$status)
  return(status_ob)
}

#' tests if job is finished
#'
#' @param status_ob output from run_status
is_status_finished <- function(status_ob){

  finished <- FALSE
  if(status_ob$status %in% c("error","finished")) {
    finished <- TRUE
  }
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
  status_ob <- run_status(r,initial_timeout=initial_timeout)
  is_status_finished(status_ob)
}

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

#' clean_run files
#'
#' @param r object class nm
#' @param delete_dir logical. NA (default), TRUE or FALSE. Should run_dir be deleted.
#' @param update_db logical (default=TRUE). Should run_status be updated
#' @export
clean_run <- function(r,delete_dir=c(NA,TRUE,FALSE),update_db=TRUE){
  ## assumes ctrl file is run[run_id].mod and -dir=[run_id] was used
  tidyproject::check_if_tidyproject()
  unlink(r$output$ctl_out_files)
  delete_dir <- delete_dir[1]
  if(is.na(delete_dir)){
    match_info <- nmdb_match_info(r)
    matched_entry <- match_info$entry[match_info$dependent_run_dir]
    if(length(matched_entry)>0) {
      stop("Dependent run(s) found: ",paste(matched_entry,collapse=","),
           "\nRerun with delete_dir=TRUE or delete_dir=FALSE or remove dependent run")
    }
    unlink(r$run_dir,recursive=TRUE,force = TRUE)
  }
  if(TRUE %in% delete_dir) unlink(r$run_dir,recursive=TRUE,force = TRUE) ## delete run directory
  if(update_db){
    matched_entry <- nmdb_match_entry(r)
    status_ob <- list(status="not run")
    update_object_field(get_db_name(r),matched_entry,run_status_ob=status_ob)
    update_char_field(get_db_name(r),matched_entry,run_status=status_ob$status)
  }
  invisible()
}

ctl_out_files <- function(ctl_file){ ## will get vector of $TABLE file names from control file.
  if(!file.exists(ctl_file)) stop(paste(ctl_file, "doesn't exist"))
  dir0 <- dir(dirname(ctl_file))

  s0 <- rem_comment(readLines(ctl_file))
  s <- grep("FILE\\s*=\\s*(\\S+)",s0,value=TRUE)
  table.files <- gsub(".*FILE\\s*=\\s*(\\S+)\\s*.*$","\\1", s)


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

write.csv.nm <- function(...,na=".",
                         row.names=FALSE,
                         quote=FALSE){
  utils::write.csv(...,na=na,row.names=row.names,quote=quote)
}

#' Setup demo files
#'
#' @param file_stub character. Default = "run1". Stub to some file names
#' @param overwrite logical. Default changed to FALSE.
#' @param demo_name character. Name of demo. Default = "theopp-demo"
#' @param exclude character. Name of extension to exclude from copying
#' @export

setup_nm_demo <- function(file_stub = paste0(getOption("model_file_stub"),1),
                          overwrite=FALSE,
                          demo_name="theopp",
                          exclude=NULL){
  tidyproject::check_if_tidyproject()
  examples_dir <- system.file("extdata","examples",demo_name,package = "NMproject")

  files_to_copy <- dir(examples_dir,all.files = TRUE,recursive = TRUE)
  dest_names <- files_to_copy
  dest_names <- gsub("^Models/",paste0(getOption("models.dir"),"/"),dest_names)
  dest_names <- gsub("^Scripts/",paste0(getOption("scripts.dir"),"/"),dest_names)
  dest_names <- gsub("run1\\.",paste0(file_stub,"."),dest_names)

  already_there <- file.exists(dest_names)
  if(any(already_there) & !overwrite)
    stop("File(s) already exist:\n",paste(paste0("  ",dest_names[already_there]),collapse="\n"),"\nRename or rerun with overwrite=TRUE",call. = FALSE)

  for(i in seq_along(files_to_copy)){
    if(is.null(exclude)) do_copy <- TRUE else {
      if(grepl(paste0("\\.",exclude,"$"),files_to_copy[i]))
        do_copy <- FALSE else do_copy <- TRUE
    }
    if(do_copy) copy_file(file.path(examples_dir,files_to_copy[i]),
                          dest_names[i],version_control=TRUE,overwrite = TRUE)
  }
}

#' Get NONMEM output tables
#'
#' This combines $TABLE output with the input data, allowing text columns to be retained for plotting/summaries.
#'
#' @param r data.frame.  object of class nm_execute
#' @param read_fun function (default = read.csv). Function to read in original NONMEM data
#' @param dorig data.frame. optional NONMEM input dataset. if missing, will read in using read_fun
#' @param ... additional arguments to pass on to read_fun
#' @export
nm_output <- function(r,read_fun=utils::read.csv,dorig,...){

  if(!requireNamespace("xpose4")) stop("require xpose4 to be installed")
  xpdb <- xpose4::xpose.data(r$run_id,directory=paste0(r$run_in,"/"))
  d <- xpdb@Data

  if(missing(dorig)){
    data_loc <- file.path(r$run_in,r$input$data_name)
    if(!"na" %in% names(list)) dorig <- read_fun(data_loc,na=".",...) else
      dorig <- read_fun(data_loc,...)
  }

  ctl_content <- readLines(r$ctl)
  dol_data <- ctl_nm2r(ctl_content)$DATA
  dol_data <- dol_data[!dol_data %in% ""]

  ignore_present <- any(grepl(".+IGNORE\\s*=\\s*\\S\\S",dol_data))
  accept_present <- any(grepl(".+ACCEPT\\s*=\\s*\\S\\S",dol_data))

  type <- NA
  if(ignore_present & accept_present) stop("cannot identify ignore columns")
  if(ignore_present) type <- "IGNORE"
  if(accept_present) type <- "ACCEPT"
  no_filter <- is.na(type)

  if(!no_filter){
    filter_statements <- paste0(".*",type,"\\s*=\\s*\\(*(\\S[^\\)]+)\\)*.*")
    filter_statements <- gsub(filter_statements,"\\1",dol_data)
    filter_statements <- strsplit(filter_statements,",")[[1]]
    filter_statements <- gsub("\\.EQ\\.","==",filter_statements)
    filter_statements <- gsub("\\.NE\\.","!=",filter_statements)
    filter_statements <- gsub("\\.EQN\\.","==",filter_statements)
    filter_statements <- gsub("\\.NEN\\.","!=",filter_statements)
    filter_statements <- gsub("\\./E\\.","!=",filter_statements)
    filter_statements <- gsub("\\.GT\\.",">",filter_statements)
    filter_statements <- gsub("\\.LT\\.","<",filter_statements)
    filter_statements <- gsub("\\.GE\\.",">=",filter_statements)
    filter_statements <- gsub("\\.LE\\.","<=",filter_statements)
    filter_statements <- paste(filter_statements, collapse= " & ")

    expre <- parse(text=filter_statements)
    if("IGNORE" %in% type) dORD <- which(!with(dorig,eval(expre)))
    if("ACCEPT" %in% type) dORD <- which(with(dorig,eval(expre)))
    if(length(dORD) != nrow(d)) stop("something wrong with IGNORE/ACCEPT. debug")
  } else {
    dORD <- seq_len(nrow(dorig))
  }

  if("PRKEY" %in% names(d)) stop("name conflict with PRKEY in xpose table. aborting...")
  if("PRKEY" %in% names(dorig)) stop("name conflict with PRKEY in original data. aborting...")

  d$PRKEY <- dORD
  dorig$PRKEY <- 1:nrow(dorig)

  d$INNONMEM <- TRUE

  dorig <- dorig[,c(setdiff(names(dorig),names(d)),"PRKEY")]

  d2 <- merge(dorig,d,all.x = TRUE)
  if(nrow(d2) != nrow(dorig)) stop("merge went wrong. debug")

  return(d2)
}

