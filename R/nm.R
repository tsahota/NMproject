#' Make nm object
#'
#' @param cmd character. system command to launch NONMEM (PsN)
#' @param psn_command character. Name of PsN command (optional)
#' @param shell_script_name character. Name of shell script for grid submission (optional)
#' @param shell_script_extn character. File extension shell script for grid submission (optional)
#' @param ctl_name character. Name of control file (optional)
#' @param run_id character or numeric. Run identifier (optional)
#' @param run_dir character or numeric. Run directory (optional)
#' @param run_in character. Directory to execute. Default = getOption("models.dir")
#' @return object of class nm
#' @export
nm <- function(cmd,psn_command,
               shell_script_name,shell_script_extn="sh",
               ctl_name,run_id,run_dir,
               run_in=getOption("models.dir")){
  tidyproject::check_if_tidyproject()
  r <- list()
  class(r) <- "nm"

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
    subcmd <- gsub(paste0("^.*(",r$type,".*)$"),"\\1",cmd)
    matched_ctl <- gsub2(paste0("^.*(",getOption("model_file_stub"),"\\S+)\\s*.*$"),"\\1",cmd)
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
    if(length(matched_run_id)==0)
      stop("couldn't infer run id type.\nRerun with run_id argument",call. = FALSE)
    r$run_id <- matched_run_id
  } else r$run_id <- run_id

  if(!file.exists(r$ctl)) stop(paste("cannot find model file:",r$ctl),call. = FALSE)
  ## class for execute runs
  class(r) <- c(paste0("nm",r$type),class(r))

  r$input <- input_files(run_type = r$type,ctl_name = r$ctl)
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
  
  overlapped_run_dir_entries <- 
    match_info$entry[match_info$match_run_dir & match_info$match_run_in &
                       !(match_info$entry %in% matched_entry)]
  
  overlapped_output_entries <- unique(c(overlapped_output_entries,
                                        overlapped_run_dir_entries))
  
  if(length(overlapped_output_entries)>0)
    stop("Outputs overlap with entries: ",
         paste(overlapped_output_entries,collapse=","),
         "\nView runs with: show_runs()",
         "\nDelete old runs with: delete_nm(entry)",call. = FALSE)

  if(length(matched_entry)==0) {
    nmdb_add_entry(r)
  } else if(length(matched_entry)==1) {
    delete_nm(matched_entry)
    message("Overwriting database entry: ",matched_entry)
    nmdb_add_entry(r,matched_entry,silent=TRUE)
  } else stop("Matched more than one database entry. Debug")
  union_write(".gitignore",getOption("nmproj_gitignore"))
  return(r)
}

union_write <- function (path, new_lines){
  if (file.exists(path))
    lines <- readLines(path, warn = FALSE) else
      lines <- character()
    all <- union(lines, new_lines)
    writeLines(all, path)
}

#' delete run from database
#' @param entry numeric. entry name to delete
#' @export
delete_nm <- function(entry){
  query <- paste('DELETE FROM runs WHERE entry ==',entry)
  my_db <- DBI::dbConnect(RSQLite::SQLite(), "runs.sqlite")
  DBI::dbExecute(my_db, query)
  DBI::dbDisconnect(my_db)
}

nmdb_match_info <- function(r){
  if(!file.exists("runs.sqlite"))
    return(data.frame(entry=numeric(),
                      match_type=logical(),
                      match_run_in=logical(),
                      match_run_dir=logical(),
                      match_ctl = logical(),
                      overlap_outputs=logical()))

  my_db <- DBI::dbConnect(RSQLite::SQLite(), "runs.sqlite")
  tryCatch({
    d <- DBI::dbGetQuery(my_db, 'SELECT * FROM runs')
    DBI::dbDisconnect(my_db)
    dproposal <- nmdb_make_db_row(r)

    ## objectives
    ## if a run is of the same type, run_dir, and outputfiles,
    ans <- list()

    ans$entry <- d$entry
    ans$match_type <- d$type %in% r$type
    ans$match_run_dir <- d$run_dir %in% r$run_dir
    ans$match_run_in <- d$run_in %in% r$run_in
    ans$match_ctl <- d$ctl %in% r$ctl

    dependent_run_dir <- sapply(d$run_dir,function(i)grepl(paste0("^",i,"$"),r$run_dir))
    ans$dependent_run_dir <- dependent_run_dir & !ans$match_run_dir

    overlap_outputs <- sapply(strsplit(d$output_files,","),function(out_filesi){
      length(intersect(out_filesi,unlist(r$output)))>0
    })
    ans$overlap_outputs <- overlap_outputs

    ans <- as.data.frame(ans)
    return(ans)
  },
  error=function(e) {
    if(DBI::dbIsValid(my_db)) DBI::dbDisconnect(my_db)
    stop(e)
  })
}

nmdb_make_db_row <- function(r){
  if(r$type %in% "execute") r$input$data <- NULL
  data.frame(object=I(list(serialize(r,NULL))),
             ctl = r$ctl,
             description=r$description,
             type=r$type,
             input_files=paste(unlist(r$input),collapse=","),
             output_files=paste(unlist(r$output),collapse=","),
             run_in=r$run_in,
             run_dir=r$run_dir,
             cmd = r$cmd)
}

nmdb_printable_db <- function(d){
  d$object <- NULL
  d$output_files <- NULL
  d$lst_exists <- NULL
  d$stop_time_reached <- NULL
  d$input_files[!d$input_files %in% d$ctl]
  d$input_files <- lapply(strsplit(d$input_files,","),function(char_vec){
    paste(basename(char_vec),collapse=" ")
  })
  d$run_in <- NULL
  ## include if statements for columns no in the db
  if("outputs_present" %in% names(d))
    d$outputs_present <- signif(d$outputs_present,2)
  for(i in names(d)[!names(d) %in% "entry"]) d[,i] <- as.character(d[,i]) ## convert everything to characters
  names(d) <- gsub("_"," ",names(d))
  d
}

nmdb_add_entry <- function(r,entry=NULL,silent=FALSE){
  new_db_flag <- !file.exists("runs.sqlite")
  dnew <- nmdb_make_db_row(r)
  my_db <- DBI::dbConnect(RSQLite::SQLite(), "runs.sqlite")
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
    invisible()
  },
  error=function(e){
    if(DBI::dbIsValid(my_db)) DBI::dbDisconnect(my_db)
    if(new_db_flag) unlink("runs.sqlite",force = TRUE)
    stop(e)
  })
}

nmdb_get <- function(readable=FALSE){
  my_db <- DBI::dbConnect(RSQLite::SQLite(), "runs.sqlite")
  tryCatch({
    d <- DBI::dbGetQuery(my_db, 'SELECT * FROM runs')
    if(readable) d <- nmdb_printable_db(d)
    DBI::dbDisconnect(my_db)
    d <- d[order(d$entry),]
    return(d)
  },
  error=function(e) {
    if(DBI::dbIsValid(my_db)) DBI::dbDisconnect(my_db)
    stop(e)
  })
}

#' Show table of runs in database
#'
#' @param ... arguments to pass to nmdb_get
#' @export
show_runs <- function(...) nmdb_get(readable = TRUE,...)

#' Extract nm object from data.base
#' @param entry numeric. Database ID of nm object
#' @export

extract_nm <- function(entry){
  my_db <- DBI::dbConnect(RSQLite::SQLite(), "runs.sqlite")
  tryCatch({
    d <- DBI::dbGetQuery(my_db, 'SELECT * FROM runs')
    DBI::dbDisconnect(my_db)
    d <- d[d$entry==entry, ]
    if(nrow(d)>1) stop("More than one entry found",call. = FALSE)
    if(nrow(d)==0) stop("No entry found",call. = FALSE)
    r <- unserialize(d$object[[1]])
    return(r)
  },
  error=function(e){
    if(DBI::dbIsValid(my_db)) DBI::dbDisconnect(my_db)
    stop(e)
  })
}

#' @export
print.nm <- function(x,...) utils::str(x)

#' Edit control file
#'
#' @param r object of class nm
#' @export
ctl <- function(r) {
  utils::file.edit(r$ctl)
}

gsub2 <- function(pattern,replacement,x,...){
  match <- grepl(pattern,x,...)
  if(!match) return(character())
  gsub(pattern,replacement,x,...)
}

get_run_id <- function(ctl_name){
  match <- paste0("^.*",getOption("model_file_stub"),"(.*)\\..*$")
  gsub2(match,"\\1",ctl_name)
}

#' Run NONMEM
#' @param ... objects of class nm
#' @param overwrite logical. Should run directory be overwritten (default=FALSE)
#' @param delete_dir logical. NA (default - directory will be deleted if no dependencies exists)
#' TRUE or FALSE. Should run_dir be deleted.
#' @param wait logical (default=FALSE). Should R wait for run to finish.
#' Default can be changed with  wait_by_default() function
#' @export
run <- function(...,overwrite=.sso_env$run_overwrite,delete_dir=c(NA,TRUE,FALSE),wait=.sso_env$wait){
  tidyproject::check_if_tidyproject()
  rl <- list(...)
  lapply(rl,function(r){
    ## if directory exists, and if it's definately a directory stop
    if(file.exists(r$run_dir) & !overwrite)if(file.info(r$run_dir)$isdir %in% TRUE) stop("run already exists. To rerun select overwrite=TRUE\n  or use the switch overwrite_default(TRUE)",call.=FALSE)
    if(!is.null(getOption("kill_run"))) getOption("kill_run")(r)
    clean_run(r,delete_dir=delete_dir[1])
    system_nm(cmd = r$cmd, dir = r$run_in, wait = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    message(paste0("Running: ",r$type,":",r$ctl))
  })
  if(wait) wait_for_finished(...)
  invisible()
}

#' wait for a run to finish
#'
#' @param ... objects of class nm
#' @export
wait_for_finished <- function(...){
  rl <- list(...)
  message("Waiting for jobs:\n",paste(sapply(rl,function(i)paste0(" ",i$type,":",i$ctl)),collapse = "\n"))

  i <- 0
  while(length(rl)>0){
    j <- (i %% length(rl))+1
    r <- rl[[j]]
    ######################################
    ## apply finishing test(s) here

    finished <- nm_steps_finished(r)
    last_update <- last_modified(r)

    if(finished){ ## check again in 5 seconds
      if(r$type != "execute") Sys.sleep(5) else Sys.sleep(1)
      finished2 <- nm_steps_finished(r)
      last_update2 <- last_modified(r)
      ## if it is still finished and last_update is still the same then finish
      finished <- finished2 & identical(last_update,last_update2)
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

run_status <- function(r){ # for db
  status <- "not run"
  if(file.exists(r$run_dir)) {
    status <- "dir created"
    execution_dirs <- dirname(dir(r$run_dir,
                                  pattern = "psn\\.mod$",
                                  recursive = TRUE,
                                  full.names = TRUE))
    execution_dirs <- unique(execution_dirs)
    lst_names <- file.path(execution_dirs,"psn.lst")

    if(length(execution_dirs) > 0){
      status <- "running"
      ## check these all exist
      statusi <- sapply(lst_names,function(lst_name){
        if(!file.exists(lst_name)) return("running")
        lst <- try(readLines(lst_name),silent = TRUE)
        if(inherits(lst,"try-error")) return("running")
        lst <- lst[max(1,(length(lst)-5)):length(lst)]
        stopped <- any(grepl("Stop Time:",lst))
        psn_error <- file.exists(file.path(dirname(lst_name),"psn_nonmem_error_messages.txt"))
        if(psn_error) return("error")
        if(stopped) return("finished")
        return("running")
      })
      running <- length(which(statusi %in% "running"))
      finished <- length(which(statusi %in% "finished"))
      error <- length(which(statusi %in% "error"))
      status <- paste0("running:",running," finished:",finished," errors:",error)
    }
  }
  return(status)
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

wait_default <- function(x) .sso_env$wait <- x

#' Should run() overwrite previously run jobs
#'
#' @param x logical. TRUE means run() will overwrite previous runs by default,
#' @export

overwrite_default <- function(x) .sso_env$run_overwrite <- x

#' Should run() work in non interactive mode
#'
#' @export
non_interactive_mode <- function() {
  overwrite_default(TRUE)
  wait_default(TRUE)
}

#' Should run() work in interactive mode
#'
#' @export
interactive_mode <- function() {
  overwrite_default(FALSE)
  wait_default(FALSE)
}

#' @export
nm_tran.nm <- function(x){
  tidyproject::check_if_tidyproject()
  nm_tran.default(x$ctl)
}

#' clean_run files
#'
#' @param r character or numeric. run identifier
#' @param delete_dir logical. NA (default), TRUE or FALSE. Should run_dir be deleted.
#' @export
clean_run <- function(r,delete_dir=c(NA,TRUE,FALSE)){
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
    unlink(r$run_dir,recursive=TRUE)
  }
  if(TRUE %in% delete_dir) unlink(r$run_dir,recursive=TRUE) ## delete run directory
}

ctl_out_files <- function(ctl_file){ ## will get vector of $TABLE file names from control file.
  if(!file.exists(ctl_file)) stop(paste(ctl_file, "doesn't exist"))
  dir0 <- dir(dirname(ctl_file))

  s0 <- readLines(ctl_file)
  s <- grep("FILE *= *[A-Za-z0-9_\\-\\.]+",s0,value=TRUE)
  table.files <- gsub(".*FILE *= *([A-Za-z0-9_\\-\\.]+) *.*$","\\1",s)
  #table.files <- dir0[dir0%in%table.files]

  stub <- basename(ctl_file)
  stub <- gsub("(.+)\\.\\w+$","\\1",stub)

  out.files <- dir0[grepl(paste(stub,"\\.",sep=""),dir0)]
  out.files <- out.files[!grepl("scm",out.files)]
  out.files <- out.files[!out.files%in%basename(ctl_file)]

  out.files <- c(table.files,out.files)
  out.files
}

input_files <- function(run_type,ctl_name){
  r <- list()
  r$ctl <- ctl_name
  if(run_type %in% "execute"){
    r$data_name <- data_name(ctl_name)
  }
  return(r)
}

output_files <- function(run_in,run_type,run_dir,ctl_name){
  r <- list()
  if(run_type %in% "execute"){
    extn <- tools::file_ext(ctl_name)
    if(extn %in% "mod") r$lst <- gsub("^(.*\\.).*$","\\1lst",ctl_name) else
      r$lst <- paste0(ctl_name,".lst")
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

