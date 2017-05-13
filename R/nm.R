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
      stop("cannot find shell script in ",r$run_in," directory")
    ss_contents <- readLines(file.path(r$run_in,shell_script_name))
    message("reading shell script: ",shell_script_name)
    if(length(ss_contents)==0) stop(shell_script_name," is empty")
    cmd <- c(cmd,ss_contents)
  } else {
    match_string <- paste0("^.*\\b(.+\\.",shell_script_extn,")\\b.*")
    ss_file_name_detected <- grepl(match_string,cmd)
    if(ss_file_name_detected){
      shell_script_name <- gsub(match_string,"\\1",cmd)
      if(file.exists(file.path(r$run_in,shell_script_name))){
        message("reading shell script: ",shell_script_name)
        ss_contents <- readLines(file.path(r$run_in,shell_script_name))
        if(length(ss_contents)==0) stop(shell_script_name," is empty")
        cmd <- c(cmd,ss_contents)
      }
    }
  }
  ## cmd is now set
  cmd <- paste(cmd,collapse = " ")

  ## infer things that havne't been specified.
  ## Need to know:
  ##  run type (psn_command)
  ##  run_dir (-dir or run_dir), ctl - everything else will follow

  if(!missing(psn_command)) r$type <- psn_command
  if(!missing(ctl_name)) {
    if(any(!file.exists(file.path(r$run_in,ctl_name))))
      stop(paste(ctl_name,collapse = ",")," do(es) not exist")
    r$ctl <- file.path(r$run_in,ctl_name)
  }

  if(is.null(r$type)){
    matched_psn_commands <- unlist(sapply(getOption("psn.commands"),
                                          function(i) gsub2(paste0("^.*(",i,")\\b\\s.+$"),"\\1",cmd)))
    matched_psn_commands <- as.character(matched_psn_commands)
    if(length(matched_psn_commands)==0)
      stop("couldn't infer psn command type.\nRerun with psn_command argument")
    if(length(matched_psn_commands)>1)
      stop("couldn't infer unique psn command type.\nRerun with psn_command argument")
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
      gsub2(paste0("^.*-dir[a-z]*=\\s*(.*)\\b.*$"),"\\1",cmd)
    if(length(matched_run_dir)==0)
      stop("couldn't infer run directory type.\nRerun with run_dir argument")
    if(length(matched_run_dir)>1)
      stop("couldn't infer unique run directory type.\nRerun with run_dir argument")
    message(paste("inferring run directory :",matched_run_dir))
    r$run_dir <- file.path(r$run_in,matched_run_dir)
  } else r$run_dir <- run_dir

  if(missing(run_id)){
    matched_run_id <- get_run_id(r$ctl[1])
    if(length(matched_run_id)==0)
      stop("couldn't infer run id type.\nRerun with run_id argument")
    r$run_id <- matched_run_id
  } else r$run_id <- run_id

  if(!file.exists(r$ctl)) stop(paste("cannot find model file:",r$ctl))
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

  if(length(overlapped_output_entries)>0)
    stop("Outputs overlap with entries: ",
         paste(overlapped_output_entries,collapse=","),
         "\nView runs with: show_runs()",
         "\nDelete old runs with: delete_run(entry)")

  if(length(matched_entry)==0) {
    nmdb_add_entry(r)
  } else if(length(matched_entry)==1) {
    delete_run(matched_entry)
    message("Overwriting database entry: ",matched_entry)
    nmdb_add_entry(r,matched_entry,silent=TRUE)
  } else stop("Matched more than one database entry. Debug")

  return(r)
}

#' delete run from database
#' @param entry numeric. entry name to delete
#' @export
delete_run <- function(entry){
  query <- paste('DELETE FROM runs WHERE entry ==',entry)
  my_db <- DBI::dbConnect(RSQLite::SQLite(), "runs.sqlite")
  DBI::dbExecute(my_db, query)
  DBI::dbDisconnect(my_db)
}

nmdb_match_info <- function(r){
  match_rows <- c("type","run_dir")
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

    dependent_run_dir <- sapply(d$run_dir,function(i)grepl(paste0("^",i),r$run_dir))
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
             input_files=paste(unlist(r$input),collapse=","),
             output_files=paste(unlist(r$output),collapse=","),
             description=r$description,
             type=r$type,
             run_in=r$run_in,
             run_dir=r$run_dir,
             ctl = r$ctl,
             cmd = r$cmd)
}

nmdb_printable_db <- function(d){
  d$object <- NULL
  d$output_files <- NULL
  d$input_files <- lapply(strsplit(d$input_files,","),function(char_vec){
    paste(basename(char_vec),collapse=" ")
  })
  d$ctl <- NULL
  ## include if statements for columns no in the db
  if("outputs_present" %in% names(d))
    d$outputs_present <- signif(d$outputs_present,2)
  for(i in names(d)) d[,i] <- as.character(d[,i]) ## convert everything to characters
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
    if(nrow(d)>1) stop("More than one entry found")
    if(nrow(d)==0) stop("No entry found")
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

gsub2 <- function(pattern,replacement,x,...){
  match <- grepl(pattern,x,...)
  if(!match) return(character())
  gsub(pattern,replacement,x,...)
}

get_run_id <- function(ctl_name){
  match <- paste0("^.*",getOption("model_file_stub"),"(.*)\\.",getOption("model_file_extn"),".*$")
  gsub2(match,"\\1",ctl_name)
}

#' Run NONMEM
#' @param ... objects of class nm
#' @param overwrite logical. Should run directory be overwritten (default=FALSE)
#' @param delete_dir logical. NA (default - directory will be deleted if no dependencies exists)
#' TRUE or FALSE. Should run_dir be deleted.
#' @export
run <- function(...,overwrite=FALSE,delete_dir=c(NA,TRUE,FALSE)){
  rl <- list(...)
  lapply(rl,function(r){
    ## if directory exists, and if it's definately a directory stop
    if(file.exists(r$run_dir) & !overwrite)if(file.info(r$run_dir)$isdir %in% TRUE) stop("run already exists. To rerun select overwrite=TRUE")
    if(!is.null(getOption("kill_run"))){
      getOption("kill_run")(r)
    }
    clean_run(r,delete_dir=delete_dir[1])
    system_nm(cmd = r$cmd,dir = r$run_in)
  })
  invisible()
}

#' @export
nm_tran.nm <- function(x){
  nm_tran.default(x$ctl)
}

#' clean_run files
#'
#' @param r character or numeric. run identifier
#' @param delete_dir logical. NA (default), TRUE or FALSE. Should run_dir be deleted.
#' @export
clean_run <- function(r,delete_dir=c(NA,TRUE,FALSE)){
  ## assumes ctrl file is run[run_id].mod and -dir=[run_id] was used
  unlink(ctl_out_files(r$ctl))
  delete_dir <- delete_dir[1]
  if(is.na(delete_dir)){
    match_info <- nmdb_match_info(r)
    matched_entry <- match_info$entry[match_info$dependent_run_dir]
    if(length(matched_entry)>0) {
      stop("Dependent run(s) found: ",paste(matched_entry,collapse=","),
           "\nRerun with delete_dir=TRUE or delete_dir=FALSE or remove dependent run")
    }
  }
  if(TRUE %in% delete_dir) unlink(r$run_dir,recursive=TRUE) ## delete run directory
}

ctl_out_files <- function(ctrl.file){ ## will get vector of $TABLE file names from control file.
  if(!file.exists(ctrl.file)) stop(paste(ctrl.file, "doesn't exist"))
  dir0 <- dir(dirname(ctrl.file))

  s0 <- readLines(ctrl.file)
  s <- grep("FILE *= *[A-Za-z0-9_\\-\\.]+",s0,value=TRUE)
  table.files <- gsub(".*FILE *= *([A-Za-z0-9_\\-\\.]+) *.*$","\\1",s)
  table.files <- dir0[dir0%in%table.files]

  stub <- basename(ctrl.file)
  stub <- gsub("(.+)\\.\\w+$","\\1",stub)

  out.files <- dir0[grepl(paste(stub,"\\.",sep=""),dir0)]
  out.files <- out.files[!grepl("scm",out.files)]
  out.files <- out.files[!out.files%in%basename(ctrl.file)]

  out.files <- c(table.files,out.files)
  out.files
}

input_files <- function(run_type,ctl_name){
  r <- list()
  if(run_type %in% "execute"){
    r$ctl <- ctl_name
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


