get_psn_type <- function(cmd,types=getOption("psn.commands")){
  run_type <- sapply(types,function(run_type){
    any(grepl(paste0("^[^#]*\\b",run_type,"\\b.+$"),cmd))
  })
  types[run_type]
}

get_psn_cmd <- function(type,cmd){
  gsub(paste0("^.*(",type,".*)$"),"\\1",cmd)
}

#' Make nm object
#'
#' @param cmd character. system command to launch NONMEM (PsN)
#' @param psn_command character. Name of PsN command (optional)
#' @param shell_script_name character. Name of shell script for grid submission (optional)
#' @param shell_script_extn character. File extension shell script for grid submission (optional)
#' @param ctl_name character. Name of control file (optional)
#' @param run_id character or numeric. Run identifier (optional)
#' @param run_dir character or numeric. Run directory (optional)
#' @return object of class nm
#' @export
nm <- function(cmd,psn_command,
               shell_script_name,shell_script_extn="sh",
               ctl_name,run_id,run_dir){
  tidyproject::check_if_tidyproject()
  r <- list()
  class(r) <- "nm"
  r$cmd <- cmd

  if(!missing(shell_script_name)){
    if(!file.exists(from_models(shell_script_name)))
      stop("cannot find shell script in ",getOption("models.dir")," directory")
    ss_contents <- readLines(from_models(shell_script_name))
    message("reading shell script: ",shell_script_name)
    if(length(ss_contents)==0) stop(shell_script_name," is empty")
    cmd <- c(cmd,ss_contents)
  } else {
    match_string <- paste0("^.*\\b(.+\\.",shell_script_extn,")\\b.*")
    ss_file_name_detected <- grepl(match_string,cmd)
    if(ss_file_name_detected){
      shell_script_name <- gsub(match_string,"\\1",cmd)
      if(file.exists(from_models(shell_script_name))){
        message("reading shell script: ",shell_script_name)
        ss_contents <- readLines(from_models(shell_script_name))
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
    if(any(!file.exists(from_models(ctl_name))))
      stop(paste(ctl_name,collapse = ",")," do(es) not exist")
    r$ctl <- ctl_name
  }

  if(is.null(r$type)){
    matched_psn_commands <- unlist(sapply(getOption("psn.commands"),
                                          function(i) gsub2(paste0("^.*(",i,")\\b\\s.+$"),"\\1",cmd)))
    if(length(matched_psn_commands)==0)
      stop("couldn't infer psn command type.\nRerun with psn_command argument")
    if(length(matched_psn_commands)>1)
      stop("couldn't infer unique psn command type.\nRerun with psn_command argument")
    r$type <- matched_psn_commands
    message(paste("inferring run type :",r$type))
  }

  if(is.null(r$ctl)){
    subcmd <- gsub(paste0("^.*(",r$type,".*)$"),"\\1",cmd)
    matched_ctl <- gsub2(paste0("^.*(",getOption("model_file_stub"),"\\S+)\\s*.*$"),"\\1",cmd)
    r$ctl <- matched_ctl
  }

  if(missing(run_dir)){
    matched_run_dir <-
      gsub2(paste0("^.*-dir[a-z]*=\\s*(.*)\\b.*$"),"\\1",cmd)
    if(length(matched_run_dir)==0)
      stop("couldn't infer run directory type.\nRerun with run_dir argument")
    if(length(matched_run_dir)>1)
      stop("couldn't infer unique run directory type.\nRerun with run_dir argument")
    message(paste("inferring run directory :",r$run_dir))
    r$run_dir <- matched_run_dir
  }

  if(missing(run_id)){
    matched_run_id <- get_run_id(r$ctl[1])
    if(length(matched_run_id)==0)
      stop("couldn't infer run id type.\nRerun with run_id argument")
    r$run_id <- matched_run_id
  }

  if(!file.exists(from_models(r$ctl))) stop(paste("cannot find model file:",r$ctl))
  r$extn <- tools::file_ext(r$ctl)
  ## class for execute runs
  class(r) <- c(paste0("nm",r$type),class(r))

  if(r$type %in% "execute"){
    r$data_name <- data_name(file.path(tidyproject::models_dir(),r$ctl))
    if(r$extn %in% "mod") r$lst <- gsub("^(.*\\.).*$","\\1lst",r$ctl) else
      r$lst <- paste0(r$ctl,".lst")
    r$psn.lst <- file.path(r$run_dir,"psn.lst")
    r$psn.ext <- file.path(r$run_dir,"psn.ext")
  }
  ## TODO develop classes nmboot, nmvpc,....
  return(r)
}

print.nm <- function(x,...) str(x)

ctl_name <- function(run_id)
  paste0(getOption("model_file_stub"),run_id,getOption("model_file_extn"))

gsub2 <- function(pattern,replacement,x,...){
  match <- grepl(pattern,x,...)
  if(!match) return(character())
  gsub(pattern,replacement,x,...)
}

get_run_id <- function(ctl_name){
  match <- paste0("^.*",getOption("model_file_stub"),"(.*)\\.",getOption("model_file_extn"),".*$")
  gsub2(match,"\\1",ctl_name)
}



# data_name0 <- function(ctl){
#   s <- ctl
#   if(length(ctl)==1) {
#     if(file.exists(ctl)) s <- readLines(ctl) else stop("can't process argument")
#   }
#   data.row <- grep("^ *\\$DATA",s)
#   if(length(data.row)<1) stop("can't identify data row")
#   if(length(data.row)>1) stop("multiple data rows found")
#   s <- paste(s[data.row:length(s)],collapse = " ")
#   gsub("^ *\\$DATA\\s*([^ ]+).*$","\\1",s)
# }

#' Load nm object into memory from script
#'
# nm_load <- function(script){
#   script <- tidyproject::locate_file(script,getOption("scripts.dir"))
#   s <- readLines(script)
#   s <- rem_comment(s,"#")  ## this is simplistic but good enough for now
#   ## wont ignore a # in a string for example or an escaped #
#   s <- s[grepl("\\s*nm\\(\"",s)] ## detect nm(...) rows
#   if(length(s)==0) stop("no nm() lines detected")
#   message("running...")
#   message(paste(s,collapse="\n"))
#   eval(parse(text = s),envir = .GlobalEnv)
# }


#' Run NONMEM
#' @param ... objects of class nm
#' @param overwrite logical. Should run directory be overwritten (default=FALSE)
#' @export
run <- function(...,overwrite=FALSE){
  rl <- list(...)
  lapply(rl,function(r){
    ## if directory exists, and if it's definately a directory stop
    if(file.exists(r$dir) & !overwrite)if(file.info(r$dir)$isdir %in% TRUE) stop("run already exists. To rerun select overwrite=TRUE")
    if(!is.null(get("kill_run"))){
      get("kill_run")(r)
    }
    clean_run(r$run_id)
    system_nm(r$cmd)
  })
  invisible()
}

#' clean_run files
#'
#' @param run_id character or numeric. run identifier
#' @export
clean_run <- function(run_id){
  ## assumes ctrl file is run[run_id].mod and -dir=[run_id] was used
  unlink(out_files0(from_models(ctl_name(run_id))))
  unlink(from_models(run_id),recursive=TRUE) ## delete run directory
}

from_models <- function(x) file.path(getOption("models.dir"),x) ## change a path to be in models dir

out_files0 <- function(ctrl.file){ ## will get vector of $TABLE file names from control file.
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
