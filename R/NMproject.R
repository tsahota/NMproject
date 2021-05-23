set_nm_opts <- function(){
  ## Internal function: will set all global variables
  ## put as much AZ specific code in here.
  ## add internal code library to code library
  
  if(is.null(getOption("system_cmd"))) options(system_cmd=function(cmd,...) {
    if(.Platform$OS.type == "windows") shell(cmd,...) else system(cmd,...)
  })
  if(is.null(getOption("system_nm"))) options(system_nm=function(cmd, ...) system_nm_default(cmd, ...))
  if(is.null(getOption("quiet_run"))) options(quiet_run=TRUE)
  if(is.null(getOption("intern"))) options(intern=FALSE)

  if(is.null(getOption("available_nm_types")))
    options(available_nm_types = c("SIZES","PROB","INPUT","DATA","SUB","MODEL","PK","DES","PRED","ERROR",
                                   "THETA","OMEGA","SIGMA","EST","SIM","COV","TABLE"))

  if(is.null(getOption("nm_default_dirs"))) options(nm_default_dirs = list(models = "Models", 
                                                                           scripts = "Scripts",
                                                                           results = "Results",
                                                                           "SourceData",
                                                                           "DerivedData"))
  
  if(is.null(getOption("kill_job"))) options(kill_job=identity)

  if(is.null(getOption("nm.overwrite_behaviour"))) options(nm.overwrite_behaviour="ask")
  if(is.null(getOption("nm.force_render"))) options(nm.force_render=FALSE)
  if(is.null(getOption("nm.cmd_default"))) options(nm.cmd_default = "execute {ctl_name} -dir={run_dir}")
  
  if(is.null(getOption("nmtran_exe_path"))) options(nmtran_exe_path=find_nm_tran_path(warn = FALSE))
  
  if(is.null(getOption("code_library_path")))
    options(code_library_path=system.file("extdata", "CodeLibrary", package = "NMproject"))
  
  
}


#' Setup analysis subdirectories
#' 
#' This allows organisations/individuals with their own directory to customize their directory structure
#' 
#' @param dir_list optional named list or vector. Names "scripts_dir" and "models_dir" must be present.  Rest can be unnamed
#' 
#' @return
#' if `dir_list` is missing, will return value of `getOption("nm_default_dirs")` otherwise will set option `nm_default_dirs`
#' @examples
#' \dontrun{
#' 
#' nm_default_dirs()
#' nm_default_dirs(list(models = "Models", 
#'                      scripts = "Scripts",
#'                      results = "Results",
#'                      "SourceData",
#'                      "Data"))
#' }
#' @export
nm_default_dirs <- function(dir_list){
  if(missing(dir_list)) return(getOption("nm_default_dirs"))
  if(!missing(dir_list)){
    ## now assume we're setting
    validate_dir_list(dir_list)
    options(nm_default_dirs = dir_list)
  }
}

validate_dir_list <- function(dir_list){
  if(!"scripts" %in% names(dir_list)) 
    stop("one entry of the directory needs the name \"scripts\"", call. = FALSE)
  if(!"models" %in% names(dir_list)) 
    stop("one entry of the directory needs the name \"models\"", call. = FALSE)  
}

set_default_dirs_in_rprofile <- function(path = ".Rprofile", dir_list = nm_default_dirs()){
  
  validate_dir_list(dir_list)
  if(!file.exists(path)) file.create(path)
  
  start_flag <- "# nm_default_dir modify - start"
  end_flag <- "# nm_default_dir modify - end"
  
  current_lines <- readLines(path)
  
  start_flag_hit <- grep(start_flag, current_lines)
  end_flag_hit <- grep(end_flag, current_lines)
  
  error_msg <- "something wrong with .Rprofile, remove previous nm_default_dir lines and try again"
  
  if(!length(start_flag_hit) %in% c(0,1)) stop(error_msg, call. = FALSE)
  if(!length(end_flag_hit) %in% c(0,1)) stop(error_msg, call. = FALSE)
  total_hits <- length(start_flag_hit) + length(start_flag_hit)
  if(!total_hits %in% c(0,2)) stop(error_msg, call. = FALSE)
  ## hits validated

  if(total_hits == 2){  ## existing statement detected
    ## remove lines
    remove_lines <- start_flag_hit:(end_flag_hit +1)
    current_lines <- current_lines[!seq_along(current_lines) %in% remove_lines]
    writeLines(current_lines, path)
    return(set_default_dirs_in_rprofile(path = path, dir_list = dir_list))
  }  
  # now can just set
  
  txt <- paste0("options(nm_default_dirs=", paste(deparse(dir_list), collapse = ""), ")")
  txt <- gsub("\\s+", " ", txt)
  txt <- c(start_flag, txt, end_flag, "", current_lines)
  writeLines(txt, path)
  usethis::ui_done("setting {usethis::ui_path('nm_default_dirs')} in {usethis::ui_path('.Rprofile')}")
  
}

#' Get a default directory
#' 
#' @param name character. Directory type
#' @param ... not used yet
#' 
#' @examples 
#' nm_default_dir("scripts")
#' nm_default_dir("models")
#' nm_default_dir("results")
#' @export

nm_default_dir <- function(name = c("scripts", "models", "results"), ...){
  if(missing(name)) stop("need argument")
  name < match.arg(name)
  nm_default_dirs()[[name]]
}

#' Generic execute command for SGE grids
#'
#' Character to be used with the [cmd()] function to launch a
#' parallelised job on SGE
#'
#' @details Requires `cores` and `parafile` fields to be set.
#' @seealso [nm_getsetters()]
#'
#' @examples
#' \dontrun{
#'
#' m1 <- m1 %>%
#'   cmd(sge_parallel_execute) %>%
#'   parafile("/opt/NONMEM/nm75/run/mpilinux8.pnm") %>%
#'   cores(8) %>%
#'   run_nm()
#'
#' m2 <- m1 %>% child("m2") ## inherits same command as above
#'
#' sge_parallel_execute ## view the character to see how psn interfaces with SGE
#'
#' }
#' @export
sge_parallel_execute <- "execute -run_on_sge -parafile={parafile} -sge_prepend_flags='-pe orte {cores} -V' {ctl_name} -dir={run_dir} -nodes={cores}"



#' Get/set nm_tran_command
#'
#' `nm_tran` needs the location of NMTRAN.exe on your system.  This
#' is guessed on package load, assuming PsN is on the $PATH environmental
#' variable, if this is not the case, then you can manually set the path and
#' command used.
#'
#' @param text Optional character. If specified will set `nm_tran_command`.
#'
#' @details `text` can just be the path to NMTRAN.exe.  In this case
#'   `nm_tran_command` will use the format `/path/to/NMTRAN.exe < {ctl_name}` to
#'   launch NMTRAN.exe where `{ctl_name}` is the name of the control file `text`
#'   can use the `{ctl_name}` glue field however for more complicated commands.
#'   See examples
#'
#'   Set this up either at the beginning of your script, in your `.Rprofile` or
#'   for all users in `Rprofile.site`
#'
#' @return if `text` is missing will get and return the current NMTRAN command
#' @seealso [find_nm_tran_path()]
#' @examples
#' \dontrun{
#'
#'   # the following two are equivalent
#'   nm_tran_command("/opt/NONMEM/nm75/tr/NMTRAN.exe")
#'   nm_tran_command("/opt/NONMEM/nm75/tr/NMTRAN.exe < {ctl_name}")
#'
#' }
#' @export
nm_tran_command <- function(text){
  if(missing(text)){
    if(!is.null(getOption("nmtran_command"))) return(getOption("nmtran_command"))
    if(!is.null(getOption("nmtran_exe_path"))) return(paste(getOption("nmtran_exe_path"),"< {ctl_name}"))
    return(NULL)
  }
  ## set text
  options(nmtran_command = text)
  invisible()
}

#' Default system_nm
#'
#' Not intended to be used directly in most cases.
#'
#' @param cmd Character. system call.
#' @param ... Additional args to be passed to system.
#' @export

system_nm_default <- function(cmd, ...) {
  if(.Platform$OS.type == "windows"){
    local_env_vars <- Sys.getenv()
    stdout_unit_vars <- local_env_vars[grepl("STDOUT_UNIT|STDERR_UNIT",names(local_env_vars))]
    for(i in seq_along(stdout_unit_vars)){
      Sys.unsetenv(names(stdout_unit_vars)[i])
    }
    on.exit({
      if(length(stdout_unit_vars)>0)
        do.call(Sys.setenv,as.list(stdout_unit_vars))
    })
    args <- list(...)
    if(!"wait" %in% names(args)) wait <- FALSE else wait <- args$wait
    if(wait==FALSE){
      shell(paste("START CMD /C",cmd),...)
    } else {
      shell(cmd,...)
    }
  } else system(cmd,...)
}


#' System command for NONMEM execution
#'
#' @param cmd character. command to send to shell
#' @param dir character. directory to run command in
#' @param ... other arguments passed to system command
#' @export
system_nm <- function(cmd,dir=nm_default_dir("models"),...){
  if(is.null(dir) | !file.exists(dir)) dir <- "."
  if(file.exists(dir)) {currentwd <- getwd(); setwd(dir) ; on.exit(setwd(currentwd))} else
    stop(paste0("Directory \"",dir,"\" doesn't exist."))
  getOption("system_nm")(cmd,...)
}


#' Kill cluster job
#' 
#' Requires setting "kill_job" `option()`
#'
#' @param m nm object 
#' @export
kill_job <- function(m){
  getOption("kill_job")(m)
}

#' Overwrite behaviour of NMproject
#' 
#' Requires setting "nm.overwrite_behaviour" `option()`
#'
#' @param txt character either "run", "stop", "skip" 
#' @export
overwrite_behaviour <- function(txt = c("ask", 
                                        "overwrite", 
                                        "stop", 
                                        "skip")){
  if(missing(txt)){
    return(getOption("nm.overwrite_behaviour"))
  }
  txt <- match.arg(txt)
  options(nm.overwrite_behaviour = txt)
  
  # if(txt %in% "ask") message("ask: NMproject will ask before overwriting old files")
  # if(txt %in% "overwrite") message("overwrite: NMproject will overwrite previous runs without prompt")
  # if(txt %in% "stop") message("stop: NMproject will stop R execution with an error instead of running NONMEM")
  # if(txt %in% "skip") message("skip: NMproject will not run NONMEM")
  return(invisible())
}

#' @export
.overwrite_behaviour <- dplyr::tibble(
  txt = c("ask", "overwrite", "stop", "skip"),
  description =   c("ask before overwrite (default)",
                    "overwrite all",
                    "no overwriting (stop with error)",
                    "no overwriting (skip, no error)")
)

