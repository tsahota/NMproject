#' NONMEM interface with code management
#'
#' The NMproject an inteface to NONMEM (via PsN) and a package to manage code in a modelling project
#'
#' @section Strategy:
#'
#' Assume access to NONMEM
#'
#' Assume access to PsN (or PsN wrapper) via command line - new NONMEM methodology will is being developed there
#'
#' Compatible with git
#'
#' @section Requirements:
#' Controls stream name: \code{runXX.mod}
#'
#' Execution in numbered directories: \code{execute run55.mod -dir=55}
#'
#' @section Options:
#' \code{scripts.dir} = names of the "scripts" directory in the project
#'
#' \code{models.dir} = names of the "models" directory in the project
#'
#' \code{git.exists} = \code{TRUE} if git is installed (only works on unix currently)
#'
#' \code{nmtran_exe_path} = path to nm_tran file
#'
#'
#' @examples
#' \dontrun{
#' new_project("~/AZDXXXX/PKAE1")          ## creates new_project
#' library(NMproject,lib="ProjectLibrary") ## from project load local NMproject package
#' update_project_package()                ## update local NMproject package
#' code_library()                          ## display summary of code_library
#' copy_script("nm.log.R")                 ## copy_script from code_library to project
#' preview("nm.log.R")                     ## preview file in code_library
#'
#' copy_control("ADVAN2.mod","run1.mod")   ## copy template from code_library
#' copy_control("run1.mod","run2.mod")     ## create run2.mod from run1.mod
#' new_script("test.R")                    ## Creates empty test.R
#' copy_script("output.data.R")            ## copies from code_library
#' copy_script("../pathto/script.R")       ## copies from other location
#'
#' m1 <- nm("qpsn -m -c auto -t 1440 -- execute run1.mod -dir=1") ## create object
#' nm_tran(m1)              ## run nm_tran step only
#' run(m1)                  ## run model
#' lst(m1)                  ## View lst file (may hit Rstudio's size limit)
#' (on linux terminal) tail -f 55/NM_run1/OUTPUT on command line ## tail output
#' monitor(m1)              ## monitor run
#' run_record(m1)           ## get table of parameters
#' run_record(m1,m2)        ## get table of parameters (multiple runs)
#' summary(m1)              ## run psns sumo command
#'
#' nm_load("nm.log.R")      ## Bring all runs in nm.log.R into memory
#'
#' squ()                    ## see queued runs
#' }
#' @docType package
#' @name NMproject

NULL

#' Set NMproject options
set_nm_opts <- function(){
  ## Internal function: will set all global variables
  ## put as much AZ specific code in here.
  ## add internal code library to code library

  #if(is.null(getOption("code_library_path"))) options(code_library_path=c(system.file("extdata/CodeLibrary",package="NMproject")))
  ## if not null, leave alone

  if(is.null(getOption("psn.commands")))
    options(psn.commands=
              c("boot_scm","bootstrap","cdd","crossval","execute","extended_grid","frem","gls",
                "lasso","llp","mcmp","mimp","nca","npc","parallel_retries","pvar","randtest","rawresults","runrecord",
                "scm","se_of_eta","sir","sse","sumo","update","update_inits","vpc","xv_scm"))

  if(is.null(getOption("system_nm"))) options(system_nm=function(cmd, ...) system_nm_default(cmd, ...))
  if(is.null(getOption("quiet_run"))) options(quiet_run=TRUE)
  if(is.null(getOption("intern"))) options(intern=FALSE)

  if(is.null(getOption("model_file_stub"))) options(model_file_stub="run")
  if(is.null(getOption("model_file_extn"))) options(model_file_extn="mod")
  if(is.null(getOption("available_nm_types")))
    options(available_nm_types = c("SIZES","PROB","INPUT","DATA","SUB","MODEL","PK","DES","PRED","ERROR",
                                   "THETA","OMEGA","SIGMA","EST","SIM","COV","TABLE"))


  models.ignore <- file.path(getOption("models.dir"),c("*tab*",
                                                       "*.phi",
                                                       "*.ext",
                                                       "*.cov",
                                                       "*.coi",
                                                       "*.cov",
                                                       "*.cor",
                                                       "*.lst",
                                                       "*/",
                                                       "parafile*"))

  if(is.null(getOption("nmproj_gitignore"))) options(nmproj_gitignore=c(models.ignore,
                                                                        "Results"))

  if(is.null(getOption("run_overwrite"))) options(run_overwrite=FALSE)
  if(is.null(getOption("wait"))) options(wait=FALSE)
  if(is.null(getOption("kill_job"))) options(kill_job=identity)

  if(is.null(getOption("nm.overwrite_behaviour"))) options(nm.overwrite_behaviour="ask")
  if(is.null(getOption("nm.force_render"))) options(nm.force_render=FALSE)
  
  if(is.null(getOption("nmtran_exe_path"))) options(nmtran_exe_path=find_nm_tran_path(warn = FALSE))
  
  if(is.null(getOption("code_library_path"))) 
    options(code_library_path=system.file("extdata", "CodeLibrary", package = "NMproject"))
  
}

#' Generic execute command for SGE grids
#' 
#' @details requires \code{cores} and \code{parafile} fields to be set
#' @seealso \code{\link{nm_getsetters}}
#' 
#' @export
sge_parallel_execute <- "execute -run_on_sge -parafile={parafile} -sge_prepend_flags='-pe orte {cores} -V' {ctl_name} -dir={run_dir} -nodes={cores}"


#' Find location of NONMEM
#' 
#' @param name character name of nonmem installation (according PsN)
#' 
#' @export
find_nm_install_path <- function(name = "default"){

  nm_versions <- try(system_nm("psn -nm_versions", intern = TRUE, ignore.stderr = TRUE, wait = TRUE),
                     silent = TRUE)
  if(inherits(nm_versions, "try-error")) stop("can't find nonmem installation")
  
  nm_version <- nm_versions[grepl(paste0("^", name), nm_versions)]
  
  if(length(nm_version) != 1){
    warning("Could not determine path to nonmem from psn -nm_versions.")
    return(NULL)
  }
  
  ## can now assume version is unique
  path <- gsub(".*\\((.*),.*\\)", "\\1", nm_version)
  if(!file.exists(path)) warning("directory ", path, "doesn't exist")
  path
}


find_nm_tran_path <- function(name = "default", warn = TRUE){

  nm_versions <- try(system_nm("psn -nm_versions", intern = TRUE, ignore.stderr = TRUE, wait = TRUE), 
                     silent = TRUE)
  
  warn_func <- function(){
    if(warn) warning("Could not determine path to nmtran from psn -nm_versions.\n",
                     "To set manually add the following command:\n",
                     "  options(nmtran_exe_path=\"path/to/nmtran\")\n",
                     "     1. (for this session only) in the console\n",
                     "     2. (for this user) to ~/.Rprofile\n",
                     paste0("     3. (for all users) to ",file.path(R.home(component = "home"), "etc", "Rprofile.site")))
  }
  
  if(inherits(nm_versions, "try-error")) {
    warn_func()
    return(NULL)
  }
  
  nm_version <- nm_versions[grepl(paste0("^", name), nm_versions)]
  
  if(length(nm_version) != 1){
    warn_func()
    return(NULL)
  }
  
  ## can now assume version is unique
  path <- gsub(".*\\((.*),.*\\)", "\\1", nm_version)
  tr_path <- file.path(path, "tr")
  if(file.exists(path)){
    path <- dir(tr_path, pattern = "NMTRAN.exe", ignore.case = TRUE, full.names = TRUE)
  } else { ## guess NMTRAN.exe
    path <- file.path(tr_path, "NMTRAN.exe")    
  }
  path
  
}

#' get/set nm_tran_command
#' 
#' @param text optional character. If specified will set nm_tran_command
#' 
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

#' default system_nm
#'
#' Not to be used directly
#' @param cmd character. system call
#' @param ... additional args to be passed to system
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


#' system command for NONMEM execution
#'
#' @param cmd character. command to send to shell
#' @param dir character. directory to run command in
#' @param ... other arguments passed to system command
#' @export
system_nm <- function(cmd,dir=getOption("models.dir"),...){
  if(is.null(dir) | !file.exists(dir)) dir <- "."
  if(file.exists(dir)) {currentwd <- getwd(); setwd(dir) ; on.exit(setwd(currentwd))} else
    stop(paste0("Directory \"",dir,"\" doesn't exist."))
  getOption("system_nm")(cmd,...)
}


#' kill cluster job
#' 
#' Requires setting "kill_job" \code{option()}
#'
#' @param m nm object 
#' @export
kill_job <- function(m){
  getOption("kill_job")(m)
}

#' overwrite behaviour of NMproject
#' 
#' Requires setting "nm.overwrite_behaviour" \code{option()}
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
.overwrite_behaviour <- tibble::tibble(
  txt = c("ask", "overwrite", "stop", "skip"),
  description =   c("ask before overwrite (default)",
                    "overwrite all",
                    "no overwriting (stop with error)",
                    "no overwriting (skip, no error)")
)

#' get/set new_jobs
#' 
#' Requires setting "nm.overwrite_behaviour" \code{option()}
#'
#' @param txt character either "run", "stop", "skip" 
#' @export
new_jobs <- function(txt = c("ask", "overwrite", "stop new", "skip")){
  .Deprecated(overwrite_behaviour)
  overwrite_behaviour(match.arg(txt))
}

#' path of directory from models dir
#'
#' @param x character vector. Relative path from models.dir
#' @param models_dir character. Models directory
#' @export
from_models <- function(x, models_dir=getOption("models.dir")) {
  file.path(models_dir,x)
}

#' Run NMTRAN step only
#'
#' Runs initial part of NONMEM where control file and dataset checks are performed.
#' Stops before running NONMEM.  Useful especially on grid infrastructures where 
#' it may take a while for NONMEM to start return ctl/dataset errors
#'
#' @param x nm object
#' @details
#' Requires \code{options("nmtran_exe_path")} to be set.
#' @export
nm_tran <- function(x) UseMethod("nm_tran")

#' @export
nm_tran.default <- function(x){
  
  if(is.null(nm_tran_command())){
    warning("Set nm_tran_command() not set. For example :\n",
            "  nm_tran_command(\"path/to/nmtran\")\n",
            "     1. (for this session only) in the console\n",
            "     2. (for this user) to ~/.Rprofile\n",
            paste0("     3. (for all users) to ",file.path(R.home(component = "home"), "etc", "Rprofile.site")))
    stop("nm_tran failed")
  }
  nm_tran_command <- getOption("nmtran_exe_path")

  tempdir0 <- basename(tempdir()) ## make temporary directory in current directory
  dir.create(tempdir0) ; on.exit(unlink(tempdir0,recursive=TRUE,force = TRUE))
  file.copy(x,tempdir0) ## copy_control file
  data_path <- file.path(dirname(x),data_name(x))
  file.copy(data_path,tempdir0) ## copy dataset
  dataset.name <- basename(data_path)
  suppressMessages({
    ctl_text <- update_dollar_data(file.path(tempdir0,basename(x)),dataset.name)
    write(ctl_text, file.path(tempdir0,basename(x)))
  })
  message("running NMTRAN on ", x)
  
  nm_tran_command <- nm_tran_command()
  cmd <- stringr::str_glue(nm_tran_command, .envir = list(ctl_name= basename(x)), .na = NULL)
  ## if non-glue - append the control file name
  if(cmd == nm_tran_command) cmd <- paste(cmd, "<", basename(x))
  
  system_nm(cmd, dir=tempdir0, wait=TRUE)
}

#' Get NONMEM dataset name from control stream
#'
#' @param x argument to be passed to method.
#' @export
data_name <- function(x) UseMethod("data_name")

#' @export
data_name.default <- function(x){
  unlist(lapply(x,function(x){
    if(!file.exists(x)) x <- from_models(x)
    if(!file.exists(x)) stop("can't find control stream")
    x <- normalizePath(x)
    ctl <- readLines(x,warn = FALSE)
    data.row <- grep("^ *\\$DATA",ctl)
    if(length(data.row)<1) stop("can't identify data row")
    if(length(data.row)>1) {
      warning("multiple data rows found. Using first")
      data.row <- data.row[1]
    }
    ctl <- paste(ctl[data.row:length(ctl)],collapse = " ")
    data_name <- gsub("^ *\\$DATA\\s*([^ ]+).*$","\\1",ctl)
    data_name
  }))
}


#' get data name
#'
#' @param ctl object of class coercible into ctl_list
#' @export
get_data_name <- function(ctl){
  ctl <- ctl_list(ctl)
  data_name <- gsub("^ *\\$DATA\\s*([^ ]+).*$","\\1",ctl$DATA)[1]
  return(data_name)
}

#' update dollar data (name of dataset) in NONMEM control stream
#'
#' Generic function
#' @param ctl_name object coercible to ctl_character
#' @param new_data_name character. Name of new dataset
#' @export
update_dollar_data <- function(ctl_name,new_data_name){
  if(is_single_na(ctl_name)) return(NA)
  ctl <- ctl_character(ctl_name)
  ctl <- gsub("^(\\s*\\$DATA\\s*)[^ ]+(.*)$",paste0("\\1",new_data_name,"\\2"),ctl)
  ctl
}

#' Check tidyproject for best practice compliance
#'
#' @param proj_name character. default = current working directory. path to directory.
#' @param silent logical. default = FALSE. suppress messages or not
#' @param check_rstudio logical (default = FALSE). Check rstudio studio project directory
#' @export

check_session <- function(proj_name = getwd(), silent = FALSE, check_rstudio = TRUE) {
  dtidy <- tidyproject::check_session(proj_name = proj_name,
                                      silent = silent,
                                      check_rstudio = check_rstudio)

  d <- tidyproject::do_test(
    "NM run database present" = {
      file.exists("runs.sqlite")
    },
    silent = silent)

  d <- rbind(dtidy,d)
  invisible(d)
}

#' Create new R script
#' @param name character indicating name of script to create
#' @param overwrite logical. Whether to overwrite existing file (default = FALSE)
#' @param open_file logical. Whether function should open script (default = TRUE)
#' @param libs character. What libraries to add.
#' @export
new_script <- function(name, overwrite = FALSE, open_file = TRUE, libs=c("NMproject")) {
  tidyproject::new_script(name=name,overwrite = overwrite,open_file = open_file,libs = libs)
}

#' Download code repositor from github
#'
#' @param local_path character. Path to install repository
#' @param giturl character. URL to github repository
#' @param config_file character. Name of config file (e.g. "~/.Rprofile")
#' @export
get_PMX_code_library <- function(local_path,
                                 giturl="https://github.com/tsahota/PMXcodelibrary",
                                 config_file){
  tidyproject::get_github_code_library(local_path=local_path,
                                       giturl=giturl,
                                       config_file=config_file)
}

#' Get Omega matrix from run
#'
#' @param r object of class nm
#' @export
omega_matrix <- function(r){
  dc <- coef(r,trans=FALSE)
  dc <- dc[dc$type %in% c("OMEGAVAR","OMEGACOV"),]
  dc <- dc[,c("parameter","FINAL")]
  dc$ROW <- as.numeric(gsub("OMEGA\\.([0-9]+)\\..*","\\1",dc$parameter))
  dc$COL <- as.numeric(gsub("OMEGA\\.[0-9]+\\.([0-9]+).*","\\1",dc$parameter))
  dc <- dc[order(dc$ROW,dc$COL),]
  max_size <- max(c(dc$ROW,dc$COL))
  dc <- dc[,c("FINAL","ROW","COL")]
  dc_mirror <- dc
  dc_mirror$COLOLD <- dc_mirror$COL
  dc_mirror$ROWOLD <- dc_mirror$ROW
  dc_mirror$COL <- dc_mirror$ROWOLD
  dc_mirror$ROW <- dc_mirror$COLOLD
  dc_mirror$COLOLD <- NULL
  dc_mirror$ROWOLD <- NULL

  dc <- rbind(dc,dc_mirror)
  dc <- unique(dc)

  d_all <- expand.grid(ROW=1:max_size,COL=1:max_size)
  d_all <- merge(dc,d_all,all=TRUE)
  d_all$FINAL[is.na(d_all$FINAL)] <- 0
  d_all <- d_all[order(d_all$ROW,d_all$COL), ]

  matrix(d_all$FINAL,nrow=max_size)
}

#' commit individual file(s)
#'
#' Has side effect that staged changed will be updated to working tree
#'
#' @param file_name character vector. File(s) to be committed
#' @export
#' @examples
#' \dontrun{
#' commit_file("Scripts/script1.R")
#' commit_file(18)  # will commit Models/run18.mod - if it exists
#' }
commit_file <- function(file_name){
  file_name <- search_ctl_name(file_name)
  tidyproject::commit_file(file_name)
}

replace_DV_with_DV_OUT <- function(x){
  if(identical(x, quote(DV))){
    x <- quote(DV_OUT) 
  }
  if(identical(x, quote("DV"))){
    x <- quote("DV_OUT") 
  }
  else if (is.call(x) | is.pairlist(x)) {
    for(i in seq_along(x)){
      x[[i]] <- replace_DV_with_DV_OUT(x[[i]])
    }
  }
  return(x)
}

#' Get NONMEM version info
#' 
#' @return returns list with version info for NONMEM, PsN, perl and
#' fortran compiler (only gfortran currently)
#' 
#' @export

NONMEM_version <- function(){
  
  ## scrape version info
  psn_nm_versions <- system_nm("psn --nm_versions", intern = TRUE)
  psn_nm_version <- psn_nm_versions[grepl("default is", psn_nm_versions)]
  psn_nm_version <- basename(gsub("^.* (/.*)$","\\1", psn_nm_version))
  
  if(length(psn_nm_version) == 0) psn_nm_version <- "not found"
  
  nm_compiler_version <- system_nm("gfortran --version", intern = TRUE)[1]
  if(length(nm_compiler_version) == 0) {
    nm_compiler_version <- "not found"
  }
  
  psn_version <- system_nm("psn --version", intern = TRUE)
  if(length(psn_version) == 0) psn_version <- "not found"
  
  perl_version <- system_nm("perl --version", intern = TRUE)
  perl_version <- perl_version[grepl("This is", perl_version)]
  perl_version <- gsub("^.*\\((v.*)\\).*$","\\1",perl_version)
  if(length(perl_version) == 0) perl_version <- "not found"
  
  c(NONMEM = psn_nm_version,
    compiler = nm_compiler_version,
    psn = psn_version,
    perl = perl_version)
  
}

#' Make R session record
#'
#' Create a record of the R version and package versions used in a particular NMproject
#'
#' @export


environment_info <- function() {
  txt <- tidyproject::environment_info0()
  
  NONMEM_version <- NONMEM_version()
  NONMEM_version <- paste(names(NONMEM_version), NONMEM_version, sep = ": ")
  
  txt <- c(txt, 
           "\nNONMEM version info (see NONMEM_version()):\n",
           NONMEM_version)
  
  uname <- Sys.info()["user"]
  if(length(uname) == 1){
    uname <- as.character(uname)
    uname <- gsub("\\s", "", uname)
    if(nchar(uname) > 0) uname <- paste0("_", uname)
  } else uname <- ""
  
  
  log_file_name <- paste0("environment_info",uname,".txt")
  
  writeLines(txt, log_file_name)
  tidyproject::setup_file(log_file_name)
  message(paste0("Environment info produced: ",log_file_name))
  
}

