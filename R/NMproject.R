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
                "lasso","llp","mcmp","mimp","nca","npc","pvar","randtest","rawresults","runrecord",
                "scm","se_of_eta","sir","sse","sumo","update","update_inits","vpc","xv_scm"))

  if(is.null(getOption("system_cmd"))) options(system_cmd=function(cmd,...) {
    if(.Platform$OS.type == "windows") shell(cmd,...) else system(cmd,...)
  })
  if(is.null(getOption("system_nm"))) options(system_nm=function(cmd,...) {
    if(.Platform$OS.type == "windows"){
      local_env_vars <- Sys.getenv()
      stdout_unit_vars <- local_env_vars[grepl("STDOUT_UNIT|STDERR_UNIT",names(local_env_vars))]
      for(i in seq_along(stdout_unit_vars)){
        Sys.unsetenv(names(stdout_unit_vars)[i])
      }
      on.exit(do.call(Sys.setenv,as.list(stdout_unit_vars)))
      args <- list(...)
      if(!"wait" %in% names(args)) wait <- FALSE else wait <- args$wait
      if(wait==FALSE){
        shell(paste("START CMD /C",cmd),...)        
      } else {
        shell(cmd,...)        
      }
    } else system(cmd,...)
  })
  
  if(is.null(getOption("model_file_stub"))) options(model_file_stub="run")
  if(is.null(getOption("model_file_extn"))) options(model_file_extn="mod")
  if(is.null(getOption("available_nm_types")))
    options(available_nm_types = c("SIZES","PROB","INPUT","DATA","SUB","MODEL","PK","DES","PRED","ERROR",
                                   "THETA","OMEGA","SIGMA","EST","SIM","COV","TABLE"))
}

#' system/shell command wrapper
#'
#' @param cmd character. command to send to shell
#' @param dir character. directory to run command in
#' @param ... other arguments passed to system command
#' @export
system_cmd <- function(cmd,dir=".",...){
  if(!dir %in% ".") if(file.exists(dir)) {currentwd <- getwd(); setwd(dir) ; on.exit(setwd(currentwd))} else
    stop(paste0("Directory \"",dir,"\" doesn't exist."))
  getOption("system_cmd")(cmd,...)
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

#' Copy NONMEM control stream
#'
#' @param from character. File to copy from
#' @param to character. File to copy to
#' @param overwrite logical. Should to file be overwritten? Default = FALSE.
#' @param alt_paths character vector. paths to other candidate files to search
#'
#' @export
copy_control <- function(from,to,overwrite=FALSE,alt_paths){
  ## if from = NONMEM control in current directory, it will copy and update $TABLE numbers
  ## if from = control file code_library(), it will copy it.
  ## First it will look for "from" in current directory, then it will look in code_library()
  to <- from_models(to)
  if(file.exists(to) & !overwrite) stop("file already exists. Rerun with overwrite = TRUE")

  use_code_library <- missing(alt_paths)
  from_path <- tidyproject::locate_file(from,search_path = NULL)
  if(length(from_path)==0) from_path <- tidyproject::locate_file(from,getOption("models.dir"))
  using_code_library <- length(from_path)==0

  if(length(from_path)==0){ ## if file is not found directory or in scripts.dir
    if(use_code_library) alt_paths <- getOption("code_library_path")
    from_path <- tidyproject::locate_file(from,search_path = alt_paths,recursive = TRUE)
    if(length(from_path)==0) stop(paste(from,"not found"))
    if(length(from_path)>1 & use_code_library)
      stop("Matched more than one file with that name in code library.\n Try:\n  1) specifying full path OR\n  2) ensuring getOption(\"code_library_path\") points to non-overlapping directories")
    if(length(from_path)>1 & !use_code_library)
      stop("Matched more than one file with that name in alt_paths.\n Try specifying full path")
  }

  is_project_file <- normalizePath(dirname(from_path))==normalizePath(getOption("models.dir"))

  is_nm_file_name(to,error_if_false = TRUE)
  run_id <- run_id(to)
  ctl <- readLines(from_path)
  ## Modify the file here.
  if(is_nm_file_name(from_path))
    run_id_from <- run_id(from_path) else
      run_id_from <- basename(from_path)

  ctl <- gsub(paste0("(FILE\\s*=\\s*.*)",run_id_from,"\\b"),paste0("\\1",run_id),ctl)

  if(is_project_file)
    ctl <- gsub("^(\\s*;;\\s*[0-9]*\\.\\s*Based on:).*",paste("\\1",run_id_from),ctl) else
      ctl <- gsub("^(\\s*;;\\s*[0-9]*\\.\\s*Based on:).*",paste("\\1",from_path),ctl)
  ctl <- gsub("^(\\s*;;\\s*\\w*\\.\\s*Author:).*",paste("\\1",Sys.info()["user"]),ctl)
  writeLines(ctl,to)
  if(!requireNamespace("git2r", quietly = TRUE))
    warning("git2r is recommended for this function. Please install it.")
  tidyproject::setup_file(to)
}

#' Get run id from file name or path
#'
#' @param x character vector. name of file or path to get run id from
run_id <- function(x){
  file.regex <- paste0("^.*",getOption("model_file_stub"),"(.*)\\.",getOption("model_file_extn"),"$")
  gsub(file.regex,"\\1",x)
}


#' path of directory from models dir
#'
#' @param x character vector. Relative path from models.dir
#' @param models_dir character. Models directory
#' @export
from_models <- function(x, models_dir=getOption("models.dir")) {
  file.path(models_dir,x)
}

#' test if NONMEM file name conforms to convention
#'
#' @param x character vector. file name or path
#' @param error_if_false logical. Default=FALSE. If true, will make an error if test fails
is_nm_file_name <- function(x,error_if_false=FALSE){
  file.regex <- paste0("^.*",getOption("model_file_stub"),"(.*)\\.",getOption("model_file_extn"),"$")
  out <- grepl(file.regex,x)
  if(error_if_false & !out)
    stop(paste0("file.name doesn't match ",getOption("model_file_stub"),"XX.",getOption("model_file_extn")," convention")) else
      return(out)
}

#' Run NMTRAN step only
#'
#' Requires options("path/to/nmtran") to be set up.
#'
#' @param x character. file name of NONMEM control stream
#' @export
nm_tran <- function(x) UseMethod("nm_tran")

#' @export
nm_tran.default <- function(x){
  if(is.null(getOption("nmtran_exe_path"))) {
    message("Path to nmtran not set. To set add the following command:")
    message("  options(nmtran_exe_path=\"path/to/nmtran\")")
    message("     1. (for this session only) in the console")
    message("     2. (for this user) to ~/.Rprofile")
    message(paste0("     3. (for all users) to ",file.path(R.home(component = "home"), "etc", "Rprofile.site")))
    stop("nmtran failed")
  }
  tempdir0 <- basename(tempdir()) ## make temporary directory in current directory
  dir.create(tempdir0) ; on.exit(unlink(tempdir0,recursive=TRUE,force = TRUE))
  file.copy(x,tempdir0) ## copy_control file
  data_path <- file.path(dirname(x),data_name(x))
  file.copy(data_path,tempdir0) ## copy dataset
  dataset.name <- basename(data_path)
  update_dollar_data(file.path(tempdir0,basename(x)),dataset.name)
  system_nm(paste(getOption("nmtran_exe_path"),"<",basename(x)),dir=tempdir0,wait=TRUE) ## run nmtran in tempdir0
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
    ctl <- readLines(x)
    data.row <- grep("^ *\\$DATA",ctl)
    if(length(data.row)<1) stop("can't identify data row")
    if(length(data.row)>1) stop("multiple data rows found")
    ctl <- paste(ctl[data.row:length(ctl)],collapse = " ")
    data_name <- gsub("^ *\\$DATA\\s*([^ ]+).*$","\\1",ctl)
    data_name
  }))
}

#' update dollar data (name of dataset) in NONMEM control stream
#'
#' Generic function
#' @param ctl_name character. Name of control stream
#' @param new_data_name character. Name of new dataset
#' @export
update_dollar_data <- function(ctl_name,new_data_name){
  ctl <- readLines(ctl_name)
  ctl <- gsub("^(\\s*\\$DATA\\s*)[^ ]+(.*)$",paste0("\\1",new_data_name,"\\2"),ctl)
  writeLines(ctl,ctl_name)
}

#' Shiny view of NMproject
#' @export
shiny_nm <- function(){
  if(!requireNamespace("DT", quietly = TRUE))
    stop("DT needed for this function to work. Please install it.",
         call. = FALSE)
  shiny_dir <- system.file("extdata/shiny",package="NMproject")
  .sso_env$.currentwd <- getwd()  # see zzz.R for .sso_env
  on.exit(.sso_env$.currentwd <- NULL, add = TRUE)
  shiny::runApp(shiny_dir,launch.browser = TRUE)
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
