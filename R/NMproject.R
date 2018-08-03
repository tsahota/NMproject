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

  if(is.null(getOption("system_cmd"))) options(system_cmd=function(cmd,...) {
    if(.Platform$OS.type == "windows") shell(cmd,...) else system(cmd,...)
  })
  if(is.null(getOption("system_nm"))) options(system_nm=system_nm_default)
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

}

#' default system_nm
#'
#' Not to be used directly
#' @param cmd character. system call
#' @param ... additional args to be passed to system
#' @export

system_nm_default <- function(cmd,...) {
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
#' @param from_base logical (default = FALSE). Should "to" be taken to be relative to base.
#'
#' @export
copy_control <- function(from,to,overwrite=FALSE,alt_paths,from_base=FALSE){
  ## if from = NONMEM control in current directory, it will copy and update $TABLE numbers
  ## if from = control file code_library(), it will copy it.
  ## First it will look for "from" in current directory, then it will look in code_library()
  if(!from_base) to <- from_models(to)
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
  ctl <- readLines(from_path,warn = FALSE)
  ## Modify the file here.
  if(is_nm_file_name(from_path))
    run_id_from <- run_id(from_path) else
      run_id_from <- basename(from_path)

  ctl <- gsub(paste0("(FILE\\s*=\\s*\\S*)",run_id_from,"\\b"),paste0("\\1",run_id),ctl)
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

  if(is.null(getOption("nmtran_exe_path"))){
    message("Path to nmtran not set properly. To set add the following command:")
    message("  options(nmtran_exe_path=\"path/to/nmtran\")")
    message("     1. (for this session only) in the console")
    message("     2. (for this user) to ~/.Rprofile")
    message(paste0("     3. (for all users) to ",file.path(R.home(component = "home"), "etc", "Rprofile.site")))
    stop("nm_tran failed")
  }
  nm_tran_command <- getOption("nmtran_exe_path")

  tempdir0 <- basename(tempdir()) ## make temporary directory in current directory
  dir.create(tempdir0) ; on.exit(unlink(tempdir0,recursive=TRUE,force = TRUE))
  file.copy(x,tempdir0) ## copy_control file
  data_path <- file.path(dirname(x),data_name(x))
  file.copy(data_path,tempdir0) ## copy dataset
  dataset.name <- basename(data_path)
  update_dollar_data(file.path(tempdir0,basename(x)),dataset.name)
  system_nm(paste(nm_tran_command,"<",basename(x)),dir=tempdir0,wait=TRUE) ## run nmtran in tempdir0
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

#' update dollar data (name of dataset) in NONMEM control stream
#'
#' Generic function
#' @param ctl_name character. Name of control stream
#' @param new_data_name character. Name of new dataset
#' @export
update_dollar_data <- function(ctl_name,new_data_name){
  ctl <- readLines(ctl_name,warn = FALSE)
  ctl <- gsub("^(\\s*\\$DATA\\s*)[^ ]+(.*)$",paste0("\\1",new_data_name,"\\2"),ctl)
  writeLines(ctl,ctl_name)
}

#' Shiny view of NMproject
#' @param db_name character. Name of db
#' @export
shiny_nm <- function(db_name="runs.sqlite"){
  if(!requireNamespace("DT", quietly = TRUE))
    stop("DT needed for this function to work. Please install it.",
         call. = FALSE)
  if(!requireNamespace("dygraphs", quietly = TRUE))
    stop("dygraphs needed for this function to work. Please install it.",
         call. = FALSE)
  dygraphs::dygraph
  DT::datatable
  shiny_dir <- system.file("extdata/shiny",package="NMproject")
  .sso_env$.currentwd <- getwd()  # see zzz.R for .sso_env
  .sso_env$.db_name <- db_name  # see zzz.R for .sso_env
  on.exit({
    .sso_env$.currentwd <- NULL
    .sso_env$.db_name <- NULL
  }, add = TRUE)
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
  dc <- dc[dc$Type %in% c("OMEGAVAR","OMEGACOV"),]
  dc <- dc[,c("Parameter","FINAL")]
  dc$ROW <- as.numeric(gsub("OMEGA\\.([0-9]+)\\..*","\\1",dc$Parameter))
  dc$COL <- as.numeric(gsub("OMEGA\\.[0-9]+\\.([0-9]+).*","\\1",dc$Parameter))
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

#' Document manual step
#'
#' @param comment character. Description of change
#' @return error with comment name
#' @export
manual_step <- function(comment){
  ## TODO record this information somewhere
  stop(paste0("manual step: ",comment), call. = FALSE)
}

#' Write derived data file.
#'
#' @param d data.frame. Data frame to be saved
#' @param name name of file (without extension)
#' @param ...  additional arguments to be passed dto write.csv
#' @export

write_derived_data <- function(d, name, ...){
  if(grepl("\\.", name)) stop("name should be extensionless")

  RData_name <- file.path("DerivedData",paste0(name,".RData"))
  csv_name <- file.path("DerivedData",paste0(name,".csv"))

  d <- as.data.frame(d)
  if(!inherits(d, "data.frame")) stop("d needs to be a data.frame or coercible into one")

  save(d, file = RData_name)
  write.csv.nm(d, file = csv_name, ...)

  message("written: ")
  message(RData_name)
  message(csv_name)
}

#' Read derived data
#'
#' @param name name of file (without extension)
#' @param ...  additional arguments to be passed dto write.csv
#' @export

read_derived_data <- function(name, ...){

  ## TODO: expand to other types of argument

  if(grepl("\\.", name)) stop("name should be extensionless")

  RData_name <- file.path("DerivedData",paste0(name,".RData"))
  csv_name <- file.path("DerivedData",paste0(name,".csv"))

  if(file.exists(RData_name)){
    message("loading: ", RData_name)
    load(file = RData_name)
    return(get("d"))
  } else stop("RData file not found, check name or read csv manually")

}
