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
#' Compatible with git - optional
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
#' \code{path.nm_tran} = path to nm_tran file
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
  if(is.null(getOption("nmproject.src"))) options(nmproject.src = file.path(getOption("home"),"NMproject"))  ## can be hard coded later
  ## eventually nmproject.src can be a name, then CRAN can be used.
  ## maybe split into nmproject.src and nmproject.install.name - for now they are the same
  if(is.null(getOption("psn.commands"))) options(psn.commands=c("execute","vpc","bootstrap","sse"))
  if(is.null(getOption("system_cmd"))) options(system_cmd=function(cmd,...) {
    if(.Platform$OS.type == "windows") shell(cmd,...) else
      system(cmd,...)
  })
  options(git.ignore.files=c("psn-*","hostfile*","submitscript*"))
  if(is.null(getOption("path.nm_tran"))) options(path.nm_tran = "/opt/az/icondevsolutions/nonmem/standard.7.3.0/tr/NMTRAN.exe")
  if(is.null(getOption("system_nm"))) options(system_nm=function(cmd,...) {
    if(.Platform$OS.type == "windows") shell(cmd,...) else
      system(paste0("ssh -q calvin.seml.astrazeneca.net \"cd $(pwd); module unload psn git r && module load psn nonmem git r && ",cmd,"\""),...)
  })
  if(is.null(getOption("model.file.stub"))) options(model.file.stub="run")
  if(is.null(getOption("model.file.extn"))) options(model.file.extn="mod")
  if(is.null(getOption("available_nm_types"))) options(available_nm_types = c("SIZES","PROB","INPUT","DATA","SUB","MODEL","PK","DES","PRED","ERROR",
                                                                              "THETA","OMEGA","SIGMA","EST","SIM","COV","TABLE"))
}
set_nm_opts()

#' system/shell command wrapper
#' @export
system_cmd <- function(cmd,dir=".",...){
  if(!dir %in% ".") if(file.exists(dir)) {currentwd <- getwd(); setwd(dir) ; on.exit(setwd(currentwd))} else
    warning(paste0("Directory \"",dir,"\" doesn't exist.  Running in current working directory"))
  getOption("system_cmd")(cmd,...)
}

#' system command for NONMEM execution
#' @export
system_nm <- function(cmd,dir=getOption("models.dir"),...){
  if(is.null(dir)) dir <- "."
  if(file.exists(dir)) {currentwd <- getwd(); setwd(dir) ; on.exit(setwd(currentwd))} else
    warning(paste0("Directory \"",dir,"\" doesn't exist.  Running in current working directory"))
  getOption("system_nm")(cmd,...)
}

#' Copy NONMEM control stream
#' @export
copy_control <- function(from,to,update=FALSE,overwrite=FALSE){
  ## if from = NONMEM control in current directory, it will copy and update $TABLE numbers
  ## if from = control file code_library(), it will copy it.
  ## First it will look for "from" in current directory, then it will look in code_library()
  to <- from_models(to)
  if(file.exists(to) & !update & !overwrite) stop("file already exists. Rerun with overwrite = TRUE")

  from <- locate_file(from,c(getOption("models.dir"),
                             file.path(getOption("code_library_paths"),"NONMEM")))

  run.id <- gsub(from_models("run([^\\.]+)\\.mod"),"\\1",to)
  if(from_models(paste0("run",run.id,".mod"))!=to) stop("file.name doesn't match runXX.mod convention")
  if(update){ ## if TRUE, then just use psn's "update" command.
    system_nm(paste0("update ",basename(from)," --out=",to))
    setup_file(to)
    return()
  }
  s <- readLines(from)
  ## Modify the file here.
  run.id.from <- gsub("run(.*)\\.mod","\\1",basename(from))
  s <- gsub(paste0("(FILE\\s*=\\s*.*)",run.id.from,"\\b"),paste0("\\1",run.id),s)
  s <- gsub("^(\\s*;;\\s*[0-9]*\\.\\s*Based on:).*",paste("\\1",run.id.from),s)
  s <- gsub("^(\\s*;;\\s*\\w*\\.\\s*Author:).*",paste("\\1",user()),s)
  writeLines(s,to)
  setup_file(to)
}
