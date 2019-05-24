## TODO: desired interface

## execute runs are default
##  children can be execute, vpc, ppc, ....

## Assume only executes can have children?
##  for now, no

## Unique identifiers:
##  run_id = "m0"       ## run_m0.mod
##    used for TABLEs, results tracking, 
##  type = "execute"    ## assume everything is "execute"
##  run_in = "Models"   ## assume everything is in "Models"

## current - create from "execute runm0.mod -dir=m0"

## instead, specify run_id, type, run_in and create "execute ..."

m0 <- nm(run_id = "m0") %>%
  cmd("qpsn -c auto -t 10 -- execute run_{run_id}.mod -dir={run_id}")
  # run_in("Models") %>% 
  # ctl("run_{run_id}.mod") %>%   ## current ctl -> show_ctl, show_run_dir, show_lst, ... same help
  # type("execute") %>% ## default from cmd
  # db("runs.sqlite")  ## default (saved to db at run time by run process)

## ctl derived data is read "just in time" - e.g. data name, params, table names, ... need functions

m1 <- nm(run_id = "m1") %>% parent(m0)
build_ctl({  ## segment for ctl file manipulation
  m1 %>% 
    ctl_update_parameters(m0) %>%  ## if m0 is running, wait.
    ctl_manual_edit("KEFF log-normal -> normal")
  ## all ctl functions prefixed with ctl - easy to remember to include in build_ctl
  ##   easy to check.
  ## single help for all of them
})

m1 %>% nm_tran  ## can be interactive
m1 %>% nm_check  ## can be interactive
m1 <- run_nm(m1, db = "runs.sqlite",       ## default
             check_for_conflicts = TRUE)   ## default
               ## (this saves m1 to db,
               ##  records SHA of ctl/data)
               ##   this will help determine is run is invalidated
               ## do nothing if SHA matches
               ##   (force rerun with force arg)
               ## only run/post process can write to db  - this will speed up nmproject a lot
               ##   but it will hamper the "safety" bits of overlapping file names
   ## adds m1$promise = the promise of the run

# r asynchronous queue
#http://jaehyeon-kim.github.io/2016/05/Asynchronous-Processing-Using-Job-Queue.html

## how to use future.  Maybe future only for post()

## database (need concurrency):
##  minimise functions that use it - only run/post
##  spread out database
##    one for run status
##    one for other stuff
##    this will only reduce - not eliminate it.
##  only have workers = 2
##    can't make really big routines - maybe good on multiuser system
##  can I use promises?

m1 <- m1 %>% post(gof_xpose)
## m1$promise %>% then(gof_xpose(m1))
## will wait for run to finish and then launch in parallel
## saves results in object/db

m1boot <- nm("m1boot", parent = m1) %>%
  cmd("vpc run_{run_id}.mod -dir={run_id}") %>%
  type("vpc")


###############################
## attempt with future

resolved.nm <- function(x, ...) is_finished(x, ...)
value.nm <- function(...) identity(...)

post <- function(r, f, ...){
  promise <- future({
    wait_for_finished(r)
    outputs <- f(r, ...)
    ## save outputs to db
  })
  invisible(promise)
}

after_results <- function(r, expr){
  
  expr <- c(wait_for_finished(r),expr)
  f1 %<-% future(expr)
  
}

############################
## I'm using S3 classes more - so need unified strategy on different classes
## need empty versions of each - use NA with right class.

## behaviour - if working on single objects, want errors, if working on vectors/lists want NAs
###               are there exceptions to this?

## classes:

## nm/nmexecute/nm...
## ctl_list/ctl_character
## list of ctl_lists   ## for mass control stream editing.
## nmcoef/nmres - outputs computed once and ready for subsequent functions.
## nmlist  - No - because I want people to create their own lists with list(m1, ....)

############################


## want makefile like functionality (can't use future)

## want a separate process to offload computation and waiting (future)

## https://ropenscilabs.github.io/drake-manual/index.html  ## this looks like the tool to do both

## drake centres around PLANS - can combine plans with bind_rows
## phew it's difficult to imagine how I can get drake to work in the background.

## would need a master plan and mini-plans one per function.  And a way of connecting mini-plans
##  where would these be stored?
##  global workspace is a bit...
##  

## need to look at more examples - get more familiarity around drake

m0 = nm(run_id = "m0") %>%
  cmd("qpsn -c auto -t 10 -- execute run_{run_id}.mod -dir={run_id}")

m0 %>% run_nm()

m1 <- nm(run_id = "m1") %>% parent(m0)
build_ctl({  ## segment for ctl file manipulation
  m1 %>% update_parameters(m0) %>%  ## if m0 is running, wait.
    manual_edit("KEFF log-normal -> normal")
})


##or

m1 <- nm(parent = "m0")

## why do we need a run id?  Why not just the object?
## it's a short hand 


nm <- function(r){
  plan <- drake_plan(...)
  
  ## where do I store plan?
  
  
  
}



plan <- drake_plan()

plan_tmp <- drake_plan(
  m0 = nm(run_id = "m0") %>% 
    cmd("qpsn -c auto -t 10 -- execute run_{run_id}.mod -dir={run_id}")
)
plan <- bind_rows(plan, plan_tmp)
make(plan)

plan_tmp <- drake_plan(
  res = run_nm(m0)
)
plan <- bind_rows(plan, plan_tmp)
make(plan)


plan_m1 <- drake_plan(m1 = nm(run_id = "m1") %>% parent(m0),
  temp = build_ctl({  ## segment for ctl file manipulation
    m1 %>% update_parameters(m0) %>%  ## if m0 is running, wait.
      manual_edit("KEFF log-normal -> normal")
  }),
  res = run_nm(m1))

plan <- rbind(plan_m0, plan_m1)

make(plan)   ## this will build and run m1

## example dependencies

## a build_ctl depends on results of m0



## how to do sets of runs?
dm <- tibble(run_id = c("m1", "m2"))

## why do I need specification of run_id?

## Object option 1 (db based):
##   row identifier of db
## pros: no need for run_id, neater
## cons:
##  system becomes dependent on db (need it to be bulletproof)
##   concurrency and 
##  possibly slower, more dependent of file.system

## Object option 2 (hybrid object-db based):
##   row identifier of db + immutable fields
##   ob fields subset of db fields
## pros: no need for run_id, neater
## cons:
##  system as dependent on db as now
##   concurrency and speed still need fixing

## Object option 3 (object based):
##   all fields
## pros: simple fast, 
## cons:
##  how to handle concurrency
##   one process might change an object.

## DB
## pros: persistence (multiple read processes)
## cons: slower more complicated


#m1$cmd("qpsn -c 1 -t 9999 -- execute run_{run_id}.mod -dir={run_id}")

## want to be able to refer to objects.
## need to recreate objects with ctrl+alt+b

rerun(FALSE) ## means run_nm(),nm_tran(),post(),build_ctl() does nothing
## make all object creation/db access much faster

## link outputs to runs
##   Have: a function that goes "r -> output file names"

post(m1, gof_xpose)  ## will write to db
## runs gof_xpose(m1) and save return obs (characters) to results db

## TODO:
## Need consistent way of handling nm object and lists of nm objects and data.frames
##  if class nm - fun(m)
##  if class list - lapply(m, fun) or sapply(m, fun) depending on fun
##  if class data.frame  - fun(m$m)   - this is a list
##  if is.na(m) - return(NA)

## it's pretty complex
##  option 1: use S3 classes
##   reasonably simple and general
##   list is pretty general will this conflict with other packages?
##    can always rename if it does

#' Fun title
#'
#' fun
#'
#' @param x arg
#' @export
fun <- function(x) UseMethod("fun")

#' @export
fun.nm <- function(x) "the meat of the function"

#' @export
fun.list <- function(x) sapply(x, fun)

#' @export
fun.data.frame <- function(x) call_fun_on_nm_data_frame(x, fun)

call_fun_on_nm_data_frame <- function(x, fun){
  is_nm <- sapply(x, is_list_nm)
  if(length(which(is_nm)) == 1) return(fun(x[is_nm])) else {
    if(length(which(is_nm)) == 0) stop("can't find column of nm objects")
    if(length(which(is_nm)) > 1) stop("can't find unique column of nm objects")
  }
}
  
## run_nm
## status
## ofv
## AIC*
## BIC*

## TODO:
## User level accessor functions for function template construction

## run_id(r) - DONE
## run_in(r)
## type(r)
## results_loc(r)

## TODO: make snapshot work even if no db is present - this should
##  be a tidyproject function as opposed to NMproject?

## TODO: functions to be used in function templates:
##     - require_finished(r) - will wait if not finsihed
##     - require_processed(r) - will process if needed

## TODO: register_output(r, "Results/file.pdf")
##     - adds file to $output tracker

## TODO: new convention run_ids start with letter

## TODO: Better database access
##       For read, use type, run_id, run_in

## TODO: Store SHA's to all inputs - check before running.
##   then eliminate interactive/non-interactive options
## TODO: Principle: ctrl + alt + b/source works like "make".
##    Need dependency graphs.
##    VPCs/outputs are dependent on run results.
##    Plan behaviour

## TODO: create a project_library(TRUE/FALSE) flag option

## TODO: an easy way to copy-modify this code:
##  -- add ons

## TODO: need a modular way of referring to previous run

e14 <- nm("e14", type = "execute", based_on = e13)
e14 <- nm("e14", type = "execute", based_on = "e13") ## picks execute
e14 <- nm("e14", type = "execute", based_on = "ADVAN13.mod") ## code library

## would be easiest if i had all info in e13 e.g. cmd

## TODO: function to save nm objects and load them easily


## TODO: make functions work on lists of nm objects
##   e.g. dsc$ofv <- ofv(dsc$m)
##        run_nm(dsc$m, batches = 10)  ## works like run_batch_list

## TODO: import_code()
##     Scripts  - import_script_file()
##       "~/script.R" - no change
##       "../scripts.R" - no change
##       "script.R" - [code_library]/Scripts/script.R
##       "script.R" - [code_library]/Scripts/script.R
##       will add R comment to top

##     Models   - import_model_file()
##       "~/runTTE.mod" - no change
##       "../runTTE.mod" - no change
##       "runTTE.mod" - [code_library]/Models/script.R
##       "runTTE.mod" - [code_library]/Models/script.R

##     Projects  - import_project
##       "~/PROJ1" - no change
##       "../PROJ1" - no change
##       "PROJ1" - [code_library]/Projects/PROJ1
##       should this just be a wrapper around import_code? then you get all the stamp
##       how to handle merge conflicts? answer: rename and copy

## separate functions:
## import_project()
## import_code()
## import_file() - straight foward copy - this is just file.copy

##      find_code("script.R") %>% import_code

###################################################
## start low level

## Need to know what to do in the event of a clash
##   stop (current behaviour - safest) with message of import conflicts. Option to overwrite.

## tidyproject::import_r_script(overwrite = FALSE)
##   ifmissing(finds script in code library/Scripts) and copies into Scripts and stamps with R comment
## tidyproject::import_file(overwrite = FALSE)
##   ifmissing(finds file in code library) and copies to relevant subdir
## NMproject::import_nm_file(overwrite = FALSE)
##   ifmissing(finds model file in code library/Models), copies into Models and modifies.

## tidyproject::import_code(search_location = code_library_path())
##  detects what type of file it is:
##   if it's an r scirpt it uses tidyproject::import_r_script()
##   if it's unknown, it searchs one level from search_location, into relevant subdir

## NMproject::import_code(search_location = code_library_path())
##  detects what type of file it is:
##   if it's a mod file it uses NMproject::import_nm_file()
##   otherwise it uses tidyproject::import_code()

## tidyproject::import_project(proj_name, search_location = code_library_path())
##  find unique project path (look in code_library_path/Projects)
##  list all files.
##  run tidyproject::import_code

## NMproject::import_project(proj_name, search_location = code_library_path())
##  find unique project path (look in code_library_path/Projects)
##  list all files.
##  run tidyproject::import_code

## the following is better - allows you to pause before importing.

search_code_library("script.R") %>% import
search_code_library("aztheopp") %>% import

## tidyproject

list.files_maxdepth1 <- function(path, ..., full.names = TRUE){
  res <- unlist(sapply(path, function(i){
    base_files <- list.files(i, ..., recursive = FALSE, full.names = TRUE)
    base_files <- file.info(base_files)
    base_files <- base_files[!base_files$isdir, ]
    base_files <- row.names(base_files)
    
    base_dirs <- list.dirs(i, ..., recursive = FALSE, full.names = TRUE)
    base_dirs <- base_dirs[!grepl(".git", base_dirs)]
    
    subdir_files <- unlist(sapply(base_dirs, list.files, recursive = FALSE, full.names = TRUE))
    names(subdir_files) <- NULL
    c(base_files, subdir_files)    
  }))
  names(res) <- NULL
  normalizePath(res)
}

list_code_library <- function(search_location = code_library_path()){
  list.files_maxdepth1(rev(search_location))
}

search_code_library <- function(file_name, search_location = code_library_path()){
  
  cl_object <- list_code_library(search_location = search_location)
  
  matched_object <- cl_object[grepl(paste0(".*",.Platform$file.sep,file_name,"$"), cl_object)]
  
  if(length(matched_object) == 0)  return(character())
  if(length(matched_object) > 1)  stop("multiple matches for ", file_name)
  
  matched_object
  
}

## tidyproject
import <- function(paths){
  
  ## convert folder names to lists of files. 
  path_info <- file.info(paths)
  paths <- sapply(paths, function(path){
    if(!file.info(path)$isdir) return(path)
    normalizePath(list.files(path, full.names = TRUE, recursive = TRUE))
  })
  
  for(path in paths){
    ## detect type of path
    path_type <- "unknown"
    file_extn <- tools::file_ext(path)
    if(file_extn %in% c("R", "r")) path_type <- "rscript"
    
    ## launch corresponding import sub function.
    switch(path_type,
           rscript = import_rscript(path),
           unknown = import_file(path))
    
  }
  
}

import_rscript <- function(path){
  check_if_tidyproject(".")
  to_path <- file.path(scripts_dir(),basename(path))
  depends.on <- dependency_tree(path)
  if (length(depends.on) > 0) 
    message("Copying dependencies...")
  for (i in depends.on) {
    if (file.exists(file.path(scripts_dir(), i))) 
      message(paste("Dependency", file.path(getOption("scripts.dir"), i), "already exists. Will not overwrite")) else 
        tidyproject::import(file.path(dirname(path), i), dependencies = FALSE, alt_paths = alt_paths)
  }
  suppressWarnings(s0 <- readLines(path))
  ## modify text at top of 'path'
  s <- c(paste0("## Copied from ", path, "\n##  (", 
                Sys.time(), ") by ", Sys.info()["user"]), s0) else s <- s0
  writeLines(s, to_path)
  setup_file(to_path)
}

## new idea.
## structure the code_library identically to a big tidyproject - yes do this.
##  DerivedData, Models, ...
##  dependency_tree will extract all dependencies and they are all imported a la vez

## New for tidyproject
##  subdirectories

## Scripts/plot_functions/gof.R
## Scripts/..

## Models: no subdirs, use database searching instead.
##  User might want subdirs.... how?
##   R script reverse dependencies would need to be changed












import_code <- function(file_name,
                        script_extns = c("R", "r")){
  
  ## if a model file, do copy_control
  
  ## detect type of code, if it's a script use:
  tidyproject::import_code(file_name)
  
}


## tidyproject function
import_code <- function(file_name,
                        script_extns = c("R", "r")){
  
  ## detect type of file.
  file_type <- "unknown"
  
  file_extn <- tools::file_ext(file_name)
  if(file_extn %in% c("R", "r")) file_type <- "r script"

  
  if(!file.exists(file_name)){
    ## need to search for file in code_library and redefine file_name
    
    search_dirs <- file.path(code_library_path(), "Scripts")
    
    ## find file_name in 
    
  }
  
}



