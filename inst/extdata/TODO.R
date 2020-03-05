## TODO: desired interface

## Unique identifiers:
##  run_id = "m0"       ## run_m0.mod used for dir name, sdtabs, ...
##    used for TABLEs, results tracking, 
##  type = "execute"    ## default =  "execute"
##  run_in = "Models"   ## default =  "Models"

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
               ##   this will help determine if run is invalidated
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

m1vpc <- nm("m1boot", parent = m1) %>%
  cmd("vpc run_{run_id}.mod -dir={run_id}") %>%
  type("vpc")


## how to handle concurrency with multiple runs, but sqlite
## one process manages all database connections.

## need database for run management.

## better detection of completed runs
## https://stackoverflow.com/questions/23456170/get-the-number-of-lines-in-a-text-file-using-r
## possibly useful
## https://www.rdocumentation.org/packages/R.utils/versions/2.8.0/topics/countLines


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

## objects:
## nm
## list  - is_nm_list()

## want all functions to work on both.

## orrrr.....
## one type of object that can be concatenated
##  new object type is called "nm"
##  i.e. "new nm" = list("old nm")

## nm then becomes an expandable concatenatable object.
## sub-object is then nm_generic, nm_execute, nm_bootstrap, ...

## specific methods can then be dispatched as needed on the back end.

## TODO: insert_dollar(MODEL = "sdlkfjsfdlj")  will insert a $MODEL after $SUB (i.e. a default order)

## TODO:
## User level accessor functions for function template construction

## run_id(r) - DONE
## run_in(r) - DONE
## type(r)
## results_loc(r)

## TODO: make snapshot work even if no db is present - this should
##  be a tidyproject function as opposed to NMproject?

## TODO: register_output(r, "Results/file.pdf")
##     - adds file to $output tracker

## TODO: new convention run_ids start with letter

## TODO: Better database access
##       For read, use type, run_id, run_in

## TODO: Store SHA's to all inputs - check before running.
##   then eliminate interactive/non-interactive options

## TODO: an easy way to copy-modify code:
##  -- add ons

## TODO: possible to combine with caret?
## - would need parameters passed as arguments, and automatic fitting
## - maybe as long term goal 
## - could create wrapper around nm(), run_nm(), wait_for_finished()...

## TODO: import_code()  - (import instead of "copy")
##     Scripts  - import_script_file()
##       "~/script.R" - no change
##       "../scripts.R" - no change
##       "script.R" - [code_library]/Scripts/script.R
##       will add R comment to top

##     Models   - import_model_file()
##       "~/runTTE.mod" - no change
##       "../runTTE.mod" - no change
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

# "/project/qcp/..../azd6094/poppksdfklkj/DerivedData/data.csv"
# "/project/qcp/..../azd6094/poppksdfklkj/DerivedData/data2.csv"
#->
#  detect analysis directory "/project/qcp/..../azd6094/poppksdfklkj"
#   "DerivedData/data.csv"
#   "DerivedData/data2.csv"

file_mapping <- function(files){
  d <- data.frame(files, analysis_dir)
  dest_path <- sapply(seq_len(nrow(d)), function(i) relative_path(d$files[i], d$analysis_dir[i]))
  tibble::tibble(from = files, to = dest_path)
}


import_file <- function(files, analysis_dir, overwrite){
  d <- data.frame(files, analysis_dir)
  dest_path <- sapply(seq_len(nrow(d)), function(i) relative_path(d$files[i], d$analysis_dir[i]))
  file.copy(files, dest_path, overwrite = overwrite)
}


##      find_code("script.R") %>% import_code

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



