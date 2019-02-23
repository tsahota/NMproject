## TODO: desired interface

e14 <- nm("e14", type = "execute", based_on = "e13")
build_run({
  e14 %>% update_parameters() %>%
    manual_edit("KEFF log-normal -> normal")
})

e14$cmd("qpsn -c auto -r 1000 -t 3000 -- execute run_e14.mod -dir=e14")

e14 %>% run_nm  ## will commit and save hashes
e14 %>% wait_for_finished %>% update_parameters %>% run_nm

## following uses wait_for_finished if needed
future({ ## asynchronous events
  #e14 %>% wait_for_finished - no longer needed
  e14 %>% covariance_result() %>% file.show()
  e14 %>% basic_diag1(type = 12)
  e14 %>% ind_diag1(type = 12, n = 20)  
})

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



