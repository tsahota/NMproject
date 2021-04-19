options(nm_dirs = list(models_dir = "Models", 
                       scripts_dir = "Scripts",
                       results_dir = "Results",
                       "SourceData",
                       "Data"))

## how to set config - use options like above
##  either multiple options or a fixed set
## worst case scenario use the AZ standard, that's what the existing package is using
## but there is an opportunity to make it make much more flexible.

## Organisations will have to set their options via some global means.
## Would be good to have an nm_dirs() function to get/set
## models_dir and scripts_dir are mandatory

## This is good enough

nm_dirs <- function(dir_list){
  if(missing(dir_list)) return(getOption("nm_dirs"))
  ## now assume we're setting
  ## validate input
  if(!"scripts_dir" %in% names(dir_list)) 
    stop("one entry of the directory needs the name \"scripts_dir\"")
  if(!"models_dir" %in% names(dir_list)) 
    stop("one entry of the directory needs the name \"models_dir\"")
  options(nm_dirs = dir_list)
}

scripts_dir <- function(proj_name = getwd()) {
  normalizePath(file.path(proj_name, nm_dirs$scripts_dir), 
                winslash = "/", 
                mustWork = FALSE)
}

models_dir <- function(proj_name = getwd()) {
  normalizePath(file.path(proj_name, nm_dirs$models_dir),
                winslash = "/", 
                mustWork = FALSE)
}

nm_create_simple_analysis_project <- function(name, folder, dirs = nm_dirs(), 
                                              style = c("simple",
                                                        "starters-simple", 
                                                        "starters-package"), ...){
  
  style <- match.arg(style)
  
  if(style == "simple"){
    path <- file.path(folder, name)
    
    dir.create(path)
    usethis::use_git()
    usethis::create_project(path = path, rstudio = TRUE, open = FALSE)
    current_proj <- usethis::proj_get()
    usethis::proj_set(path)
    on.exit(usethis::proj_set(current_proj))
    for(dir_name in dirs) usethis::use_directory(dir_name, ignore = TRUE)

    tryCatch(usethis::use_description(), error = function(e){
      usethis::ui_info("skipping creation of {usethis::ui_path('DESCRIPTION')}")
    })
    
    tryCatch(usethis::use_namespace(), error = function(e){
      usethis::ui_info("skipping creation of {usethis::ui_path('NAMESPACE')}")
    })
    
    tryCatch(usethis::use_vignette(name = name), error = function(e){
      usethis::ui_info("skipping creation of {usethis::ui_path('vignette/')}")
    })
    
    usethis::use_readme_rmd(open = FALSE)

  }
  
}

# Thin wrapper around starters::create_analysis project
nm_create_analysis_project <- function(name, folder, dirs = nm_dirs(), 
                                       style = c("non-package", "package"), ...){
  starters::create_analysis_project(name = name, 
                                    folder = folder,
                                    dirs = dirs, 
                                    git = FALSE,
                                    external_setup = NULL)
}

# This doesn't allow dirs
nm_create_package_project <- function(name, folder, dirs = nm_dirs(), ...){
  starters::create_package_project(name = name, 
                                   folder = folder,
                                   git = FALSE,
                                   external_setup = NULL)
}

