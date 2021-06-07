#' Convert R markdown scripts to vignettes
#'
#' @description 
#' 
#' `r lifecycle::badge("experimental")`
#'
#' Copies (by default) all scripts `s01_XXX.Rmd`, `s02_XXX.Rmd` into the
#' "vignettes" and reformats so they meet vignette standards.  Use of
#' [devtools::build_vignettes()] can then be used to build vignettes.
#' 
#' @param script_files Optional character vector of scripts.  If empty will find
#' scripting making the `s##_XXX.Rmd` convention.  Must be .Rmd files
#' @param overwrite Logical (default = `FALSE`). Overwrites existing vignettes
#'   of the same name.
#'
#' @details Uses of [decision()] must pass without stopping so these must have
#'   been run interactively prior to use of [devtools::build_vignettes()].
#'   
#' @export
rmd_to_vignettes <- function(script_files, overwrite = FALSE){
  
  if(missing(script_files))
    script_files <- dir(nm_default_dir("scripts"), "s[0-9]+_.*?\\.Rmd$", full.names = TRUE)
  
  for(script in script_files){
    
    contents <- readLines(script)
    vignette_path <- file.path("vignettes", basename(script))
    
    match_start <- grep("\\{r setup", contents)
    if(length(match_start) == 0) stop("can't find setup block in", script, call. = FALSE)
    if(length(match_start) > 1) stop("multiple setup block matches in", script, call. = FALSE)
    
    vignette_title <- tools::file_path_sans_ext(basename(script))
    
    vignette_start <- c("---", 
                        paste0("title: \"",vignette_title,"\""), 
                        "output: rmarkdown::html_vignette", 
                        "vignette: >", 
                        paste0("  %\\VignetteIndexEntry{",vignette_title,"}"), 
                        "  %\\VignetteEngine{knitr::rmarkdown}", 
                        "  %\\VignetteEncoding{UTF-8}", 
                        "---", 
                        "", 
                        "```{r, include = FALSE}", 
                        "knitr::opts_chunk$set(", "  collapse = TRUE,", "  comment = \"#>\"", ")", 
                        "```", 
                        "")
    
    contents <- c(vignette_start, contents[match_start:length(contents)])
    
    if(!file.exists(vignette_path) | overwrite){
      if(!file.exists("vignettes")) {
        ## switch to local project
        current_proj <- try(usethis::proj_get(), silent = TRUE)
        if (inherits(current_proj, "try-error")) {
          current_proj <- NULL
        }
        usethis::proj_set(getwd())
        on.exit(usethis::proj_set(current_proj))
        
        ## the following follows the usethis::use_vignette() steps without opening a file
        usethis::ui_silence(usethis::use_package("knitr", "Suggests"))
        desc::desc_set("VignetteBuilder", "knitr")
        usethis::use_git_ignore("inst/doc")
        usethis::use_directory("vignettes")
        usethis::use_git_ignore(c("*.html", "*.R"), directory = "vignettes")
        usethis::ui_silence(usethis::use_package("rmarkdown", "Suggests"))
        usethis::proj_set(current_proj)
      }
      write(contents, vignette_path)
      usethis::ui_done(paste0("Writing '", vignette_path, "'"))      
    } else {
      if(file.exists(vignette_path))
        usethis::ui_oops(paste0("Cannot overwrite '", vignette_path, "' use 'overwrite' argument is TRUE"))      
    }
    
  }
  
}

