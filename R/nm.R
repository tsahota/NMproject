
search_ctl_name <- function(r, models_dir=nm_default_dir("models")){
  if(inherits(r,"nm")) ctl_name <- r$ctl
  if(inherits(r,"numeric") | inherits(r,"character")) {
    r <- as.character(r)
    rtemp <- normalizePath(r, mustWork = FALSE)
    if(file.exists2(rtemp)) ctl_name <- rtemp else {
      stop("cant find ctl_name")
    }
  }
  ctl_name
}

file.exists2 <- function(x){ ## only true is file exists and is not a directory
  if(!file.exists(x)) return(FALSE)
  !file.info(x)$isdir
}

ctl_table_files <- function(ctl){
  UseMethod("ctl_table_files") 
}

ctl_table_files.default <- function(ctl){ 
  ctl <- ctl_character(ctl)
  s0 <- rem_comment(ctl)
  s <- grep("FILE\\s*=\\s*(\\S+)",s0,value=TRUE)
  table_files <- gsub(".*FILE\\s*=\\s*(\\S+)\\s*.*$","\\1", s)
  table_files
}

#' Setup demo in current directory
#'
#' @description
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Following through the demo is the fastest way to learn the syntax of
#' NMproject. The default demo is a Theophylline ("theopp") pharmacometric analysis.  Scripts will
#' be copied numbered `s01_XXX.Rmd`, `s02_XXX.Rmd` in the `"Scripts"` directory and a
#' dataset into `"SourceData"`. The `"staging"` area will also be pre-filled with
#' the code library model, `"ADVAN2.mod"`.  To practice copying this yourself, see
#' [code_library()] for how the app works.
#'
#' @param demo_name Character. Name of demo. Default = "theopp".  See details to find other demos
#' @param overwrite Logical. Default changed to FALSE.
#' @param additional_demo_locations Character vector. default = NULL. Locations
#'   for demo directories.
#'   
#' @details Available `demo_name` correspond to directory locations in 
#' `system.file("extdata","examples",package = "NMproject")`
#' 
#' @seealso [code_library()]
#' @export

setup_nm_demo <- function(demo_name = "theopp",
                          overwrite=FALSE,
                          additional_demo_locations = NULL){
  
  examples_dir <- character()
  examples_dirs <- character()
  
  if(length(additional_demo_locations) > 0) {
    examples_dir <- normalizePath(additional_demo_locations, mustWork = FALSE)
    examples_dirs <- list.files(examples_dir, full.names = TRUE, recursive = FALSE)
    #examples_dir <- examples_dirs[grepl(paste0(.Platform$file.sep, demo_name,"$"), examples_dirs)]
  }
  ## TODO: rename examples to demos
  examples_dirs <- append(examples_dirs, 
                          list.files(system.file("extdata","examples",package = "NMproject"),
                                     full.names = TRUE, recursive = FALSE))
  matched_examples_dirs <- examples_dirs[grepl(paste0(.Platform$file.sep, demo_name,"$"), examples_dirs)]
  
  if(length(matched_examples_dirs) == 0)
    stop("demo not found.\nAvailable demos:\n ",
         paste(unique(basename(examples_dirs)), collapse = "\n "), call. = FALSE)
  
  #examples_dir <- append(examples_dir, system.file("extdata","examples",demo_name,package = "NMproject"))
  examples_dir <- matched_examples_dirs[1]
  
  files_to_copy <- dir(examples_dir, all.files = TRUE, full.names = TRUE, recursive = TRUE)
  
  stage_info <- stage(files_to_copy, overwrite = overwrite, silent = TRUE)
  
  import(stage_info, overwrite = overwrite)
  
}


#' Run all project scripts sequentially
#'
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#'
#' Runs/renders all scripts `s01_XXX`, `s02_XXX` in the designated "scripts"
#' directory.
#' 
#' @param index Numeric index for subsetting list of scripts before running.
#' @param quiet Argument passed to [rmarkdown::render()].
#'
#' @details Works with .R and .Rmd extensions.  Behaviour is to [source()] .R
#'   files and use [rmarkdown::render()] on .Rmd files
#'
#' @export
run_all_scripts <- function(index, quiet = FALSE){
  
  script_files <- dir(nm_default_dir("scripts"), "s[0-9]+_.*?\\.R(md)?$", full.names = TRUE)
  
  dplan <- dplyr::tibble(script_files) %>%
    dplyr::mutate(rmd = grepl("\\.Rmd", .data$script_files))
  
  dplan <- dplan %>%
    dplyr::mutate(
      cmd = ifelse(.data$rmd, 
                   paste0("rmarkdown::render(\"",script_files,"\", quiet = ", quiet, ")"),

                                      paste0("source(\"",script_files,"\")"))
    )
  
  if(!missing(index)) dplan <- dplan[index, ]
  
  exprs <- rlang::parse_exprs(dplan$cmd)
  
  res <- lapply(exprs, rlang::eval_tidy)
  return(invisible(TRUE))
  
}


#' Convert Rmarkdown scripts to vignettes
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
      write(contents, vignette_path)
      usethis::ui_done(paste0("Writing '", vignette_path, "'"))      
    } else {
      if(file.exists(vignette_path))
        usethis::ui_oops(paste0("Cannot overwrite '", vignette_path, "' use 'overwrite' argument is TRUE"))      
    }
    
  }

}



update_ignore <- function(ctl, ignore_char){
  ctl <- ctl_list(ctl)
  
  ignore_present <- any(grepl(".*IGNORE\\s*=\\s*\\(",ctl$DATA))
  if(ignore_present){
    ## remove any row that matches exactly
    ctl$DATA <- ctl$DATA[!grepl("^(\\s*)IGNORE\\s*=\\s*\\(*\\S[^\\)]+\\)*(\\s*)$",ctl$DATA)]
    ## remove only bracketed IGNORE statement if other things are on the line.
    ctl$DATA <- gsub("(.*)IGNORE\\s*=\\s*\\(+\\S[^\\)]+\\)+(.*)",
                     "\\1\\2", ctl$DATA)
  }
  
  ignore_char <- gsub("\\s*\\|\\s*", ", ", ignore_char)
  
  ignore_char <- gsub("==",".EQ.",ignore_char)
  ignore_char <- gsub("!=",".NE.",ignore_char)
  ignore_char <- gsub(">",".GT.",ignore_char)
  ignore_char <- gsub("<",".LT.",ignore_char)
  ignore_char <- gsub(">=",".GE.",ignore_char)
  ignore_char <- gsub("<=",".LE.",ignore_char)
  ignore_char <- gsub("\\s+(\\.\\S+\\.)\\s+", "\\1", ignore_char)
  
  ignore_char <- paste0("IGNORE=(",ignore_char,")")
  
  last_line <- ctl$DATA[length(ctl$DATA)]
  
  if(grepl("^\\s*$", last_line)){
    ctl$DATA[length(ctl$DATA)] <- ignore_char
  } else {
    ctl$DATA <- append(ctl$DATA, ignore_char) 
  }
  ctl$DATA <- append(ctl$DATA, "")
  ctl
  
}

#' Exclude rows of NONMEM dataset
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#'
#' A mechanism for excluding outliers during data cleaning.  Create exploratory
#' plots, identify rows of the dataset to be considered outliers for exclusion,
#' and then feed that filtered dataset into this function to exclude them from
#' the dataset.  Requires a corresponding `IGNORE` statement - see argument
#' descriptions for more details.
#' 
#' 
#' @param d A `data.frame` for containing the full NONMEM dataset.  Should
#'   contain a column for identifying excluded rows named with the `exclude_col`
#'   argument.
#' @param dexcl A smaller `data.frame` consisting of rows to be ignored.  Need
#'   not contain all columns of `d` but each column should be present in `d`.
#' @param exclude_col Character (default = `"EXCL"`). Name of a binary exclude
#'   column in `d`. This should be accompanied with a `IGNORE=(EXCL.GT.0)`
#'   statement in $DATA.
#'
#' @return A modified version of `d` with `exclude_col` set to `1` for rows
#'   coinciding with `dexcl`.
#' 
#' @seealso [read_derived_data()], [write_derived_data()]
#' 
#' @examples
#' \dontrun{
#' ## use with dplyr
#' dexcl <- d %>% filter(ID == 23, TIME > 18, TIME < 24) %>% select(ID, TIME, DV, EXCL)
#' dexcl  ## view rows to be excluded
#' d <- d %>% exclude_rows(dexcl)
#' }
#' @export

exclude_rows <- function(d, dexcl, exclude_col = "EXCL"){
  if(any(!names(dexcl) %in% names(d))) stop("dexcl must contain a subset of the columns of d")
  excluded <- do.call(paste, d[, names(dexcl)]) %in% do.call(paste, dexcl)
  if(nrow(dexcl) != length(which(excluded))) stop("couldn't find all rows")
  if(!exclude_col %in% names(d)) d[[exclude_col]] <- 0
  d[[exclude_col]][excluded] <- 1
  d
}

