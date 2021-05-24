#' Get job information (if it exists)
#'
#' Requires 'get_job_info' to be defined as an option - function that takes stdout console output
#' from a job and returns a character
#'
#' @param m object class nm
#' @param text optional character to set job_info
#' @export
job_info <- function(m, text){
  UseMethod("job_info")  
}

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

#' Condition number of run
#' 
#' @param r object of class nm
#' @export

cond_num <- function(r){
  UseMethod("cond_num")
}

#' @export
cond_num.default <- function(r){
  if(is_single_na(r)) return(as.numeric(NA))
  if(is.data.frame(r)){
    dc <- r
    ans <- as.numeric(dc$FINAL[dc$parameter %in% "CONDNUM"])
    if(length(ans) == 0) ans <- as.numeric(NA)
    return(ans)
  }
  stop("don't know how to get cond_num of this")
}

#' @export
cond_num.list <- function(r){
  sapply(r, cond_num)
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
#' Runs all scripts s01_XXX, s02_XXX in the designated "scripts" directory
#' 
#' @param quiet Argument passed to `rmarkdown::render`
#'
#' @details works with .R and .Rmd extensions.  Behaviour is to `source` .R
#'   files and use `rmarkdown::render` on .Rmd files
#'
#' @export
run_all_scripts <- function(quiet = FALSE){
  
  script_files <- dir(nm_default_dir("scripts"), "s[0-9]+_.*?\\.R(md)?$", full.names = TRUE)
  
  dplan <- dplyr::tibble(script_files) %>%
    dplyr::mutate(rmd = grepl("\\.Rmd", .data$script_files))
  
  dplan <- dplan %>%
    dplyr::mutate(
      cmd = ifelse(.data$rmd, 
                   paste0("rmarkdown::render(\"",script_files,"\", quiet = ", quiet, ")"),
                   paste0("source(\"",script_files,"\")"))
    )
  
  exprs <- rlang::parse_exprs(dplan$cmd)
  
  res <- lapply(exprs, rlang::eval_tidy)
  return(invisible(TRUE))
  
}


#' Get NONMEM output tables
#'
#' This combines $TABLE output with the input data, allowing text columns to be retained for plotting/summaries.
#'
#' @param r data.frame.  object of class nmexecute
#' @param dorig data.frame. optional NONMEM input dataset.
#' @param ... additional arguments to pass on to read.csv
#' @export

nm_output <- function(r,dorig,...){
  UseMethod("nm_output")  
}

#' Get ignore statement
#' @param r object coercible into ctl_list
#' @param data data.frame (default = missing) optional input dataset from r
#' @export
data_ignore_char <- function(r, data){
  UseMethod("data_ignore_char")
}


#' Get filter statement
#' 
#' Opposite of data_ignore_char 
#' 
#' @param r object coercible into ctl_list
#' @param ... arguments passed to data_ignore_char
#' @export
data_filter_char <- function(r, ...){
  ignore_char <- data_ignore_char(r, ...)
  if(ignore_char == "FALSE") return("TRUE")
  ignored <- !grepl("^!\\((.*)\\)", ignore_char)
  accepted <- !ignored
  if(accepted){
    return(gsub("^!\\((.*)\\)", "\\1", ignore_char) )
  } else {
    return(paste0("!(",ignore_char,")"))
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
#' @param d data.frame for NONMEM dataset
#' @param dexcl data.frame consisting of rows to be ignored
#' @param exclude_col character.  Name of exclude column in d
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

