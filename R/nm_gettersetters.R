#' @include utilsExtra.R

NULL

#' Path to NONMEM control file
#' 
#' Get and set path to control file.
#' 
#' @param m nm object
#' @param text optional character. Name of path to control file (see details).
#'   Typically, this file does not yet normally exist, but will house the code
#'   code for this run
#' 
#' @details "text" can contain "{run_id}" string.  E.g. "Models/run{run_id}.mod"
#'   will use the name "Models/runm1.mod" if run_id(m1) is "m1".
#' 
#' @examples
#' \dontrun{
#' 
#' m0 <- nm(run_id = "m0")
#' ctl_name(m0)
#'
#' m0 <- m0 %>% ctl_name("run{run_id}.mod")
#' ctl_name(m0)
#' ## warning: this has make the run identifier in old xpose4 "_m0" not "m0"! ## deleteme
#'
#' m0 <- m0 %>% ctl_path("Models/run_{run_id}.mod")
#' ctl_path(m0)
#' 
#' 
#' }
#' @export
ctl_path <- function(m, text) {
  if(missing(text)) file.path(run_in(m), ctl_name(m)) else {
    m <- m %>% run_in(dirname(text))
    m <- m %>% ctl_name(basename(text))
    m
  }
}

#' @export
run_dir <- function(m, text, full_path = FALSE) {
  UseMethod("run_dir")
}
#' @export
run_dir.nm <- function(m, text, full_path = FALSE) m$run_dir
#' @export
run_dir.nm_generic <- function(m, text, full_path = FALSE) {
  if(missing(text)) {
    if(!full_path) custom_1d_field(m, "run_dir") else
      file.path(run_in(m), custom_1d_field(m, "run_dir"))
  } else custom_1d_field(m, "run_dir", text, glue = TRUE)
}
#' @export
run_dir.nm_list <- run_dir.nm_generic
#run_dir.nm_list <- Vectorize_nm_list(run_dir.nm_generic, SIMPLIFY = TRUE)

#' @export
cmd <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "cmd") else {
    clean_text <- text[grepl("\\-clean\\=", text)]
    if(length(clean_text)) {
      clean_number <- gsub(".*\\-clean\\=([0-9]+).*", "\\1", clean_text)
      clean_number <- as.numeric(clean_number)
      if(clean_number > 2)
        warning("use of -clean flag more than 2 is not recommended with NMproject.
NMproject uses files in the NM_run directories instead of files copied back to main directory.
see ?ls_tempfiles for post run clean up options")
    }
    custom_1d_field(m, "cmd", text, glue = TRUE)
  }
}

#' @export
type <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "type") else custom_1d_field(m, "type", text)
}

#' @export
parent_run_id <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "parent_run_id") else custom_1d_field(m, "parent_run_id", as.character(text))
}

#' @export
parent_run_in <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "parent_run_in") else custom_1d_field(m, "parent_run_in", as.character(text))
}

#' @export
parent_ctl_name <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "parent_ctl_name") else custom_1d_field(m, "parent_ctl_name", as.character(text))
}

#' @export
unique_id <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "unique_id") else custom_1d_field(m, "unique_id", text, glue = TRUE)
}

#' @export
ctl_name <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "ctl_name") else custom_1d_field(m, "ctl_name", text, glue = TRUE)
}

#' @export
results_dir <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "results_dir") else custom_1d_field(m, "results_dir", text, glue = TRUE)
}

#' @export
version <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "version") else custom_1d_field(m, "version", text)
}

#' @export
executed <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "executed") else custom_1d_field(m, "executed", text)
}

#' @export
lst_path <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "lst_path") else custom_1d_field(m, "lst_path", text, glue = TRUE)
}

#' @export
job_info.nm_generic <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "job_info") else custom_1d_field(m, "job_info", as.character(text))
}
#' @export
job_info.nm_list <- Vectorize_nm_list(job_info.nm_generic)

## run_in UseMethod already defined
#' @export
run_in.nm_generic <- function(x, text){
  if(missing(text)) custom_1d_field(x, "run_in") else {
    x <- custom_1d_field(x, "run_in", text)
    
    ## additional code to ensure data_path is redone
    data_path <- data_path(x)
    x <- x %>% data_path(data_path) ## reset data path
    x
  }
}
#' @export
run_in.nm_list <- Vectorize_nm_list(run_in.nm_generic, pre_glue = TRUE)

## run_id UseMethod already definied
#' @export
run_id.nm_generic <- function(m, text, ...){
  if(missing(text)) {
    #if(is_single_na(m)) return(NA_character_)
    if(length(m[["run_id"]]) > 0) return(m[["run_id"]]) else return(NA_character_)
  }
  
  m[["job_info"]] <- NA_character_  ## wipe job info
  #m[["paUseMethod()rent_run_id"]] <- m[["run_id"]]
  m[["run_id"]] <- as.character(text)
  
  ## Do all glueing first.
  for(field in names(m$glue_fields)){
    m <- replace_tag(m, field)
  }
  
  ## why was this here?
  #if("ctl_contents" %in% names(m)) m <- m %>% ctl_contents(m[["ctl_contents"]]) ## update ctl_contents object
  m
}
#' @export
run_id.nm_list <- Vectorize_nm_list(run_id.nm_generic)

#' @export
result_files <- function(r, text){
  UseMethod("result_files")
}
#' @export
result_files.nm_generic <- function(r, text){
  if(missing(text)) return(r[["result_files"]])
  r[["result_files"]] <- unique(c(r[["result_files"]], text))
  r
}
#' @export
result_files.nm_list <- Vectorize_nm_list(result_files.nm_generic, SIMPLIFY = FALSE)

#' @export
find_result_files <- function(r, pattern = "."){
  UseMethod("find_result_files")
}
#' @export
find_result_files.nm_generic <- function(r, pattern = "."){
  res <- r[["result_files"]]
  res <- res[grepl(pattern, res)]
  res <- file.path(results_dir(r), res)
  res
}
#' @export
find_result_files.nm_list <- Vectorize_nm_list(find_result_files.nm_generic, SIMPLIFY = TRUE)

#' set/get control file object
#'
#' @param m nm object
#' @param ctl_ob optional path to control file
#' @param update_numbering logical. Should table numbers and author fields be updated
#' @param update_dollar_data logical. Should $DATA in control file be updated
#' @param ... additional arguments
#' 
#' @export

ctl_contents <- function(m, ctl_ob, update_numbering = TRUE, update_dollar_data = TRUE, ...){
  #.Deprecated("show_ctl")
  UseMethod("ctl_contents")
}

#' @export
ctl_contents.nm_generic <- function(m, ctl_ob, update_numbering = TRUE, update_dollar_data = TRUE, ...){
  
  if(missing(ctl_ob)){
    if(length(m[["ctl_contents"]]) > 0) return(m[["ctl_contents"]]) else return(NA_character_)
  }
  
  #if(inherits(try(ctl_list2(ctl_ob)), "try-error")) browser()
  
  ## if NA just set field as NA
  if(is_single_na(ctl_ob)) {
    m[["ctl_contents"]] <- NA_character_
    return(m)
  }
  
  ctl <- ctl_list2(ctl_ob)
  
  if(update_numbering){
    for(i in which(names(ctl) %in% "TABLE"))
      ctl[[i]] <- gsub(paste0("(FILE\\s*=\\s*\\S*tab)\\S*\\b"),paste0("\\1",run_id(m)),ctl[[i]])
    ctl[[1]] <- gsub("^(\\s*;;\\s*[0-9]*\\.\\s*Based on:).*",paste("\\1",parent_run_id(m)),ctl[[1]])
    ctl[[1]] <- gsub("^(\\s*;;\\s*\\w*\\.\\s*Author:).*",paste("\\1",Sys.info()["user"]),ctl[[1]])
  }
  
  m[["ctl_contents"]] <- ctl
  ## set as ctl_orig only if ctl_orig doesn't exist
  #if(!"ctl_orig" %in% names(m)) m[["ctl_orig"]] <- ctl
  if(is_single_na(m[["ctl_orig"]])) m[["ctl_orig"]] <- ctl
  
  ## overwrite the data_path field if it's blank
  data_path <- data_path(m)
  if(is.na(data_path) & "DATA" %in% names(ctl)){# & !from_staging){
    ## NOTE: this assumes run is from same run_in(m) directory
    data_name <- gsub("^ *\\$DATA\\s*([^ ]+).*$","\\1",ctl$DATA)[1]
    data_path <- normalizePath(file.path(run_in(m), data_name), mustWork = FALSE)
    
    if(file.exists(data_path)){
      ## issue - run_in might not exist
      ## remove dir/.. segments
      while(grepl("\\.\\.", data_path)){
        data_path <- gsub(
          paste0("[^",.Platform$file.sep,"\\.]+", .Platform$file.sep, "\\.\\.", .Platform$file.sep),
          "",
          data_path)
      }
      
      data_path <- relative_path(data_path, getwd())
      m[["data_path"]] <- data_path
    }
  }
  
  if(!is.na(data_path) & update_dollar_data)
    m <- m %>% fill_dollar_data(data_path)
  
  m
}
#' @export
ctl_contents.nm_list <- Vectorize_nm_list(ctl_contents.nm_generic, SIMPLIFY = FALSE, replace_arg = "ctl_ob", pre_glue = TRUE)

## minimal version of ctl_contents (just sets ctl_contents without mods)
## for internal package use
ctl_contents_simple <- function(m, ctl_ob, ...){
  ctl_contents(m, ctl_ob, update_numbering = FALSE, update_dollar_data = FALSE, ...)
}

#' Add a prior ctl file contents to object
#'
#' @param m nm object
#' @param ctl_ob path to control file
#' @param update_numbering logical. Should table numbers and author fields be updated
#' @param update_dollar_data logical. Should $DATA in control file be updated
#' @param ... additional arguments
#' 
#' @export

prior_ctl <- ctl_contents
