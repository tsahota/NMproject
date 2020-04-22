#' @include utilsExtra.R

## internal function
nm_generic <- function(run_id = NA_character_,
                       run_in = getOption("models.dir"),
                       parent_run_id = NA_character_,
                       parent_run_in = NA_character_,
                       ctl_name = "run{run_id}.mod",
                       type = "execute",
                       run_dir = "{run_id}_{version}",
                       results_dir = "Results",
                       lst_path = "{run_dir}/NM_run1/psn.lst"){
  
  #m <- new.env()
  m <- list()
  #if(is.na(run_id)) stop("require run_id argument to be specified", call. = FALSE)
  
  ## test if run_id = "execute run... ..." stop with error
  ## about backward compatibility issue
  ##  1. it has spaces
  if(grepl("\\s", run_id)) 
    stop("run_id cannot contain spaces.
NOTE: If you are trying to do something like:
         nm('execute run1.mod -dir=1')
You are trying to use the old NMproject alpha interface.
This is the newer beta interface which is not backwards compatible. 
To use the alpha interface, install NMproject 0.3.2",
         call. = FALSE)
  
  class_name <- paste0("nm_",type)
  class(m) <- c(class_name, "nm_generic",class(m))
  
  m[["type"]] <- type
  m[["run_id"]] <- as.character(run_id)
  m[["run_in"]] <- as.character(run_in)
  m[["parent_run_id"]] <- as.character(parent_run_id)
  m[["parent_run_in"]] <- as.character(parent_run_in)
  m[["job_info"]] <- NA_character_
  m$target <- NA_character_
  m$version <- as.integer(1)
  m$executed <- FALSE
  m$result_files <- c()
  
  unique_id <- "{type}.{file.path(run_in,run_dir)}"
  ## the following is in order of glueing
  m$glue_fields <- c(run_dir, ctl_name, results_dir, unique_id, lst_path, NA_character_)
  names(m$glue_fields) <- c("run_dir", "ctl_name", "results_dir", "unique_id", "lst_path", "data_path")
  
  for(field in names(m$glue_fields)){
    m <- replace_tags(m, field)
  }
  
  # ## if ctl file already exists, bring it into object
  # ctl_path <- ctl_path(m)
  # if(file.exists(ctl_path)){
  #   m <- m %>% ctl_contents(ctl_path)
  # }
  
  ## leave space to track output files (do in run_nm)
  m
}

#' Create core NM object
#' 
#' Experimental new nm object interface.  Not compatible with previous system.
#'   The basic object this package centres around.  Most package functions act on this object.
#' 
#' @param run_id character vector. Run identifier
#' @param run_in character vector. ctl file and run location  
#' @param parent_run_id character vector (optional). Run identifier of previous run 
#' @param parent_run_in character vector (optional). Run location of previous run 
#' @param ctl_name character. Name of control file
#' @param type character (default = "execute").  Type of run to run
#' @param run_dir character (default = "{run_id}").  Subdirectory where PsN wll run NONMEM
#' @param results_dir character (default = "Results").
#'    Directory to store results of this run
#' @param lst_path character (default = "{run_dir}/NM_run1/psn.lst") expected location of lst file
#' @param data_path character (default = NA) expected location of dataset file
#' 
#' @return An object of class nm_list.  Object is concatenatable.
#'    Length of object corresponds to length of run_id
#' @examples 
#' \dontrun{
#' 
#' m0 <- nm(run_id = "m0")
#' m0  ## a human readable representation
#'   
#' ## nm objects can be put into tibbles to group runs together
#' d <- tibble(run_id = c("m1","m2"))
#' d$m <- nm(d$run_id)
#' d  
#' 
#' }
#' 
#' @export
nm <- Vectorize_nm_list(nm_generic, SIMPLIFY = FALSE)

#' @export
is.na.nm_generic <- function(x) is.na(run_id(x))
#' @export
is.na.nm_list <- function(x) is.na(run_id(x))

#' Make child nm object from parent
#' 
#' Child objects inherit attributes of parent but with a new run_id.
#' The control file will be inherited too with $TABLEs updated
#' 
#' @param m parent nm object
#' @param run_id character.  New run id to assign to child object
#' @param type character (default = "execute"). type of child object
#' @param silent logical (default = FALSE)
#' 
#' @examples 
#' \dontrun{
#' 
#' m2 <- m1 %>% child("m2")
#' 
#' }
#' @export
child <- function(m, run_id = NA_character_, type = "execute", silent = FALSE){
  UseMethod("child")
}
#' @export
child.nm_generic <- function(m, run_id = NA_character_, type = "execute", silent = FALSE){
  mparent <- m
  if(is.environment(m)){
    old_classes <- class(m)
    m <- as.environment(as.list(m, all.names=TRUE))
    class(m) <- old_classes 
  }

  m <- m %>% executed(FALSE)
  m <- m %>% job_info(NA_character_)
  m[["result_files"]] <- c()
  m <- m %>% version(as.integer(1))
  m <- m %>% parent_run_id(run_id(m))
  m <- m %>% parent_run_in(run_in(m))
  if(!is.na(run_id)) m <- m %>% run_id(run_id)
  if(!type %in% "execute") m <- m %>% type(type)
  m[["ctl_orig"]] <- m[["ctl_contents"]]  ## reset ctl_orig
  
  ## check for file conficts
  file_conflicts <- intersect(psn_exported_files(mparent), psn_exported_files(m))
  if(length(file_conflicts) > 0){
    if(!silent) warning("Child file(s) currently in conflict with parent:\n",
                        paste(paste0(" ",file_conflicts),collapse="\n"),
                        "\nYou will overwrite parent object outputs if you run now", call. = FALSE)
  }
  
  ## warn if ctl_name or run_dir aren't glueable
  relevant_glue_fields <- m$glue_fields[c("ctl_name", "run_dir")]
  non_glued_glue_fields <- relevant_glue_fields[!grepl("\\{", relevant_glue_fields)]
  if(length(non_glued_glue_fields) > 0){
    if(!silent) warning("Following parents attributes do not use {glue} fields:\n", 
                        paste(paste0(" ",names(non_glued_glue_fields)), collapse="\n"), 
                        "\nThese fields will be identical to parent and may result in conflicts",
                        "\nIf this is unintended, make sure parent object uses {glue} notation for these attributes", call. = FALSE)
  }
  m
}
#' @export
child.nm_list <- Vectorize_nm_list(child.nm_generic, SIMPLIFY = FALSE)

#' Determine if runs are expected to have overlapping output files (from PsN)
#' 
#' @param m nm_list object
#' @export 
overlapping_outputs <- function(m){
  ## count number of times each appears
  files <- psn_exported_files(m)
  #files[[3]] <- c(files[[3]], files[[1]][1]) ## for testing only
  tibble::tibble(uid = names(files), files = files) %>%
    tidyr::unnest(files) %>%
    dplyr::group_by(files) %>%
    dplyr::summarise(conflicts = length(unique(.data$uid))-1,
              runs = paste(.data$uid, collapse = ", ")) %>% #add
    dplyr::arrange(dplyr::desc(.data$conflicts)) %>%
    dplyr::filter(.data$conflicts > 0)
}


#' @export
print.nm_generic <- function(x, ...){
  x <- as.list(x)
  collapse_fields <- c("ctl_contents", "ctl_orig")
  for(field in collapse_fields){
    if(field %in% names(x)) x[[field]] <- "...[collapsed]..."    
  }
  ## remove all raw fields from output
  remove_fields <- c("glue_fields")
  for(field in remove_fields) x[[field]] <- NULL    
  utils::str(x, ...)
}

#' @export
print.nm_list <- function(x, ...){
  for(i in seq_along(x)) {
    x[[i]] <- as.list(x[[i]])
    collapse_fields <- c("ctl_contents", "ctl_orig")
    for(field in collapse_fields){
      if(field %in% names(x[[i]])) x[[i]][[field]] <- "...[collapsed]..."
    }
    remove_fields <- c("glue_fields")
    for(field in remove_fields) x[[i]][[field]] <- NULL    
  }
  utils::str(x, ...)
}

custom_1d_field <- function(m, field, replace, glue = FALSE){
  UseMethod("custom_1d_field")
}
custom_1d_field.nm_generic <- function(m, field, replace, glue = FALSE){
  if(missing(replace)){
    if(length(m[[field]]) > 0) return(m[[field]]) else return(NA_character_)
  }
  
  ## Only update if there is a change
  if(field %in% names(m)){
    if(glue) old_glue_field <- m$glue_fields[[field]] else old_glue_field <- m[[field]]
    if(identical(replace, old_glue_field)) return(m)
  }

  if(glue){
    m$glue_fields[[field]] <- replace
    m[[field]] <- replace
    ## glue the field
    if(!is.na(replace)) m <- replace_tags(m, field)
  } else {
    m[[field]] <- replace
  }
  ## reglue all other glueable fields
  for(other_col in names(m$glue_fields)[!names(m$glue_fields) %in% field]) {
    m <- replace_tags(m, other_col)
  }

  m
}
custom_1d_field.nm_list <- Vectorize_nm_list(custom_1d_field.nm_generic, replace_arg = "replace")

#' @export
set_simple_field <- function(m, ...){
  UseMethod("set_simple_field")  
}

#' Add a simple field to nm object
#' 
#' @param m nm object
#' @param ... arguments to set fields
#' 
#' @examples 
#' \dontrun{
#' 
#' core_list <- c(1,4,12)
#' 
#' mc <- m1 %>% child(run_id = paste0(corelist)) %>%
#'   set_simple_field(cores = corelist) %>%
#'   cmd("qpsn -c {cores} -t 59 -- execute {ctl_name} -dir={run_dir}")
#' 
#' }
#' @export
set_simple_field <- function(m, ...){

  dots <- list(...)
  fields <- names(dots)

  for(i in seq_along(dots)){
    m <- m %>% custom_1d_field(field = fields[i], replace = dots[[i]])
  }
  m
}


#' @export
get_simple_field <- function(m, field){

  field <- rlang::enquo(field)
  field <- rlang::quo_name(field)
  
  m %>% custom_1d_field(field = field)
  
}

glue_text_nm <- function(m, text){
  UseMethod("glue_text_nm")
}
glue_text_nm.nm_generic <- function(m, text){
  stringr::str_glue(text, .envir = m)
}
glue_text_nm.nm_list <- Vectorize_nm_list(glue_text_nm.nm_generic, SIMPLIFY = TRUE)

replace_tags <- function(m, field){
  ## this function is rate limiting - use it as little as possible.
  ## only proceed if "raw" field exists
  if(field %in% names(m$glue_fields)){
    if(!is.na(m$glue_fields[[field]])){
      ## start by resetting to raw
      m[[field]] <- glue_text_nm(m, m$glue_fields[[field]])
      #m[[field]] <- stringr::str_glue(m$glue_fields[[field]], .envir = m)
      m[[field]] <- as.character(m[[field]])
    }
  }
  m
}


glue_fields <- function(m){
  UseMethod("glue_fields")
}

glue_fields.nm_generic <- function(m) m$glue_fields

glue_fields.nm_list <- Vectorize_nm_list(glue_fields.nm_generic, SIMPLIFY = FALSE)

## following is only to get, not to set 
get_glue_field <- function(m, field){
  UseMethod("get_glue_field")
}
get_glue_field.nm_generic <- function(m, field) m$glue_fields[[field]]
get_glue_field.nm_list <- Vectorize_nm_list(get_glue_field.nm_generic, SIMPLIFY = FALSE)

custom_vector_field <- function(m, field, replace){
  UseMethod("custom_vector_field")
}
custom_vector_field.nm_generic <- function(m, field, replace){
  if(missing(replace)){
    if(length(m[[field]]) > 0) return(m[[field]]) else return(NA_character_)
  }
  m[[field]] <- replace
  m
}
custom_vector_field.nm_list <- Vectorize_nm_list(custom_vector_field.nm_generic, SIMPLIFY = FALSE, replace_arg = "replace")


ctl_list2 <- function(r){
  UseMethod("ctl_list2")
}

ctl_list2.ctl_list <- function(r) r

ctl_list2.character <- function(r){
  if(length(r) == 1){
    ctl <- readLines(r)
  } else {
    ctl <- r
  }
  ctl_nm2r(ctl)
}

ctl_list2.nm_generic <- function(r) r[["ctl_contents"]]

ctl_list2.nm_list <- Vectorize_nm_list(ctl_list2.nm_generic, SIMPLIFY = FALSE)



new_ctl_extra <- function(m, ctl, dir = getOption("models.dir")){
  
  ctl$TABLE <- gsub(paste0("(FILE\\s*=\\s*\\S*tab)\\S*\\b"),paste0("\\1",run_id(m)),ctl$TABLE)
  ctl[[1]] <- gsub("^(\\s*;;\\s*[0-9]*\\.\\s*Based on:).*",paste("\\1",parent_run_id(m)),ctl[[1]])
  ctl[[1]] <- gsub("^(\\s*;;\\s*\\w*\\.\\s*Author:).*",paste("\\1",Sys.info()["user"]),ctl[[1]])
  
  ctl
}

#' Write control file to disk
#' 
#' Normally used by other functions
#' 
#' @param m nm object
#' 
#' @export
write_ctl <- function(m){
  UseMethod("write_ctl")
}

#' @export
write_ctl.nm_generic <- function(m){

  ctl_name <- ctl_path(m)
  ctl_ob <- ctl_contents(m) %>% ctl_character()
  dir_name <- run_in(m)
  
  if(!file.exists(dir_name)) 
    dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
  
  writeLines(ctl_ob, ctl_name)
  invisible(m)
}
#' @export
write_ctl.nm_list <- Vectorize_nm_list(write_ctl.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

delete_ctl <- function(m){
  UseMethod("delete_ctl")
}
delete_ctl.nm_generic <- function(m){
  unlink(ctl_path(m))
  invisible(m)
}
delete_ctl.nm_list <- Vectorize_nm_list(delete_ctl.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

#' @include ctl_handling.R
#' @export
update_dollar.nm_generic <- function(ctl,..., which_dollar = 1, append = FALSE){
  dots <- list(...)
  m <- ctl
  ctl <- ctl_contents(m)
  ctl_new <- update_dollar(ctl, ..., which_dollar = which_dollar, append = append)
  m <- m %>% ctl_contents(ctl_new)
  m
}
#' @export
update_dollar.nm_list <- Vectorize_nm_list(update_dollar.nm_generic, SIMPLIFY = FALSE)

  
#' Target part of control object for further modification
#' 
#' @param m nm object
#' @param dollar character. name of subroutine to target
#' @param lines optional character.  Assignment of lines
#' @export
target <- function(m, dollar, lines){
  UseMethod("target")
}
#' @export
target.nm_generic <- function(m, dollar, lines){
  if(missing(dollar)){
    if(length(m[["target"]]) > 0) return(m[["target"]]) else return(NA_character_)
  }
  
  ## TODO: expand to be able to target lines instead
  ## TODO: step 1 - make it vectorised
  
  #if(!missing(dollar) & !missing(lines)){
  #  stop("can't have both 'dollar' and 'lines' arguments")
  #}
  
  dollar_text <- gsub("\\$","",dollar)
  m <- m %>% custom_1d_field(field = "target", replace = dollar_text)
  #m <- m %>% custom_vector_field(field = "target", replace = list(dollar_text))
  
  #if(!missing(dollar)){
  #  dollar_text <- gsub("\\$","",dollar)
  #  m <- m %>% custom_1d_field(field = "target_dollar", replace = dollar_text)
  #}
  
  #if(!missing(lines)){
  #  m <- m %>% custom_vector_field(field = "target_lines", replace = list(lines))
  #}
  
  
  m
}
#' @export
target.nm_list <- Vectorize_nm_list(target.nm_generic, replace_arg = "dollar")
#target.nm_list <- Vectorize_nm_list(target.nm_generic, SIMPLIFY = FALSE, replace_arg = "dollar")

#' Remove target on control object
#' 
#' @param m nm object
#' @param dollar character. name of subroutine to target
#' @export
untarget <- function(m, dollar){
  UseMethod("untarget")
}
#' @export
untarget.nm_generic <- function(m, dollar){
  m[["target"]] <- NA_character_
  m
}
#' @export
untarget.nm_list <- Vectorize_nm_list(untarget.nm_generic, SIMPLIFY = FALSE)

get_target_text <- function(m){
  ## m is nm_generic
  ctl <- ctl_contents(m)
  target <- target(m) # m[["target"]]
  if(!is.na(target)) {
    if(target %in% names(ctl)) text <- ctl[[target]] else text <- NA_character_
  } else {
    text <- ctl_character(ctl)
  }
  text <- as.character(text)
  class(text) <- c("nm_ctl_text", class(text))
  text
}

#' @export
print.nm_ctl_text <- function(x, ...){
  cat(paste0(format(seq_along(x), width = 3),"| ",x), sep = "\n")
}

set_target_text <- function(m, text){
  ## m is nm_generic
  ctl <- ctl_contents(m)
  target <- target(m) # m[["target"]]
  if(!is.na(target)) {
    #if(append) text <- c(ctl[[target]],"",text)
    ctl[[target]] <- setup_dollar(text, paste0("$",target), add_dollar_text = FALSE)
    m <- m %>% ctl_contents(ctl, update_dollar_data = FALSE)
  } else {
    #if(append) text <- c(ctl_character(ctl),"",text)
    text <- ctl_list2(text)
    #text <- ctl_nm2r(text)
    m[["ctl_contents"]] <- text
  }
  m
}

#' @importFrom graphics text
#' @export
text.nm_generic <- function(x, text, append = FALSE, ...){
  m <- x
  current_text <- get_target_text(m)
  if(missing(text)) return(current_text)
  
  text <- paste(text, collapse = "\n")
  text <- strsplit(text, split = "\n")[[1]]
  #text <- trimws(text)
  
  if(append) text <- c(current_text,text)
  
  if(is.na(target(m))) {
    #stop("not developed yet")
  } else {
    text <- setup_dollar(text, paste0("$",target(m)), add_dollar_text = FALSE) 
  }
  m <- m %>% set_target_text(text)
  m
}
#' @export
text.nm_list <- Vectorize_nm_list(text.nm_generic, SIMPLIFY = FALSE, vectorize.args = c("x"))

#' comment lines of control file
#' 
#' @param m nm object
#' @param pattern optional character regex.  Passed to gsub
#' @export
comment_out <- function(m, pattern = ".*"){
  m %>% gsub_ctl(paste0("(",pattern,")"), "; \\1")
}

#' uncomment lines of control file
#' 
#' @param m nm object
#' @param pattern optional character regex.  Passed to gsub
#' @export
uncomment <- function(m, pattern = ".*"){
  m %>% gsub_ctl(paste0("^;+\\s*(",pattern,")"), "\\1")
}

#' Get/set path to dataset
#' 
#' @param m nm object
#' @param text optional character. Path to input dataset
#' @export
data_path <- function(m, text){
  UseMethod("data_path")
}
#' @export
data_path.nm_generic <- function(m, text){
  if(missing(text)){
    if(length(m[["data_path"]]) > 0) {
      return(custom_1d_field(m, "data_path")) 
    } else { 
      return(NA_character_)
    }
  }
  m <- m %>% custom_1d_field(field = "data_path", replace = text, glue = TRUE)
  
  if(!is.na(data_path(m))){
    ## update ctl contents
    m <- m %>% fill_dollar_data(text)
    # old_target <- m %>% target()
    # m <- m %>% target("$DATA")
    # 
    # data_name <- relative_path(text, run_in(m))
    # m <- m %>% gsub_ctl("^(\\s*\\$DATA\\s+)\\S+(.*)$",paste0("\\1",data_name,"\\2"))
    # 
    # m <- m %>% target(old_target)
  }
  
  m
}
#' @export
data_path.nm_list <- Vectorize_nm_list(data_path.nm_generic)

#' @export
fill_dollar_data <- function(m, data_name){
  old_target <- m %>% target()
  m <- m %>% target("$DATA")
  
  data_name <- relative_path(data_name, run_in(m))
  m <- m %>% gsub_ctl("^(\\s*\\$DATA\\s+)\\S+(.*)$",paste0("\\1",data_name,"\\2"))
  
  m <- m %>% target(old_target)
  m
}

#' set $INPUT
#' 
#' @param m nm object
#' @param keep character vector. Names of columns to keep
#' @param rename named character vector. Renaming instructions
#' 
#' @examples 
#' \dontrun{
#' 
#'  m1 <- m1 %>% fill_input(rename = c("DAT0" = "DATE"))
#' 
#' }
#' @export
fill_input <- function(m, ...){
  UseMethod("fill_input")
}
#' @export
fill_input.nm_generic <- function(m, ...){
  ctl <- ctl_contents(m)
  d <- suppressMessages(input_data(m))
  replace_with <- c("$INPUT", suppressMessages(dollar_input(d, ...)))
  old_target <- m %>% target()
  m <- m %>% target("INPUT") %>% text(replace_with) %>%
    target(old_target)
  m
}
#' @export
fill_input.nm_list <- Vectorize_nm_list(fill_input.nm_generic, SIMPLIFY = FALSE)


#' Read in input dataset
#' 
#' @param m nm object
#' @param filter logical (default = FALSE). should NONMEM ignore statement be applied
#' @param na character. passed to read.csv
#' @param ... additional arguments passed to either read_derived_data or read.csv
#' 
#' @export
input_data <- function(m, filter = FALSE, na = ".", ...){
  UseMethod("input_data")
}
#' @export
input_data.nm_generic <- function(m, filter = FALSE, na = ".", ...){
  file_name <- data_path(m)
  if(is.na(file_name)) return(tibble::tibble())
  
  if(normalizePath(dirname(file_name), mustWork = FALSE) == normalizePath("DerivedData", mustWork = FALSE)){
    d <- read_derived_data(basename(tools::file_path_sans_ext(file_name)),...)
  } else {
    d <- utils::read.csv(file_name, na = na, ...)
  }
  
  if(filter) {
    data_filter <- parse(text = data_filter_char(m, data = d))
    d <- subset(d, eval(data_filter))
  }
  d
}
#' @export
input_data.nm_list <- function(m, filter = FALSE, na = ".", ...){
  data_paths <- data_path(m)
  if(length(unique(data_paths)) != 1) stop("multiple data files detected. Aborting...")
  m <- as_nm_generic(m[[1]])
  d <- input_data(m, filter = filter, na = na, ...)
  d
}

#' @export
input_data.default <- function(m, filter = FALSE, na = ".", ...){   ## old get_data
  ## doesn't rely on data base or r object contents
  if(inherits(m, "nm")) {
    file_name <- file.path(run_in(m), get_data_name(ctl_character(m)))
  } else {
    from <- dirname(attr(m, "file_name"))
    file_name <- file.path(from, get_data_name(ctl_character(m)))
  }
  if(!grepl("[a-zA-Z0-9]",basename(file_name))) stop("$DATA doesn't look like it refers to a file. Is this correct?")
  
  if(normalizePath(dirname(file_name), mustWork = FALSE) == normalizePath("DerivedData")){
    d <- read_derived_data(basename(tools::file_path_sans_ext(file_name)),...)
  } else {
    d <- utils::read.csv(file_name, ...)
  }
  
  if(filter) {
    data_filter <- parse(text = data_filter_char(m, data = d))
    d <- subset(d, eval(data_filter))
  }
  d
}

#' Get/set ignore statement
#' 
#' @param ctl nm object
#' @param ignore_char optional character. Ignore statement
#' @export
ignore <- function(ctl, ignore_char){
  UseMethod("ignore")
}
#' @export
ignore.nm_generic <- function(ctl, ignore_char){
  m <- ctl
  if(missing(ignore_char)){
    return(data_ignore_char(m))
  }
  ctl <- ctl_contents(m)
  ctl <- update_ignore.default(ctl, ignore_char)
  m <- m %>% ctl_contents(ctl)
  m
}
#' @export
ignore.nm_list <- Vectorize_nm_list(ignore.nm_generic, replace_arg = "ignore_char")

#' @export
gsub_ctl.nm_generic <- function(ctl, pattern, replacement, ..., dollar = NA_character_){
  m <- ctl
  
  text <- get_target_text(m)
  text <- gsub(pattern, replacement, text, ...)
  
  m <- m %>% set_target_text(text)
  m
}
#' @export
gsub_ctl.nm_list <- Vectorize_nm_list(gsub_ctl.nm_generic, SIMPLIFY = FALSE)

#' Deleted subroutine
#' 
#' @param m nm object
#' @param dollar character. Name of subroutine
#' @export
delete_dollar <- function(m, dollar){
  UseMethod("delete_dollar")
}
#' @export
delete_dollar.nm_generic <- function(m, dollar){
  ctl <- m %>% ctl_contents()
  dollar_text <- gsub("\\$","",dollar)
  ctl[[dollar_text]] <- NULL
  m <- m %>% ctl_contents(ctl)
  m
}
#' @export
delete_dollar.nm_list <- Vectorize_nm_list(delete_dollar.nm_generic, SIMPLIFY = FALSE)

#' Create new subroutine
#' 
#' @param m nm object
#' @param dollar character. Name of subroutine
#' @param text character vector. Text to fill
#' @param after_dollar character. Positioning of subroutine
#' @export
insert_dollar <- function(m, dollar, text, after_dollar){
  UseMethod("insert_dollar")
}
#' @export
insert_dollar.nm_generic <- function(m, dollar, text, after_dollar = NA){
  ctl <- m %>% ctl_contents()
  
  dollar_text <- gsub("\\$","",dollar)
  text <- setup_dollar(text, paste0("$", dollar_text), add_dollar_text = FALSE)
  text <- list(text)
  names(text) <- dollar_text
  
  save_attributes <- attributes(ctl)
  if(!is.na(after_dollar)) {
    after_dollar <- gsub("\\$","",after_dollar)
    after <- match(after_dollar, names(ctl)) 
    ctl <- append(ctl, text, after)
  } else {
    ctl <- append(ctl, text)
  }
  save_names <- names(ctl)
  attributes(ctl) <- save_attributes
  names(ctl) <- save_names
  
  m <- m %>% ctl_contents(ctl)
  m
}
#' @export
insert_dollar.nm_list <- Vectorize_nm_list(insert_dollar.nm_generic, SIMPLIFY = FALSE)

#' @export
param_info.nm_generic <- function(ctl){
  ctl <- ctl_list2(ctl)
  if("THETA" %in% names(ctl)) return(theta_nm2r(ctl$THETA)) else
    return(data.frame())
}
#' @export
param_info.nm_list <- function(ctl) param_info(as_nm_generic(ctl))

param_info2 <- function(m){
  p_info <- dplyr::bind_rows(
    raw_init_theta(m),
    raw_init_omega(m)
  )
  p_info[!is.na(p_info$parameter),]
}

#' @export
coef.nm_generic <- function(object,trans=TRUE,...){
  
  if(!is_finished(object)) {
    #warning(unique_id(object)," not finished, returning empty data.frame")
    d <- data.frame()
    class(d) <- append(class(d), "nmcoef")
    return(invisible(d))
  }
  
  ext_file_path <- object %>% nm_output_path("ext")
  #ext_file_path <- file.path(run_dir(object, full_path = TRUE), "NM_run1","psn.ext")
  
  d <- coef_ext0(ext_file_path)
  if(nrow(d) == 0) {
    d <- data.frame()
    class(d) <- append(class(d), "nmcoef")
    return(d)
  }
  
  d$run_name <- unique_id(object)
  if(!unique(d$is_final)) d$run_name <- paste0(d$run_name,"*")
  d$is_final <- NULL
  if(!trans) {
    class(d) <- append(class(d), "nmcoef")
    return(d)
  }
  
  p <- param_info2(object)
  
  d0 <- d[,names(d)[!names(d) %in% "unit"]]
  d1 <- p[,c("name","parameter","unit","trans")]
  
  d <- merge(d0,d1,all.x = TRUE,by="parameter")
  d$name[is.na(d$name)] <- as.character(d$parameter)[is.na(d$name)]
  d$name <- factor(d$name,levels=d$name)
  d$trans_unit <- d$unit
  d$transSEunit <- d$SEunit
  ## transformations
  d$FINAL.TRANS <- d$FINAL
  d$SE.TRANS <- d$SE
  
  th <- d$type %in% "THETA"
  om <- d$type %in% "OMEGAVAR"
  sg <- d$type %in% "SIGMA"
    
  ## RATIO data
  d$SE.TRANS[d$trans %in% "RATIO" & th] <- 100*d$SE[d$trans %in% "RATIO" & th]/d$FINAL[d$trans %in% "RATIO" & th]
  d$transSEunit[d$trans %in% "RATIO" & th] <- "%"
  ## LOG
  d$FINAL.TRANS[d$trans %in% c("LOG","LOGODDS") & th] <- exp(d$FINAL[d$trans %in% c("LOG","LOGODDS") & th])
  d$SE.TRANS[d$trans %in% c("LOG","LOGODDS") & th] <- 100*sqrt((exp(d$SE[d$trans %in% c("LOG","LOGODDS") & th]^2)-1))
  d$transSEunit[d$trans %in% c("LOG","LOGODDS") & th] <- "%"
  ## LOGIT
  if("LOGIT" %in% d$trans){
    d$FINAL.TRANS[d$trans %in% "LOGIT" & th] <- 100*1/(1+exp(-d$FINAL[d$trans %in% "LOGIT" & th]))
    d$trans_unit[d$trans %in% "LOGIT" & th] <- "%"
    # delt <- lapply(which(d$trans %in% "LOGIT"),function(i){
    #   par <- c(logit=d$FINAL[i])
    #   separ <- c(logit=d$SE[i])
    #   car::deltaMethod(par,"1/(1+exp(-logit))",vcov.=separ^2)
    # })
    # delt <- do.call(rbind,delt)
    # d$SE.TRANS[d$trans %in% "LOGIT"] <- 100*delt$SE
  }
  ## OMEGA
  #d$trans[grepl("OMEGA.([0-9]+\\.)\\1",d$parameter)] <- "OM"   ## temp code - make an identifyer for OMEGA.X.X
  d$SE.TRANS[d$trans %in% "LOG" & om] <- 100*(d$SE[d$trans %in% "LOG" & om]/d$FINAL[d$trans %in% "LOG" & om])/2
  d$FINAL.TRANS[d$trans %in% "LOG" & om] <- 100*sqrt(exp(d$FINAL[d$trans %in% "LOG" & om])-1)
  d$trans_unit[d$trans %in% "LOG" & om] <- "CV%"
  d$transSEunit[d$trans %in% "LOG" & om] <- "%"
  ## COV
  d$trans[d$type %in% "OMEGACOV"] <- "COV" ## temp code
  # if("COV" %in% d$trans){
  #   omx <- gsub("^OMEGA\\.([0-9]+)\\.([0-9]+)\\.","\\1",d$parameter[d$trans %in% "COV"])
  #   omy <- gsub("^OMEGA\\.([0-9]+)\\.([0-9]+)\\.","\\2",d$parameter[d$trans %in% "COV"])
  #   omx <- paste0("OMEGA.",omx,".",omx,".")
  #   omy <- paste0("OMEGA.",omy,".",omy,".")
  #   sdx <- sqrt(d$FINAL[match(omx,d$parameter)])
  #   sdy <- sqrt(d$FINAL[match(omy,d$parameter)])
  #   d$FINAL.TRANS[d$trans %in% "COV"] <- d$FINAL[d$trans %in% "COV"]/(sdx*sdy)
  #   d$trans_unit[d$trans %in% "COV"] <- "CORR.COEF"
  #   ## COV[X,Y]/(SD[X]*SD[Y])
  #   ## know SE(COV[X,Y]) and SE[SDX^2] and SE[SDY^2]
  #   ## Need covariance matrix between these though - from .cov file.
  #   ## SQRT(VAR(COV[X,Y]/(SD[X]*SD[Y])))
  #   cov.file <- object$output$psn.cov
  #   dc <- utils::read.table(cov.file,skip=1,header = TRUE)
  #   for(i in seq_along(which(d$trans %in% "COV"))){
  #     ## loop through each COV variable and generate absolute SE
  #     names.c <- c(omx[i],omy[i],as.character(d$parameter[d$trans %in% "COV"][i]))
  #     names.c <- d$parameter[d$parameter %in% names.c] ## reorder
  #     names.c2 <- gsub("\\.([0-9]+)\\.([0-9]+)\\.","(\\1,\\2)",names.c)
  #
  #     ## same order as names.c - important
  #     vcov <- dc[match(names.c2,dc$NAME),as.character(names.c)]
  #     rownames(vcov) <- names(vcov)
  #     vcov <- as.matrix(vcov)
  #
  #     pmean <- d$FINAL[match(names.c,d$parameter)]  ## may as well recompute FINALs
  #     names(pmean) <- d$name[match(names.c,d$parameter)]
  #
  #     formula.i <- paste0(names.c[3],"/(sqrt(",names.c[1],")*sqrt(",names.c[2],"))")
  #     #tmp <- car::deltaMethod(pmean,formula.i,vcov.=vcov)
  #     #d$SE.TRANS[d$trans %in% "COV"][i] <- tmp$SE
  #   }
  #
  # }
  
  ## get names back to what they should be
  d$FINAL <- d$FINAL.TRANS
  d$FINAL.TRANS <- NULL
  d$SE <- d$SE.TRANS
  d$SE.TRANS <- NULL
  d$unit <- d$trans_unit
  d$trans_unit <- NULL
  d$SEunit <- d$transSEunit
  d$transSEunit <- NULL
  d$parameter <- d$name
  d$name <- NULL
  class(d) <- append(class(d), "nmcoef")
  d
}

#' @export
coef.nm_list <- function(object,trans=TRUE,...){
  d <- lapply(object, coef, trans = trans)
  #do.call(rbind, d)
  d
}

#' @export
cond_num.nm_generic <- function(r){
  dc <- try(coef(r, trans = FALSE), silent = TRUE)
  if(inherits(dc, "try-error")) return(as.numeric(NA))
  cond_num(dc)
}

#' @export
cond_num.nm_list <- function(r){
  cond_nums <- lapply(r, cond_num.nm_generic)
  names(cond_nums) <- NULL
  unlist(cond_nums)
}

#' run record
#' 
#' @param m nm object
#' @param trans logical. if TRUE (default) will transform using control file $THETA/OMEGA conventions
#' @export
rr <- function(m, trans = TRUE){
  UseMethod("rr")
}
  
#' @export
rr.nm_list <- function(m, trans = TRUE){
  d <- coef(m, trans = trans)
  d <- do.call(rbind, d)
  if(nrow(d) == 0) return(data.frame())
  d$file <- NULL
  d$unit[is.na(d$unit)] <- ""
  d$SEunit[is.na(d$SEunit)] <- ""
  if("trans" %in% d$trans) d$trans[is.na(d$trans)] <- ""   ## optional item
  d <- d[,names(d)[!names(d) %in% c("EVALUATION", "EST.NO","EST.NAME")]]
  d$Estimate <- NA
  d$Estimate[d$parameter!="OBJ"] <- paste0(signif(d$FINAL[d$parameter!="OBJ"],3)," (",signif(d$SE[d$parameter!="OBJ"],3),d$SEunit[d$parameter!="OBJ"],")")
  d$Estimate[d$parameter=="OBJ"] <- round(d$FINAL[d$parameter=="OBJ"],3)
  d <- d[,names(d)[!names(d) %in% c("SE","FINAL")]]
  d <- reshape2::dcast(data = d,
                       stats::as.formula(paste(paste(names(d)[!names(d) %in% c("run_name","Estimate")],collapse=" + "),
                                               "~ run_name")),
                       value.var = "Estimate")
  ## fix ordering of columns so it's same as m - dcast ruins it
  non_matches <- names(d)[!seq_along(names(d)) %in% match(unique_id(m), names(d))]
  matches <- unique_id(m[!is.na(m)])
  matches <- matches[matches %in% names(d)]
  
  d <- d[, c(non_matches, matches)]
  
  d <- d[order(d$type,d$parameter),]
  d$SEunit <- NULL
  names(d) <- gsub("execute:","",names(d))
  tmp <- sapply(d, is.factor)
  d[tmp] <- lapply(d[tmp], as.character)
  d
}

rr.nm_generic <- function(m, trans = TRUE){
  rr(as_nm_list(m), trans = trans)
}

#' @export
coef_wide <- function(m, trans = TRUE){
  
  d <- coef(m, trans = trans)
  d <- lapply(seq_along(d), function(i) {
    d <- d[[i]]
    if(nrow(d) == 0) return(d)
    d$par_no <- seq_len(nrow(d))
    d$m_no <- i
    d
  })
  d <- do.call(rbind, d)
  if(nrow(d) == 0) return(data.frame())
  d$file <- NULL
  d$unit[is.na(d$unit)] <- ""
  d$SEunit[is.na(d$SEunit)] <- ""
  if("trans" %in% d$trans) d$trans[is.na(d$trans)] <- ""   ## optional item
  d <- d[,names(d)[!names(d) %in% c("EVALUATION", "EST.NO","EST.NAME")]]
  
  d <- d[grepl("THETA|OMEGA|SIGMA", d$type), ]
  
  d <- d[order(paste(d$m_no, d$par_no, d$key)), ]
  d$m_no <- NULL
  #d$par_no <- NULL
  d$run_name <- gsub("execute:","",d$run_name)
  
  tmp <- sapply(d, is.factor)
  d[tmp] <- lapply(d[tmp], as.character)
  
  d
  
}

#' @export
coef_long <- function(m, trans = TRUE){
  d <- coef(m, trans = trans)
  d <- lapply(seq_along(d), function(i) {
    d <- d[[i]]
    if(nrow(d) == 0) return(d)
    d$par_no <- seq_len(nrow(d))
    d$m_no <- i
    d
  })
  d <- do.call(rbind, d)
  if(nrow(d) == 0) return(data.frame())
  d$file <- NULL
  d$unit[is.na(d$unit)] <- ""
  d$SEunit[is.na(d$SEunit)] <- ""
  if("trans" %in% d$trans) d$trans[is.na(d$trans)] <- ""   ## optional item
  d <- d[,names(d)[!names(d) %in% c("EVALUATION", "EST.NO","EST.NAME")]]
  
  d <- d[grepl("THETA|OMEGA|SIGMA", d$type), ]
  
  d <- d %>% tidyr::gather(key = "key", value = "estimate", .data$FINAL : .data$SE)
  
  d <- d[order(paste("m", d$m_no, "p", d$par_no, d$key)), ]
  d$m_no <- NULL
  #d$par_no <- NULL
  d$run_name <- gsub("execute:","",d$run_name)
  
  tmp <- sapply(d, is.factor)
  d[tmp] <- lapply(d[tmp], as.character)
  
  d
}



#' run record
#' 
#' @param m nm object
#' @param trans logical. if TRUE (default) will transform using control file $THETA/OMEGA conventions
#' @export
rr2 <- function(m, trans = TRUE){
  d <- coef_long(m, trans = trans)

  if(nrow(d) == 0) return(data.frame())
  
  index <- !d$unit %in% "" & !is.na(d$unit)
  d$parameter[index] <- 
    paste0(d$parameter[index], " (", d$unit[index], ")")
  
  if("trans" %in% names(d)){
    index <- !d$trans %in% "" & !is.na(d$trans)
    d$parameter[index] <- 
      paste0(d$parameter[index], " (", d$trans[index],")")
  }
  
  d$parameter[d$key %in% "SE"] <- paste0("se_", d$parameter[d$key %in% "SE"])
  d <- d %>% dplyr::group_by(.data$parameter) %>% 
    dplyr::mutate(par_no = max(.data$par_no))

  m_names <- unique(d$run_name)  
  d <- d %>% tidyr::spread(key = "run_name", value = "estimate")
  names1 <- names(d)[!names(d) %in% m_names]
  d <- d[, c(names1, m_names)]
  #d <- d[order(d$key, d$par_no), ]
  d <- d[order(d$par_no, d$key), ]
  row.names(d) <- NULL
  
  d 
}

#' update parameters from a control stream
#'
#' @param ctl object coercible into ctl_list
#' @param from class nm. object from which to extract results
#' @export

update_parameters <- function(ctl, from){
  UseMethod("update_parameters")
}


#' @export
update_parameters.nm_generic <- function(ctl, from){
  
  m <- ctl
  ctl <- m %>% ctl_contents()
  if(missing(from)) from <- m
  wait_finish(from)
  ctl_lines <- ctl
  
  coef_from <- coef(from, trans=FALSE)
  ctl_lines <- update_parameters0(ctl_lines, coef_from, type = "THETA")
  message("bug in updating IOV model parameters - unresolved")
  ctl_lines <- update_parameters0(ctl_lines, coef_from, type = "OMEGA")
  ctl_lines <- update_parameters0(ctl_lines, coef_from, type = "SIGMA")
  
  m <- m %>% ctl_contents(ctl_lines)
  m
}
#' @export
update_parameters.nm_list <- Vectorize_nm_list(update_parameters.nm_generic, SIMPLIFY = FALSE)

#' Name of result file
#' 
#' @param m nm object
#' @param name character. path
#' @export
result_file <- function(m, name){
  UseMethod("result_file")
}
#' @export
result_file.nm_generic <- function(m, name){
  name <- glue_text_nm(m, name)
  #name <- stringr::str_glue(name, .envir = m)
  file.path(results_dir(m), name)
}
#' @export
result_file.nm_list <- Vectorize_nm_list(result_file.nm_generic)

## generic already defined

#' @export
nm_tran.nm_generic <- function(x){
  write_ctl(x)
  nm_tran.default(ctl_path(x))
  invisible(x)
}
#' @export
nm_tran.nm_list <- Vectorize_nm_list(nm_tran.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

#' @export
in_cache <- function(r,
                     cache_ignore_cmd = FALSE, cache_ignore_ctl = FALSE, cache_ignore_data = FALSE){
  UseMethod("in_cache")
}
#' @export
in_cache.nm_generic <- function(r,
                                cache_ignore_cmd = FALSE, cache_ignore_ctl = FALSE, cache_ignore_data = FALSE){
  r %>% write_ctl()
  ## get all md5_files
  
  run_cache_disk <- lapply(run_cache_paths(r), readRDS)
  if(length(run_cache_disk) > 0){
    current_checksums <- run_checksums(r)
    matches <- sapply(run_cache_disk, function(i) {
      
      if(cache_ignore_cmd){  ## remove cmd check
        keep <- !names(current_checksums) %in% "cmd"
        i$checksums <- i$checksums[keep]
        current_checksums <- current_checksums[keep]
      }
      
      if(cache_ignore_ctl){  ## remove cmd check
        keep <- !names(current_checksums) %in% "ctl"
        i$checksums <- i$checksums[keep]
        current_checksums <- current_checksums[keep]
      }        
      
      if(cache_ignore_data){  ## remove cmd check
        keep <- !names(current_checksums) %in% "data"
        i$checksums <- i$checksums[keep]
        current_checksums <- current_checksums[keep]
      }
      
      ## ignore names
      names(current_checksums) <- NULL
      names(i$checksums) <- NULL
      
      identical(i$checksums, current_checksums)
    })
    if(any(matches)){
      return(TRUE)    ## if up to date, skip
    }
  }
  return(FALSE)
}
#' @export
in_cache.nm_list <- Vectorize_nm_list(in_cache.nm_generic)

#' @export
cache_history <- function(r){
  UseMethod("cache_history")
}
#' @export
cache_history.nm_generic <- function(r){
  lapply(run_cache_paths(r), readRDS)
}
#' @export
cache_history.nm_list <- Vectorize_nm_list(cache_history.nm_generic, SIMPLIFY = FALSE)

#' @export
cache_current <- function(m) run_checksums(m)

#' @export
clear_cache <- function() unlink(".cache", recursive = TRUE)

#' @export
nm_prev_version <- function(m){
  version <- version(m)
  if(version > 1) m %>% version(version - 1) else m
}

#' get status of a run
#' 
#' @param x nm object
#' @param simple logical. deprecated
#' @export

status <- function(x, simple = TRUE) { ## TODO: delete simple
  UseMethod("status")
}

#' @export
status.nm_generic <- function(x, simple = TRUE){
  m <- x
  get_sub_dirs <- function(path){
    ## only checks 1 level deep - good enough for execute/boostrap/sse
    contents <- dir(path,full.names = TRUE)
    contents[file.info(contents)$isdir]
  }
  
  execution_dirs <- get_sub_dirs(run_dir(m, full_path = TRUE))
  
  ##############################
  ## for bootstraps, etc. go to modelfit_dir1
  modelfit_dir <- execution_dirs[grepl("modelfit_dir1", execution_dirs)]
  if(length(modelfit_dir) > 0){
    execution_dirs <- get_sub_dirs(modelfit_dir)
  }
  ##############################
  if(length(execution_dirs) == 0) return("not_started")
  
  ## for each execution dir get vector of status'
  
  statuses <- sapply(execution_dirs, function(execution_dir){
    if(file.exists(file.path(execution_dir,"psn_nonmem_error_messages.txt"))) return("error")
    lst_name <- file.path(execution_dir,"psn.lst")
    if(!file.exists(lst_name)) return("not_started")
    lst <- try(readLines(lst_name),silent = TRUE)
    if(inherits(lst,"try-error")) return("running")
    #lst <- lst[max(1,(length(lst)-5)):length(lst)]
    stopped <- any(grepl("Stop Time:",lst))
    if(stopped) return("finished")
    stopped <- any(grepl("No nonmem execution",lst))
    if(stopped) return("error")
    return("running")
  })
  
  if("execute" %in% type(m)) {
    if(any(statuses == "finished")) {
      ## NOTE: removed the following because don't want status and reverse dependencies to care about 
      ##   exported outputs - only the raw outputs in the nonmem run directory NM_run1.
      # psn_exports <- psn_exported_files(m, minimal = TRUE)
      # psn_exports_exist <- file.exists(psn_exports)
      # if(!all(psn_exports_exist)){
      #   #warning("incomplete_tables in runs\n ", paste(psn_exports[!psn_exports_exist], collapse = "\n"))
      #   statuses[statuses %in% "finished"] <- "running"#"stopped-incomplete_tables"
      # }
    }
  }
  
  statuses
    
}
#' @export
status.nm_list <- Vectorize_nm_list(status.nm_generic)

#' @export
status_table <- function(m){
  tab <- m %>% status %>% 
    factor(levels = c("finished", "error", "running", "not started")) %>%
    table
  tibble::as_tibble(tab)
}

#' @export
clean_run.nm_generic <- function(r,delete_dir=c(NA,TRUE,FALSE),update_db=!is.null(r$db_name)){
  ## assumes ctrl file is run[run_id].mod and -dir=[run_id] was used
  
  ## get and remove all ctl output files
  
  ## files are in run_in(r)
  ## assume same stub as input.
  
  psn_exported_files <- psn_exported_files(r)
  
  # ## do not include scm or mod
  # output_files <- paste0(tools::file_path_sans_ext(ctl_path(r)),
  #       c(".phi", ".ext", ".cov", ".coi", ".cor", ".lst"))
  
  ## and output - in case it's different  
  lst_path <- file.path(run_in(r), lst_path(r))
  
  # ## need to get table files too
  # ctl_table_files <- file.path(run_in(r), ctl_table_files(r))
  
  ## run_dir
  run_dir_to_delete <- file.path(run_in(r), run_dir(r))
  if(!file.exists(run_dir_to_delete)) run_dir_to_delete <- c() else {
    ## can now assume directory exists
    ## make sure it's not the models directory
    if(run_dir_to_delete %in% c(".", ".\\")) run_dir_to_delete <- c() else {
      if(normalizePath(run_dir_to_delete) == normalizePath(run_in(r))) run_dir_to_delete <- c()
    }
  }
  
  #ctl_out_files <- c(lst_path, output_files, ctl_table_files, run_dir_to_delete)
  ctl_out_files <- c(lst_path, psn_exported_files, run_dir_to_delete)
  
  unlink(ctl_out_files, recursive = TRUE, force = TRUE)
  
  invisible()
}


psn_exported_files <- function(r, minimal = FALSE){
  UseMethod("psn_exported_files")
}

psn_exported_files.nm_generic <- function(r, minimal = FALSE){
  ## do not include scm or mod
  if(minimal){
    output_files <- paste0(tools::file_path_sans_ext(ctl_path(r)),
                           c(".ext"))
  } else {
    output_files <- paste0(tools::file_path_sans_ext(ctl_path(r)),
                           c(".phi", ".ext", ".cov", ".coi", ".cor", ".lst"))
  }
  
  if(minimal){
    ctl_out_files <- c(output_files)
  }  else {
    exported_table_paths <- file.path(run_in(r), ctl_table_files(ctl_contents(r)))
    ctl_out_files <- c(output_files, exported_table_paths)
  }
  
  ctl_out_files
}

psn_exported_files.nm_list <- Vectorize_nm_list(psn_exported_files.nm_generic, SIMPLIFY = FALSE)
  

#' @export
clean_run.nm_list <- Vectorize_nm_list(clean_run.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

#' tests if job is finished
#'
#' @param r object class nm
#' @param initial_timeout numeric. time in seconds.
#' time period to give up on a run if directory hasn't been created.
#' @export
is_finished <- function(r,initial_timeout=NA){
  UseMethod("is_finished")
}

#' @export
is_finished.nm_generic <- function(r,initial_timeout=NA){
  all(status(r) %in% c("finished", "error") | is.na(r))
}

#' @export
is_finished.nm_list <- Vectorize_nm_list(is_finished.nm_generic)

#' @export
wait_finish <- function(r, timeout=NA){
  UseMethod("wait_finish")
}
#' @export
wait_finish.nm_generic <- function(r, timeout=NA){
  if(is.na(timeout)) wait_for(all(is_finished(r))) else
    wait_for(all(is_finished(r)), timeout = timeout)
  invisible(r)
}
#' @export
wait_finish.nm_list <- wait_finish.nm_generic

#' @export
show_ctl.nm_generic <- function(r) {
  r %>% write_ctl()
  file.show(ctl_path(r))
}
#' @export
show_ctl.nm_list <- show_ctl.nm_generic

#' Get/set existing subroutine
#' 
#' @param m nm object
#' @param dollar character.  Subroutine to target
#' @param ... arguments to be passed to text()
#' @export
dollar <- function(m, dollar, ...) {
  m %>% target(dollar) %>% text(...)
}

## pipe for string functions

#' @export
'%ns>%' <- function(m, expr){
  UseMethod('%ns>%')
}
#' @export
'%ns>%.nm_generic' <- function(m, expr){
  if(!requireNamespace("stringr")) stop("install stringr to use this pipe")
  string <- m %>% text
  expr <- substitute(expr)
  list_expr <- as.list(expr)
  list_expr <- append(list_expr, list(string), after = 1)
  list_expr <- as.call(list_expr)
  string <- eval(list_expr)
  m %>% text(string)
}

#' @export
'%ns>%.nm_list' <- function(m, expr){
  if(!requireNamespace("stringr")) stop("install stringr to use this pipe")
  strings <- m %>% text ## list of chars
  
  expr <- substitute(expr)
  list_expr <- as.list(expr)
  
  ## vectorize the stringr function
  stringr_fun <- list_expr[[1]]
  stringr_fun <- eval(stringr_fun) 
  stringr_fun_vec <- Vectorize(stringr_fun, SIMPLIFY = FALSE)
  
  ## evaluate args
  args <- list_expr
  args[[1]] <- strings  ## only args remaining
  args <- lapply(args, eval) ## evaluate them
  
  ## make new call with vectorized stringr fun
  ##  and pre-evaluated args
  new_call <- rlang::call2("stringr_fun_vec", !!!args)
  strings_new <- eval(new_call)
  
  ## text() isn't vectorized for lists of characters
  ## need to loop
  m <- lapply(seq_along(strings_new), function(i){
    mi <- m[[i]] ## nm_generic
    string_new <- strings_new[[i]]
    mi %>% text(string_new)
  })
  
  as_nm_list(m)
}


#' @export
n_thetas <- function(m){
  param_info <- param_info(m)
  nrow(param_info[grepl("THETA", param_info$parameter)])
}

grab_variables0 <- function(text, pattern){
  text_separated <- text %>% paste0(collapse = "\n") %>% 
    stringr::str_split("(\n|\\s|\\+|\\-|\\=|\\*|\\/)") %>% unlist
  
  text_separated <- text_separated[grepl(pattern, text_separated)]
  text_separated <- gsub(paste0(".*(", pattern, ").*"), "\\1", text_separated)
  unique(text_separated)
}

grab_variables <- function(m, pattern){
  text <- m %>% text
  grab_variables0(text, pattern)
}

dp <- rbind(
  data.frame(advan = 1,trans = 1,
             base_name = c("R20","V2"),
             relation = NA,
             nm_name = c("K","V")),
  data.frame(advan = 1,trans = 2,
             base_name = c("R20","V2"),
             relation = c("R20*V2", NA),
             nm_name = c("CL","V")),
  data.frame(advan = 2,trans = 1,
             base_name = c("R12","R20","V2"),
             relation = NA,
             nm_name = c("KA","K","V")),
  data.frame(advan = 2,trans = 2,
             base_name = c("R12","R20","V2"),
             relation = c(NA, "R20*V2", NA),
             nm_name = c("KA","CL","V")),
  data.frame(advan = 3,trans = 1,
             base_name = c("R20","R23","R32","V2"),
             relation = NA,
             nm_name = c("K","K12","K21","V")),
  data.frame(advan = 3,trans = 3,
             base_name = c("R20","R23","V2","V3"),
             relation = c("R20*V2", "R23*V2", NA , "V2+V3"),
             nm_name = c("CL","Q","V","VSS")),
  data.frame(advan = 3,trans = 4,
             base_name = c("R20","R23","V2","V3"),
             relation = c("R20*V2", "R23*V2", NA , NA),
             nm_name = c("CL","Q","V2","V3")),
  ## skip advan 3 trans 5/6
  data.frame(advan = 4,trans = 1,
             base_name = c("R12","R20","R23","R32","V2"),
             relation = NA,
             nm_name = c("KA","K","K23","K32","V2")),
  data.frame(advan = 4,trans = 3,
             base_name = c("R12","R20","R23","V2","V3"),
             relation = c(NA, "R20*V2", "R23*V2", NA , "V2+V3"),
             nm_name = c("KA","CL","Q","V","VSS")),
  data.frame(advan = 4,trans = 4,
             base_name = c("R12","R20","R23","V2","V3"),
             relation = c(NA, "R20*V2", "R23*V2", NA , NA),               
             nm_name = c("KA","CL","Q","V2","V3")),
  data.frame(advan = 5,trans = NA,
             base_name = c("R12","R20","R23","R32","V2","..."),
             relation = NA,
             nm_name = c("K12","K20","K23","K32","V2","...")),
  data.frame(advan = 6,trans = NA,
             base_name = c("R12","R20","R23","R32","V2","..."),
             relation = NA,
             nm_name = c("K12","K20","K23","K32","V2","...")),
  data.frame(advan = 7,trans = NA,
             base_name = c("R12","R20","R23","R32","V2","..."),
             relation = NA,
             nm_name = c("K12","K20","K23","K32","V2","...")),
  data.frame(advan = 8,trans = NA,
             base_name = c("R12","R20","R23","R32","V2","..."),
             relation = NA,
             nm_name = c("K12","K20","K23","K32","V2","...")),
  data.frame(advan = 9,trans = NA,
             base_name = c("R12","R20","R23","R32","V2","..."),
             relation = NA,
             nm_name = c("K12","K20","K23","K32","V2","...")),
  data.frame(advan = 13,trans = NA,
             base_name = c("R12","R20","R23","R32","V2","..."),
             relation = NA,
             nm_name = c("K12","K20","K23","K32","V2","..."))
)
dp <- tibble::as_tibble(dp)

available_advans <- dplyr::group_by(dp, .data$advan, .data$trans)
available_advans <- dplyr::summarise(available_advans, 
                                     params = paste(nm_name, collapse = ","))
available_advans <- dplyr::mutate(available_advans,
                                  label = ifelse(is.na(trans), paste0("a",advan), paste0("a",advan,"t",trans)))
available_advans <- dplyr::ungroup(available_advans)

default_trans <- function(advan){
  sapply(advan, function(advan){
    default_trans_vec <- available_advans$trans[available_advans$advan %in% advan]
    if(any(is.na(default_trans_vec))) return(NA) else return(1)
  })
}

#' Subroutine
#' 
#' This is currently experimental. Set/modify subroutine of an nm object
#' 
#' @param m nm object
#' @param advan character. desired ADVAN
#' @param trans character. desired TRANS
#' @param recursive logical (default = TRUE). Internal argument, do not modify
#' 
#' @details
#'  Can only switch between subroutines listed in \code{available_advans}
#'  TODO: modify parameter initial estimates based on conversion
#' 
#' @examples
#' \dontrun{
#' 
#' advan(m)  ## 2
#' trans(m)  ## 1
#' 
#' m <- m %>% subroutine(ADVAN = 2, TRANS = 2)
#' 
#' }
#' @export

subroutine <- function(m, advan = NA, trans = NA, recursive = TRUE){
  UseMethod("subroutine")
}

#' @export
subroutine.nm_generic <- function(m, advan = NA, trans = NA, recursive = TRUE){
  
  dps <- available_advans
  
  old_m <- m
  old_advan <- advan(m)
  old_trans <- trans(m)
  old_ctl <- ctl_contents(m)
  
  if(advan %in% old_advan & trans %in% old_trans) return(m)
  
  if(recursive){
    orig_call <- match.call()
    
    if(old_advan %in% c(5, 7, 6, 8, 9, 13)){

      # the following code breaks down:
      # ADVAN5 -> ADVAN2 TRANS2
      # to:
      # ADVAN5 -> ADVAN2 TRANS1 -> ADVAN2 TRANS2

      # ADVAN5 -> ADVAN2 TRANS1
      call1 <- orig_call
      call1[["m"]] <- m
      call1[["advan"]] <- advan
      call1[["trans"]] <- default_trans(advan)
      call1[["recursive"]] <- FALSE
      m <- eval(call1)
      
      # ADVAN2 TRANS1 -> ADVAN2 TRANS2
      call2 <- orig_call
      call2[["m"]] <- m
      call2[["advan"]] <- advan
      call2[["trans"]] <- trans
      call2[["recursive"]] <- FALSE
      m <- eval(call2)
      
    }
    
    # the following code breaks down:
    # ADVAN2 TRANS2 -> ADVAN5
    # to:
    # ADVAN2 TRANS2 -> ADVAN2 TRANS1 -> ADVAN5
    
    dadvan <- available_advans[
      available_advans$advan %in% old_advan & 
        !available_advans$trans %in% c(1, NA), ## non default trans
      ]
    
    if(nrow(dadvan) > 0){       ## if non-default trans and non
      
      # ADVAN2 TRANS2 -> ADVAN2 TRANS1
      call3 <- orig_call
      call3[["m"]] <- m
      call3[["advan"]] <- old_advan
      call3[["trans"]] <- default_trans(old_advan)
      call3[["recursive"]] <- FALSE
      m <- eval(call3)
      
      # ADVAN2 TRANS1 -> ADVAN5
      call4 <- orig_call
      call4[["m"]] <- m
      call4[["advan"]] <- advan
      call4[["trans"]] <- trans
      call4[["recursive"]] <- FALSE
      m <- eval(call4)
      
    }    
    
    return(m)
  }

  ## check source is valid
  if(!any(dps$advan %in% old_advan & dps$trans %in% old_trans)){
    message("advan/trans not available:")
    message("compatible combinations:")
    print(dps)
    stop("stopping...", call. = FALSE)
  }
  
  if(is.na(advan)) advan <- old_advan
  if(is.na(trans)) trans <- old_trans
  
  new_advan <- advan
  new_trans <- trans
  
  if(!any(dps$advan %in% new_advan & dps$trans %in% new_trans)){
    message("advan/trans not available:")
    message("compatible combinations:")
    print(dps)
    stop("stopping...", call. = FALSE)
  }
  
  m <- m %>% advan(new_advan) %>% trans(new_trans)
  
  if(old_advan %in% c(5, 7)){
    ## TODO: if general linear, get parameters in control
    ##   and replace dp (or available_advans?)
    
    ## detect KXY parameters
    browser()
    
  }
  
  if(old_advan %in% c(6, 8, 9, 13)){
    ## TODO: if $DES, get parameters in control
    ##   and replace dp (or available_advans?)
    
    ## could be CL, Q, KXY and multiple combinations
    ## how to specify which?
    ## could pull out variables from $DES?
    
    ## restriction: for now
    ## $DES -> closed form, will only work if parameters are compatible
    
    ## TODO: detect 
    
    browser()
  }
  
  thetas <- raw_init_theta(m)
  thetas$init_trans <- thetas$init
  thetas$init_trans[thetas$trans %in% c("LOG","LOGODDS")] <- 
    exp(thetas$init_trans[thetas$trans %in% c("LOG","LOGODDS")])
  thetas$init_trans[thetas$trans %in% "LOGIT"] <- 
    100*1/(1+exp(-thetas$init_trans[thetas$trans %in% "LOGIT"]))
  
  omegas <- raw_init_omega(m)
  
  dold <- dp %>% dplyr::filter(.data$advan %in% old_advan,
                               .data$trans %in% old_trans)
    
  dnew <- dp %>% dplyr::filter(.data$advan %in% new_advan,
                               .data$trans %in% new_trans)

  d <- dplyr::full_join(dold, dnew, by = "base_name")
  
  ## loop through rows 
  
  for(i in seq_len(nrow(d))){
    di <- d[i, ]
    strategy <- "none"
    if(is.na(di$nm_name.x) & !is.na(di$nm_name.y)) strategy <- "add_new" else {
      if(!is.na(di$nm_name.x) & is.na(di$nm_name.y)) strategy <- "remove" else {
        if(di$nm_name.x != di$nm_name.y) strategy <- "rename"
      }
    }
    if(strategy == "none") next
    if(strategy == "rename") {

      ## set initial estimates from current
      
      relation <- di$relation.y
      for(j in seq_len(nrow(d))){
        dj <- d[j, ]
        relation <- gsub(dj$base_name,
                         dj$nm_name.x,
                         relation) 
      }
      
      ## find initial estimates of TVK and TVV2
      
      theta_vec <- as.list(thetas$init_trans)
      names(theta_vec) <- thetas$name
      
      relation_expr <- parse(text = relation)
      new_theta <- 
        try(with(theta_vec, eval(relation_expr)), silent = TRUE)
      
      m <- m %>% rename_parameter_(new_name = di$nm_name.y,
                                   name = di$nm_name.x)
      
      if(!inherits(new_theta, "try-error")){
        ithetai <- init_theta(m)
        ithetai$init[ithetai$name == di$nm_name.y] <- new_theta
        if(ithetai$trans[ithetai$name == di$nm_name.y] %in% c("LOG", "LOGODDS"))
          ithetai$init[ithetai$name == di$nm_name.y] <- log(new_theta)
        if(ithetai$trans[ithetai$name == di$nm_name.y] %in% c("LOGIT")){
          p <- new_theta/100
          ithetai$init[ithetai$name == di$nm_name.y] <-
            log(p/(1-p))
        }
        ithetai$init[ithetai$name == di$nm_name.y] <- 
          signif(ithetai$init[ithetai$name == di$nm_name.y], 5)
        
        m <- m %>% init_theta(ithetai)
        
      }
      
    }
    if(strategy == "add_new") {
      m <- m %>% add_mixed_param(di$nm_name.y, init = 1.1)
    }
    if(strategy == "remove") {
      m <- m %>% remove_parameter(di$nm_name.x)
    }
  }

  ## parameters included now
  
  ##########################  
  ## ensure parameter numbering is correct
  
  m <- m %>% update_variable_in_text_numbers("THETA(", ")")
  m <- m %>% update_variable_in_text_numbers("ETA(", ")")
  ## syncronise MUs with ETAs
  m <- m %>% gsub_ctl("MU_[0-9]+(\\s*\\+\\s*)ETA\\(([0-9]+)\\)",
                      "MU_\\2\\1ETA\\(\\2\\)")
  
  ##########################
  
  ## need a list of all relevant parameters
  
  ## for advans 1-4 they are in d
  ## for advan 5, they are KXY
  
  ## update $DES if present
  
  ## update $MODEL if present
  #browser()
  
  m
}
#' @export
subroutine.nm_list <- Vectorize_nm_list(subroutine.nm_generic, SIMPLIFY = FALSE)

#' Compute diff between two NONMEM runs
#' 
#' NMproject's control file manipulation functions (e.g. subroutine())
#'  may not work for all control files. It is the responsibilty of 
#'  the user to check automatic manipulations are done properly.
#'  Displaying diffs provides a means of manually checking.
#' 
#' @param m nm object
#' @param ref_m nm object (base/reference object)
#' @param format character (default = "raw") argument passed to diffobj::diffChr
#' 
#' @return diff object
#' @examples 
#' \dontrun{
#' 
#' m1 <- nm(run_id = "m1") %>%
#'   ctl_contents("staging/Models/run1.mod")
#' 
#' m2 <- m1 %>% child(run_id = "m2") %>%
#'   subroutine(advan = 2, trans = 2)
#' 
#' nm_diff(m2, m1)
#' 
#' }
#' @export
nm_diff <- function(m, ref_m, format = "raw"){
  requireNamespace("diffobj", quietly = TRUE)
  
  if(missing(ref_m)){
    old_ctl <- as.character(ctl_character(
      as_nm_generic(m)[["ctl_orig"]]
    ))
  } else {
    old_ctl <- as.character(ctl_character(ctl_contents(as_nm_generic(ref_m)))) 
  }
  new_ctl <- as.character(ctl_character(ctl_contents(as_nm_generic(m))))
  #"ansi256"
  dff <- diffobj::diffChr(old_ctl, new_ctl, format = format)
  
  if(grepl("No visible differences between objects", as.character(dff)[1])){
    dff <- character()
  }
  
  #message("--- file diff: new_ctl and old_ctl colours show additions/deletions---")
  dff
}

#' @export
remove_parameter <- function(m, name){
  old_target <- target(m)
  m <- m %>% untarget %>%
    gsub_ctl(paste0(".*\\b",name,"\\b.*"), "") %>% 
    gsub_ctl(paste0(".*\\bTV",name,"\\b.*"), "") %>% 
    gsub_ctl(paste0(".*\\bIIV_",name,"\\b.*"), "")
  m %>% target(old_target)
}


#' @export
add_mixed_param <- function(m, name, 
                            init = 1, unit ="", trans = c("LOG"),
                            position = NA_integer_){
  
  trans <- match.arg(trans)
  
  old_target <- target(m)
  
  sub_names <- names(ctl_contents(as_nm_generic(m)))
  
  if("PK" %in% sub_names) PK_PRED <- "PK"
  if("PRED" %in% sub_names) PK_PRED <- "PRED"
  
  m <- m %>% target(PK_PRED)

  etas <- m %>% grab_variables("\\bETA\\([0-9]+\\)") %>% sort
  max_etas_in_pk <- 1
  if(length(etas) > 0){
    max_etas_in_pk <- gsub("\\bETA\\(([0-9]+)\\)","\\1",etas[length(etas)])    
    max_etas_in_pk <- as.numeric(max_etas_in_pk)
  }
  
  n_thetas <- n_thetas(m)
  
  if(trans == "LOG"){
    m <- m %>% target(PK_PRED) %>%
      text("TV_NEWPARAM_=EXP(THETA(_N_PARAM_))
            MU__MU_PARAM_=LOG(TV_NEWPARAM_)
            _NEWPARAM_ = EXP(MU__MU_PARAM_+ETA(_MU_PARAM_))
          ", append = TRUE) %>%
      target("THETA") %>%
      text(paste(signif(log(init),2), "      ; TV_NEWPARAM_ ; _UNIT_PARAM_ ; ",
                 trans), append = TRUE) %>%
      target("OMEGA") %>%
      text("0.1     ; IIV__NEWPARAM_ ; LOG", append = TRUE)
  }
  
  m <- m %>% untarget() %>%
    gsub_ctl("_N_PARAM_", n_thetas + 1) %>%
    gsub_ctl("_NEWPARAM_", name) %>%
    gsub_ctl("_MU_PARAM_", max_etas_in_pk + 1) %>%
    gsub_ctl("_UNIT_PARAM_", unit)    
  
  m %>% target(old_target)
}

#' @export
rename_parameter_ <- function(m, new_name, name){
  UseMethod("rename_parameter_")
}

#' @export
rename_parameter_.nm_generic <- function(m, new_name, name){
  
  ## comment out "new_param =" rows
  m <- m %>% comment_out(paste0("^\\s*",new_name,"\\s*\\="))
  
  text <- get_target_text(m)
  
  commented <- grepl("^\\s*;", text)

  text[!commented] <- text[!commented] %>% 
    stringr::str_replace_all(paste0("\\b",name,"\\b"), paste0(new_name)) %>% 
    stringr::str_replace_all(paste0("\\bTV",name,"\\b"), paste0("TV",new_name)) %>% 
    stringr::str_replace_all(paste0("\\bIIV_",name,"\\b"), paste0("IIV_",new_name)) 

  m <- m %>% set_target_text(text)
  m
  
  # m <- m %>% untarget %>%
  #   gsub_ctl(paste0("\\b",name,"\\b"), paste0(new_name)) %>% 
  #   gsub_ctl(paste0("\\bTV",name,"\\b"), paste0("TV",new_name)) %>% 
  #   gsub_ctl(paste0("\\bIIV_",name,"\\b"), paste0("IIV_",new_name)) 
  # m %>% target(old_target)
  
}

#' @export
rename_parameter_.nm_list <- Vectorize_nm_list(rename_parameter_.nm_generic, SIMPLIFY = FALSE)

#' @export
rename_parameter <- function(m, ...){
  rename_list <- list(...)
  new_name <- names(rename_list)
  name <- unlist(rename_list)
  rename_parameter_(m, new_name, name)
}


#' @export
advan <- function(m, text){
  UseMethod("advan")
}
#' @export
advan.nm_generic <- function(m, text){
  if(missing(text)){
    sub_text <- m %>% dollar("SUB")
    if(is_single_na(sub_text)) return(NA_integer_)
    advan_match_text <- ".*\\bADVAN([0-9]+)\\b.*"
    advan_match <- grepl(advan_match_text, sub_text)
    advan <- gsub(advan_match_text, "\\1", sub_text[advan_match])
    advan <- as.integer(advan)    
    return(advan)
  } 
  old_target <- target(m)
  m <- m %>% target("SUB") %>%
    gsub_ctl("ADVAN[0-9]+", paste0("ADVAN", text)) %>% 
    target(old_target)
  
  if(text %in% c(6, 8, 9)){ ## $DES 
    m <- m %>% tol(7)
  }
  if(text %in% c(13)){ ## $DES 
    m <- m %>% tol(12)
  }
  
  m
}
#' @export
advan.nm_list <- Vectorize_nm_list(advan.nm_generic)

#' @export
trans <- function(m, text){
  UseMethod("trans")
}
#' @export
trans.nm_generic <- function(m, text){
  if(missing(text)){
    sub_text <- m %>% dollar("SUB")
    if(is_single_na(sub_text)) return(NA_integer_)
    trans_match_text <- ".*\\bTRANS([0-9]+)\\b.*"
    trans_match <- grepl(trans_match_text, sub_text)
    trans <- gsub(trans_match_text, "\\1", sub_text[trans_match])
    trans <- as.integer(trans)
    if(length(trans) == 0) {
      base_advans <- available_advans[!duplicated(available_advans$advan), ]
      base_trans <- base_advans$trans[base_advans$advan %in% advan(m)]
      trans <- base_trans
    }
    return(trans)
  } 
  old_target <- target(m)
  m <- m %>% target("SUB") 
  
  if(any(grepl("TRANS", text(m)))){ ## TRANS already exists
    m <- m %>% gsub_ctl("TRANS\\s?\\=?\\s?[0-9]+", paste0("TRANS", text))
  } else { ## append
    
    new_text <- text(m)
    blanks <- grepl("^\\s*$", new_text)
    
    new_text[max(which(!blanks))] <- 
      paste0(new_text[max(which(!blanks))], " TRANS", text)
    
    new_text <- gsub("\\s+", " ", new_text)
    
    m <- m %>% text(new_text)
  }
  
  ## if not 
  m <- m %>% gsub_ctl("\\s*TRANSNA", "") %>%
    target(old_target)
  m
}
#' @export
trans.nm_list <- Vectorize_nm_list(trans.nm_generic)

#' @export
advan <- function(m, text){
  UseMethod("advan")
}

#' @export
tol.nm_generic <- function(m, text){
  if(missing(text)){
    sub_text <- m %>% dollar("SUB")
    if(is_single_na(sub_text)) return(NA_integer_)
    tol_match_text <- ".*\\bTOL([0-9]+)\\b.*"
    tol_match <- grepl(tol_match_text, sub_text)
    tol <- gsub(tol_match_text, "\\1", sub_text[tol_match])
    tol <- as.integer(tol)  
    if(length(tol) == 0) tol <- NA
    return(tol)
  } 
  old_target <- target(m)
  m <- m %>% target("SUB")
  
  if(grepl("TOL", text(m))){ ## TOL already exists
    m <- m %>% gsub_ctl("TOL[0-9]+", paste0("TOL", text))
  } else {
    m <- m %>% text(paste(text(m), paste0("TOL", text)))
  }
  
  m <- m %>% target(old_target)
  m
}
#' @export
tol.nm_list <- Vectorize_nm_list(tol.nm_generic)

#' @export
tol <- function(m, text){
  UseMethod("tol")
}

#' @export
raw_init_theta <- function(m, replace){

  if(missing(replace)){
    m <- as_nm_generic(m)
    
    ctl <- m %>% ctl_contents()
    ctl_char <- ctl_character(ctl)
    
    sub_names <- names(ctl_contents(m))
    if("PK" %in% sub_names) PK_PRED <- "PK"
    if("PRED" %in% sub_names) PK_PRED <- "PRED"
    
    x_pk <- ctl[[PK_PRED]]
    x_orig <- ctl[["THETA"]]
    x <- x_orig
    x <- as.character(x)
    
    ## test
    
    # dol_theta <- "$THETA 0.1 -2 FIX (3,4 ) ; pk parameters
    # $THETA 4 ; PD
    # (2, 3) \t ; KA2 ; h ; LOG
    # 
    # (0, 3, 3) ; EC50 ; ; EXP
    # (0 3 ) ; ; EC50 ; ml/h
    # ( 4 5 6) FIX 4  ; EMAX
    # ( 4 5 6 FIX),-4.2
    # ( 4,,6), -4.25  ; EMAX2"
    # x <- strsplit(dol_theta, "\n")[[1]]
    
    d <- data.frame(x = x)
    d$line <- seq_len(nrow(d))
    ## clean up x to just what's necessary
    d$x_nc <- rem_comment(d$x)
    d$comment <- NA
    d$comment[grepl(".*;.*", d$x)] <- gsub(".+?;(.*)", "\\1", d$x[grepl(".*;.*", d$x)])
    
    ## -- clean up separators -- ##
    ## sort spaces out
    d$x_nc <- gsub("\\t","  ",d$x_nc, perl = TRUE)
    d$x_nc <- gsub("\\s+"," ",d$x_nc, perl = TRUE)
    d$x_nc <- trimws(d$x_nc)
    
    d$x_nc <- gsub("\\s+FIX", "FIX", d$x_nc)  ## remove seps so it stays with parameter and survives split
    
    ## remove spaces near commas and brackets
    d$x_nc <- gsub("\\s*,\\s*",",",d$x_nc, perl = TRUE)
    d$x_nc <- gsub("\\(\\s*","\\(",d$x_nc, perl = TRUE)  
    d$x_nc <- gsub("\\s*\\)","\\)",d$x_nc, perl = TRUE)
    d$x_nc <- gsub_in_brackets("\\s+", ",", d$x_nc)  
    d$x_nc <- gsub_out_brackets(",", " ", d$x_nc)  
    
    x_nc <- d$x_nc
    x_nc <- paste(x_nc, collapse = " \n ")
    x_nc <- strsplit(x_nc, " ")[[1]]
    
    d$x_nc2 <- strsplit(d$x_nc, " ")
    d$x_nc2[sapply(d$x_nc2, length) == 0] <- ""
    
    d <- by(d, d$line, function(d){
      d <- merge(data.frame(value = d$x_nc2[[1]]), d)
      #d <- merge(tibble::tibble(value = d$x_nc2[[1]]), d) ## causes seg faults
      d$pos <- seq_len(nrow(d))
      d
    })
    d <- do.call(rbind, d)
    
    d$x <- NULL
    d$x_nc <- NULL
    d$x_nc2 <- NULL
    
    d$name <- NA
    d$parameter <- NA
    d$lower <- NA
    d$init <- NA
    d$upper <- NA
    
    number_regex <- "\\(?\\-?[0-9\\.]+\\)?F?I?X?\\)?"
    single_number_regex <- paste0("^(",number_regex,")$")
    lower_init_regex <- paste0("^\\((",number_regex,"),(",number_regex,")\\)F?I?X?$")  
    lower_init_upper_regex <- paste0("^\\((",number_regex,"),(",number_regex,"),(",number_regex,")\\)F?I?X?$")
    lower_upper_regex <- paste0("^\\((",number_regex,"),,(",number_regex,")\\)F?I?X?$")
    
    d$format <- NA
    d$format[grepl(single_number_regex, d$value)] <- "single_number"
    d$format[grepl(lower_init_regex, d$value)] <- "lower_init"
    d$format[grepl(lower_init_upper_regex, d$value)] <- "lower_init_upper"
    d$format[grepl(lower_upper_regex, d$value)] <- "lower_upper"
    
    d$theta <- NA
    d$theta[!is.na(d$format)] <- seq_along(d$parameter[!is.na(d$format)])
    
    d$parameter[!is.na(d$format)] <- 
      paste0("THETA",seq_along(d$parameter[!is.na(d$format)]))
    
    d$FIX <- grepl("FIX", d$value)
    d$value <- gsub("FIX", "", d$value)
    
    d$init[d$format %in% "single_number"] <- 
      as.numeric(grep(single_number_regex,
                      d$value[d$format %in% "single_number"], value = TRUE))
    
    d$lower[d$format %in% "lower_init"] <- 
      as.numeric(gsub(lower_init_regex,"\\1",d$value[d$format %in% "lower_init"]))
    d$init[d$format %in% "lower_init"] <- 
      as.numeric(gsub(lower_init_regex,"\\2",d$value[d$format %in% "lower_init"]))
    
    d$lower[d$format %in% "lower_init_upper"] <- 
      as.numeric(gsub(lower_init_upper_regex,"\\1",d$value[d$format %in% "lower_init_upper"]))
    d$init[d$format %in% "lower_init_upper"] <- 
      as.numeric(gsub(lower_init_upper_regex,"\\2",d$value[d$format %in% "lower_init_upper"]))
    d$upper[d$format %in% "lower_init_upper"] <- 
      as.numeric(gsub(lower_init_upper_regex,"\\3",d$value[d$format %in% "lower_init_upper"]))
    
    d$lower[d$format %in% "lower_upper"] <- 
      as.numeric(gsub(lower_upper_regex,"\\1",d$value[d$format %in% "lower_upper"]))
    d$upper[d$format %in% "lower_upper"] <- 
      as.numeric(gsub(lower_upper_regex,"\\2",d$value[d$format %in% "lower_upper"]))
    
    d$format <- NULL
    
    ## grab the names 
    
    d$comment_nfields <- NA
    two_field_regex <- "^(.*?);(.*?)$"
    three_field_regex <- "^(.*?);(.*?);(.*?)$"
    d$comment_nfields[grepl(two_field_regex, d$comment)] <- 2
    d$comment_nfields[grepl(three_field_regex, d$comment)] <- 3
    
    d$name[!is.na(d$comment)] <- d$comment[!is.na(d$comment)]
    d$name[d$comment_nfields %in% 2] <- gsub(two_field_regex, "\\1", d$comment[d$comment_nfields %in% 2])
    d$name[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\1", d$comment[d$comment_nfields %in% 3])
    d$name <- trimws(d$name)
    
    d$unit <- NA
    d$unit[d$comment_nfields %in% 2] <- gsub(two_field_regex, "\\2", d$comment[d$comment_nfields %in% 2])
    d$unit[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\2", d$comment[d$comment_nfields %in% 3])
    d$unit <- trimws(d$unit)
    
    d$trans <- NA
    d$trans[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\3", d$comment[d$comment_nfields %in% 3])
    d$trans <- trimws(d$trans)
    
    d$comment_nfields <- NULL
    
    PK_thetas <- m %>% target(PK_PRED) %>% grab_variables("THETA\\([0-9+]\\)")
    ERROR_thetas <- m %>% target("ERROR") %>% grab_variables("THETA\\([0-9+]\\)")
    
    PK_thetas <- gsub("\\(","", PK_thetas)
    PK_thetas <- gsub("\\)","", PK_thetas)
    ERROR_thetas <- gsub("\\(","", ERROR_thetas)
    ERROR_thetas <- gsub("\\)","", ERROR_thetas)
    
    if(length(c(PK_thetas,ERROR_thetas)) > nrow(d))
      stop("Found more THETAs in code than in $THETA")
    
    d$SUB <- NA
    d$SUB[d$parameter %in% PK_thetas] <- PK_PRED
    d$SUB[d$parameter %in% ERROR_thetas] <- "ERROR"
    
    d <- d[, c(names(d)[!names(d) %in% c("line", "pos")], "line", "pos")]
    
    d
    
  } else {
    replace <- setup_dollar(param_r2nm_extra(replace), "$THETA", add_dollar_text = FALSE)
    old_target <- target(m)
    m <- m %>% target("THETA") %>% text(replace) %>% target(old_target)
    m
  }
}
  
raw_init_random <- function(m, replace, dollar = "OMEGA"){
  
  if(missing(replace)){
    
    m <- as_nm_generic(m)
    
    dollar_text <- gsub("\\$","",dollar)
    
    ctl <- m %>% ctl_contents()
    ctl_char <- ctl_character(ctl)
    
    sub_names <- names(ctl_contents(m))
    if("PK" %in% sub_names) PK_PRED <- "PK"
    if("PRED" %in% sub_names) PK_PRED <- "PRED"
    
    x_pk <- ctl[[PK_PRED]]
    x_orig <- ctl[[dollar_text]]
    x <- x_orig
    x <- as.character(x)
    
    # dol_omega <- "$OMEGA 0.1 -2 (3,4 ) ; pk parameters
    # $OMEGA 4 ; PD
    # (2, 3) \t ; IIV_KA2 ; CV% ; LOG
    # 
    # ; random comment
    # (0, 3, 3) ; IIV_EC50 ; ;
    # (0 3 ) ; ; ng/ml ; LOG
    # ( 4 5 6) 4  ; IIV_EMAX
    # $OMEGA BLOCK (3)
    # 0.1             ; IIV_KA ; CV% ; LOG
    # 0.1 0.1         ; IIV_V2 ; CV% ; LOG
    # 3 , 0.1 0.1 FIX     ; IIV_CL ; CV% ; LOG
    # $OMEGA ( 4 5 6) FIX,-4.2
    # 
    # $OMEGA ( 4,,6), -4.25  ; IIV_EMAX2
    # "
    # x <- strsplit(dol_omega, "\n")[[1]]
    
    d <- data.frame(x = x)
    d$line <- seq_len(nrow(d))
    ## clean up x to just what's necessary
    d$x_nc <- rem_comment(d$x)
    d$comment <- NA
    d$comment[grepl(".*;.*", d$x)] <- gsub(".+?;(.*)", "\\1", d$x[grepl(".*;.*", d$x)])
    
    ## -- clean up separators -- ##
    ## sort spaces out
    d$x_nc <- gsub("\\t","  ",d$x_nc, perl = TRUE)
    d$x_nc <- gsub("\\s+"," ",d$x_nc, perl = TRUE)
    d$x_nc <- trimws(d$x_nc)
    
    d$x_nc <- gsub("\\s+FIX", "FIX", d$x_nc)  ## remove seps so it stays with parameter and survives split
    
    ## remove spaces near commas and brackets
    d$x_nc <- gsub("\\s*,\\s*",",",d$x_nc, perl = TRUE)
    d$x_nc <- gsub("\\(\\s*","\\(",d$x_nc, perl = TRUE)  
    d$x_nc <- gsub("\\s*\\)","\\)",d$x_nc, perl = TRUE)
    d$x_nc <- gsub_in_brackets("\\s+", ",", d$x_nc)  
    d$x_nc <- gsub_out_brackets(",", " ", d$x_nc)  
    
    x_nc <- d$x_nc
    x_nc <- paste(x_nc, collapse = " \n ")
    x_nc <- strsplit(x_nc, " ")[[1]]
    
    d$x_nc2 <- strsplit(d$x_nc, " ")
    d$x_nc2[sapply(d$x_nc2, length) == 0] <- ""
    
    d <- by(d, d$line, function(d){
      d <- merge(data.frame(value = d$x_nc2[[1]]), d)
      #d <- merge(tibble::tibble(value = d$x_nc2[[1]]), d) ## causes seg faults
      d$pos <- seq_len(nrow(d))
      d
    })
    d <- do.call(rbind, d)
    
    d$x <- NULL
    d$x_nc <- NULL
    d$x_nc2 <- NULL
    
    d$name <- NA
    d$omega1 <- NA
    d$omega2 <- NA
    d$parameter <- NA
    d$lower <- NA
    d$init <- NA
    d$upper <- NA
    
    number_regex <- "\\(?\\-?[0-9\\.]+\\)?F?I?X?\\)?"
    single_number_regex <- paste0("^(",number_regex,")$")
    lower_init_regex <- paste0("^\\((",number_regex,"),(",number_regex,")\\)F?I?X?$")  
    lower_init_upper_regex <- paste0("^\\((",number_regex,"),(",number_regex,"),(",number_regex,")\\)F?I?X?$")
    lower_upper_regex <- paste0("^\\((",number_regex,"),,(",number_regex,")\\)F?I?X?$")
    
    d$format <- NA
    d$format[grepl(single_number_regex, d$value)] <- "single_number"
    d$format[grepl(lower_init_regex, d$value)] <- "lower_init"
    d$format[grepl(lower_init_upper_regex, d$value)] <- "lower_init_upper"
    d$format[grepl(lower_upper_regex, d$value)] <- "lower_upper"
    
    ##########################
    ## label parameters
    
    d$block <- NA
    get_block_size <- FALSE
    current_block_size <- 1
    current_block_start <- 1
    current_block_end <- 1
    omega1_counter <- 0
    omega2_counter <- 0
    for(i in seq_len(nrow(d))){
      if(is.na(d$format[i])) {
        if(d$value[i] %in% "BLOCK") get_block_size <- TRUE
        next
      }
      if(get_block_size){  ## get new block information
        n <- gsub("\\(([0-9]+)\\)", "\\1", d$value[i])
        current_block_size <- as.numeric(n)
        current_block_start <- omega1_counter + 1
        current_block_end <- current_block_start - 1 + current_block_size
        get_block_size <- FALSE
        next
      }
      
      ## current_block_size + friends are now all correct
      ## update counters
      
      if(current_block_size == 1) {             ## normal omega
        omega1_counter <- omega1_counter + 1
        omega2_counter <- omega2_counter + 1
      } else {                                     ## in a block
        if(omega1_counter != omega2_counter){      ##   on same row of omega matrix
          omega2_counter <- omega2_counter + 1
        } else {                                   ##   on a new row of omega matrix
          omega1_counter <- omega1_counter + 1
          omega2_counter <- current_block_start
        }
      }
      
      ## get name
      d$omega1[i] <- omega1_counter
      d$omega2[i] <- omega2_counter
      d$parameter[i] <- paste0(dollar_text, ".", omega1_counter, ".", omega2_counter, ".")
      
      #if(omega1_counter == 1 & omega2_counter == 1) {
      #  d$block[i] <- 1
      #} else {
      #  d$block[i] <- max(stats::na.omit(c(d$block[seq_len(i-1)], 0))) + 1
      #}
      
      if(omega1_counter == current_block_start & 
         omega2_counter == current_block_start) {
        d$block[i] <- max(stats::na.omit(c(d$block[seq_len(i-1)], 0))) + 1
      } else {
        d$block[i] <- max(stats::na.omit(c(d$block[seq_len(i-1)], 0)))
      }
      
      ## if at end of a block, reset the block size to 1
      if(omega1_counter == current_block_end & 
         omega2_counter == current_block_end) {
        current_block_size <- 1
        current_block_start <- omega1_counter + 1
        current_block_end <- current_block_start - 1 + current_block_size
      } 
      
    }
    
    #######################
    blocks <- d$block[!is.na(d$block)]
    
    tab <- table(blocks)
    diffs <- c(0,diff(tab))
    diffs[diffs != 0] <- 1
    mblock <- cumsum(diffs) + 1
    names(mblock) <- names(tab)
    d$mblock[!is.na(d$block)] <- mblock[blocks]
    
    ## lines above block are part of block
    d$block <- rev(na.locf(rev(d$block)))
    
    d$FIX <- grepl("FIX", d$value)
    d$value <- gsub("FIX", "", d$value)
    
    d$init[d$format %in% "single_number"] <- 
      suppressWarnings(as.numeric(grep(single_number_regex,
                                       d$value[d$format %in% "single_number"], value = TRUE)))
    
    d$lower[d$format %in% "lower_init"] <- 
      as.numeric(gsub(lower_init_regex,"\\1",d$value[d$format %in% "lower_init"]))
    d$init[d$format %in% "lower_init"] <- 
      as.numeric(gsub(lower_init_regex,"\\2",d$value[d$format %in% "lower_init"]))
    
    d$lower[d$format %in% "lower_init_upper"] <- 
      as.numeric(gsub(lower_init_upper_regex,"\\1",d$value[d$format %in% "lower_init_upper"]))
    d$init[d$format %in% "lower_init_upper"] <- 
      as.numeric(gsub(lower_init_upper_regex,"\\2",d$value[d$format %in% "lower_init_upper"]))
    d$upper[d$format %in% "lower_init_upper"] <- 
      as.numeric(gsub(lower_init_upper_regex,"\\3",d$value[d$format %in% "lower_init_upper"]))
    
    d$lower[d$format %in% "lower_upper"] <- 
      as.numeric(gsub(lower_upper_regex,"\\1",d$value[d$format %in% "lower_upper"]))
    d$upper[d$format %in% "lower_upper"] <- 
      as.numeric(gsub(lower_upper_regex,"\\2",d$value[d$format %in% "lower_upper"]))
    
    d$format <- NULL
    
    ## grab the names 
    d$comment_nfields <- NA
    two_field_regex <- "^(.*?);(.*?)$"
    three_field_regex <- "^(.*?);(.*?);(.*?)$"
    d$comment_nfields[grepl(two_field_regex, d$comment)] <- 2
    d$comment_nfields[grepl(three_field_regex, d$comment)] <- 3
    
    d$name[!is.na(d$comment)] <- d$comment[!is.na(d$comment)]
    d$name[d$comment_nfields %in% 2] <- gsub(two_field_regex, "\\1", d$comment[d$comment_nfields %in% 2])
    d$name[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\1", d$comment[d$comment_nfields %in% 3])
    d$name <- trimws(d$name)
    
    d$unit <- NA
    d$unit[d$comment_nfields %in% 2] <- gsub(two_field_regex, "\\2", d$comment[d$comment_nfields %in% 2])
    d$unit[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\2", d$comment[d$comment_nfields %in% 3])
    d$unit <- trimws(d$unit)
    
    d$trans <- NA
    d$trans[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\3", d$comment[d$comment_nfields %in% 3])
    d$trans <- trimws(d$trans)
    
    d$comment_nfields <- NULL
    
    PK_etas <- m %>% target(PK_PRED) %>% grab_variables("\\bETA\\([0-9+]\\)")
    ERROR_etas <- m %>% target("ERROR") %>% grab_variables("\\bETA\\([0-9+]\\)")
    
    PK_etas_n <- as.numeric(gsub("ETA\\(([0-9]+)\\)", "\\1", PK_etas))
    ERROR_etas_n <- as.numeric(gsub("ETA\\(([0-9]+)\\)", "\\1", ERROR_etas))
    
    d$SUB <- NA
    if(length(PK_etas_n) > 0){
      d$SUB[grepl(paste0(dollar_text, PK_etas_n, "\\.[0-9]+", collapse = "|"), d$parameter)] <- PK_PRED
      d$SUB[grepl(paste0(dollar_text,"[0-9]+\\.", PK_etas_n, "\\b", collapse = "|"), d$parameter)] <- PK_PRED
    }
    if(length(ERROR_etas_n) > 0){
      d$SUB[grepl(paste0(dollar_text, ERROR_etas_n, "\\.[0-9]+", collapse = "|"), d$parameter)] <- "ERROR"
      d$SUB[grepl(paste0(dollar_text, "[0-9]+\\.", ERROR_etas_n, "\\b", collapse = "|"), d$parameter)] <- "ERROR"
    }
    
    d <- d[, c(names(d)[!names(d) %in% c("line", "pos")], "line", "pos")]  
    d
    
  } else {
    replace <- setup_dollar(param_r2nm_extra(replace), paste0("$",dollar), add_dollar_text = FALSE)
    old_target <- target(m)
    m <- m %>% target(dollar) %>% text(replace) %>% target(old_target)
    m
  }
}

raw_init_omega <- raw_init_random
formals(raw_init_omega)$dollar <- "OMEGA"

raw_init_sigma <- raw_init_random
formals(raw_init_sigma)$dollar <- "SIGMA"

param_r2nm_extra <- function(d){
  
  single_number <- !is.na(d$init) & is.na(d$lower) & is.na(d$upper)
  lower_init <- !is.na(d$init) & !is.na(d$lower) & is.na(d$upper)
  lower_init_upper <- !is.na(d$init) & !is.na(d$lower) & !is.na(d$upper)
  lower_upper <- !is.na(d$init) & is.na(d$lower) & !is.na(d$upper)
  
  d$value[single_number] <- d$init[single_number]
  d$value[lower_init] <- paste0("(",d$lower[lower_init],",",d$init[lower_init],")")
  d$value[lower_init_upper] <- paste0("(",d$lower[lower_init_upper],",",d$init[lower_init_upper],",",d$upper[lower_init_upper],")")
  d$value[lower_upper] <- paste0("(",d$lower[lower_upper],",,",d$upper[lower_upper],")")
  
  name <- !is.na(d$name) & is.na(d$unit) & is.na(d$trans)
  name_unit <- !is.na(d$name) & !is.na(d$unit) & is.na(d$trans)
  name_unit_trans <- !is.na(d$name) & !is.na(d$unit) & !is.na(d$trans)
    
  d$comment[name_unit_trans] <- 
    paste(d$name[name_unit_trans], d$unit[name_unit_trans], d$trans[name_unit_trans],
          sep = " ; ")
  d$comment[name_unit] <- 
    paste(d$name[name_unit], d$unit[name_unit],
          sep = " ; ")
  d$comment[name] <-  d$name[name]
  
  d$value[d$FIX %in% TRUE] <- paste(d$value[d$FIX %in% TRUE], "FIX")
  
  d <- d %>% dplyr::group_by(.data$line) %>%
    dplyr::summarise(value = paste0(.data$value, collapse = " "),
                     comment = dplyr::first(stats::na.omit(.data$comment)))
  
  com <- !is.na(d$comment)
  
  d$value[com] <- paste(d$value[com], d$comment[com], sep = "   ; ")
  d$value
  
}

#' @export
nm_row <- function(m){
  UseMethod("nm_row")
}
#' @export
nm_row.nm_generic <- function(m){
  m_orig <- m
  m <- as.list(m)
  eligible_rows <- sapply(m, function(field){
    length(field) == 1
  })
  d <- tibble::as_tibble(m[eligible_rows])
  d
}
#' @export
nm_row.nm_list <- function(m){
  d <- lapply(m, nm_row)
  d <- suppressWarnings(dplyr::bind_rows(d))
  d
}

#' @export
ofv.nm_generic <- function(r){
  if(!is_finished(r)) return(NA_real_)
  dc <- try(coef.nm_generic(r, trans = FALSE), silent = TRUE)
  if(inherits(dc, "try-error")) return(NA_real_)
  dc$FINAL[dc$parameter %in% "OBJ"]
}
#' @export
ofv.nm_list <- Vectorize_nm_list(ofv.nm_generic)

#' @export
AIC.nm_generic <- function(object, ..., k = 2){
  if(!is_finished(object)) return(NA_real_)
  if(is_single_na(object)) return(NA_real_)
  params <- try(coef.nm_generic(object),silent = TRUE)
  if(inherits(params, "try-error")) return(NA_real_)
  params <- params[grepl("THETA|OMEGA|SIGMA", params$type), ]
  
  n_parameters <- nrow(params)
  ofv(object) + k*n_parameters
}
#' @export
AIC.nm_list <- Vectorize_nm_list(AIC.nm_generic)

#' @export
nobs.nm_generic <- function(object, ...){
  if(is_single_na(object)) return(NA)
  suppressMessages(
    d <- input_data(object, filter = TRUE)
  )
  if("AMT" %in% names(d))
    d <- d %>% dplyr::filter(is.na(.data$AMT))
  if("EVID" %in% names(d))
    d <- d %>% dplyr::filter(.data$EVID %in% 0)
  if("MDV" %in% names(d)){
    d <- d %>% dplyr::filter(!.data$MDV %in% 1) 
  }
  nrow(d)
}
#' @export
nobs.nm_list <- Vectorize_nm_list(nobs.nm_generic)

#' @export
BIC.nm_generic <- function(object, ...){
  AIC(object, ..., k = log(nobs.nm_generic(object)))
}
#' @export
BIC.nm_list <- Vectorize_nm_list(BIC.nm_generic)

rr_row <- function(m){
  d <- nm_row(m)
  d$m <- m
  d
}

is_try_nm_list <- function(m){
  if(inherits(m, "try-error")) ob_exists <- FALSE else {
    ob_exists <- is_nm_list(m)
  }
  ob_exists
}

nm_object_exists <- function(object_name, env){
  object_exists <- exists(object_name, envir = env)
  object_is_nm_list <- try(is_nm_list(get(object_name, envir = env)),
                           silent = TRUE)
  if(inherits(object_is_nm_list, "try-error"))
    object_is_nm_list <- FALSE
  object_exists & object_is_nm_list 
}



## functions to locate output files 

#' @export
run_dir_path <- function(m) file.path(run_in(m), run_dir(m))
#nm_run_dir_path <- function(m, subdir = "NM_run1") file.path(run_dir_path(m), subdir)
#nm_out_file <- function(m, file_name) file.path(nm_run_dir_path(m), file_name)

#' @export
nm_output_path <- function(m, extn, file_name) {
  lst_file <- lst_path(m)
  if(!missing(extn)){
    current_extn <- tools::file_ext(lst_file)
    out_file <- gsub(paste0("\\.", current_extn, "$"), paste0(".", extn), lst_file)
  }
  if(!missing(file_name)){
    out_file <- file.path(dirname(lst_file), file_name)
  }
  file.path(run_in(m), out_file)
}

#' @export
output_location <- function(m) file.path(run_in(m), dirname(lst_path(m)))

#' @export
ls_output <- function(m, pattern = ".", recursive = TRUE) {
  output <- dir(output_location(m), recursive = recursive, full.names = TRUE, pattern = pattern)
  return(normalizePath(output, winslash = "/"))
}


read_ext.nm_list <- function(r,trans=FALSE){
  exts <- lapply(r, read_ext)
  names(exts) <- NULL
  exts
}

plot_iter_data.nm_list <- function(r, trans = TRUE, skip = 0, yvar = "OBJ"){
  if(length(r) > 1) stop("currently can't do multiple plots at the same time", call. = FALSE)
  plot_iter_data(as_nm_generic(r), trans = trans, skip = skip, yvar = yvar)
}

#' @export
show_out.nm_generic <- function(r) {
  out_file <- file.path(run_in(r), lst_path(r))
  #out_file <- nm_output_path(r, extn = "lst")
  show_file(out_file)
}

#' @export
show_out.nm_list <- show_out.nm_generic

#' @export
print.nm_subroutine <- function(x, ...){
  cat(paste0(format(seq_along(x), width = 3),"| ",x), sep = "\n")
}


init_theta <- function(m, replace){
  d <- raw_init_theta(m)
  d$orig_line <- d$line
  if(missing(replace)){  ## get
    d <- d[!is.na(d$parameter), ]
    d$value <- NULL
    d$comment <- NULL
    d$SUB <- NULL
    return(d)
  } else {               ## set
    d$row <- seq_len(nrow(d))
    d_new <- dplyr::full_join(d, replace, by = c("line", "pos"))
    d_new <- d_new[, !grepl("\\.x$", names(d_new))]
    names(d_new) <- gsub("(.*)\\.y", "\\1", names(d_new))
    d_new <- d_new[order(d_new$row), ]
    d_new$row <- NULL
    m <- m %>% raw_init_theta(d_new)
    m
  }
}

init_omega <- function(m, replace){
  d <- raw_init_omega(m)
  d$orig_line <- d$line
  d$orig_pos <- d$pos
  if(missing(replace)){  ## get
    d$value <- NULL
    d$comment <- NULL
    d$parameter <- NULL
    d$SUB <- NULL
    return(d)
  } else {               ## set
    d_derived <- d[,c("value","comment","parameter","SUB", ## same as what was deleted above
                      "orig_line", "orig_pos")] 

    replace <- dplyr::left_join(replace, d_derived, by = c("orig_line", "orig_pos"))
    if("new_value" %in% names(replace)) {  ## for characters
      replace$value[!is.na(replace$new_value)] <- as.character(replace$new_value[!is.na(replace$new_value)])
    }
    if("new_line" %in% names(replace)) replace$line <- replace$new_line
    if("new_pos" %in% names(replace)) replace$pos <- replace$new_pos
    
    m <- m %>% raw_init_omega(replace)
    m
  }
}

init_sigma <- function(m, replace){
  d <- raw_init_sigma(m)
  d$orig_line <- d$line
  d$orig_pos <- d$pos
  if(missing(replace)){  ## get
    d$value <- NULL
    d$comment <- NULL
    d$parameter <- NULL
    d$SUB <- NULL
    return(d)
  } else {               ## set
    d_derived <- d[,c("value","comment","parameter","SUB", ## same as what was deleted above
                      "orig_line", "orig_pos")] 
    
    replace <- dplyr::left_join(replace, d_derived, by = c("orig_line", "orig_pos"))
    if("new_value" %in% names(replace)) {  ## for characters
      replace$value[!is.na(replace$new_value)] <- as.character(replace$new_value[!is.na(replace$new_value)])
    }
    if("new_line" %in% names(replace)) replace$line <- replace$new_line
    if("new_pos" %in% names(replace)) replace$pos <- replace$new_pos
    
    m <- m %>% raw_init_sigma(replace)
    m
  }
}

update_variable_in_text_numbers <- function(m, before_number, after_number){
  
  before_regex <- paste0("\\b", before_number)
  before_regex <- gsub("\\(","\\\\(", before_regex)
  after_regex <- gsub("\\)","\\\\)", after_number)
  
  regex <- paste0(before_regex, "([0-9]+)", after_regex)
  vars <- grab_variables(m, regex)
  old_n <- as.numeric(gsub(regex, "\\1", vars))
  vars <- vars[order(old_n)]
  old_n <- old_n[order(old_n)]
  vars_new <- paste0(before_number,seq_along(vars),after_number)
  new_n <- as.numeric(gsub(regex, "\\1", vars_new))
  for(i in which(vars != vars_new)){
    m <- m %>% gsub_ctl(paste0(before_regex,old_n[i],after_regex),
                        paste0(before_number,new_n[i],after_number))
  }
  m
}

insert_theta <- function(itheta,
                         theta_number = NA,
                         init = 0.1,
                         name = NA,
                         lower = NA,
                         upper = NA,
                         FIX = FALSE,
                         unit = NA,
                         trans = NA){
  
  if(missing(theta_number)) theta_number <- max(c(0,stats::na.omit(itheta$theta))) +1
  
  insert_line <- max(c(0, itheta$line[itheta$theta %in% (theta_number-1)])) + 1
  
  itheta_pre <- itheta[itheta$line < insert_line, ]
  itheta_post <- itheta[itheta$line >= insert_line, ]
  itheta_post$line <- itheta_post$line + 1
  
  itheta_post$theta[(itheta_post$theta >= theta_number) %in% TRUE] <- 
    itheta_post$theta[(itheta_post$theta >= theta_number) %in% TRUE] + 1
  

  dinsert <- data.frame(theta = theta_number,
                        init = init, 
                        name = name,
                        lower = lower,
                        upper = upper,
                        FIX = FIX,
                        unit = unit,
                        trans = trans,
                        line = insert_line,
                        pos = 1)
  
  suppressWarnings(dplyr::bind_rows(itheta_pre, dinsert, itheta_post))
  
}

insert_omega <- function(iomega,
                         omega_number = NA,
                         init = 0.1,
                         name = NA,
                         lower = NA,
                         upper = NA,
                         FIX = FALSE,
                         unit = NA,
                         trans = NA){
  
  #iomega$new_line <- iomega$line
  #iomega$new_pos <- iomega$pos
  
  if(missing(omega_number)) omega_number <- max(c(0,stats::na.omit(iomega$omega1))) +1
  
  insert_line <- max(c(0, iomega$line[iomega$omega1 %in% (omega_number-1)])) + 1
  
  #iomega_pre <- iomega[iomega$line[iomega$line < insert_line], ]
  #iomega_post <- iomega[iomega$line[iomega$line >= insert_line], ]
  iomega_pre <- iomega[iomega$line < insert_line, ]
  iomega_post <- iomega[iomega$line >= insert_line, ]
  iomega_post$line <- iomega_post$line + 1
  
  iomega_post$omega1[(iomega_post$omega1 >= omega_number) %in% TRUE] <- 
    iomega_post$omega1[(iomega_post$omega1 >= omega_number) %in% TRUE] + 1
  
  iomega_post$omega2[(iomega_post$omega2 >= omega_number) %in% TRUE] <- 
    iomega_post$omega2[(iomega_post$omega2 >= omega_number) %in% TRUE] + 1
  
  iomega_post$block <- iomega_post$block + 1
  
  insert_block <- max(c(0, stats::na.omit(iomega$block[iomega$omega1 %in% (omega_number-1)]))) + 1

  insert_mblock <- max(c(0, stats::na.omit(iomega$mblock[iomega$omega1 %in% (omega_number-1)])))
  if(length(which(iomega$omega1 %in% (omega_number-1))) > 1)
    insert_mblock <- insert_mblock + 1
  
  dinsert <- data.frame(omega1 = omega_number,
                        omega2 = omega_number,
                        init = init, 
                        name = name,
                        lower = lower,
                        upper = upper,
                        block = insert_block,
                        mblock = insert_mblock,
                        FIX = FIX,
                        unit = unit,
                        trans = trans,
                        line = insert_line,
                        pos = 1)
  
  suppressWarnings(dplyr::bind_rows(iomega_pre, dinsert, iomega_post))
  
}

block <- function(iomega,
                  eta_numbers = NA,
                  diag_init = 0.01){
  
  eta_numbers <- sort(eta_numbers)
  
  if(!all(diff(eta_numbers) == 1)) stop("etas must be adjacent", call. = FALSE)
  
  start_eta <- min(eta_numbers)
  end_eta <- max(eta_numbers)
  
  #start_index <- match(iomega$block[iomega$omega1 %in% start_eta], iomega$block)
  start_index <- match(start_eta, iomega$omega1)
  end_index <- match(end_eta, iomega$omega1)
  
  if(is.na(start_index) | is.na(end_index)) stop("etas not found", call. = FALSE)
  
  omega_counts <- iomega$omega1[!is.na(iomega$omega1)]
  omega_counts <- table(omega_counts)[eta_numbers]

  if(any(omega_counts > 1)) stop("etas cannot already be a block", call. = FALSE)

  start_block <- iomega$block[iomega$omega1 %in% start_eta]
  iomega$remove <- FALSE
  iomega$remove[iomega$block %in% start_block & is.na(iomega$omega1)] <- TRUE
    
  iomega_block <- iomega[start_index:end_index, ]
  
  all_indexes <- seq_len(nrow(iomega))  ## defined to save code
  iomega_pre <- iomega[all_indexes[all_indexes < start_index], ]
  iomega_post <- iomega[all_indexes[all_indexes > end_index], ]
  
  new_block <- min(iomega_block$block)
  
  ################################
  ## insert rows for covariances - match what raw_init_omega does

  ## add diagonals:
  
  ddiag <- expand.grid(omega1 = stats::na.omit(iomega_block$omega1),
                       omega2 = stats::na.omit(iomega_block$omega1))
  ddiag <- ddiag[ddiag$omega1 >= ddiag$omega2, ]
  
  iomega_block <- merge(ddiag, iomega_block, all = TRUE)
  diag_index <- (iomega_block$omega1 != iomega_block$omega2) %in% TRUE
  
  iomega_block$init[diag_index] <- diag_init
  iomega_block$block <- new_block

  block_text_rows <- data.frame(new_value = c("$OMEGA", "BLOCK", 
                                         paste0("(",length(eta_numbers),")")),
                                block = new_block,
                                line = min(iomega_block$line, na.rm = TRUE),
                                pos = 1:3)
  
  iomega_block$line <- rev(na.locf(rev(iomega_block$line)))
  
  iomega_block <- iomega_block %>% dplyr::group_by(.data$omega1) %>%
    dplyr::mutate(pos = 1:length(.data$omega1)) %>% as.data.frame
  
  iomega_block$line <- iomega_block$line + 1
  suppressWarnings({
    iomega_block$mblock[!is.na(iomega_block$mblock)] <- 
      max(c(0, max(iomega_pre$mblock, na.rm = TRUE))) + 1
  })
  
  iomega_block <- suppressWarnings(dplyr::bind_rows(block_text_rows, iomega_block))

  ## post will also be one line shifted
  iomega_post$line <- iomega_post$line + 1

  suppressWarnings({
    should_be <- unique(stats::na.omit(iomega_block$mblock))+1
    iomega_post$mblock[!is.na(iomega_post$mblock)] <- 
      iomega_post$mblock[!is.na(iomega_post$mblock)] - 
      (min(iomega_post$mblock[!is.na(iomega_post$mblock)], na.rm = TRUE) - 
         should_be)
  })
  
  if(any(!is.na(iomega_post$block))){
    iomega_post$block <- iomega_post$block - 
      (min(iomega_post$block, na.rm = TRUE) - (new_block + 1))
  }
  ################################
  iomega <- suppressWarnings(dplyr::bind_rows(iomega_pre, iomega_block, iomega_post))
  iomega <- iomega[!(iomega$remove %in% TRUE), ]
  iomega$remove <- NULL
  iomega
  
}

unblock <- function(iomega, eta_numbers){

  eta_numbers <- sort(eta_numbers)
  
  if(!all(diff(eta_numbers) == 1)) stop("etas must be adjacent", call. = FALSE)
  
  start_eta <- min(eta_numbers)
  end_eta <- max(eta_numbers)
  
  start_index <- match(start_eta, iomega$omega1)
  end_index <- match(end_eta, iomega$omega1)
  
  if(is.na(start_index) | is.na(end_index)) stop("etas not found", call. = FALSE)
  
  block_to_dismantle <- iomega$block[iomega$omega1 %in% eta_numbers]
  block_to_dismantle <- unique(block_to_dismantle)
  
  if(length(block_to_dismantle) > 1) stop("etas belong to multiple blocks", call. = FALSE)
  if(length(block_to_dismantle) == 0) stop("couldn't find block", call. = FALSE)
  
  iomega_block <- iomega[iomega$block %in% block_to_dismantle,]
  
  iomega_pre <- iomega[iomega$line < min(iomega_block$line, na.rm = TRUE), ]
  iomega_post <- iomega[iomega$line > max(iomega_block$line, na.rm = TRUE), ]
  
  ## remove $OMEGA block lines
  
  iomega_block <- iomega_block[!is.na(iomega_block$omega1),]
  iomega_block <- iomega_block[iomega_block$omega1 == iomega_block$omega2, ]
  iomega_block$pos <- 1
  suppressWarnings({
    iomega_block$block <- seq_along(iomega_block$block) +
      max(c(0, max(iomega_pre$block, na.rm = TRUE)))
  })
  
  iomega_block$mblock <- max(c(1, iomega_pre$mblock), na.rm = TRUE)
    
  if(any(!is.na(iomega_pre$line))){
    iomega_block$line <- iomega_block$line + 
      max(iomega_pre$line) + 1 - min(iomega_block$line)
  }
  
  if(any(!is.na(iomega_post$line))){
    iomega_post$line <- iomega_post$line + max(iomega_block$line) + 1 - min(iomega_post$line)
  }
  
  suppressWarnings({
    should_be <- max(iomega_block$mblock, na.rm = TRUE)
    iomega_post$mblock[!is.na(iomega_post$mblock)] <- 
      iomega_post$mblock[!is.na(iomega_post$mblock)] - 
      (min(iomega_post$mblock, na.rm = TRUE) - should_be)
  })
  
  if(any(!is.na(iomega_post$block))){
    iomega_post$block <- iomega_post$block - 
      (min(iomega_post$block, na.rm = TRUE) - (max(iomega_block$block) + 1))
  }
  
  ################################
  suppressWarnings(dplyr::bind_rows(iomega_pre, iomega_block, iomega_post))  
  
}

#' Create new R function template
#' @param function_name character indicating name of function
#' @param overwrite logical. Whether to overwrite existing file (default = FALSE)
#' @param open_file logical. Whether function should open script (default = TRUE)
#' @param libs character. What libraries to add.
#' @export
new_function_template <- function(function_name, overwrite = FALSE, open_file = TRUE, libs=c("NMproject")) {
  ## create black script with comment fields. Add new_script to git
  check_if_tidyproject()
  file_name <- paste0(function_name, ".R")
  if (file_name != basename(file_name))
    stop("name must not be a path")
  to_path <- file.path("localpackage", "R", file_name)  ## destination path
  if (file.exists(to_path) & !overwrite)
    stop(paste(to_path, "already exists. Rerun with overwrite = TRUE"))
  s <- c(paste0("## ", "Author: ", Sys.info()["user"]),
         paste0("## ", "First created: ", Sys.Date()),
         paste0("## ", "Description: "),
         paste0("## ", "Keywords: "),
         "",
         "########################################",
         "## load packages and source functions here", 
         "", paste0("library(",libs,")"),
         "",
         "########################################", 
         "## main script here", "",
         "",
         paste0(function_name, " <- Vectorize(function(m){"),
         "  wait_finish(m)",
         "  # d <- output_table(m) ## read in combined output ",
         "  ",
         "  ###############",
         "  ## include plotting code here",
         "  ",
         "  ",
         "  ###############",
         "  ## return plotting object here",
         "  ",
         "})")
  writeLines(s, to_path)
  setup_file(to_path)
  if (open_file) 
    get("file.edit")(to_path)
}


#' @export
ctl_table_paths <- function(ctl) {
  UseMethod("ctl_table_paths")
}
#' @export
ctl_table_paths.nm_generic <- function(ctl) {
  ## path should go from base directory
  ## in psn directory
  file.path(output_location(ctl), ctl_table_files(ctl_contents(ctl)))
}
#' @export
ctl_table_paths.nm_list <- Vectorize_nm_list(ctl_table_paths.nm_generic, SIMPLIFY = FALSE)


nm_output.nm_generic <- function(r,dorig,...){
  
  r <- as_nm_generic(r)  ## because nm_list method is identical
  wait_finish(r)
  
  # if(requireNamespace("xpose4", quietly = TRUE)) {
  #   xpdb <- xpose4::xpose.data(run_id(r), directory=paste0(run_in(r),"/"))
  #   d <- xpdb@Data
  # } else 
    d <- data.frame()
  
  if(nrow(d) == 0){
    ctl_out_files <- ctl_table_paths(as_nm_generic(r))
    
    d <- lapply(ctl_out_files, function(out_file){
      d <- nm_read_table(out_file, skip = 1, header = TRUE)
    })
    
    ## TODO: this will break if some tables have FIRSTONLY
    nrows <- sapply(d, nrow)
    if(length(unique(nrows[!nrows %in% 0])) > 1)
      stop("output tables are different sizes")
    
    d <- do.call(cbind,d)
    d <- d[,!duplicated(names(d))]
  }
  
  if(missing(dorig)) dorig <- input_data(r,...)
  
  filter_statements <- data_filter_char(r)
  if(identical(filter_statements, "TRUE")){
    dORD <- seq_len(nrow(dorig))
  } else {
    expre <- parse(text=filter_statements)
    dORD <- which(with(dorig,eval(expre)))    
  }
  
  if(nrow(d) %% length(dORD) != 0) {
    stop("something wrong... when R reads in original dataset
         and applies filter ",filter_statements,",
         there's ",length(dORD),"rows, but NONMEM output has ", nrow(d), " rows")
  }    
  
  ctl_contents <- ctl_character(ctl_contents(r))
  sim_ctl <- any(grepl("^\\s*\\$SIM",rem_comment(ctl_contents)))
  
  nreps <- nrow(d) / length(dORD)
  
  if("PRKEY" %in% names(d)) stop("name conflict with PRKEY in xpose table. aborting...")
  if("PRKEY" %in% names(dorig)) stop("name conflict with PRKEY in original data. aborting...")
  
  d$PRKEY <- dORD
  dorig$PRKEY <- 1:nrow(dorig)
  if(sim_ctl){
    if("SIM" %in% names(d)) stop("name conflict with SIM in xpose table. aborting...")
    if("SIM" %in% names(dorig)) stop("name conflict with SIM in original data. aborting...")
    d$SIM <- rep(1:nreps,each=length(dORD))
    message("Adding column: SIM")
  }
  
  d$INNONMEM <- TRUE
  
  ## want a DV_OUT columsn
  if("DV_OUT" %in% names(d)) warning("name conflict with DV_OUT in xpose table. replacing...")
  d$DV_OUT <- d$DV
  d$DV <- NULL
  d <- d[,c(setdiff(names(d),names(dorig)[!names(dorig) %in% c("PRKEY")]))]
  #dorig <- dorig[,names(dorig)[!names(dorig) %in% c("DV")]]
  
  #d$.tempORD <- 1:nrow(d) ## to preserve order (old code merge())
  d2 <- dplyr::full_join(d, dorig, by = "PRKEY")
  #d2 <- d2[order(d2$.tempORD), ]
  #d2$.tempORD <- NULL
  
  d2$INNONMEM <- d2$INNONMEM %in% TRUE
  if(nreps > 1) d2$SIM[is.na(d2$SIM)] <- 0
  
  ## row number check
  if(nrow(d2) != nrow(d)*(nreps-1)/nreps + nrow(dorig)) stop("merge went wrong. debug")
  
  message("Adding column: PRKEY")
  
  return(d2)
}

nm_output.nm_list <- nm_output.nm_generic


run_checksums <- function(m){  ## only works on single m
  ## information determinative to whether run should be rerun
  m %>% write_ctl()
  files <- c(ctl_path(m), data_path(m))
  
  checksums <- tools::md5sum(files)
  names(checksums) <- c("ctl", "data")
  checksums <- c(checksums, 
                 cmd = get_glue_field(as_nm_generic(m), "cmd"))
  
  checksums
  
}

render_checksums <- function(m, input){  ## only works on single m
  ## information determinative to whether run should be rerun
  #m %>% write_ctl()
  files <- c(ctl_table_paths(m), data_path(m), input)
  c(tools::md5sum(files))
}

unique_run_cache_path <- function(m){
  file.path(".cache", 
            paste0(gsub(.Platform$file.sep, "--", unique_id(m))))
}

run_cache_paths <- function(m){
  
  ## sort by version number
  hack_m <- m %>% version("[0-9]+")  ## use regex to get all versions
  pattern <- hack_m %>%
    unique_run_cache_path() %>%
    basename()
  pattern <- paste0("^",pattern,"$")

  dir(".cache", pattern = pattern, full.names = TRUE)
  
}

#' @export
cached_object <- function(m){
  UseMethod("cached_object")
}
#' @export
cached_object.nm_generic <- function(m){
  path <- unique_run_cache_path(m)
  if(!file.exists(path)) return(nm(NA))
  readRDS(path)$object
}
#' @export
cached_object.nm_list <- Vectorize_nm_list(cached_object.nm_generic, SIMPLIFY = FALSE)


unique_render_cache_path <- function(m, input){
  file.path(".cache", 
            paste0(gsub(.Platform$file.sep, "--", unique_id(m)),
                   "-.-", 
                   gsub(.Platform$file.sep, "--", input)))
}

render_cache_paths <- function(m, input){
  
  pattern <- m %>%
    unique_render_cache_path(input) %>%
    basename()
  pattern <- paste0("^",pattern,"$")
  
  dir(".cache", pattern = pattern, full.names = TRUE)
  
}

save_run_cache <- function(m) {
  ## this is for after a run has been submitted
  unique_run_cache_path <- unique_run_cache_path(m)
  dir.create(dirname(unique_run_cache_path), recursive = TRUE, showWarnings = FALSE)
  
  run_cache_disk <- list(version = version(m), 
                         job_info = job_info(m),
                         object = m,
                         checksums = run_checksums(m))
  saveRDS(run_cache_disk, file = unique_run_cache_path)
  
  invisible(m)
}

save_render_cache <- function(m, input) {
  ## this is for after a run has been submitted
  unique_render_cache_path <- unique_render_cache_path(m, input)
  dir.create(dirname(unique_render_cache_path), recursive = TRUE, showWarnings = FALSE)
  
  render_cache_disk <- list(object = m,
                            checksums = render_checksums(m, input))
  saveRDS(render_cache_disk, file = unique_render_cache_path)
  
  invisible(m)
}

#' @export
parent_run <- function(m, n = 1L){
  UseMethod("parent_run")
}
#' @export
parent_run.nm_generic <- function(m, n = 1L){
  
  if(n == 0) return(as_nm_generic(nm(NA)))
  
  hack_m <- m %>% 
    run_id(parent_run_id(m)) %>% 
    run_in(parent_run_in(m)) %>% 
    version("[0-9]+")

  pattern <- hack_m %>%
    unique_run_cache_path() %>%
    basename()
  pattern <- paste0("^",pattern,"$")
  
  ## sort by most recent

  dir_list <- dir(".cache", pattern = pattern, full.names = TRUE)
  
  file_info <- file.info(dir_list)
  
  md5_files <- dir_list[order(file_info$mtime, decreasing = TRUE)]
  
  ## get proposed parent objects and update
  if(length(md5_files) == 0) return(as_nm_generic(nm(NA)))

  md5_file <- md5_files[1]
  parent_ob <- readRDS(md5_file)$object

  if(n > 1) return(parent_run(parent_ob, n = n - 1))
  parent_ob
  
}
#' @export
parent_run.nm_list <- Vectorize_nm_list(parent_run.nm_generic, SIMPLIFY = FALSE)





#' Save plots in results_dir
#' 
#' @param r nm object
#' @param plot_ob a list of plotting objects
#' @param plot_name character. Name of results file
#' @param plot_dir character (default = results_dir(r)). Where to save.
#' @param width passed to ggsave
#' @param height passed to ggsave
#' @param dpi passed to ggsave
#' @param ... passed to ggsave
#' @export
nmsave_plot <- function(r, plot_ob, plot_name, plot_dir = results_dir(r), 
                        width = 7, height = 5, dpi = 300, ...){
  UseMethod("nmsave_plot")
}
#' @export
nmsave_plot.nm_generic <- function(r, plot_ob, plot_name, plot_dir = results_dir(r), 
                                   width = 7, height = 5, dpi = 300, ...){
  plot_name <- glue_text_nm(r, plot_name)
  plot_name <- unique(plot_name)
  if(length(plot_name) > 1) stop("multiple plot names", call. = FALSE)
  dir.create(unique(plot_dir), showWarnings = FALSE, recursive = TRUE)
  requireNamespace("grid")
  ggplot2::ggsave(filename = file.path(unique(plot_dir), plot_name),
                  plot = plot_ob, 
                  width = width, height = height, dpi = dpi, ...)
  r <- r %>% result_files(plot_name)
  invisible(r)
}
#' @export
nmsave_plot.nm_list <- Vectorize_nm_list(nmsave_plot.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

#' @export
nmsave_table <- function(r, table_ob, table_name, table_dir = results_dir(r), ...){
  UseMethod("nmsave_table")
}
#' @export
nmsave_table.nm_generic <- function(r, table_ob, table_name, table_dir = results_dir(r), ...){
  table_name <- glue_text_nm(r, table_name)
  table_name <- unique(table_name)
  if(length(table_name) > 1) stop("multiple table names", call. = FALSE)
  dir.create(unique(table_dir), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(table_ob, 
                   file = file.path(unique(table_dir), table_name),
                   row.names = FALSE, ...)
  r <- r %>% result_files(table_name)
  invisible(r)
}
#' @export
nmsave_table.nm_list <- Vectorize_nm_list(nmsave_table.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

#' @export
nmsave_multiplot <- function(r, plot_ob, plot_name, plot_dir = results_dir(r), 
                             width = 7, height = 5, dpi = 300, ...){
  UseMethod("nmsave_multiplot")
}

#' @export
nmsave_multiplot.nm_generic <- function(r, plot_ob, plot_name, plot_dir = results_dir(r), 
                             width = 7, height = 5, ...){
  
  plot_name <- glue_text_nm(r, plot_name)
  plot_name <- unique(plot_name)
  if(length(plot_name) > 1) stop("multiple plot names", call. = FALSE)
  dir.create(unique(plot_dir), showWarnings = FALSE, recursive = TRUE)
  
  grDevices::pdf(file.path(unique(plot_dir), plot_name), ...)
  print(plot_ob)
  grDevices::dev.off()
  r <- r %>% result_files(plot_name)
  invisible(r)
  
}

#' @export
nmsave_multiplot.nm_list <- Vectorize_nm_list(nmsave_multiplot.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)


#' render function for nm objects
#' 
#' @param m nm object
#' @param input character. Same as rmarkdown::render() arg
#' @param output_file character. Same as rmarkdown::render() arg
#' @param args list. Same as "params" arg in rmarkdown::render()
#' @param force logical (default = FALSE). will force execution
#' 
#' @details 
#' \code{input} must refer to a properly specified Rmd document.
#' The R markdown template "model diagnostic" in RStudio sets this up 
#' for you.
#' 
#' These R markdown templates are usable as R Notebooks (e.g. for code
#' development and debugging) if the object \code{.m} is defined in the
#' global work space first.
#' 
#' @examples 
#' \dontrun{
#'   m1 %>% nm_render("Scripts/basic_gof.Rmd")
#' 
#'   ## to run "Scripts/basic_gof.Rmd" as an R Notebook
#'   ## first define .m
#'   
#'   .m <- m1 ## Now you can run "Scripts/basic_gof.Rmd" as a Notebook
#' }
#' @export
nm_render <- function(m, 
                      input, 
                      output_file = 
                        paste0(
                          tools::file_path_sans_ext(input),
                          ".",run_dir(m),
                          ".html"
                        ),
                      args = list(),
                      force = FALSE,
                      async = FALSE,
                      ...){
  UseMethod("nm_render")
}

#' @export
nm_render.nm_generic <- function(m, 
                                 input, 
                                 output_file = 
                                   paste0(
                                     tools::file_path_sans_ext(input),
                                     ".",run_dir(m),
                                     ".html"
                                   ),
                                 args = list(),
                                 force = FALSE,
                                 async = FALSE,
                                 ...){
  
  if("m" %in% names(args))
    stop("can't have m in arg.  m is reserved for model object")
  
  args <- c(args, list(m = as_nm_list(m)))
  
  output_dir <- results_dir(m)
  output_path <- file.path(output_dir, output_file)
  
  ## if force is TRUE skip caching and run
  if(!force){
    ## if output_path doesn't exist skip caching and run
    ##if(file.exists(output_path)){
      ## pull existing checksum info
      render_cache_disk <- lapply(render_cache_paths(m, input), readRDS)
      if(length(render_cache_disk) > 0){
        ## get current checksum
        current_checksums <- render_checksums(m, input)
        ## determine matches
        matches <- sapply(render_cache_disk, function(i) {
          identical(i$checksums, current_checksums)
        })
        if(any(matches)){
          message("nm_render cache found, skipping... use nm_render(force = TRUE) to override")
          ## pick highest available version
          # ## update object and return
          # m <- m %>% result_files(...)
          return(invisible(m))    ## if up to date, skip
        }
      } 
    #}
  }

  if(async){
    f0 <- future::future({
      rmarkdown::render(input = input,
                        output_file = output_file,
                        output_dir = output_dir,
                        params = args,
                        envir = new.env(),
                        ...)
      
    })
  } else {
    rmarkdown::render(input = input,
                      output_file = output_file,
                      output_dir = results_dir(m),
                      params = args,
                      envir = new.env(),
                      ...)
  }
  
  ## use as_nm_generic incase m is redefined in rmd
  m <- m %>% result_files(output_file)

  m <- m %>% save_render_cache(input)
  
  invisible(m)

}

#' @export
nm_render.nm_list <- Vectorize_nm_list(nm_render.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

# TODO: nm_render_async

#' Create new R notebook
#' @param script_name character
#' @param overwrite logical. Whether to overwrite existing file (default = FALSE)
#' @param open_file logical. Whether function should open script (default = TRUE)
#' @param libs character. What libraries to add.
#' @export
new_notebook_template <- function(script_name, overwrite = FALSE, open_file = TRUE, libs=c("NMproject")) {
  tidyproject::new_notebook_template(script_name = script_name,
                                     overwrite = overwrite,
                                     open_file = open_file,
                                     libs = libs)
}



#' @export
summary.nm_list <- function(object, ref_model = NA, parameters = c("none", "new", "all"), keep_m = FALSE, ...){

  d <- rr_row(object)
  d <- d %>% dplyr::select(.data$run_id,
                           .data$m,
                           .data$parent_run_id,
                           .data$parent_run_in,
                           .data$data_path)
  cat("reading outputs...")
  d$coef_obs <- coef(d$m)  ## slowest step - crashes
  cat("done\n", append = TRUE)
  cat("summarising...")
  d$status <- status(d$m)

  n_parameters_fun <- function(coef){
    if(!"type" %in% names(coef)) return(NA)
    coef <- coef[grepl("THETA|OMEGA|SIGMA", coef$type), ]
    nrow(coef)
  }

  d <- d %>% dplyr::group_by(.data$parent_run_id, .data$parent_run_in) %>%
    dplyr::mutate(
      parent = parent_run(.data$m[1]),
      parent_coef_obs = coef(.data$parent[1]),
      n_params = sapply(.data$coef_obs, n_parameters_fun),
      parent_n_params = n_parameters_fun(.data$parent_coef_obs[[1]])
      ) %>%
    dplyr::group_by(.data$data_path) %>% ## nobs reads data - only once per data_path
    dplyr::mutate(nobs = nobs(.data$m[1])) %>%
    dplyr::group_by(.data$parent_run_id, .data$parent_run_in) %>%
    dplyr::mutate(
      status = status(.data$m),
      ofv = ofv(.data$coef_obs),
      dofv = .data$ofv - ofv(.data$parent_coef_obs[[1]]),
      df = .data$n_params - .data$parent_n_params,
      p_chisq =
        ifelse(.data$df >=0,
               1-stats::pchisq(-.data$dofv, df = .data$df),
               1-stats::pchisq(.data$dofv, df = -.data$df)),
      AIC = .data$ofv + 2*.data$n_params,
      BIC = .data$ofv + log(.data$nobs)*.data$n_params,
      ref_cn = cond_num(.data$parent_coef_obs[[1]]),
      cond_num = cond_num(.data$coef_obs)
      )
  d$coef_obs <- NULL
  d$parent_coef_obs <- NULL

  parameters <- match.arg(parameters)
  if(parameters != "none"){
    ## for each row, compute rr(d$m[i]) and rr(d$parent[i])
    ds <- split(d, seq_len(nrow(d)))

    ds <- lapply(ds, function(d){

      # rri <- rr(c(d$parent,d$m), ...)
      # rri <- rri[grepl("THETA|OMEGA|SIGMA", rri$type), ]
      #
      # index <- !rri$unit %in% "" & !is.na(rri$unit)
      # rri$parameter[index] <-
      #   paste0(rri$parameter[index], " (", rri$unit[index], ")")
      #
      # if("trans" %in% names(rri)){
      #   index <- !rri$trans %in% "" & !is.na(rri$trans)
      #   rri$parameter[index] <-
      #     paste0(rri$parameter[index], " (", rri$trans[index],")")
      # }

      rri <- rr2(c(d$parent,d$m), ...)
      rri$type <- NULL
      rri$unit <- NULL
      rri$SEunit <- NULL
      rri$trans <- NULL
      rri$par_no <- NULL
      rri$key <- NULL

      if(ncol(rri) < 2) return(d)
      if(ncol(rri) == 2) {
        names(rri)[-1] <- c("m")
      }
      if(ncol(rri) == 3) {
        names(rri)[-1] <- c("parent", "m")
      }
      if(ncol(rri) > 3) browser()#stop("stop something wrong, debug")

      if(parameters == "new") {

        param_names <- rri$parameter[!grepl("se_", rri$parameter) &
                                      !is.na(rri$m)]

        parent_param_names <- rri$parameter[!grepl("se_", rri$parameter) &
                                              !is.na(rri$parent)]

        new_param_names <- param_names[!param_names %in% parent_param_names]

        se_param_names <- paste0("se_", new_param_names)

        rri <- rri[rri$parameter %in% c(new_param_names, se_param_names), ]
        if(nrow(rri) == 0) return(d)

        # rri$parameter[is.na(rri$parent)]
        #
        # rri <- rri[is.na(rri$parent), ]
        # rri <- rri[!is.na(rri$m), ]
      }

      if(inherits(try(t(rri$m)), "try-error")) browser()

      pars_to_add <- tibble::as_tibble(t(rri$m))
      names(pars_to_add) <- rri$parameter

      dplyr::bind_cols(d, pars_to_add)
    })

    d <- suppressWarnings(dplyr::bind_rows(ds))
    d$m <- as_nm_list(d$m)
    d$parent <- as_nm_list(d$parent)
  }

  #############################
  ## remove columns that we dont want
  ##  Note: may need reinsert them if they ever are needed in reverse dependencies

  d <- d %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$data_path,
                  -.data$parent,
                  -.data$parent_run_id,
                  -.data$parent_run_in,
                  -.data$parent_n_params,
                  -.data$n_params)

  if(!keep_m) d$m <- NULL

  #############################


  cat("done", append = TRUE)
  d <- d %>% dplyr::ungroup()
  d
}

#' @export
summary.nm_generic <- function(object, ref_model = NA, parameters = c("none", "new", "all"), keep_m = FALSE, ...){
  summary(object = as_nm_list(object), ref_model = ref_model, parameters = parameters, keep_m = keep_m, ...)
}


#' #' @export
#' summary_wide <- function(..., parameters = c("none", "new", "all")){
#'   UseMethod("summary_wide")
#' }

#' @export
summary_wide <- function(..., parameters = c("none", "new", "all")){
  parameters <- match.arg(parameters)
  summary(..., parameters = parameters)
}

#' @export
summary_long <- function(..., parameters = c("none", "new", "all")){
  parameters <- match.arg(parameters)
  ds <- summary(..., parameters = parameters, keep_m = TRUE)
  m <- ds$m
  ds$m <- NULL
  d <- t(ds)
  dnames <- row.names(d)
  d <- tibble::as_tibble(d)
  names(d) <- gsub("execute:","", unique_id(m))
  d <- d %>% dplyr::mutate_all(trimws)
  dcol <- tibble::tibble("field" = dnames)
  d <- dplyr::bind_cols(dcol, d)
  d
}

#' @export
output_table.nm_generic <- output_table.default
#' @export
output_table.nm_list <- Vectorize_nm_list(output_table.nm_generic, SIMPLIFY = FALSE)

#' @export
output_table_first <- function(r, ...){
  UseMethod("output_table_first")
}

#' @export
output_table_first.nm_list <- function(r, ...){
  if(length(r) > 1) stop("only works on length 1 objects", call. = FALSE)
  outtab <- output_table(r, ...)
  outtab <- outtab[[1]]
  outtab
}

#' @export
add_cov.nm_generic <- function(ctl, param, cov, state = 2, continuous = TRUE,
                    time_varying, additional_state_text, id_var = "ID",
                    force = FALSE, force_TV_var = FALSE,
                    init, lower, upper){

  m <- ctl
  ctl <- ctl_contents(m)
  param <- as.character(param)
  cov <- as.character(cov)
  state <- as.character(state)
  continuous <- as.logical(continuous)

  if("PK" %in% names(ctl)) dol_PK <- "PK" else dol_PK <- "PRED"

  PK_section <- rem_comment(ctl[[dol_PK]])

  data <- suppressMessages(input_data(m, filter = TRUE))

  if(!cov %in% names(data)) {
    if(force) {
      warning("can't find ",cov," in data", call. = FALSE)
    } else {
      stop("can't find ",cov," in data", call. = FALSE)
    }
  }

  if(any(is.na(data[[cov]]))) {
    if(force) {
      warning("missing values in ",cov," detected", call. = FALSE)
    } else {
      stop("missing values in ",cov," detected", call. = FALSE)
    }
  }

  if(length(unique(data[[cov]])) > 5 & !continuous)
    warning(length(unique(data[[cov]])), " unique values for ", cov, " found. are you sure it's categorical?",
            call. = FALSE)

  if(length(unique(data[[cov]])) <= 1)
    warning(length(unique(data[[cov]])), " unique values for ", cov, " found. are you sure about this?",
            call. = FALSE)

  if(missing(time_varying)){
    max_levels <- max(tapply(data[[cov]], data[[id_var]], function(x) length(unique(x))), na.rm = TRUE)
    if(max_levels > 1) time_varying <- TRUE else time_varying <- FALSE
  }

  if(force_TV_var){
    tvparam <- paste0("TV",param)
  } else {
    if(time_varying){
      tvparam <- param
    } else {
      ## try TV param if exists
      if(any(grepl(paste0("\\bTV",param,"\\b"), PK_section)))
        tvparam <- paste0("TV",param)
    }
  }

  if(!any(grepl(paste0("\\bTV",param,"\\b"), PK_section)))
    stop("cant find parameter in control file", call. = FALSE)

  existing_param_rel <- any(grepl(paste0("\\b",tvparam,"COV"), PK_section))
  existing_param_cov_rel <- any(grepl(paste0("\\b",tvparam,cov), PK_section))
  if(existing_param_cov_rel) {
    return(as_nm_generic(nm(NA)))
    #stop("covariate relation already exists, cannot add", call. = FALSE)
  }

  param_info <- param_info(ctl)
  theta_n_start <- max(param_info$N) + 1

  relation_start_txt <- paste0(";;; ",tvparam,"-RELATION START")
  relation_end_txt <- paste0(";;; ",tvparam,"-RELATION END")

  definition_start_txt <- paste0(";;; ",tvparam,cov,"-DEFINITION START")
  definition_end_txt <- paste0(";;; ",tvparam,cov,"-DEFINITION END")

  if(!existing_param_rel){
    par_relation_text <- paste0(tvparam,"COV=",tvparam,cov)

    ## insert at beginning
    ctl[[dol_PK]] <- c(ctl[[dol_PK]][1],"",
                       relation_start_txt,
                       par_relation_text,
                       relation_end_txt,
                       ctl[[dol_PK]][-1])

    tv_definition_row <- which(grepl(paste0("^\\s*",tvparam,"\\s*="), rem_comment(ctl[[dol_PK]])))
    dont_count <- which(grepl(paste0("^\\s*",tvparam,"\\s*=.*\\b",tvparam), rem_comment(ctl[[dol_PK]])))
    tv_definition_row <- setdiff(tv_definition_row, dont_count)
    if(length(tv_definition_row) > 1) stop("can't find unique TV parameter definition in $PK")
    if(length(tv_definition_row) == 0) stop("can't find TV parameter definition in $PK")

    ctl[[dol_PK]] <- c(ctl[[dol_PK]][1:tv_definition_row],"",
                       paste0(tvparam," = ", tvparam,"COV*",tvparam),
                       ctl[[dol_PK]][(tv_definition_row+1):length(ctl[[dol_PK]])])

  }

  if(existing_param_rel){
    ctl[[dol_PK]] <- gsub(paste0(tvparam,"COV="),
                          paste0(tvparam,"COV=",tvparam,cov,"*"),ctl[[dol_PK]])
  }

  ## use state to get the relationship in there.
  if(!missing(additional_state_text)) {
    param_cov_text <- param_cov_text(param=tvparam,cov=cov,state = state,
                                     data = data,
                                     theta_n_start = theta_n_start,
                                     continuous = continuous,
                                     additional_state_text = additional_state_text)
  } else {
    param_cov_text <- param_cov_text(param=tvparam,cov=cov,state = state,
                                     data = data,
                                     theta_n_start = theta_n_start,
                                     continuous = continuous)
  }

  ctl[[dol_PK]] <- c(ctl[[dol_PK]][1],"",
                     definition_start_txt,
                     param_cov_text,
                     definition_end_txt,
                     ctl[[dol_PK]][-1])

  ## add thetas
  n_add_thetas <- attr(param_cov_text, "n")
  if(n_add_thetas > 0){

    if(missing(init)){
      init <- rep("0.0001", n_add_thetas)
      if(state == 3 | state == "power") {
        init <- rep(0.8, n_add_thetas)
      }
    }

    if(missing(lower)){
      lower <- rep(-1, n_add_thetas)
    }

    if(missing(upper)){
      upper <- rep(5, n_add_thetas)
    }


    if(any(lower > init)) stop("lower bound > initial estimate")
    if(any(upper < init)) stop("upper bound < initial estimate")

    if(n_add_thetas == 1) {
      theta_lines <- paste0("$THETA  (",lower,",",init,",",upper,") ; ",tvparam, cov, state)
    } else {
      theta_lines <- paste0("$THETA  (",lower,",",init,",",upper,") ; ",tvparam, cov, state,"_",seq_len(n_add_thetas))
    }
    ctl$THETA <- c(ctl$THETA,theta_lines)
  }

  m <- m %>% ctl_contents(ctl)

}

#' @export
add_cov.nm_list <- Vectorize_nm_list(add_cov.nm_generic, SIMPLIFY = FALSE)

#' @export
remove_cov.nm_generic <- function(ctl, param, cov, state = 2, continuous = TRUE,
                                  time_varying, additional_state_text, id_var = "ID"){

  m <- ctl
  ctl <- ctl_contents(m)
  param <- as.character(param)
  cov <- as.character(cov)
  state <- as.character(state)
  continuous <- as.logical(continuous)

  if("PK" %in% names(ctl)) dol_PK <- "PK" else dol_PK <- "PRED"

  PK_section <- rem_comment(ctl[[dol_PK]])

  data <- suppressMessages(input_data(m, filter = TRUE))

  if(any(is.na(data[[cov]]))) warning("missing values in ",cov," detected")

  if(missing(time_varying)){
    max_levels <- max(tapply(data[[cov]], data[[id_var]], function(x) length(unique(x))), na.rm = TRUE)
    if(max_levels > 1) time_varying <- TRUE else time_varying <- FALSE
  }

  if(time_varying){
    tvparam <- param
  } else {
    tvparam <- paste0("TV",param)
  }

  existing_param_rel <- which(grepl(paste0("\\b",tvparam,"COV"), PK_section))
  existing_param_cov_rel <- which(grepl(paste0("\\b",tvparam,cov), PK_section))

  ## remove parm vs specific cov code
  match_start <- grep(paste0(";;; ",tvparam,cov,"-DEFINITION START"),ctl[[dol_PK]])
  match_end <- grep(paste0(";;; ",tvparam,cov,"-DEFINITION END"),ctl[[dol_PK]])
  if(length(match_start) == 0 | length(match_end) == 0){
    return(as_nm_generic(nm(NA)))
    #stop("can't find cov definition code - did you add with add_cov()?")
  }

  ctl_matched <- ctl[[dol_PK]][match_start:match_end]
  theta_match <- gregexpr("THETA\\([0-9]+\\)", ctl_matched)

  thetas <- lapply(seq_along(theta_match), function(i){
    matchi <- theta_match[[i]]
    ctl_matchedi <- ctl_matched[i]
    if(length(matchi) == 1)
      if(matchi %in% -1)
        return(NULL)
    sapply(seq_along(matchi), function(j){
      matchij <- matchi[j]
      len <- attr(matchi, "match.length")[j]
      return(substr(ctl_matchedi, matchij, matchij+len-1))
    })
  })
  thetas <- unlist(thetas)
  theta_n <- gsub("THETA\\(([0-9]+)\\)","\\1", thetas)
  theta_n <- sort(as.numeric(theta_n))
  reduce_thetas <- length(theta_n)
  if(reduce_thetas > 0){
    ctl_char <- ctl_character(ctl)

    next_theta <- max(theta_n)+1
    keep_going <- TRUE
    while(keep_going){
      next_text_to_match <- paste0("THETA\\(",next_theta,"\\)")
      next_text_to_replace <- paste0("THETA\\(",next_theta-reduce_thetas,"\\)")
      if(!any(grepl(next_text_to_match, ctl_char))){
        keep_going <- FALSE
      } else {
        ctl_char <- gsub(next_text_to_match, next_text_to_replace, ctl_char)
        next_theta <- next_theta + 1
      }
    }
    ctl <- ctl_list(ctl_char)
  }

  ctl[[dol_PK]] <- ctl[[dol_PK]][setdiff(seq_along(ctl[[dol_PK]]), match_start:match_end)]

  ## adjust/remove parm vs any cov code

  match_start <- grep(paste0(";;; ",tvparam,"-RELATION START"),ctl[[dol_PK]])
  match_end <- grep(paste0(";;; ",tvparam,"-RELATION END"),ctl[[dol_PK]])
  if(length(match_start) == 0 | length(match_end) == 0)
    stop("can't find cov relation code - did you add with add_cov()?")

  rel_section <- ctl[[dol_PK]][match_start:match_end]

  rel_index_all <- grep(paste0(tvparam,"COV=",tvparam,cov), rel_section)
  unique_rel_match <- grep(paste0(tvparam,"COV=",tvparam,cov,"$"), rel_section)
  if(length(unique_rel_match) > 1)
    stop("can't identify unique cov relation code line- did you add with add_cov()?")
  if(length(unique_rel_match) == 1){ ## only covariate on this param
    ctl[[dol_PK]] <- ctl[[dol_PK]][setdiff(seq_along(ctl[[dol_PK]]), match_start:match_end)]
    match_param_rel <- grep(paste0("\\b",tvparam, "COV\\b"), rem_comment(ctl[[dol_PK]]))
    if(length(match_param_rel) != 1)
      stop("can't identify parameter modification line- did you add with add_cov()?")
    ctl[[dol_PK]] <- ctl[[dol_PK]][setdiff(seq_along(ctl[[dol_PK]]), match_param_rel)]
  }
  if(length(unique_rel_match) == 0){ ## (maybe - test this) other covariates
    if(length(rel_index_all) > 1)
      stop("can't identify unique cov relation code line- did you add with add_cov()?")

    text_match_at_start <- paste0("^(",tvparam,"COV=)",tvparam,cov,"\\*(.+)")
    match_at_start <- grep(text_match_at_start,ctl[[dol_PK]])

    text_match_after_start <- paste0("^(",tvparam,"COV=.+)\\*",tvparam,cov,"(.*)")
    match_after_start <- grep(text_match_after_start,ctl[[dol_PK]])

    if(length(match_at_start) + length(match_after_start) != 1)
      stop("couldn't identify cov relation in relation code line- did you add with add_cov()?")

    if(length(match_at_start)){
      ctl[[dol_PK]] <- gsub(text_match_at_start,"\\1\\2",ctl[[dol_PK]])
    }

    if(length(match_after_start)){
      ctl[[dol_PK]] <- gsub(text_match_after_start,"\\1\\2",ctl[[dol_PK]])
    }

  }

  matched_theta <- grep(paste0("\\$THETA\\s.*;.*",tvparam, cov), ctl$THETA)
  if(length(matched_theta) == 0)
    stop("can't find $THETA entry to remove- did you add with add_cov()?")

  ctl$THETA <- ctl$THETA[setdiff(seq_along(ctl$THETA), matched_theta)]

  m <- m %>% ctl_contents(ctl)
}

#' @export
remove_cov.nm_list <- Vectorize_nm_list(remove_cov.nm_generic, SIMPLIFY = FALSE)


#' produce dataset for covariate forest plotting
#'
#' @param m nm object
#' @param covariate_scenarios data.frame. need names "cov", "value" and (optional) "text"
#' 
#' @examples 
#' \dontrun{
#' 
#' dcov <- get_data(m1, filter = TRUE)
#' dcov <- dcov[!duplicated(dcov$NMSEQSID), ]
#' covariate_scenarios <- bind_rows(
#'   tibble(cov = "HEALTHGP", value = c(0, 1)),
#'   tibble(cov = "HEPATIC", value = unique(dcov$HEPATIC[dcov$HEPATIC > -99])),
#'   tibble(cov = "BWTIMP", value = c(50, 80, 120)),
#'   tibble(cov = "ECOG", value = c(0,1,2,3)),
#'   tibble(cov = "BEGFRIMP", value = quantile(dcov$BEGFR[dcov$BEGFR > -99])),
#'   tibble(cov = "RACE", value = c(1,2),text=c("white","black")),
#'   tibble(cov = "PPI", value = c(0,1)),
#'   tibble(cov = "H2RA", value = c(0,1))
#' )
#' 
#' dplot <- cov_forest_data(m1, covariate_scenarios = covariate_scenarios)
#' cov_forest_plot(dplot)
#' 
#' }
#' 
#' @export
cov_forest_data <- function(m, covariate_scenarios){
  if(!is_finished(m)) wait_finish(m)
  # d <- nm_output(m) ## read in combined output

  ## assume m is nm_list
  m <- as_nm_generic(m)

  ###############
  ## include plotting code here
  dpar <- coef(m, trans = FALSE)
  dpar$name <- coef(m)$parameter

  dpar$SE[is.na(dpar$SE)] <- 0

  dpar$lower <- dpar$FINAL - 1.96*dpar$SE
  dpar$upper <- dpar$FINAL + 1.96*dpar$SE

  PK_text <- ctl_contents(m)$PK
  PK_text_R <- nonmem_code_to_r(PK_text)

  par_covs <- PK_text[grepl(";;; .*-DEFINITION START", PK_text)]
  par_covs <- gsub(";;; (.*)-.*", "\\1", par_covs)

  pars <- PK_text[grepl(";;; .*-RELATION START", PK_text)]
  pars <- gsub(";;; (.*)-.*", "\\1", pars)

  dd <- input_data(m, filter = TRUE)

  ## to be used later in evaluation of R expressions
  dpar_mid <- dpar$FINAL
  names(dpar_mid) <- dpar$parameter

  dpar_low <- dpar$lower
  names(dpar_low) <- dpar$parameter

  dpar_upp <- dpar$upper
  names(dpar_upp) <- dpar$parameter

  dd[is.na(dd)] <- 0
  dd_first <- dd[1,]
  
  dpar_df <- as.data.frame(as.list(dpar_mid))
  dd_first <- cbind(dd_first, dpar_df)
  d <- lapply(seq_along(par_covs), function(i){

    par_cov <- par_covs[i]
    
    potential_pars <- sapply(pars, function(par) grepl(par, par_cov))
    potential_pars <- pars[potential_pars]
    
    matches <- unlist(lapply(potential_pars, function(potential_par){
      grep(paste0(potential_par, "COV = .*", par_cov), PK_text_R)
    }))
    
    if(length(matches) != 1) stop("can't get param value for ", par_cov, call. = FALSE)
    
    PK_matched_row <- PK_text_R[matches]
    par <- gsub("^(.*)COV .*$", "\\1", PK_matched_row)
    
    #par <- sapply(pars, function(par) grepl(paste0("^", par), par_cov))
    #par <- pars[par]
    #if(length(par) != 1) stop("can't get param value for ", par_cov, call. = FALSE)
    
    cov <- gsub(paste0(par,"(.*)"), "\\1", par_cov)
    
    if(!any(grepl(paste0(par_cov, "\\s*\\="), PK_text_R)))
      stop("can't find TVPARCOV= rows")
    
    # ## use code before TVPARCOV lines to compute derived covariates
    # prior_indicies <- seq_len(min(grep(paste0(par_cov, "\\s*\\="), PK_text_R))-1)
    # prior_code <- PK_text_R[prior_indicies]
    # prior_code <- c(prior_code, cov)
    # prior_exprs <- parse(text = prior_code)
    
    
    #theta_lines <- PK_text_R[seq_len(max(grep(paste0(par_cov, "\\s*\\="), PK_text_R)))]
    theta_lines <- PK_text_R[grepl(paste0(par_cov, "\\s*\\="), PK_text_R)]
    theta_lines <- c(theta_lines, par_cov)
    exprs <- parse(text = theta_lines)
    
    
    ## redefine data covariates with 
    ##browser()
    dcov_sc <- covariate_scenarios[covariate_scenarios$cov %in% cov, ]
    if(nrow(dcov_sc) == 0)
      stop("couldn't find covariate ", cov, " in covariate scenarios")
    
    dcov_sc_simple <- tibble::tibble(dcov_sc$value)
    names(dcov_sc_simple) <- cov
    
    dd_first[[cov]] <- NULL
    dd_first <- merge(dd_first, dcov_sc_simple)
    
    #dd_first[[cov]] <- with(dd_first, eval(prior_exprs))
    
    # cov_col <- sapply(seq_len(nrow(dd)), function(j) {
    #   with(dd[j,], eval(prior_exprs))
    # })     
    
    #dd[[cov]] <- cov_col
    #browser()
    #cov_col <- dd[[cov]]
    #cov_col[is.na(cov_col)] <- 0 #stats::na.omit(cov_col)
    
    # categorical <- TRUE
    # if(length(unique(dd[[cov]])) > 10) categorical <- FALSE  ## too many levels = FALSE
    # 
    # if(!all(stats::na.omit(floor(dd[[cov]]) == dd[[cov]]))) categorical <- FALSE  ## not round = FALSE
    
    # print(cov)
    # if(cov=="BWTIMP") browser()
    ### Need to change it to generate quantile based on original BWT
    # without accounting the imputed BWT to the median value
    
    # if(categorical) {
    #   levs <- unique(cov_col) 
    #   lev_text <- paste0(cov,"_",levs)
    # } else {
    #   levs <- stats::quantile(cov_col, probs = c(0.05, 0.5, 0.95))
    #   levs <- signif(levs, 2)
    #   lev_text <- paste0(cov,"_",c("low5","mid","upp95"),"_",levs)
    # }
    
    levs <- dd_first[[cov]]
    if(!"text" %in% names(covariate_scenarios)){
      lev_text <- paste0(cov, "_", levs) 
    } else {
      if(is.na(dcov_sc$text)) lev_text <- paste0(cov, "_", levs) else
        lev_text <- dcov_sc$text
    }

    d <- tibble::tibble(par, cov, levs, lev_text)

    d$mask_mid <- lapply(levs, function(lev){
      d <- tibble::tibble(lev)
      names(d) <- cov
      cbind(d, as.data.frame(as.list(dpar_mid)))
    })

    d$mask_low <- lapply(levs, function(lev){
      d <- tibble::tibble(lev)
      names(d) <- cov
      cbind(d, as.data.frame(as.list(dpar_low)))
    })

    d$mask_upp <- lapply(levs, function(lev){
      d <- tibble::tibble(lev)
      names(d) <- cov
      cbind(d, as.data.frame(as.list(dpar_upp)))
    })

    d$mid <- sapply(seq_along(levs), function(i){
      with(d$mask_mid[[i]], eval(exprs))
    })

    d$low <- sapply(seq_along(levs), function(i){
      with(d$mask_low[[i]], eval(exprs))
    })

    d$upp <- sapply(seq_along(levs), function(i){
      with(d$mask_upp[[i]], eval(exprs))
    })

    return(d)

  })

  d <- dplyr::bind_rows(d)
  d
  
}

#' plotting covariate forest plots
#'
#' @param d data.frame from cov_forest_data
#' @export

cov_forest_plot <- function(d){
  requireNamespace("ggplot2")
  
  ggplot2::ggplot(d, ggplot2::aes_string(x = "mid", y = "lev_text")) + ggplot2::theme_bw() +
    ggplot2::geom_rect(ggplot2::aes(ymin = -Inf, ymax = Inf, xmin = 1-0.2, xmax = 1+0.2), 
                       colour = "grey100") +
    ggplot2::geom_point() +
    ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "low", xmax = "upp"), height = 0.1) + 
    ggplot2::geom_vline(xintercept = 1, color='black', linetype='dashed') +
    ggplot2::facet_grid(par~., scales = "free_y", space = "free") +
    ggplot2::scale_y_discrete("") +
    ggplot2::scale_x_continuous("effect size \n (bars: 95% CI)", breaks = seq(floor(min(d$low)), ceiling(max(d$upp)), 0.1))
}



#' @export
append_nonmem_var <- function(output_table, r, var){
  r <- as_nm_generic(r)
  do <- output_table#(r)

  dc <- coef(r, trans = FALSE)

  ctl <- ctl_contents(r)

  pk_dollar <- ifelse("PK" %in% names(ctl), "PK", "PRED")

  pk_block <- ctl[[pk_dollar]]

  pk_block <- nonmem_code_to_r(pk_block)

  pk_block_param <- parse(text = c(pk_block, var))

  wide_coef <- dc %>%
    dplyr::select(.data$parameter, .data$FINAL) %>%
    tidyr::spread(key = "parameter", value = "FINAL")

  #dos <- do[!duplicated(paste(do$ID, do[[cov]])), ]

  varcol <- try(with(wide_coef,
                     sapply(1:nrow(do), function(i){
                       with(do[i,], eval(pk_block_param))})), silent = TRUE)

  if(!inherits(varcol, "try-error")) do[[var]] <- varcol

  do
}

#' @export
param_cov_diag <- function(r, param, cov, ..., categorical = FALSE, plot_tv = TRUE){

  requireNamespace("ggplot2")
  r <- as_nm_generic(r)

  tvparam <- paste0("TV",param)
  do <- output_table(r)

  ## want scatter of posthoc values
  ## + pred line from formula

  ## want pred values on there too

  dc <- coef(r, trans = FALSE)

  ctl <- ctl_contents(r)

  pk_dollar <- ifelse("PK" %in% names(ctl), "PK", "PRED")

  pk_block <- ctl[[pk_dollar]]

  pk_block <- nonmem_code_to_r(pk_block)

  pk_block_param <- parse(text = c(pk_block, param))
  pk_block_tvparam <- parse(text = c(pk_block,tvparam))

  wide_coef <- dc %>% dplyr::select(.data$parameter, .data$FINAL) %>%
    tidyr::spread(key = "parameter", value = "FINAL")

  dos <- do[!duplicated(paste(do$ID, do[[cov]])), ]

  parcol <- try(with(wide_coef,
                     sapply(1:nrow(dos), function(i){
                       with(dos[i,], eval(pk_block_param))})), silent = TRUE)

  if(!inherits(parcol, "try-error")) dos[[param]] <- parcol

  tvcol <- try(with(wide_coef,
                    sapply(1:nrow(dos), function(i){
                      with(dos[i,], eval(pk_block_tvparam))})), silent = TRUE)

  if(!inherits(tvcol, "try-error")) dos[[tvparam]] <- tvcol else
    plot_tv <- FALSE

  if(categorical) dos[[cov]] <- factor(dos[[cov]])

  dos <- substitute(dos %>% dplyr::mutate(...)) %>% eval

  p <- ggplot2::ggplot(dos, ggplot2::aes_string(x = cov, y = param)) + ggplot2::theme_bw()
  if(!categorical){
    p <- p + ggplot2::geom_point()
    if(plot_tv) p <- p + ggplot2::geom_smooth(ggplot2::aes_string(y = tvparam), colour = "red", se = FALSE)
  } else {
    p <- p + ggplot2::geom_boxplot()
    p <- p + ggplot2::coord_flip()
    if(plot_tv) p <- p + ggplot2::geom_point(ggplot2::aes_string(y = tvparam), colour = "red", size = 2)
  }

  #if(!missing(facet)) p <- p + facet_wrap(facet)
  p

}


#' @export
convert_to_simulation <- function(m, seed = 12345, subpr = 1){
  UseMethod("convert_to_simulation")
}
#' @export
convert_to_simulation.nm_generic <- function(m, seed = 12345, subpr = 1){
  ## comment out $EST
  old_target <- target(m)
  m <- m %>% target("$EST") %>% comment_out() %>% untarget()
  m <- m %>% target("$COV") %>% comment_out() %>% untarget()
  if(any(grepl("\\$SIM", ctl_contents(m)))){  ## if there is a $SIM
    m <- m %>% uncomment(pattern = "\\$SIM")
  } else {  ## if there is NOT a $SIM
    m <- m %>% insert_dollar(dollar = "SIM", "$SIM (1234) ONLYSIM SUBPR=1", after_dollar = "SIGMA")
  }
  m <- m %>% gsub_ctl("(^.*\\$SIM.*)\\([0-9]+\\)(.*$)", paste0("\\1(", seed ,")\\2"))
  m <- m %>% gsub_ctl("(N?SUBPR.*\\=\\s*)[0-9]+", paste0("\\1", subpr))
  m <- m %>% target(old_target)
  m
}
#' @export
convert_to_simulation.nm_list <- Vectorize_nm_list(convert_to_simulation.nm_generic, SIMPLIFY = FALSE)

## TODO: get Renvironment_info to skip unparseable scripts

#' @export
ppc_whisker_plot <- function(d, group, var1, var2, statistic = "statistic"){
  requireNamespace("ggplot2")
  requireNamespace("rlang")

  do_facet_wrap <- xor(!missing(var1), !missing(var2))
  do_facet_grid <- !missing(var1) & !missing(var2)
  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)
  group <- rlang::enquo(group)
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y = statistic)) + ggplot2::aes(x = !!group) + ggplot2::theme_bw() +
    ggplot2::stat_summary(fun.ymin = function(x) stats::quantile(x, 0.025, na.rm = TRUE),
                 fun.ymax = function(x) stats::quantile(x, 0.975, na.rm = TRUE),
                 geom = "errorbar") +
    ggplot2::geom_point(ggplot2::aes_string(y = paste0(statistic, "_true")), colour = "red")

  if(do_facet_wrap) p <- p + ggplot2::facet_wrap(dplyr::vars(!!var1), scales = "free")
  if(do_facet_grid) p <- p + ggplot2::facet_grid(dplyr::vars(!!var1), dplyr::vars(!!var2), scales = "free")

  p
}

#' @export
ppc_histogram_plot <- function(d, var1, var2, statistic = "statistic"){
  requireNamespace("ggplot2")
  requireNamespace("rlang")

  do_facet_wrap <- xor(!missing(var1), !missing(var2))
  do_facet_grid <- !missing(var1) & !missing(var2)
  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)

  p <- ggplot2::ggplot(d, ggplot2::aes_string(x = statistic)) + ggplot2::theme_bw() +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes_string(xintercept = paste0(statistic, "_true")), colour = "red")

  if(do_facet_wrap) p <- p + ggplot2::facet_wrap(dplyr::vars(!!var1), scales = "free")
  if(do_facet_grid) p <- p + ggplot2::facet_grid(dplyr::vars(!!var1), dplyr::vars(!!var2), scales = "free")

  p

}

#' @export
ppc_plots <- function(r, .f, ..., statistic = "statistic"){
  dppc <- r %>% ppc_data(.f, statistic = statistic)
  p1 <- dppc %>% ppc_whisker_plot(..., statistic = statistic)
  p2 <- dppc %>% ppc_histogram_plot(..., statistic = statistic)
  list(p1, p2)
}

#' @export
#cov_cov_plot <- function(d,
#                         cov1, cov2,
#                         continuous1, continuous2,
#                         log_transform_plot1 = FALSE, log_transform_plot2 = FALSE,
#                         dcov_info,
#                         by = "ID"){
cov_cov_plot <- function(d,
                         cov,
                         continuous,
                         log_transform_plot = rep(FALSE, length(cov)),
                         dcov_info,
                         by = "ID"){

  cov1 <- cov[1]
  cov2 <- cov[2]

  if(!missing(dcov_info)){
    dcov <- rbind(
      dcov_info[dcov_info$cov %in% cov1, ],
      dcov_info[dcov_info$cov %in% cov2, ]
    )
  } else {
    continuous1 <- continuous[1]
    continuous2 <- continuous[2]
    log_transform_plot1 <- log_transform_plot[1]
    log_transform_plot2 <- log_transform_plot[2]
    dcov <- tibble::tibble(cov = c(cov1, cov2),
                           continuous = c(continuous1, continuous2),
                           log_transform_plot = c(log_transform_plot1, log_transform_plot2))
  }

  important_row_contents <-
    do.call(paste, d[, c(by, dcov$cov)])

  dplot <- d[!duplicated(important_row_contents), ]

  #dplot <- d[, c(by, dcov$cov)] %>% unique()

  if(max(table(dplot[[by]])) > 1)
    warning("time varying cov detected, taking first only")

  dplot <- dplot[!duplicated(dplot[[by]]), ]

  if(all(dcov$continuous)){
    p <- ggplot2::ggplot(dplot, ggplot2::aes_string(x = cov1, y = cov2))
    p <- p + ggplot2::theme_bw()
    p <- p + ggplot2::geom_point()
    p <- p + ggplot2::geom_smooth(method = "lm")

    if(dcov$log_transform_plot[dcov$cov %in% cov1])
      p <- p + ggplot2::scale_x_log10()

    if(dcov$log_transform_plot[dcov$cov %in% cov2])
      p <- p + ggplot2::scale_y_log10()
  }

  if(xor(dcov$continuous[1], dcov$continuous[2])){
    cov1 <- dcov$cov[!dcov$continuous]
    cov2 <- dcov$cov[dcov$continuous]
    dplot[[cov1]] <- as.factor(dplot[[cov1]])
    p <- ggplot2::ggplot(dplot, ggplot2::aes_string(x = cov1, y = cov2))
    p <- p + ggplot2::theme_bw()
    p <- p + ggplot2::geom_boxplot()

    if(dcov$log_transform_plot[dcov$cov %in% cov2])
      p <- p + ggplot2::scale_y_log10()
  }

  if(all(!dcov$continuous)){
    p <- ggplot2::ggplot(dplot, ggplot2::aes_string(x = cov1))
    p <- p + ggplot2::theme_bw()
    p <- p + ggplot2::geom_bar()
    p <- p + ggplot2::facet_wrap(cov2)
  }

  p

}


#' Write derived data file.
#'
#' @param d data.frame. Data frame to be saved
#' @param name name of file (without extension)
#' @param ...  additional arguments to be passed dto write.csv
#' @export

write_derived_data <- function(d, name, ...){

  name <- tools::file_path_sans_ext(name)

  RDS_name <- file.path("DerivedData",paste0(name,".RDS"))
  csv_name <- file.path("DerivedData",paste0(name,".csv"))

  d <- as.data.frame(d)
  if(!inherits(d, "data.frame")) stop("d needs to be a data.frame or coercible into one")

  saveRDS(d, file = RDS_name)
  write.csv.nm(d, file = csv_name, ...)

  message("written: ")
  message(RDS_name)
  message(csv_name)
}

#' Read derived data
#'
#' @param name name of file (without extension)
#' @param na character to be passed to read.csv
#' @param silent logical (default = TRUE). should messages be suppressed
#' @param ...  additional arguments to be passed to read.csv
#' @export

read_derived_data <- function(name, na = ".", silent = FALSE, ...){

  ## TODO: expand to other types of argument
  if(length(name) != 1) stop("name should have length 1", call. = FALSE)

  load_file <- NA

  if(file.exists(name)){
    if(grepl("\\.RDS", name)) load_file <- "RDS" else
      if(grepl("\\.csv", name)) load_file <- "csv" else
        stop("file is not RDS or csv")
  } else { ## file doesn't exist
    orig_name <- tools::file_path_sans_ext(name)
    name <- file.path("DerivedData", paste0(orig_name,".RDS"))
    if(file.exists(name)) load_file <- "RDS" else {
      name <- file.path("DerivedData", paste0(orig_name,".csv"))
      if(file.exists(name)) load_file <- "csv" else
        stop("file is not RDS or csv")
    }
  }

  ## load_file should be set now
  if(is.na(load_file)) stop("debug") ## unneccesary with stop()s

  if(identical(load_file, "RDS")){
    if(!silent) message("loading: ", name)
    d <- readRDS(file = name)
  }
  if(identical(load_file, "csv")){
    if(!silent) message("loading: ", name)
    d <- utils::read.csv(name, na = na, ...)
  }
  return(d)
}





if(0){

  ## specify project
  proj_name <- "/projects/qcp/QCP_MODELING/ONC/azd6094/poppk_20190326_tatton_interim3/"
  ## find files to copy
  found_files <- dir(proj_name, all.files = TRUE, recursive = TRUE, full.names = TRUE)

  ## stage unmodified files
  stage_info <- stage(found_files)
  ## import from staging area
  import(stage_info)

  ## need to import


  found_files %>% stage %>% import




  itheta <- init_theta(m1)

  ## modify one parameter
  itheta$init[itheta$name == "KA"] <- 3
  itheta$FIX[itheta$name == "KA"] <- TRUE

  itheta <- itheta %>% insert_theta(theta_number = 2,
                                    init = 0.2,
                                    unit = "u",
                                    trans = "LOG",
                                    name = "CL")

  m1 %>% init_theta(itheta) %>% dollar("THETA")

  iomega <- init_omega(m1)
  iomega$init[iomega$name %in% "IIV_KA"] <- 0.2

  iomega <- iomega %>% insert_omega(omega_number = 2,
                                    name = "IIV_sdf",
                                    unit = "mg/ml",
                                    trans = "LOG",
                                    lower = 0)
  iomega <- iomega %>% block(eta_numbers = c(2,3))
  iomega <- iomega %>% unblock(eta_numbers = c(2,3))
  iomega <- iomega %>% block(eta_numbers = c(3,4))
  iomega <- iomega %>% unblock(eta_numbers = c(3,4))
  stop()
  iomega <- iomega %>% block(eta_numbers = c(1,2))  ##error
  iomega <- iomega %>% unblock(eta_numbers = c(1,2))
  iomega <- iomega %>% block(eta_numbers = c(2,3))

  m1 %>% init_omega(iomega) %>% dollar("OMEGA")

  m1 <- m1 %>% init_omega(iomega)

}

#' @export
make_OCC_every_dose <- function(dose_trigger, new_OCC_trigger){
  # Rule for when new occasion is happening
  # whenever we have a dose, if there is a sample after it and before next dose, that dose is considered a new OCC

  ## TODO: walk the ast of new_OCC_trigger
  ## pull out variables, evaluate them to create a mini d


  new_OCC_trigger <- rlang::enquo(new_OCC_trigger)
  id_group <- rlang::enquo(id_group)
  dose_trigger <- rlang::enquo(dose_trigger)

  d <- d %>% dplyr::group_by(!!id_group) %>%
    dplyr::mutate(DPERIOD = cumsum(!!dose_trigger)) %>%
    dplyr::group_by(!!id_group, .data$DPERIOD) %>%
    dplyr::mutate(new_OCC = !!new_OCC_trigger)

  ## select temporarly unique DPERIOD and HAS PK SAMPLE for each ID
  tmp <- d %>%
    dplyr::ungroup() %>%
    dplyr::distinct(!!id_group, .data$DPERIOD,.data$new_OCC)

  tmp <- tmp %>%
    dplyr::group(!!id_group) %>%
    dplyr::mutate(OCC = cumsum(.data$new_OCC))

  d$ROW <- seq_len(nrow(d))
  d <- merge(d,tmp)
  d <- d[order(d$ROW), ]

  ## normalise to start at 1
  d$OCC <- d$OCC - min(d$OCC) + 1

  d$OCC
}

#' Shiny view of NMproject
#'
#' @param m either nm_list object, or data.frame or list contain nm_lists
#' @param envir if missing, the environment to search
#' @examples
#' \dontrun{
#'
#' m1 %>% nm_shiny()
#' d$m %>% nm_shiny()
#' d %>% nm_shiny()
#'
#' }
#' @export
shiny_nm <- function(m, envir = .GlobalEnv){
  if(missing(m)) {
    m <- get_nm_lists(envir = envir)
  } else {
    if(is_nm_generic(m)) m <- as_nm_list(m)
    if(!is_nm_list(m)){
      m <- get_nm_lists(envir = m)
      if(!is_nm_list(m))
        stop("couldn't find any nm_list objects in m")
    }
  }
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
  .sso_env$.m <- m  # see zzz.R for .sso_env
  on.exit({
    .sso_env$.currentwd <- NULL
    .sso_env$.m <- NULL
  }, add = TRUE)
  shiny::runApp(shiny_dir,launch.browser = TRUE)
}

get_nm_lists <- function(envir = .GlobalEnv){

  m <- lapply(envir, function(object){
    if(inherits(object, "nm_list")) object else NA
  })

  m <- m[!is.na(m)]
  m <- do.call(c, m)
  m

}

## generic already defined
## internal function
data_ignore_char.nm_generic <- function(r, data){
  dol_data <- r %>% dollar("$DATA")
  dol_data <- dol_data[!dol_data %in% ""]
  dol_data <- rem_comment(dol_data)
  dol_data <- unlist(strsplit(dol_data, split = "\\s"))
  
  ignore_present <- any(grepl(".*IGNORE\\s*=\\s*\\(",dol_data))
  accept_present <- any(grepl(".*ACCEPT\\s*=\\s*\\(",dol_data))
  
  type <- NA
  if(ignore_present & accept_present) stop("cannot identify ignore columns")
  if(ignore_present) type <- "IGNORE"
  if(accept_present) type <- "ACCEPT"
  no_filter <- is.na(type)
  
  if(missing(data)) data <- input_data(r, filter = FALSE, silent = TRUE)
  
  r_data_names <- names(data)
  ## now get nonmem names
  dollar_input <- r %>% dollar("INPUT")
  nonmem_data_names <- gsub("\\$\\w+", "", dollar_input)
  nonmem_data_names <- unlist(strsplit(nonmem_data_names, split = "\\s"))
  nonmem_data_names <- nonmem_data_names[!nonmem_data_names %in% ""]
  nonmem_data_names <- gsub("\\w+=(\\w+)", "\\1", nonmem_data_names)
  #if(length(r_data_names) != length(nonmem_data_names))
  #  stop("length of items in $INPUT doesn't match dataset")
  name_chart <- data.frame(r_data_names, nonmem_data_names, stringsAsFactors = FALSE)
  name_chart <- name_chart[name_chart$r_data_names != name_chart$nonmem_data_names,]
  
  if(!no_filter){
    filter_statements <- paste0(".*",type,"\\s*=\\s*\\((\\S[^\\)]+)\\)*.*")
    dol_data <- dol_data[grepl(filter_statements, dol_data)]
    filter_statements <- gsub(filter_statements,"\\1",dol_data)
    filter_statements <- unlist(strsplit(filter_statements,","))
    filter_statements <- gsub("\\.EQ\\.","==",filter_statements)
    filter_statements <- gsub("\\.NE\\.","!=",filter_statements)
    filter_statements <- gsub("\\.EQN\\.","==",filter_statements)
    filter_statements <- gsub("\\.NEN\\.","!=",filter_statements)
    filter_statements <- gsub("\\./E\\.","!=",filter_statements)
    filter_statements <- gsub("\\.GT\\.",">",filter_statements)
    filter_statements <- gsub("\\.LT\\.","<",filter_statements)
    filter_statements <- gsub("\\.GE\\.",">=",filter_statements)
    filter_statements <- gsub("\\.LE\\.","<=",filter_statements)
    
    ## substitute names from 
    for(i in seq_len(nrow(name_chart))){
      nonmem_data_name <- paste0("\\b", name_chart$nonmem_data_names[i], "\\b")
      r_data_name <- name_chart$r_data_names[i]
      filter_statements <- gsub(nonmem_data_name,
                                r_data_name,
                                filter_statements)
    }
    
    filter_statements <- paste(filter_statements, collapse= " | ")
    if("ACCEPT" %in% type) filter_statements <- paste0("!(",filter_statements,")")
  } else {
    filter_statements <- "FALSE"
  }
  filter_statements
  
}
data_ignore_char.nm_list <- Vectorize_nm_list(data_ignore_char.nm_generic, SIMPLIFY = TRUE)


###############

## mrgsolve method extension

# setOldClass(c("nm_list"))
# setOldClass(c("nm_execute"))
#
# omat.nm_execute <- function(.x, ...){
#   omegas <- raw_init_omega(.x)
#   omegas <- omegas[!is.na(omegas$parameter), ]
#   by(omegas, omegas$mblock, function(d) as.numeric(d$value))
#   #omegas
# }
# omat.nm_list <- function(.x, ...){
#   lapply(.x, omat)
# }
#
# setMethod("omat", signature = c("nm_execute"),
#           definition = omat.nm_execute)
#
# setMethod("omat", signature = c("nm_list"),
#           definition = omat.nm_list)


################

