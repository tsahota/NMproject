#' @include utilsExtra.R

## internal function
nm_generic <- function(run_id = NA_character_,
                       run_in = nm_default_dir("models"),
                       parent_run_id = NA_character_,
                       parent_run_in = NA_character_,
                       parent_ctl_name = NA_character_,
                       parent_results_dir = NA_character_,
                       ctl_name = "run{run_id}.mod",
                       type = "execute",
                       run_dir = "{run_id}",
                       results_dir = nm_default_dir("results"),
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
  m[["parent_ctl_name"]] <- as.character(parent_ctl_name)
  m[["parent_results_dir"]] <- as.character(parent_results_dir)
  m[["job_info"]] <- NA_character_
  m$target <- NA_character_
  m$executed <- FALSE
  m$result_files <- c()
  m$ctl_contents <- NA_character_
  m$ctl_orig <- NA_character_
  m$data_path <- NA_character_
  m$cmd <- getOption("nm.cmd_default")
  m$cores <- as.integer(1)
  m$parafile <- NA_character_
  m$walltime <- NA_integer_
  
  unique_id <- "{type}.{run_in}{.Platform$file.sep}{run_dir}"
  ## the following is in order of glueing
  m$glue_fields <- list(run_dir, ctl_name, results_dir, unique_id, lst_path, NA_character_)
  names(m$glue_fields) <- c("run_dir", "ctl_name", "results_dir", "unique_id", "lst_path", "data_path")
  
  for(field in names(m$glue_fields)){
    m <- replace_tag(m, field)
  }
  
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
#' @param parent_ctl_name character vector (optional). Ctl name of previous run 
#' @param parent_results_dir character vector (optional). results_dir of previous run 
#' @param ctl_name character. Name of control file
#' @param type character (default = "execute").  Type of run to run
#' @param run_dir character (default = "{run_id}").  Subdirectory where PsN wll run NONMEM
#' @param results_dir character (default = "Results").
#'    Directory to store results of this run
#' @param lst_path character (default = "{run_dir}/NM_run1/psn.lst") expected location of lst file
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

#' create a new nm object
#' 
#' Wrapper function around nm()
#' @param based_on character. Prior ctl file to base run on
#' @param run_id character. Run identifier
#' @param data_path character. Path to dataset
#' @param cmd character. Psn command to use
#' @examples 
#' \dontrun{
#'m1 <- new_nm(based_on = "staging/Models/run1.mod",
#'             run_id = "m1",
#'             data_path = "DerivedData/data.csv",
#'             cmd = "execute -run_on_sge -sge_prepend_flags='-V' {ctl_name} -dir={run_dir}")
#'}
#' @export
new_nm <- function(based_on,
                   run_id = NA_character_, 
                   data_path, 
                   cmd){
  
  m <- nm(run_id = run_id)
  if(!missing(based_on)) m <- m %>% based_on(based_on)
  if(!missing(data_path)) m <- m %>% data_path(data_path)
  if(!missing(cmd)) m <- m %>% cmd(cmd)
  m
  
}

#' @export
is.na.nm_generic <- function(x) is.na(run_id(x))
#' @export
is.na.nm_list <- function(x) is.na(run_id(x))

#' Make child nm object from parent
#'
#' Child objects inherit attributes of parent but with a new run_id. The control
#' file will be inherited too with $TABLEs updated
#'
#' @param m parent nm object
#' @param run_id character.  New run id to assign to child object
#' @param type character (default = "execute"). type of child object
#' @param parent optional (default = nm(NA)) nm object. Parent object will by
#'   default be `m`, but this argument will force parent to be a different
#'   object
#' @param silent logical (default = FALSE). Should warn if conflicts detected
#'
#' @details Specifying `parent` will run the function `change_parent` to force
#' parent to be different from `m`.  This is useful in piping when a parent
#' object is modified prior to being used in the child object.
#'
#' @examples
#' \dontrun{
#'
#' m2 <- m1 %>% child("m2")
#'
#' ## use parent object to ensure child object retain 
#' ## correct parent-child structure
#' m2 <- m1 %>% c
#'   update_parameters() %>%  ## modifying parent object
#'   child("m2", parent = m1)
#'
#' }
#' @export
child <- function(m, run_id = NA_character_, type = "execute", parent = nm(NA), silent = FALSE){
  UseMethod("child")
}
#' @export
child.nm_generic <- function(m, run_id = NA_character_, type = "execute", parent = nm(NA), silent = FALSE){
  mparent <- m
  if(is.environment(m)){
    old_classes <- class(m)
    m <- as.environment(as.list(m, all.names=TRUE))
    class(m) <- old_classes 
  }
  
  m <- m %>% executed(FALSE)
  m <- m %>% job_info(NA_character_)
  m[["result_files"]] <- c()
  m <- m %>% parent_run_id(run_id(m))
  m <- m %>% parent_run_in(run_in(m))
  m <- m %>% parent_ctl_name(ctl_name(m))
  m <- m %>% parent_results_dir(results_dir(m))
  
  ## if missing increment run_id
  if(is.na(run_id)) {
    if(!is.na(run_id(m))){
      run_id <- increment_run_id(run_id(m))
      message("child run_id automatically set to: ", run_id)
    }
  }
  if(!is.na(run_id)) m <- m %>% run_id(run_id)
  
  m <- m %>% ctl_contents(ctl_contents(m), 
                          update_numbering = TRUE,
                          update_dollar_data = FALSE)
  if(!type %in% "execute") m <- m %>% type(type)
  m[["ctl_orig"]] <- m[["ctl_contents"]]  ## reset ctl_orig
  
  ## check for file conficts
  if(!is_single_na(m[["ctl_contents"]])){
    file_conflicts <- intersect(psn_exported_files(mparent), psn_exported_files(m))
    if(length(file_conflicts) > 0){
      if(!silent) warning("Child file(s) currently in conflict with parent:\n",
                          paste(paste0(" ",file_conflicts),collapse="\n"),
                          "\nYou will overwrite parent object outputs if you run now", call. = FALSE)
    }
  }
  
  ## warn if ctl_name or run_dir aren't glueable
  relevant_glue_fields <- unlist(m$glue_fields[c("ctl_name", "run_dir")])
  non_glued_glue_fields <- relevant_glue_fields[!grepl("\\{", relevant_glue_fields)]
  if(length(non_glued_glue_fields) > 0){
    if(!silent) warning("Following parents attributes do not use {glue} fields:\n", 
                        paste(paste0(" ",names(non_glued_glue_fields)), collapse="\n"), 
                        "\nThese fields will be identical to parent and may result in conflicts",
                        "\nIf this is unintended, make sure parent object uses {glue} notation for these attributes", call. = FALSE)
  }
  
  if(!is.na(parent)) m <- m %>% change_parent(parent)
  
  m
}
#' @export
child.nm_list <- Vectorize_nm_list(child.nm_generic, SIMPLIFY = FALSE)

increment_run_id <- function(run_id){
  if(length(run_id) != 1) stop("run_id must be of length 1")
  stop_msg <- paste0("cannot automatically increment ", run_id, ", specify run_id argument explicitly")
  
  # number at end
  run_id_regex <- "^(\\w*?)([0-9]+)$" 
  if(!grepl(run_id_regex, run_id)) stop(stop_msg)
  
  prefix_part <- gsub("^(\\w*?)([0-9]+)$", "\\1", run_id)
  numeric_part <- gsub("^(\\w*?)([0-9]+)$", "\\2", run_id)
  numeric_part <- as.numeric(numeric_part)
  
  paste0(prefix_part, numeric_part + 1)
}

#' Change parent object
#' 
#' Useful when create a \code{child()} of a modified run
#' 
#' @param m nm object
#' @param mparent new parent object to child
#' @param silent logical (default = TRUE). Should warn if conflicts detected
#' @export
change_parent <- function(m, mparent, silent = TRUE){
  UseMethod("change_parent")
}
#' @export
change_parent.nm_generic <- function(m, mparent, silent = TRUE){
  
  m <- m %>% parent_run_id(run_id(mparent))
  m <- m %>% parent_run_in(run_in(mparent))
  m <- m %>% parent_ctl_name(ctl_name(mparent))
  m <- m %>% parent_results_dir(results_dir(mparent))
  
  m[["ctl_orig"]] <- mparent[["ctl_contents"]]  ## reset ctl_orig
  
  ## check for file conficts
  if(!is_single_na(m[["ctl_contents"]])){
    file_conflicts <- intersect(psn_exported_files(mparent), psn_exported_files(m))
    if(length(file_conflicts) > 0){
      if(!silent) warning("new parent file(s) currently in conflict with parent:\n",
                          paste(paste0(" ",file_conflicts),collapse="\n"),
                          "\nYou will overwrite parent object outputs if you run now", call. = FALSE)
    }
  }
  
  m
}
#' @export
change_parent.nm_list <- Vectorize_nm_list(change_parent.nm_generic, SIMPLIFY = FALSE)

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

printable_nm_generic <- function(x){
  pretty_empty_fields <- c("ctl_contents", "data_path", "cmd")
  pretty_empty_fill_f <- c("based_on", "data_path", "cmd")
  
  ## included even if NA
  minimum_fields <- c("run_id", pretty_empty_fields)
  
  for(field in names(x)){
    if(!field %in% minimum_fields)
      if(is_single_na(x[[field]])) x[[field]] <- NULL
  }
  
  ## if ctl_contents is NA, remove ctl_name
  if(is_single_na(x[["ctl_contents"]])){
    x[["ctl_name"]] <- NULL
    x[["glue_fields"]][["ctl_name"]] <- NULL
  }
  
  for(j in seq_along(pretty_empty_fields)){
    pretty_empty_field <- pretty_empty_fields[j]
    pretty_empty_f <- pretty_empty_fill_f[j]
    if(is_single_na(x[[pretty_empty_field]])){
      x[[pretty_empty_field]] <- 
        paste0("...[NA - fill with ", pretty_empty_f, "()]...")
    }
  }
  
  collapse_fields <- c("ctl_contents", "ctl_orig")
  for(field in collapse_fields){
    ## special handling of these two    
    if(field %in% names(x)) 
      if(length(x[[field]]) > 1)
        x[[field]] <- paste0("...[collapsed - view with ", field, "()]...")
  }
  # remove all raw fields from output
  remove_fields <- c("glue_fields")
  for(field in remove_fields) x[[field]] <- NULL
  
  # ##put glue fields to end
  # xglue_list <- x[["glue_fields"]]
  # 
  # for(j in seq_along(pretty_empty_fields)){
  #   pretty_empty_field <- pretty_empty_fields[j]
  #   pretty_empty_f <- pretty_empty_fill_f[j]
  #   if(is_single_na(xglue_list[[pretty_empty_field]])){
  #     xglue_list[[pretty_empty_field]] <- 
  #       paste0("...[NA - fill with ", pretty_empty_f,"()]...")
  #   }
  # }
  # 
  # x[["glue_fields"]] <- NULL
  # x[["glue_fields"]] <- xglue_list 
  
  x
}

#' @export
print.nm_generic <- function(x, ...){
  
  x <- printable_nm_generic(x)
  
  str_ob <- utils::capture.output(utils::str(x, ...))
  str_ob <- gsub("(.*?)\"\\.{3}\\[(NA.*)\\].*", 
                 paste0("\\1",crayon::underline("\\2")), str_ob)
  str_ob <- gsub("(.*?)\"\\.{3}\\[(collapsed.*)\\].*", 
                 paste0("\\1",crayon::green("\\2")), str_ob)
  cat(str_ob, sep = "\n")
  #return(invisible(x))
  #utils::str(x, ...)
}

#' @export
print.nm_list <- function(x, ...){
  for(i in seq_along(x)) {
    x[[i]] <- printable_nm_generic(x[[i]])
  }
  
  str_ob <- utils::capture.output(utils::str(x, ...))
  ## post str modification
  str_ob <- gsub(":List of.*", "", str_ob)
  str_ob <- gsub("(.*?)\"\\.{3}\\[(NA.*)\\].*", 
                 paste0("\\1",crayon::underline("\\2")), str_ob)
  str_ob <- gsub("(.*?)\"\\.{3}\\[(collapsed.*)\\].*", 
                 paste0("\\1",crayon::green("\\2")), str_ob)
  cat(str_ob, sep = "\n")
  #return(invisible(x))
  #utils::str(x, ...)
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

set_simple_field <- function(m, ...){
  
  dots <- list(...)
  fields <- names(dots)
  
  for(i in seq_along(dots)){
    m <- m %>% custom_1d_field(field = fields[i], replace = dots[[i]])
  }
  m
}


get_simple_field <- function(m, field){
  
  field <- rlang::enquo(field)
  field <- rlang::quo_name(field)
  
  m %>% custom_1d_field(field = field)
  
}

#' get/set simple fields of nm objects
#' 
#' @param m nm object
#' @param ... arguments to get/set fields
#' 
#' @examples 
#' \dontrun{
#' 
#' core_list <- c(1,4,12)
#' 
#' mc <- m1 %>% child(run_id = paste0(corelist)) %>%
#'   simple_field(cores = corelist) %>%
#'   cmd("qpsn -c {cores} -t 59 -- execute {ctl_name} -dir={run_dir}")
#'   
#' mc %>% simple_field(cores)  ## retrieve field from object
#' 
#' mc <- mc %>% simple_field(stars = 3)
#' mc %>% simple_field(stars)
#' 
#' }
#' @export

simple_field <- function(m, ...){
  dots_exp <- rlang::enexprs(...)
  if(identical(names(dots_exp), "")) {
    get_simple_field(m, ...)
  } else {
    set_simple_field(m, ...) 
  }
}

glue_text_nm <- function(m, text){
  UseMethod("glue_text_nm")
}
glue_text_nm.nm_generic <- function(m, text){
  stringr::str_glue(text, .envir = m, .na = NULL)
}
glue_text_nm.nm_list <- Vectorize_nm_list(glue_text_nm.nm_generic, SIMPLIFY = TRUE)

replace_tag <- function(m, field){
  ## this function is rate limiting - use it as little as possible.
  ## only proceed if "raw" field exists
  if(!is.na(m$glue_fields[[field]])){
    ## start by resetting to raw
    m[[field]] <- glue_text_nm(m, m$glue_fields[[field]])
    #m[[field]] <- stringr::str_glue(m$glue_fields[[field]], .envir = m)
    m[[field]] <- as.character(m[[field]])
  }
  m
}

replace_tags <- function(m, field){
  ## this function is rate limiting - use it as little as possible.
  ## only proceed if "raw" field exists
  ## this will update all tags
  if(field %in% names(m$glue_fields)){
    m <- replace_tag(m, field)
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
  dollar <- toupper(dollar)
  
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
    m <- m %>% ctl_contents_simple(ctl)
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
text.nm_generic <- function(x, text, append = FALSE, after = character(), add_dollar_text = FALSE, ...){
  m <- x
  current_text <- get_target_text(m)
  if(missing(text)) return(current_text)
  
  text <- paste(text, collapse = "\n")
  text <- strsplit(text, split = "\n")[[1]]
  text <- trimws(text)
  
  if(length(after)) append <- TRUE
  if(append) {
    if(!length(after)) text <- c(current_text,text)
    
    if(length(after)){
      matches <- grep(after, current_text)
      if(!length(matches)) stop("cannot find 'after'")
      matches <- matches[1]
      text <- append(current_text, text, after = matches)
    }
    
  }
  
  
  if(is.na(target(m))) {
    #stop("not developed yet")
  } else {
    text <- setup_dollar(text, paste0("$",target(m)), add_dollar_text = add_dollar_text) 
  }
  m <- m %>% set_target_text(text)
  m
}
#' @export
text.nm_list <- Vectorize_nm_list(text.nm_generic, SIMPLIFY = FALSE, vectorize.args = c("x"))

#' Get/set path to dataset
#' 
#' Mainly used to associate a dataset with an nm object.
#' Requires ctl_contents to already be specified.
#' 
#' @param m nm object
#' @param text (optional) character. Path to input dataset
#' 
#' @return if text is not specified, will return the data_path name
#'  otherwise will set data_path to the text provided
#' 
#' @examples 
#' \dontrun{
#' 
#' ## The following assumes a ctl file exists in the staging area:
#' ##  staging/Models/ADVAN2.mod
#' ## and a dataset
#' ##  DerivedData/data.csv
#' 
#' m1 <- nm(run_id = "m1") %>%
#'       based_on("staging/Models/ADVAN2.mod") %>%
#'       data_path("DerivedData/data.csv")
#'       
#' data_path(m1)  ## display data name
#' 
#' }
#' 
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
    ## update ctl contents (if it exists)
    if(!is_single_na(ctl_contents(m)))
      m <- m %>% fill_dollar_data(text)
  }
  
  m
}
#' @export
data_path.nm_list <- Vectorize_nm_list(data_path.nm_generic)

fill_dollar_data <- function(m, data_name){
  old_target <- m %>% target()
  m <- m %>% target("$DATA")
  
  data_name <- relative_path(data_name, run_in(m))
  m <- m %>% gsub_ctl("^(\\s*\\$DATA\\s+)\\S+(.*)$",paste0("\\1",data_name,"\\2"))
  
  m <- m %>% target(old_target)
  m
}

#' fill $INPUT
#' 
#' Uses dataset to fill $INPUT in ctl_contents
#' 
#' @param m nm object
#' @param ... either keep, drop, rename
#' 
#' @examples 
#' \dontrun{
#' 
#'  m1 <- m1 %>% fill_input(rename = c("DAT0" = "DATE"))
#'  m1 %>% dollar("INPUT") ## view $INPUT
#' 
#'  m1 <- m1 %>% fill_input(drop = "RATE")
#'  m1 %>% dollar("INPUT")
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


#' Read input dataset
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

#' Get/set ignore statement from ctl_contents
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
  m <- m %>% ctl_contents_simple(ctl)
  m
}
#' @export
ignore.nm_list <- Vectorize_nm_list(ignore.nm_generic, replace_arg = "ignore_char")


#' Delete a $ subroutine from ctl_contents
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
  m <- m %>% ctl_contents_simple(ctl)
  m
}
#' @export
delete_dollar.nm_list <- Vectorize_nm_list(delete_dollar.nm_generic, SIMPLIFY = FALSE)

#' Insert a new subroutine into ctl_contents
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
  
  m <- m %>% ctl_contents_simple(ctl)
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
    raw_init_omega(m),
    raw_init_sigma(m)
  )
  p_info[!is.na(p_info$parameter),]
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
  #message("bug in updating IOV model parameters - unresolved")
  ctl_lines <- update_parameters0(ctl_lines, coef_from, type = "OMEGA")
  ctl_lines <- update_parameters0(ctl_lines, coef_from, type = "SIGMA")
  
  m <- m %>% ctl_contents_simple(ctl_lines)
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
  file.path(results_dir(m), name)
}
#' @export
result_file.nm_list <- Vectorize_nm_list(result_file.nm_generic)

cache_history <- function(r){
  UseMethod("cache_history")
}

cache_history.nm_generic <- function(r){
  lapply(run_cache_paths(r), readRDS)
}

cache_history.nm_list <- Vectorize_nm_list(cache_history.nm_generic, SIMPLIFY = FALSE)

cache_current <- function(m) run_checksums(m)

clear_cache <- function() unlink(".cache", recursive = TRUE)

#' get all temp files
#'
#' list all tempfiles (normally for deletion)
#'
#' @param object nm object or path to project (default = ".")
#' @param output_loc character either "run_dir" (default) for psn runs or "base" for nmfe runs
#' @param run_files optional character with NM_run* file paths
#' @param include_grid_files logical (default = TRUE) should slurm files be included
#' @param ctl_extension character. Extension of control file (default = "mod")
#' @param remove_psn_exports logical (default = FALSE). should psn exports be considered temporary
#'
#' @details
#' Having \code{remove_psn_exposure = TRUE} will break pirana and xpose capability
#'   as these software use exported files 
#'
#' @export
ls_tempfiles <- function(object = ".", output_loc = c("run_dir", "base"),
                         run_files = NA_character_, include_grid_files = TRUE,
                         ctl_extension = "mod",
                         remove_psn_exports = FALSE){
  
  UseMethod("ls_tempfiles")
  
}

#' @export
ls_tempfiles.default <- function(object = ".", output_loc = c("run_dir", "base"),
                                 run_files = NA_character_, include_grid_files = TRUE,
                                 ctl_extension = "mod",
                                 remove_psn_exports = FALSE){
  
  output_loc <- match.arg(output_loc)
  
  ## get all_run_files (in NM_run1 dir)
  if(identical(run_files, NA_character_)){
    all_run_dirs <- list_dirs(
      object, 
      pattern = "NM_run[0-9]+", 
      recursive = TRUE, full.names = TRUE, maxdepth = Inf
    )
    
    all_run_files <- dir(all_run_dirs, full.names = TRUE)
    
    all_outside_run_dirs <- file.path(all_run_dirs, "..")
    all_outside_run_files <- dir(all_outside_run_dirs, full.names = TRUE)
    
    all_run_files <- c(all_run_files, all_outside_run_files)
    
  } else {
    all_run_files <- run_files 
  }
  
  ## eliminate files we don't want
  
  temp_files <- c()
  non_temp_files <- c()
  
  all_psn.mod <- all_run_files[basename(all_run_files) == "psn.mod"]
  non_temp_files <- c(non_temp_files, all_psn.mod)
  
  all_run_dir_table_files <- 
    lapply(all_psn.mod, function(psn.mod){
      file.path(dirname(psn.mod), ctl_table_files(psn.mod))
    })
  all_run_dir_table_files <- unlist(all_run_dir_table_files)
  non_temp_files <- c(non_temp_files, all_run_dir_table_files)
  
  all_base_tables <- all_run_dir_table_files
  all_base_table_dir <- dirname(all_base_tables)
  all_base_table_dir <- file.path(all_base_table_dir, "..", "..")
  all_base_table_dir <- normalizePath(all_base_table_dir)
  
  if(output_loc == "run_dir"){ ## add base tables to temp files
    all_base_tables <- file.path(all_base_table_dir, basename(all_run_dir_table_files))
    all_base_tables <- all_base_tables[file.exists(all_base_tables)]
    if(remove_psn_exports) temp_files <- c(temp_files, all_base_tables)
    
    all_base_mod_files <- dir(unique(all_base_table_dir),
                              pattern = paste0("\\.", ctl_extension, "$"),
                              full.names = TRUE)
    
    all_base_stubs <- tools::file_path_sans_ext(all_base_mod_files)
    
    all_base_psn_files <- lapply(all_base_stubs, function(base_mod_stub){
      base_dir <- dirname(base_mod_stub)
      stub <- basename(base_mod_stub)
      dir(base_dir, pattern = paste0("^", stub, "\\..*"), full.names = TRUE)
    })
    all_base_psn_files <- unlist(all_base_psn_files)
    
    all_base_psn_files <- all_base_psn_files[
      ## exclude mod and lst files
      !tools::file_ext(all_base_psn_files) %in% c("mod", "lst")
    ]
    if(remove_psn_exports) temp_files <- c(temp_files, all_base_psn_files)
    
  }
  
  ## temp_dir is temp
  temp_files <- c(temp_files, all_run_files[grepl("temp_dir", all_run_files)])
  
  ## .o, .f90, Rmd, csv
  temp_files <- c(temp_files, all_run_files[tools::file_ext(basename(all_run_files)) %in% c("o", "f90", "Rmd", "csv")])
  
  ## specific name exclusions:
  temp_files <- c(temp_files, 
                  all_run_files[basename(all_run_files) %in% 
                                  c("INTER", 
                                    "fort.2002",
                                    "model_NMrun_translation.txt",
                                    "modelfit.log",
                                    "raw_results_structure",
                                    "version_and_option_info.txt")])
  
  #####
  temp_files <- setdiff(temp_files, non_temp_files)
  
  ## expand the directories into files
  
  temp_dirs <- temp_files[file.info(temp_files)$isdir]
  
  temp_dir_files <- dir(temp_dirs, full.names = TRUE, recursive = TRUE)
  
  temp_files <- c(temp_files[!temp_files %in% temp_dirs],
                  temp_dir_files)
  
  if(length(temp_files) == 0) return(character()) ## if none return empty
  
  relative_path(temp_files, getwd())
  
  
}

#' @export
ls_tempfiles.nm_list <- function(object = ".", output_loc = c("run_dir", "base"),
                                 run_files = NA_character_, include_grid_files = TRUE,
                                 ctl_extension = "mod",
                                 remove_psn_exports = FALSE){
  
  output_loc <- match.arg(output_loc)
  
  if(output_loc %in% "run_dir"){
    all_run_dirs <- list_dirs(
      run_dir_path(object), 
      pattern = "NM_run[0-9]+", 
      full.names = TRUE) 
  } else {
    all_run_dirs <- list_dirs(
      run_in(object),
      full.names = TRUE) 
  }
  
  all_run_files <- dir(all_run_dirs, full.names = TRUE)
  
  ls_tempfiles(run_files = all_run_files, output_loc = output_loc, 
               ctl_extension = tools::file_ext(ctl_name(object)),
               remove_psn_exports = remove_psn_exports)
  
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


#' Show lst file
#'
#' @param r object of class nm
#' @export
show_out <- function(r){
  UseMethod("show_out")
}

#' @export
show_out.nm <- function(r) {
  show_file(r$output$psn.lst)
}


#' Show an uneditable version of the control file
#'
#' @param r nm object
#' @export
show_ctl <- function(r) {
  UseMethod("show_ctl")
}

#' @export
show_ctl.nm_generic <- function(r) {
  rtmp <- r %>% run_in(file.path(run_in(r), "temp"))
  r %>% write_ctl(force = TRUE)
  file.show(ctl_path(r))
}
#' @export
show_ctl.nm_list <- show_ctl.nm_generic

#' Get/set existing subroutine
#'
#' The fast way to see the contents of a particular subroutine. This can be used
#' in place of manual edits to (re)write the contents of a NONMEM subroutine
#' from within R
#'
#' @param m nm object
#' @param dollar character.  Subroutine to target
#' @param ... arguments to be passed to text()
#' @param add_dollar_text logical (default = TRUE). Should the $XXXX be added to
#'   text
#'
#' @seealso \code{\link{insert_dollar}}
#'
#' @examples
#' \dontrun{
#'
#' m1 %>% dollar("PK")  ## displays existing $PK
#'
#' m1 <- m1 %>% dollar("DES",
#' "
#' DADT(1) = -K*A(1)
#' "
#' )  
#' ## This will rewrite an existing $DES
#' ## if control file doesn't already have a $DES
#' ## use insert_dollar() instead
#'
#' }
#'
#' @export
dollar <- function(m, dollar, ..., add_dollar_text = TRUE) {
  orig_target <- m %>% target()
  ans <- m %>% target(dollar) %>% 
    text(..., add_dollar_text = add_dollar_text)
  if(is_nm_list(ans)) ans <- ans %>% target(orig_target)
  ans
}

n_thetas <- function(m){
  param_info <- param_info(m)
  nrow(param_info[grepl("THETA", param_info$parameter),])
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
#'   based_on("staging/Models/run1.mod")
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
    if(inherits(ref_m, "nm_list") | inherits(ref_m, "nm_generic")){
      old_ctl <- as.character(ctl_character(ctl_contents(as_nm_generic(ref_m)))) 
    } else {
      if(is.character(ref_m)){
        if(length(ref_m) > 1) old_ctl <- ref_m
        if(length(ref_m) == 0) stop("ref_m should not be length 0")
        if(length(ref_m) == 1) 
          if(file.exists(ref_m)) 
            old_ctl <- readLines(ref_m) 
      } else {
        stop("don't know how to handle ref_m")
      }
    }
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

#' remove parameter from NONMEM control file
#' 
#' @param m nm object
#' @param name character. Parameter to remove
#' @export
remove_parameter <- function(m, name){
  old_target <- target(m)
  m <- m %>% untarget %>%
    gsub_ctl(paste0(".*\\b",name,"\\b.*"), "") %>% 
    gsub_ctl(paste0(".*\\bTV",name,"\\b.*"), "") %>% 
    gsub_ctl(paste0(".*\\bIIV_",name,"\\b.*"), "")
  m %>% target(old_target)
}


#' add a mixed effect parameter to $PK (or $PRED)
#' 
#' This will (by default) add a parameter (mixed effect) to your code
#' 
#' @param m nm object
#' @param name character. name of NONMEM variable to create
#' @param init numeric. initial value of fixed effect
#' @param unit character. unit of variable
#' @param trans character. tranformation to log scale?
#' @param position integer. not used
#' @param after character. patter to match and include the mixed effect after
#' 
#' @export
add_mixed_param <- function(m, name, 
                            init = 1, unit ="", trans = c("LOG"),
                            position = NA_integer_,
                            after = character()){
  
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
", append = TRUE, after = after) %>%
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


rename_parameter_ <- function(m, new_name, name){
  UseMethod("rename_parameter_")
}

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

rename_parameter_.nm_list <- Vectorize_nm_list(rename_parameter_.nm_generic, SIMPLIFY = FALSE)

#' rename a parameter in NONMEM control stream
#' 
#' @param m nm object
#' @param ... renaming arguments
#' 
#' @examples 
#' \dontrun{
#' 
#' m1 %>% rename_parameter("CL" = "KE")
#' ## renames KE to CL
#' 
#' }
#' @export
rename_parameter <- function(m, ...){
  rename_list <- list(...)
  new_name <- names(rename_list)
  name <- unlist(rename_list)
  rename_parameter_(m, new_name, name)
}

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
    
    suppressWarnings({ ## ignore as.numeric("...") warnings
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
    })
    
    
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
    ## subsequent code needs OMEGA BLOCK (n) format
    d$x_nc <- gsub("BLOCK\\(", "BLOCK (",d$x_nc)
    
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
    #d$unit[d$comment_nfields %in% 2] <- gsub(two_field_regex, "\\2", d$comment[d$comment_nfields %in% 2])
    d$unit[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\2", d$comment[d$comment_nfields %in% 3])
    d$unit <- trimws(d$unit)
    
    d$trans <- NA
    d$trans[d$comment_nfields %in% 2] <- gsub(two_field_regex, "\\2", d$comment[d$comment_nfields %in% 2])
    d$trans[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\3", d$comment[d$comment_nfields %in% 3])
    d$trans <- trimws(d$trans)
    
    ## modify name, unit and trans for off diagonals
    off_diagonal <- (d$omega1 != d$omega2) %in% TRUE
    if(any(off_diagonal)){
      d$unit[off_diagonal] <- NA
      d$trans[off_diagonal] <- NA
      
      off_diagonal_names <- sapply(which(off_diagonal), function(i){
        omega1 <- d$omega1[i]
        omega2 <- d$omega2[i]
        name1 <- d$name[d$omega1 %in% omega1 & d$omega2 %in% omega1]
        name2 <- d$name[d$omega1 %in% omega2 & d$omega2 %in% omega2]
        
        name1 <- gsub("IIV_", "", name1)
        name2 <- gsub("IIV_", "", name2)
        new_name <- paste0("COV_",name1,"_",name2)
        new_name
      })
      d$name[off_diagonal] <- off_diagonal_names  
    }
    
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


#' convert nm objects to a rowwise tibble/data.frame
#'
#' used mainly internally, but this could find use with advanced users
#'
#' @param m nm object
#'
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

#' get path to run_dir
#' 
#' @param m nm object
#' @seealso \code{\link{nm_getsetters}}
#' @export
run_dir_path <- function(m) file.path(run_in(m), run_dir(m))
#nm_run_dir_path <- function(m, subdir = "NM_run1") file.path(run_dir_path(m), subdir)
#nm_out_file <- function(m, file_name) file.path(nm_run_dir_path(m), file_name)

output_location <- function(m) file.path(run_in(m), dirname(lst_path(m)))

read_ext.nm_list <- function(r,trans=FALSE){
  exts <- lapply(r, read_ext)
  names(exts) <- NULL
  exts
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

#' Get/set initial parameters
#' 
#' @param m nm object
#' @param replace optional tibble for replacement
#' @param ... mutate init_theta
#' 
#' @examples
#' \dontrun{
#' 
#'  ## fix THETA on KA
#'   ip <- init_theta(m1)
#'   ip[[1]]$FIX[ip[[1]]$name %in% "KA"] <- TRUE
#'   m1 <- m1 %>% init_theta(ip)
#'   
#'  ## fix OMEGA on KA to 0.0225
#'   ip <- init_omega(m1)
#'   ip[[1]]$init[ip[[1]]$name %in% "IIV_KA"] <- 0.0225
#'   ip[[1]]$FIX[ip[[1]]$name %in% "IIV_KA"] <- TRUE
#'   m1 <- m1 %>% init_omega(ip)
#'   
#'  ## perturb all log transformed parameters by ~10%
#'   m1 <- m1 %>% init_theta(
#'     init = ifelse(
#'       trans %in% "LOG",
#'       rnorm(length(init), mean = init, sd = 0.1),
#'       init
#'     )
#'   )
#' 
#' }
#' @name init_theta
#' @export

init_theta <- function(m, replace, ...){
  UseMethod("init_theta")
}

#' @export
init_theta.nm_generic <- function(m, replace, ...){
  d <- raw_init_theta(m)
  d$orig_line <- d$line
  mutate_args <- rlang::enquos(...)
  if(missing(replace)){  ## get
    if(length(mutate_args) > 0){
      current_init <- init_theta(m)
      replace <- current_init %>% dplyr::mutate(!!!mutate_args)
      replace <- replace %>% dplyr::mutate_if(is.numeric, ~signif(., 5))
    } else {
      d <- d[!is.na(d$parameter), ]
      d$value <- NULL
      d$comment <- NULL
      d$SUB <- NULL
      return(d) 
    }
  } else {
    if(length(mutate_args) > 0) stop("can't specify additional args and replace args at same time") 
  }
  d$row <- seq_len(nrow(d))
  d_new <- dplyr::full_join(d, replace, by = c("line", "pos"))
  d_new <- d_new[, !grepl("\\.x$", names(d_new))]
  names(d_new) <- gsub("(.*)\\.y", "\\1", names(d_new))
  d_new <- d_new[order(d_new$row), ]
  d_new$row <- NULL
  m <- m %>% raw_init_theta(d_new)
  m
}

#' @export
init_theta.nm_list <- Vectorize_nm_list(init_theta.nm_generic, SIMPLIFY = FALSE, replace_arg = "replace")


#' @rdname init_theta
#' @export
init_omega <- function(m, replace, ...){
  UseMethod("init_omega")
}

#' @export
init_omega.nm_generic <- function(m, replace, ...){
  d <- raw_init_omega(m)
  d$orig_line <- d$line
  d$orig_pos <- d$pos
  mutate_args <- rlang::enquos(...)
  if(missing(replace)){  ## get
    if(length(mutate_args) > 0){
      current_init <- init_omega(m)
      replace <- current_init %>% mutate_cond(!is.na(current_init$name), !!!mutate_args)
      replace <- replace %>% dplyr::mutate_if(is.numeric, ~signif(., 5))
    } else {
      d$value <- NULL
      d$comment <- NULL
      d$parameter <- NULL
      d$SUB <- NULL
      return(d)
    }
  } 
  ## set
  ## need to add back in column from raw_init_omega format
  d_derived <- d[,c("value","comment","parameter","SUB", ## same as what was deleted above
                    "orig_line", "orig_pos")] 
  
  replace <- dplyr::left_join(replace, d_derived, by = c("orig_line", "orig_pos"))
  if("new_value" %in% names(replace)) {  ## for characters
    replace$value[!is.na(replace$new_value)] <- as.character(replace$new_value[!is.na(replace$new_value)])
  }
  if("new_line" %in% names(replace)) replace$line <- replace$new_line
  if("new_pos" %in% names(replace)) replace$pos <- replace$new_pos
  #debugonce(raw_init_omega)
  m <- m %>% raw_init_omega(replace)
  m
}

#' @export
init_omega.nm_list <- Vectorize_nm_list(init_omega.nm_generic, SIMPLIFY = FALSE, replace_arg = "replace")

#' @name init_theta
#' @export
init_sigma <- function(m, replace, ...){
  UseMethod("init_sigma")
}

#' @export
init_sigma.nm_generic <- function(m, replace, ...){
  d <- raw_init_sigma(m)
  d$orig_line <- d$line
  d$orig_pos <- d$pos
  mutate_args <- rlang::enquos(...)
  if(missing(replace)){  ## get
    if(length(mutate_args) > 0){
      current_init <- init_sigma(m)
      replace <- current_init %>% mutate_cond(!is.na(current_init$name), !!!mutate_args)
      replace <- replace %>% dplyr::mutate_if(is.numeric, ~signif(., 5))
    } else {
      d$value <- NULL
      d$comment <- NULL
      d$parameter <- NULL
      d$SUB <- NULL
      return(d)
    }
  } 
  ## set
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

#' @export
init_sigma.nm_list <- Vectorize_nm_list(init_sigma.nm_generic, SIMPLIFY = FALSE, replace_arg = "replace")


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


#' Create omega/sigma block from init_omega() and init_sigma() output
#' 
#' @param iomega tibble.  Output from init_omega() and init_sigma()
#' @param eta_numbers numeric vector.  ETA numbers to put into a block. Must be contiguous
#' @param diag_init numeric. Default value for off diagonal elements
#' 
#' @seealso \code{\link{unblock}}, \code{\link{init_theta}}
#' 
#' @examples 
#' 
#' \dontrun{
#' io <- m1 %>% init_omega()
#' io <- io %>% block(c(2,3))
#' m1 <- m1 %>% init_omega(io)
#' m1 %>% dollar("OMEGA") ## to display $OMEGA
#' }
#' 
#' @export
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

block <- Vectorize(block, vectorize.args = "iomega", SIMPLIFY = FALSE)

#' Remove $OMEGA/$SIGMA BLOCK from init_omega() and init_sigma() output
#' 
#' @param iomega tibble.  Output from init_omega() and init_sigma()
#' @param eta_numbers numeric vector.  ETA numbers to unblock. Must be contiguous
#' 
#' @seealso \code{\link{block}}, \code{\link{init_theta}}
#' 
#' @examples 
#' 
#' \dontrun{
#' io <- m1 %>% init_omega()
#' io <- io %>% unblock(c(2,3))
#' m1 <- m1 %>% init_omega(io)
#' m1 %>% dollar("OMEGA") ## to display $OMEGA
#' }
#' 
#' @export
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

unblock <- Vectorize(unblock, vectorize.args = "iomega", SIMPLIFY = FALSE)

ctl_table_paths <- function(ctl) {
  UseMethod("ctl_table_paths")
}

ctl_table_paths.nm_generic <- function(ctl) {
  ## path should go from base directory
  ## in psn directory
  file.path(output_location(ctl), ctl_table_files(ctl_contents(ctl)))
}

ctl_table_paths.nm_list <- Vectorize_nm_list(ctl_table_paths.nm_generic, SIMPLIFY = FALSE)

#' get parent object of nm object
#' 
#' @param m nm object
#' @param n numeric. generation of parent (default = 1)
#'  
#' @export
parent_run <- function(m, n = 1L){
  UseMethod("parent_run")
}
#' @export
parent_run.nm_generic <- function(m, n = 1L){
  
  if(n == 0) return(as_nm_generic(nm(NA)))
  
  hack_m <- m %>% 
    run_id(parent_run_id(m)) %>% 
    run_in(parent_run_in(m))
  
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

nm_list2list <- function(m){
  class(m) <- "list"
  m
}

#' convert a NONMEM run to a simulation
#' 
#' replaces $EST with $SIM
#' 
#' @param m nm object
#' @param seed numeric (default = 12345). seed value to include in $SIM
#' @param subpr numeric (default = 1). SUBPR value to include in $SIM
#' 
#' @details will only change $EST/$SIM, therefore it will not be sufficient to change a categorical estimation control file to simulation 
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

#' @name ppc
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

#' @name ppc
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

#' Write derived data file.
#'
#' will write to (DerivedData) directory
#' 
#' @param d data.frame. Data frame to be saved
#' @param name name of file (without extension). If not a path, will save to
#'  DerivedData directory
#' @param ...  additional arguments to be passed to write.csv
#' @export

write_derived_data <- function(d, name, ...){
  UseMethod("write_derived_data")
}

#' @export
write_derived_data.data.frame <- function(d, name, ...){
  
  name <- tools::file_path_sans_ext(name)
  
  if(dirname(name) %in% "."){
    RDS_name <- file.path("DerivedData",paste0(name,".RDS"))
    csv_name <- file.path("DerivedData",paste0(name,".csv"))
  } else {  ## directory is specified
    RDS_name <- paste0(name,".RDS")
    csv_name <- paste0(name,".csv")
  }
  
  d <- as.data.frame(d)
  if(!inherits(d, "data.frame")) stop("d needs to be a data.frame or coercible into one")
  
  dir.create(dirname(RDS_name), showWarnings = FALSE, recursive = TRUE)
  saveRDS(d, file = RDS_name)
  utils::write.csv(d, file = csv_name, na = ".", row.names=FALSE, quote=FALSE, ...)
  
  message("written: ")
  message(RDS_name)
  message(csv_name)
}

#' @export
write_derived_data.list <- function (d, name, ...) 
{
  vectorize.args = c("d", "name")
  SIMPLIFY = TRUE
  USE.NAMES = TRUE
  FUN = write_derived_data.data.frame
  arg_call <- as.list(match.call())[-1L]
  ##########
  args <- lapply(arg_call, eval, parent.frame())
  names <- if (is.null(names(args))) 
    character(length(args))
  else names(args)
  dovec <- names %in% vectorize.args
  do.call("mapply", c(FUN = FUN, args[dovec], MoreArgs = list(args[!dovec]), 
                      SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES))
  ##########
  invisible()
}

# write_derived_data.list <- Vectorize(write_derived_data.data.frame, 
#                                      vectorize.args = c("d", "name"), 
#                                      SIMPLIFY = TRUE)


#' Read derived data
#'
#' @param name name of file (without extension)
#' @param na character to be passed to read.csv
#' @param silent logical (default = TRUE). should messages be suppressed
#' @param ...  additional arguments to be passed to read.csv
#' @export

read_derived_data <- function(name, na = ".", silent = FALSE, ...){
  
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

#' get all nm_list objects
#' 
#' @param x environment (default = .GlobalEnv) to search
#'   or data.frame with (nm_list column) or nm_list
#' @export
nm_list_gather <- function(x = .GlobalEnv){
  UseMethod("nm_list_gather")
}

#' @export
nm_list_gather.default <- function(x = .GlobalEnv){
  
  m <- lapply(x, function(object){
    if(inherits(object, "nm_list")) object else NA
  })
  
  m <- m[!is.na(m)]
  m <- do.call(c, m)
  m
}

#' @export
nm_list_gather.nm_list <- function(x = .GlobalEnv) x

## generic already defined
## internal function
data_ignore_char.nm_generic <- function(r, data){
  dol_data <- r %>% dollar("$DATA")
  dol_data <- dol_data[!dol_data %in% ""]
  dol_data <- rem_comment(dol_data)
  
  ###
  dol_data <- paste(dol_data, collapse = ";")
  ## remove IGNORE=@
  dol_data <- gsub("IGNORE\\s*=\\s*@", "", dol_data)
  ## remove IGNORE=#
  dol_data <- gsub("IGNORE\\s*=\\s*#", "", dol_data)
  
  ignore_present <- any(grepl(".*IGNORE\\s*=\\s*\\(",dol_data))
  accept_present <- any(grepl(".*ACCEPT\\s*=\\s*\\(",dol_data))
  
  
  ## can now assume that only one is TRUE
  
  type <- NA
  if(ignore_present & accept_present) stop("cannot identify ignore columns")
  if(ignore_present) type <- "IGNORE"
  if(accept_present) type <- "ACCEPT"
  if(!ignore_present & !accept_present)
    return("FALSE") ## do not ignore anything
  
  ## remove stuff before first IGNORE/ACCEPT
  dol_data <- gsub(paste0(".*?(",type,".*)"), "\\1", dol_data)
  ## remove IGNORE=
  dol_data <- gsub(paste0(type, "\\s*=*"), 
                   "", dol_data)
  #ensure bracketed ignore expression are comma separated
  dol_data <- gsub("\\)\\s+\\(", "),(", dol_data)
  ## remove brackets
  dol_data <- gsub("\\(", "", dol_data)
  dol_data <- gsub("\\)", "", dol_data)
  ## remove spaces
  dol_data <- gsub("\\s*", "", dol_data)
  ## remove blank lines
  dol_data <- gsub(";+", ";", dol_data)
  ## should now be only statemetns with ,; separators
  
  dol_data <- unlist(strsplit(dol_data, split = "[,;]"))
  
  if(missing(data)) data <- input_data(r, filter = FALSE)
  
  r_data_names <- names(data)
  ## now get nonmem names
  dollar_input <- r %>% dollar("INPUT")
  nonmem_data_names <- gsub("\\$\\w+", "", dollar_input)
  nonmem_data_names <- unlist(strsplit(nonmem_data_names, split = "\\s"))
  nonmem_data_names <- nonmem_data_names[!nonmem_data_names %in% ""]
  nonmem_data_names <- gsub("(\\w+)=DROP", "\\1", nonmem_data_names)
  nonmem_data_names <- gsub("\\w+=(\\w+)", "\\1", nonmem_data_names)
  #if(length(r_data_names) != length(nonmem_data_names))
  #  stop("length of items in $INPUT doesn't match dataset")
  name_chart <- data.frame(r_data_names, nonmem_data_names, stringsAsFactors = FALSE)
  name_chart <- name_chart[name_chart$r_data_names != name_chart$nonmem_data_names,]
  
  #filter_statements <- paste0(".*",type,"\\s*=\\s*\\((\\S[^\\)]+)\\)*.*")
  #dol_data <- dol_data[grepl(filter_statements, dol_data)]
  #filter_statements <- gsub(filter_statements,"\\1",dol_data)
  #filter_statements <- unlist(strsplit(filter_statements,","))
  filter_statements <- dol_data
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
  
  filter_statements
  
}
data_ignore_char.nm_list <- Vectorize_nm_list(data_ignore_char.nm_generic, SIMPLIFY = TRUE)


#' Make data.tree object
#' 
#' @param ... arguments passed to nm_list_gather(...)
#' @param summary logical (default = FALSE) should summary_wide variables be appended
#' @export
nm_tree <- function(..., summary = FALSE){
  
  if(!requireNamespace("data.tree")) stop("install data.tree")
  
  m <- nm_list_gather(...) 
  
  if(summary){
    sink(file="/dev/null")
    m_row <- m %>% 
      {suppressMessages(
        dplyr::right_join(nm_row(.data), summary_wide(.data)) 
      )}
    sink()
  } else {
    m_row <- m %>% nm_row()
  }
  
  network <- m_row %>%
    dplyr::mutate(
      parent_run_id = ifelse(is.na(.data$parent_run_id), 
                             "start", 
                             .data$parent_run_id),
      parent_run_id = ifelse(is.na(.data$parent_run_in),
                             .data$parent_run_id,
                             file.path(.data$parent_run_in, .data$parent_run_id)
      ),
      run_id = file.path(.data$run_in, .data$run_id)
    ) %>%
    dplyr::select(.data$parent_run_id, .data$run_id, dplyr::everything())
  
  tree <- data.tree::FromDataFrameNetwork(network)
  
  tree
}


#' make decision point
#'
#' formalise process of decision making.  Creates a decision point in the
#' workflow where subsequent parts of your workflow depend on this decision, e.g. if you compare a 1 compartment and 2 compartment and decide based on the OFV and goodness of fit plots that the 1 compartment model is better and subsequent steps will build off of this, it is worth putting a decision point in your code so that if you are to rerun the workflow with a new/updated dataset, the decision can be revisted prior to moving onto the parts of the workflow that depend on the 1 compartment decision.  The function requests inputs (\code{values} and \code{files}) that you base a
#' decision on and stop for users to remake decision if inputs change
#'
#' @param inputs (optional) non file names upon which decision depends
#' @param file_inputs (optional) file names upon which decision depends
#' @param auto (optional) logical. logical statement for automatic decisions
#' @param outcome character. Description of the decision outcome
#' @param force logical (default = FALSE). Force a stop in the workflow so decision has been remade
#' 
#' @details 
#' There are two ways to use `decision`:
#' 
#' Automatic: An `auto` decision (see examples below) works like `stopifnot()`.  It requires a logical (TRUE/FALSE) condition.  Doing this this way ensures that 
#' creates fewer points in your workflow where at the cost of removing.  If updating a workflow (e.g. with an updated dataset), so long as the TRUE/FALSE is TRUE, the workflow will proceed uninterrupted.  If the condition flips to FALSE the workflow will stop as it will be assumed that subsequent steps will no longer be valid.
#' 
#' Manual: Requires specification of either `input` or `file_inputs` (or both) AND `outcome`.  Inputs represent information you have considered in your decision and `outcome` is a text description of the resulting decision.  The assumption made is that if inputs have not changed since the last decision was made.
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' ## a decision based on summary statistics
#' decision(inputs = summary_wide(c(m1, m2, m2WT)), 
#'          outcome = "m1 is better") # next line must be end of chunk
#' 
#' ## a decision based also on goodness of fit plots
#' decision(inputs = summary_wide(c(m1, m2, m2WT)), 
#'          file_inputs = c("Results/basic_gof.m1.nb.html",
#'                          "Results/basic_gof.m2.nb.html"), 
#'          outcome = "m1 is better") # next line must be end of chunk
#' 
#' ## a decision based on an automatic TRUE/FALSE criteria
#' ## here we're ensuring m1 has the lowest AIC
#' decision(auto = (AIC(m1) == min(AIC(m1, m2, m3))))
#' 
#' }
#' 
#' @export
decision <- function(inputs = c(), 
                     file_inputs = c(), 
                     auto = logical(),
                     outcome = character(),
                     force = FALSE){
  
  if(!requireNamespace("digest"))
    stop("install digest")
  
  error_msg <- "decision needs revisiting."
  
  if((!missing(inputs) | !missing(file_inputs)) & missing(outcome))
    stop("if specifying decision inputs need to specify outcome \n'outcome' = character description of decision")
  
  if(!missing(auto)){
    if(!auto){
      stop("auto decision FAILED - ", error_msg, call. =  FALSE)
    } else {
      message("auto decision PASSED")
      return(invisible())
    }
  }
  
  wait_input <- function(inputs){
    if(!interactive()) stop("new manual decision needed. Run interactively")
    inputs  ## create inputs dependency
    cat(crayon::underline("\nmanual decision check\n"))
    cat("expected decision outcome:\n", outcome)
    ans <- readline("Does this accurately describe your decision? [y]es/[n]o/[c]heck:\n")
    if(ans %in% ""){
      stop("blank detected (if in R Notebooks, make sure decision() is at end of chunk with no blank line in between)")
    }
    if(nchar(ans) > 1){
      stop("give single character response", call. = FALSE)
    }
    if(ans %in% "n"){
      stop(error_msg, call. =  FALSE)
    }
    if(ans %in% "c"){
      stop("have a look at inputs and if you agree with decision, rerun this answering [y] ", call. =  FALSE)
    }
    if(ans %in% "y"){
      return(TRUE)
    }
    stop("invalid response", call. = FALSE)
  }
  
  if(!length(inputs)) inputs <- c()
  
  if(length(file_inputs)){
    if(!all(file.exists(file_inputs))){
      stop("file(s) do not exist", call. = FALSE)
    }
    inputs <- c(inputs, tools::md5sum(file_inputs))
  }
  
  ## generate hashes for current call
  call_ob <- match.call()
  decision_cache_path <- file.path(nm_default_dir("models"), "decision_cache")
  dir.create(decision_cache_path, recursive = TRUE, showWarnings = FALSE)
  cache_name <- paste0(digest::digest(call_ob), ".RDS")
  decision_info <- list(inputs = inputs)
  cache_path <- file.path(decision_cache_path, cache_name)
  
  ############
  ## check cache
  if(!force){
    cache_match <- file.exists(cache_path) ## and contents match
    if(cache_match){
      stored_decision <- readRDS(cache_path)
      cache_match <- identical(stored_decision, decision_info)
    }    
  } else cache_match <- FALSE
  ############
  if(!cache_match){
    decision_accurate <- wait_input(inputs)    
    
    if(decision_accurate){
      ## save cache with a record of the decision
      saveRDS(decision_info, cache_path)
    } else {
      ## delete cache of inaccurate decisions
      unlink(cache_path, force = TRUE)
    }
  } else {
    message("\ndecision inputs & outcome match prior decision")
  }
# 
#   if(!length(outdated) & previous_outcome){
#     message("decision inputs haven't changed, trusting that decision is still correct")
#     suppressMessages({
#       drake::make(drpl)
#     })  
#   } else {
#     suppressMessages({
#       drake::clean("pause")
#       drake::make(drpl, force = TRUE)
#     })
#     current_outcome <- try(drake::readd("pause"), silent = TRUE)
#     if(inherits(current_outcome, "try-error")) current_outcome <- FALSE
#     
#     if(!current_outcome) ## if outcome = FALSE, then "[c]heck". ([n]o makes error)
#       message("make decision again (check inputs and file_inputs), then re-execute")
#   }
  
}

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


