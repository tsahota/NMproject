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
#' @keywords internal
#' @export
nm <- Vectorize_nm_list(nm_generic, SIMPLIFY = FALSE)

#' Create a new (parent) nm object
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Create a new parent `nm` object.  Normally the first NONMEM object you create
#' will be using this function.  Subsequent objects created with the [child()]
#' function will inherit the properties of the parent run.
#'
#' @param based_on Character. Relative path to an existing control file from
#'   which to base this run.  NMproject will not modify or run this control
#'   file.  Instead it will create a new control file specified by the
#' `ctl_name` field (see Details below).
#' @param run_id Character. Run identifier. This is used to name the run and
#'   output files such as $TABLE outputs.
#' @param data_path Character. Path to dataset. If this is not specified,
#'   NMproject will try to guess based on the current $DATA components of the
#'   file specified in `based_on`.  However it is recommended to specify this
#'   explicitly as a relative path.
#' @param cmd Optional character. PsN command to use. If unspecified will use
#'   `getOption("nm.cmd_default")`. Use glue notation for inheritance.  See
#'   details.
#'
#' @details The `cmd` field uses `glue` notation.  So instead of specifying
#'  `execute runm1.mod -dir=m1`, it is best to specify `execute {ctl_name}
#'  -dir={run_dir}`.  The values of `ctl_name` and `run_dir` refer to object
#'  fields and if these change value like when the `child()` function is used to
#'  create a separate child object, the `cmd` field will update automatically.
#'
#' @section object fields:
#'
#'  Each field has a corresponding function (documented in [nm_getsetters]) of
#'  the same name to access and modify it's value.
#'  
#'  \describe{ 
#'  \item{type}{
#'    The PsN run type.  Default is `execute`.
#'  }
#'  \item{run_id}{
#'    The run identifier.  E.g. `m1`.
#'  }
#'  \item{run_in}{
#'    The directory to copy control files and run NONMEM.  Default = "Models".
#'  }
#'  \item{executed}{
#'    For internal use.
#'  } 
#'  \item{ctl_contents}{
#'    Stores the contents of the control file to be written to disk when the 
#'    [run_nm()] function is used on the object.
#'  } 
#'  \item{data_path}{
#'    Path to the NONMEM ready dataset (from base project directory).
#'  }
#'  \item{cmd}{
#'    See details above.
#'  }
#'  \item{cores}{
#'    Numbers of cores to use.  Requires a `cmd` value that uses the `{cores}`
#'    glue field.
#'  } 
#'  \item{run_dir}{ 
#'    PsN directory to run the NONMEM run. Default is to the be the same as 
#'    the `run_id` for simplicity.
#'  }
#'  \item{results_dir}{ 
#'    Location to store results files from diagnostic reports executed with 
#'    [nm_render()].
#'  } 
#'  \item{unique_id}{
#'    For internal use.
#'  }
#'  \item{lst_path}{
#'    Normally does not require setting.  Path to the expected .lst file.
#'  }
#'  }
#'
#' @return An object of class `nm_list`.  Attributes
#' @seealso [nm_getsetters()], [child()]
#' @examples
#' \dontrun{
#'
#' m1 <- new_nm(run_id = "m1",
#'             based_on = "staging/Models/run1.mod",
#'             data_path = "DerivedData/data.csv",
#'             cmd = "execute -run_on_sge -sge_prepend_flags='-V' {ctl_name} -dir={run_dir}") %>%
#'      fill_input() %>%
#'      run_nm()
#'
#' m1 ## display object fields
#' cmd(m1)
#' ctl_name(m1)
#' run_dir(m1)
#'
#' }
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
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Child objects inherit attributes of parent but with a new `run_id`. The control
#' file will be inherited too with $TABLEs updated
#'
#' @param m Parent nm object.
#' @param run_id Character.  New run id to assign to child object.
#' @param type Character (default = "execute"). type of child object.
#' @param parent Optional nm object (default = nm(NA)) . Parent object will by
#'   default be `m`, but this argument will force parent to be a different
#'   object.
#' @param silent Logical (default = FALSE). Should warn if conflicts detected.
#'
#' @details Specifying `parent` will force parent to be different from `m`. This
#'   is useful in piping when a parent object is modified prior to being used in
#'   the child object.
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
  # if(is.environment(m)){
  #   old_classes <- class(m)
  #   m <- as.environment(as.list(m, all.names=TRUE))
  #   class(m) <- old_classes 
  # }
  
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
#' When a `child()` has been created by a modified parent sometimes adoption is
#' the answer.  This will force parenthood to the specified object.
#'
#' @param m nm object
#' @param mparent new parent object to child
#' @param silent logical (default = TRUE). Should warn if conflicts detected
#'
#' @seealso [child()]
#'
#' @keywords internal
change_parent <- function(m, mparent, silent = TRUE){
  UseMethod("change_parent")
}

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

#' Interface for getting and setting your own simple fields in nm objects
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' @param m An nm object.
#' @param ... Arguments to get/set fields.
#' 
#' @examples 
#' \dontrun{
#' 
#' mc <- mc %>% simple_field(stars = 3)
#' mc %>% simple_field(stars)
#' mc ## see that stars is a field of the nm object.
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
custom_vector_field.nm_list <- Vectorize_nm_list(custom_vector_field.nm_generic, SIMPLIFY = FALSE, replace_arg = "replace", vectorize.args = c("m", "field"))

#' Get/set path to dataset
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Mainly used to associate a dataset with an nm object.
#' Requires ctl_contents to already be specified.
#' 
#' @param m An nm object.
#' @param text Optional character. Path to input dataset.
#' 
#' @return if text is not specified, will return the data_path name
#'  otherwise will set data_path to the text provided
#' 
#' @examples 
#' \dontrun{
#' 
#' m1 <- new_nm(run_id = "m1",
#'              based_on = "staging/Models/ADVAN2.mod",
#'              data_path = "DerivedData/data.csv") %>%
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

#' Fill $INPUT
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Uses dataset to automatically fill $INPUT in control file.
#'
#' @param m An nm object.
#' @param ... Either `keep`, `drop`, or `rename` arguments.  See examples.
#'
#' @details If a new dataset with different columns is assigned to an `nm`
#'   object, `$INPUT` will not be correct and so it may necessary to apply
#'   `fill_input()` again.
#'
#'   See examples for how to use `drop` and `rename` arguments to control how
#'   `$INPUT` is written.
#'   
#' @examples
#' \dontrun{
#'
#'  m1 <- m1 %>% fill_input() 
#'  m1 %>% dollar("INPUT") ## view $INPUT
#'
#'  ## following will will drop the "RATE" column
#'  m1 <- m1 %>% fill_input(drop = "RATE")
#'  m1 %>% dollar("INPUT")
#'
#'  ## following will rename "DATE" to be "DAT0"
#'  m1 <- m1 %>% fill_input(rename = c("DAT0" = "DATE"))
#'  m1 %>% dollar("INPUT") ## view $INPUT
#'
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


#' Read input dataset of an nm object
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Uses `data_path` field of object to locate data and read in.
#' 
#' @param m An nm object.
#' @param filter Logical (default = `FALSE`). Applies NONMEM ignore statement to
#'   filter dataset.
#' @param na Character. Passed to [utils::read.csv()]
#' @param ... Additional arguments passed to either [read_derived_data()] (if
#'   [write_derived_data()] was used to create derived dataset) or
#'   [utils::read.csv()]
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' d <- input_data(m1)
#' 
#' ## only non-ignored rows
#' d_nonignore <- input_data(m1, filter = TRUE)
#' 
#' }
#' 
#' @export
input_data <- function(m, filter = FALSE, na = ".", ...){
  UseMethod("input_data")
}
#' @export
input_data.nm_generic <- function(m, filter = FALSE, na = ".", ...){
  file_name <- data_path(m)
  if(is.na(file_name)) return(dplyr::tibble())
  
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
  ctl <- update_ignore(ctl, ignore_char)
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

cache_history <- function(r){
  UseMethod("cache_history")
}

cache_history.nm_generic <- function(r){
  lapply(run_cache_paths(r), readRDS)
}

cache_history.nm_list <- Vectorize_nm_list(cache_history.nm_generic, SIMPLIFY = FALSE)

cache_current <- function(m) run_checksums(m)

clear_cache <- function() unlink(".cache", recursive = TRUE)

#' Get/set existing subroutine
#'
#' The fast way to see the contents of a particular subroutine directly in the R
#' console. It can also be used to set the contents of a NONMEM subroutine in
#' place of manual edits
#'
#' @param m An nm object.
#' @param dollar Character. Name of NONMEM subroutine to target.
#' @param ... Additional arguments to be passed to `text()`.  If specified these
#'   will set the contents of the subroutine.  See examples below.
#' @param add_dollar_text Logical (default = TRUE). Should the $XXXX string be
#'   added to text
#'   
#' @seealso [insert_dollar()]
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
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' The easiest way to use this function is via the "view diff" RStudio addin.
#' 
#' NMproject's control file manipulation functions (e.g. subroutine())
#'  may not work for all control files. It is the responsibility of 
#'  the user to check automatic manipulations are done properly.
#'  Displaying diffs provides a means of manually checking what was done.
#' 
#' @param m An nm object.
#' @param ref_m An optional nm object (base/reference object).  If not
#'   specified, it will compute the diff the initial control file contents
#'   associated with the object at the time of object create.  This information
#'   is stored in the `ctl_orig` field.
#' @param format Character (default = `"raw"`) argument passed to
#'   [diffobj::diffChr()]
#' 
#' @return diff object
#' @examples 
#' \dontrun{
#' 
#' m1 <- new_nm(run_id = "m1",
#'              based_on = "staging/Models/run1.mod")
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

#' Convert nm objects to a rowwise tibble/data.frame
#'
#' @description 
#'
#' `r lifecycle::badge("stable")`
#'
#' Primarily an internal function. Converts an `nm_list` object to a `tibble`.
#'
#' @param m An nm object.
#' 
#' @keywords internal
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
  d <- dplyr::as_tibble(m[eligible_rows])
  d
}
#' @export
nm_row.nm_list <- function(m){
  d <- lapply(m, nm_row)
  d <- suppressWarnings(dplyr::bind_rows(d))
  d
}

rr_row <- function(m){
  d <- nm_row(m)
  d$m <- m
  d
}

#' Get path to run_dir
#'
#' The function [run_dir()] gives the directory name.  This function gets the
#' (relative) path of [run_dir()].
#'
#' @param m An nm object.
#' @seealso [nm_getsetters()]
#' @export
run_dir_path <- function(m) file.path(run_in(m), run_dir(m))

output_location <- function(m) file.path(run_in(m), dirname(lst_path(m)))

read_ext.nm_list <- function(r,trans=FALSE){
  exts <- lapply(r, read_ext)
  names(exts) <- NULL
  exts
}

#' @export
print.nm_subroutine <- function(x, ...){
  cat(paste0(format(seq_along(x), width = 3),"| ",x), sep = "\n")
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

ctl_table_paths <- function(ctl) {
  UseMethod("ctl_table_paths")
}

ctl_table_paths.nm_generic <- function(ctl) {
  ## path should go from base directory
  ## in psn directory
  file.path(output_location(ctl), ctl_table_files(ctl_contents(ctl)))
}

ctl_table_paths.nm_list <- Vectorize_nm_list(ctl_table_paths.nm_generic, SIMPLIFY = FALSE)

#' Get parent object of nm object
#' 
#' Will pull the parent run of an nm object from the run cache.
#' 
#' @param m An nm object.
#' @param n Numeric. generation of parent (default = 1).
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

#' Get all nm_list objects
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Get all nm objects in an environment.  By default this is the global
#' workspace.
#' 
#' @param x environment (default = `.GlobalEnv`) to search
#'   or data.frame with (`nm_list` column) or `nm_list`
#'   
#' @return A single `nm_list` object with all model objects
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



