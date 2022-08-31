#' @include make_project.R

NULL

#' Get and set path to NONMEM control file
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Similar to [ctl_name()] & [run_in()], this allows you to retrieve and specify
#' the relative path to the control file that will be written by the [run_nm()].
#'
#' @param m An nm object.
#' @param text Optional character. Name of path to control file (see details).
#'   Typically, this file does not yet normally exist, but will house the code
#'   code for this run.
#'
#' @details Note that `text` can contain an `"{run_id}"` string.  E.g.
#'   `"Models/run{run_id}.mod"` will use the name `"Models/runm1.mod"` if
#'   `run_id(m1)` is `"m1"`.
#'
#' @return `character` with path to NONMEM control file to be copied immediately
#'   prior to running (with [run_nm()]).
#'
#' @examples
#'
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#'
#'
#' ctl_name(m1)
#' ctl_path(m1)
#'
#' m1 <- m1 %>% ctl_path("Models/nm_{run_id}.ctl")
#' ctl_path(m1)
#'
#' 
#' @export
ctl_path <- function(m, text) {
  if (missing(text)) {
    file.path(run_in(m), ctl_name(m))
  } else {
    m <- m %>% run_in(dirname(text))
    m <- m %>% ctl_name(basename(text))
    m
  }
}

#' @name nm_getsetters
#' @rdname nm_getsetters
#'
#' @title Functions to access and modify fields of nm objects
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' The fields of an object can be viewed by printing the object.  Each field has
#' a corresponding function of the same name to access and modify it's value.
#'
#' @param m An nm object.
#' @param text Optional character for replacing field. If present function will
#'   modify field (of same name as function) otherwise will return value of
#'   field (of same name as function).
#'
#' @details Easiest way to see all fields of an object is to type the object
#'   into the console and hit enter. This will display the value of each field.
#'
#'   The fundamental structure of all these functions is the same:
#'
#'   To access the value of a field:
#'   `m %>% fieldname()` or equivalently `fieldname(m)`.
#'
#'   To modify the value of a field:
#'   `m <- m %>% fieldname("newvalue")`
#'
#' @return The value of the specified field of `m` if `text` is missing.
#'   Otherwise an nm object with modified field.
#'
#' @examples
#'
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#'
#'
#' run_dir(m1)
#'
#' m1 <- m1 %>% run_dir("{run_id}_dir")
#' run_dir(m1)
#' 
#' @export
run_dir <- function(m, text) {
  UseMethod("run_dir")
}

#' @export
run_dir.nm_generic <- function(m, text) {
  if (missing(text)) {
    custom_1d_field(m, "run_dir")
  } else {
    custom_1d_field(m, "run_dir", text, glue = TRUE)
  }
}
#' @export
run_dir.nm_list <- run_dir.nm_generic

#' @rdname nm_getsetters
#' @details Some fields like `cmd` are glue fields.  In these cases inserting
#' expressions inside braces in `text` will evaluate the expression (see
#' examples).
#' @examples
#'
#' ## set cmd field of m1
#' m1 <- m1 %>% cmd("execute {ctl_name} -dir={run_dir}")
#'
#' m1 %>% cmd()
#' ## displays "execute runm1.mod -dir=m1"
#'
#' ## can also view field when viewing object
#' m1
#' @export
cmd <- function(m, text) {
  if (missing(text)) {
    custom_1d_field(m, "cmd")
  } else {
    clean_text <- text[grepl("\\-clean\\=", text)]
    if (length(clean_text)) {
      clean_number <- gsub(".*\\-clean\\=([0-9]+).*", "\\1", clean_text)
      clean_number <- as.numeric(clean_number)
      if (clean_number > 2) {
        warning("use of -clean flag more than 2 is not recommended with NMproject.
NMproject uses files in the NM_run directories instead of files copied back to main directory.
see ?ls_tempfiles for post run clean up options")
      }
    }
    custom_1d_field(m, "cmd", text, glue = TRUE)
  }
}

#' @rdname nm_getsetters
#' @export
type <- function(m, text) {
  if (missing(text)) {
    custom_1d_field(m, "type")
  } else {
    m <- custom_1d_field(m, "type", text)
    if (text %in% "nmfe") {
      m <- m %>%
        run_in(file.path(run_in(m), "{run_dir}")) %>%
        lst_path("{gsub(\"mod\", \"lst\", ctl_name)}")
    }
    m
  }
}

#' @rdname nm_getsetters
#' @export
parent_run_id <- function(m, text) {
  if (missing(text)) custom_1d_field(m, "parent_run_id") else custom_1d_field(m, "parent_run_id", as.character(text))
}

#' @rdname nm_getsetters
#' @export
parent_run_in <- function(m, text) {
  if (missing(text)) custom_1d_field(m, "parent_run_in") else custom_1d_field(m, "parent_run_in", as.character(text))
}

#' @rdname nm_getsetters
#' @export
parent_ctl_name <- function(m, text) {
  if (missing(text)) custom_1d_field(m, "parent_ctl_name") else custom_1d_field(m, "parent_ctl_name", as.character(text))
}

#' @rdname nm_getsetters
#' @export
parent_results_dir <- function(m, text) {
  if (missing(text)) custom_1d_field(m, "parent_results_dir") else custom_1d_field(m, "parent_results_dir", as.character(text))
}

#' @rdname nm_getsetters
#' @export
unique_id <- function(m, text) {
  if (missing(text)) custom_1d_field(m, "unique_id") else custom_1d_field(m, "unique_id", text, glue = TRUE)
}

#' @rdname nm_getsetters
#' @export
ctl_name <- function(m, text) {
  if (missing(text)) custom_1d_field(m, "ctl_name") else custom_1d_field(m, "ctl_name", text, glue = TRUE)
}

#' @rdname nm_getsetters
#' @export
results_dir <- function(m, text) {
  if (missing(text)) custom_1d_field(m, "results_dir") else custom_1d_field(m, "results_dir", text, glue = TRUE)
}

#' @rdname nm_getsetters
#' @export
run_in <- function(m, text) {
  UseMethod("run_in")
}

#' @export
run_in.nm_generic <- function(m, text) {
  if (missing(text)) {
    custom_1d_field(m, "run_in")
  } else {
    m <- custom_1d_field(m, "run_in", text)

    ## additional code to ensure data_path is redone
    data_path <- data_path(m)
    m <- m %>% data_path(data_path) ## reset data path
    m
  }
}
#' @export
run_in.nm_list <- Vectorize_nm_list(run_in.nm_generic, pre_glue = TRUE)

#' @rdname nm_getsetters
#' @param update_ctl Should NONMEM code be updated to reflect the new run_id.
#'   Assumes `text` is not missing.  Default = FALSE.
#' @export
run_id <- function(m, text, update_ctl = FALSE) {
  UseMethod("run_id")
}

#' @export
run_id.nm_generic <- function(m, text, update_ctl = FALSE) {
  if (missing(text)) {
    # if(is_single_na(m)) return(NA_character_)
    if (length(m[["run_id"]]) > 0) {
      return(m[["run_id"]])
    } else {
      return(NA_character_)
    }
  }

  m[["job_info"]] <- NA_character_ ## wipe job info
  # m[["paUseMethod()rent_run_id"]] <- m[["run_id"]]
  m[["run_id"]] <- as.character(text)

  ## Do all glueing first.
  for (field in names(m$glue_fields)) {
    m <- replace_tag(m, field)
  }

  ## why was this here?
  if (update_ctl) {
    if ("ctl_contents" %in% names(m)) m <- m %>% ctl_contents(m[["ctl_contents"]]) ## update ctl_contents object
  }
  m
}
#' @export
run_id.nm_list <- Vectorize_nm_list(run_id.nm_generic)

#' @rdname nm_getsetters
#' @export
result_files <- function(m, text) {
  UseMethod("result_files")
}
#' @export
result_files.nm_generic <- function(m, text) {
  if (missing(text)) {
    return(m[["result_files"]])
  }
  m[["result_files"]] <- unique(c(m[["result_files"]], text))
  m
}
#' @export
result_files.nm_list <- Vectorize_nm_list(result_files.nm_generic, SIMPLIFY = FALSE)


#' @name nm_getsetters_execution
#' @rdname nm_getsetters_execution
#'
#' @title Execution related functions to access and modify fields of nm objects
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' The fields of an object can be viewed by printing the object.  Each field has
#' a corresponding function of the same name to access and modify it's value.
#'
#' @param m An nm object.
#' @param text Optional character for replacing field. If present function will
#'   modify field (of same name as function) otherwise will return value of
#'   field (of same name as function).
#'
#' @details Easiest way to see all fields of an object is to type the object
#'   into the console and hit enter. This will display the value of each field.
#'   some fields like `cmd` are glue fields.  In these cases inserting
#'   expressions inside braces in `text` will evaluate the expression
#'
#'   The fundamental structure of all these functions is the same:
#'
#'   To access the value of a field:
#'   `m %>% fieldname()` or equivalently `fieldname(m)`
#'
#'   To modify the value of a field:
#'   `m <- m %>% fieldname("newvalue")`
#' 
#' @examples 
#' 
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#' 
#' m1 <- m1 %>% cmd("execute -parafile={parafile} {ctl_name} -dir={run_dir} -nodes={cores}")
#' 
#' m1 <- m1 %>% cores(8) %>% parafile("mpilinux8.pnm")
#' 
#' cmd(m1)
#' cores(m1)
#' 
#'
#' @export
cores <- function(m, text) {
  if (missing(text)) custom_1d_field(m, "cores") else custom_1d_field(m, "cores", text)
}

#' @rdname nm_getsetters_execution
#' @export
parafile <- function(m, text) {
  if (missing(text)) {
    custom_1d_field(m, "parafile")
  } else {
    custom_1d_field(m, "parafile", normalizePath(text, winslash = "/", mustWork = FALSE))
  }
}

#' @rdname nm_getsetters_execution
#' @export
walltime <- function(m, text) {
  if (missing(text)) custom_1d_field(m, "walltime") else custom_1d_field(m, "walltime", text)
}

#' @rdname nm_getsetters_execution
#' @export
executed <- function(m, text) {
  if (missing(text)) custom_1d_field(m, "executed") else custom_1d_field(m, "executed", text)
}

#' @rdname nm_getsetters
#' @details The function `lst_path()` sets the expected location of the lst file
#'   and all output files relative to the `run_in` location.
#' @export
lst_path <- function(m, text) {
  if (missing(text)) custom_1d_field(m, "lst_path") else custom_1d_field(m, "lst_path", text, glue = TRUE)
}

#' Get path to run_dir
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' The function [run_dir()] gives the directory name, whereas this function gets
#' the (relative) path of [run_dir()].
#'
#' @param m An nm object.
#' 
#' @return A path to the `run_dir` field of `m`.
#' 
#' @seealso [nm_getsetters()].
#' 
#' @examples 
#' 
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#' 
#' run_dir_path(m1)
#'              
#' @export
run_dir_path <- function(m) file.path(run_in(m), run_dir(m))

#' Use file to set control file contents in nm object
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#' 
#' This function is mostly called by the `new_nm()` and will not generally be
#' used directly by users. The function populates/modifies the `ctl_contents`
#' field of an nm object, with the contents of an already existing control file.
#'
#' @param m An nm object.
#' @param ctl_ob Path to control file.
#' @param update_numbering Logical. Should table numbers and author fields be updated.
#' @param update_dollar_data Logical. Should $DATA in control file be updated.
#' @param ... Additional arguments.
#'
#' @return An nm object with modified `ctl_contents` field.
#'
#' @keywords internal
#'
#' @export

based_on <- function(m, ctl_ob, update_numbering = TRUE, update_dollar_data = TRUE, ...) {
  UseMethod("based_on")
}

#' @export
based_on.nm_generic <- function(m, ctl_ob, update_numbering = TRUE, update_dollar_data = TRUE, ...) {
  if (missing(ctl_ob)) {
    if (length(m[["ctl_contents"]]) > 0) {
      return(m[["ctl_contents"]])
    } else {
      return(NA_character_)
    }
  }

  # if(inherits(try(ctl_list2(ctl_ob)), "try-error")) browser()

  ## if NA just set field as NA
  if (is_single_na(ctl_ob)) {
    m[["ctl_contents"]] <- NA_character_
    return(m)
  }

  ctl <- ctl_list2(ctl_ob)

  if (update_numbering) {
    for (i in which(names(ctl) %in% "TABLE")) {
      ctl[[i]] <- gsub(paste0("(FILE\\s*=\\s*\\S*tab)\\S*\\b"), paste0("\\1", run_id(m)), ctl[[i]])
    }
    ctl[[1]] <- gsub("^(\\s*;;\\s*[0-9]*\\.\\s*Based on:).*", paste("\\1", parent_run_id(m)), ctl[[1]])
    ctl[[1]] <- gsub("^(\\s*;;\\s*\\w*\\.\\s*Author:).*", paste("\\1", Sys.info()["user"]), ctl[[1]])
  }

  m[["ctl_contents"]] <- ctl
  ## set as ctl_orig only if ctl_orig doesn't exist
  # if(!"ctl_orig" %in% names(m)) m[["ctl_orig"]] <- ctl
  if (is_single_na(m[["ctl_orig"]])) m[["ctl_orig"]] <- ctl

  ## overwrite the data_path field if it's blank
  data_path <- data_path(m)
  if (is.na(data_path) & "DATA" %in% names(ctl)) { # & !from_staging){
    ## NOTE: this assumes run is from same run_in(m) directory
    data_name <- gsub("^ *\\$DATA\\s*([^ ]+).*$", "\\1", ctl$DATA)[1]
    data_path <- normalizePath(file.path(run_in(m), data_name), mustWork = FALSE)

    if (file.exists(data_path)) {
      ## issue - run_in might not exist
      ## remove dir/.. segments
      while (grepl("\\.\\.", data_path)) {
        data_path <- gsub(
          paste0("[^", .Platform$file.sep, "\\.]+", .Platform$file.sep, "\\.\\.", .Platform$file.sep),
          "",
          data_path
        )
      }

      data_path <- relative_path(data_path, getwd())
      m[["data_path"]] <- data_path
    }
  }

  if (!is.na(data_path) & update_dollar_data) {
    m <- m %>% fill_dollar_data(data_path)
  }

  m
}
#' @export
based_on.nm_list <- Vectorize_nm_list(based_on.nm_generic, SIMPLIFY = FALSE, replace_arg = "ctl_ob", pre_glue = TRUE)

#' Get/set control file contents
#'
#' This function is an alias for [based_on()].
#'
#' @param ... Arguments to be passed to [based_on()].
#' @return An nm object with modified `ctl_contents` field.
#'
#' @examples 
#' 
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#'              
#' m1 %>% ctl_contents()
#'
#' @export

ctl_contents <- function(...) based_on(...)

## minimal version of ctl_contents (just sets ctl_contents without mods)
ctl_contents_simple <- function(m, ctl_ob, ...) {
  ctl_contents(m, ctl_ob, update_numbering = FALSE, update_dollar_data = FALSE, ...)
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
#' @return If `...` contains an assignment, an nm object with modified field,
#'   otherwise returns the field value.
#'
#' @examples
#' 
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#'
#' m1 <- m1 %>% simple_field(stars = 3)
#' m1 %>% simple_field(stars)
#' m1 ## see that stars is a field of the nm object.
#' 
#' @export

simple_field <- function(m, ...) {
  dots_exp <- rlang::enexprs(...)
  if (identical(names(dots_exp), "")) {
    get_simple_field(m, ...)
  } else {
    set_simple_field(m, ...)
  }
}


set_simple_field <- function(m, ...) {
  dots <- list(...)
  fields <- names(dots)

  for (i in seq_along(dots)) {
    m <- m %>% custom_1d_field(field = fields[i], replace = dots[[i]])
  }
  m
}


get_simple_field <- function(m, field) {
  field <- rlang::enquo(field)
  field <- rlang::quo_name(field)

  m %>% custom_1d_field(field = field)
}

custom_1d_field <- function(m, field, replace, glue = FALSE) {
  UseMethod("custom_1d_field")
}
custom_1d_field.nm_generic <- function(m, field, replace, glue = FALSE) {
  if (missing(replace)) {
    if (length(m[[field]]) > 0) {
      return(m[[field]])
    } else {
      return(NA_character_)
    }
  }

  ## Only update if there is a change
  if (field %in% names(m)) {
    if (glue) old_glue_field <- m$glue_fields[[field]] else old_glue_field <- m[[field]]
    if (identical(replace, old_glue_field)) {
      return(m)
    }
  }

  if (glue) {
    m$glue_fields[[field]] <- replace
    m[[field]] <- replace
    ## glue the field
    if (!is.na(replace)) m <- replace_tags(m, field)
  } else {
    m[[field]] <- replace
  }
  ## reglue all other glueable fields
  for (other_col in names(m$glue_fields)[!names(m$glue_fields) %in% field]) {
    m <- replace_tags(m, other_col)
  }

  m
}
custom_1d_field.nm_list <- Vectorize_nm_list(custom_1d_field.nm_generic, replace_arg = "replace")
