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
#'   `ctl_name` field (see Details below).
#' @param run_id Character. Run identifier. This is used to name the run and
#'   output files such as $TABLE outputs.
#' @param data_path Character. Path to dataset. If this is not specified,
#'   NMproject will try to guess based on the current $DATA components of the
#'   file specified in `based_on`.  However it is recommended to specify this
#'   explicitly as a relative path.
#' @param cmd Optional character. PsN command to use. If unspecified will use
#'   `getOption("nm_default_fields")` value of `cmd`. Use glue notation for
#'   inheritance.  See details.
#'
#' @details The `cmd` field uses `glue` notation.  So instead of specifying
#'   `execute runm1.mod -dir=m1`, it is best to specify `execute {ctl_name}
#'  -dir={run_dir}`.  The values of `ctl_name` and `run_dir` refer to object
#'  fields and if these change value like when the `child()` function is used to
#'  create a separate child object, the `cmd` field will update automatically.
#'
#' @section Object fields:
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
#' @return An object of class `nm_list`.  Attributes can be viewed by printing
#'   the object in the console.
#' @seealso [nm_getsetters()], [child()]
#' @examples 
#' 
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#'
#' m1 ## display object fields
#' cmd(m1)
#' ctl_name(m1)
#' run_dir(m1)
#' 
#' @export
new_nm <- function(based_on,
                   run_id = NA_character_,
                   data_path,
                   cmd) {
  m <- nm(run_id = run_id)
  if (!missing(based_on)) m <- m %>% based_on(based_on)
  if (!missing(data_path)) m <- m %>% data_path(data_path)
  if (!missing(cmd)) m <- m %>% cmd(cmd)
  m
}

## internal function
nm_generic <- function(run_id = NA_character_,
                       run_in = nm_dir("models"),
                       parent_run_id = NA_character_,
                       parent_run_in = NA_character_,
                       parent_ctl_name = NA_character_,
                       parent_results_dir = NA_character_,
                       ctl_name = "run{run_id}.mod",
                       type = "execute",
                       run_dir = "{run_id}",
                       results_dir = nm_dir("results"),
                       lst_path = "{run_dir}/NM_run1/psn.lst") {

  # m <- new.env()
  m <- list()
  # if(is.na(run_id)) stop("require run_id argument to be specified", call. = FALSE)

  ## test if run_id = "execute run... ..." stop with error
  ## about backward compatibility issue
  ##  1. it has spaces
  if (grepl("\\s", run_id)) {
    stop("run_id cannot contain spaces.
NOTE: If you are trying to do something like:
         nm('execute run1.mod -dir=1')
You are trying to use the old NMproject alpha interface.
This is the newer beta interface which is not backwards compatible. 
To use the alpha interface, install NMproject 0.3.2",
      call. = FALSE
    )
  }

  class_name <- paste0("nm_", type)
  class(m) <- c(class_name, "nm_generic", class(m))

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
  m$cmd <- NA_character_
  m$cores <- as.integer(1)
  m$parafile <- "path/to/parafile.pnm"
  m$walltime <- NA_integer_

  unique_id <- "{type}.{run_in}{.Platform$file.sep}{run_dir}"
  ## the following is in order of glueing
  m$glue_fields <- list(
    run_dir, ctl_name, results_dir, unique_id,
    lst_path, NA_character_, NA_character_
  )
  names(m$glue_fields) <- c(
    "run_dir", "ctl_name", "results_dir", "unique_id",
    "lst_path", "data_path", "cmd"
  )
  
  ## look through nm_default_fields and set glue_fields before replacing glue tags
  default_fields <- nm_default_fields()
  default_glue <- names(default_fields) %in% names(m$glue_fields)
  
  for (i in seq_along(default_fields)) {
    glue <- default_glue[i]
    value <- default_fields[[i]]
    name <- names(default_fields)[i]
    if (glue) {
      m$glue_fields[[name]] <- value
    } else {
      m[[name]] <- value
    }
  }

  for (field in names(m$glue_fields)) {
    m <- replace_tag(m, field)
  }

  m
}

#' Create core NM object
#'
#' Create new nm object. The function [new_nm()] is more convenient.  This is
#' mostly a back end function. This is the basic object this package centres
#' around.  Most package functions act on this object.
#'
#' @param run_id Character vector. Run identifier.
#' @param run_in Character vector. The location to copy the NONMEM control file
#'   and run location.
#' @param parent_run_id Character vector (optional). The run identifier of the
#'   parent run.
#' @param parent_run_in Character vector (optional). The location of the parent
#'   run.
#' @param parent_ctl_name Character vector (optional). The `ctl_name` of the
#'   parent run.
#' @param parent_results_dir Character vector (optional). The `results_dir` of
#'   the parent run.
#' @param ctl_name Character. Name of control file.
#' @param type Character (default = `"execute"`).  Type of run to run.
#' @param run_dir Character (default = `"{run_id}"`).  Subdirectory where PsN
#'   will run NONMEM.
#' @param results_dir Character (default = `"Results"`). Directory to store
#'   results of this run.
#' @param lst_path Character (default = `"{run_dir}/NM_run1/psn.lst"`) expected
#'   location of .lst file.
#'
#' @return An object of class `nm_list`.  Object is concatenatable. Length of
#'   object corresponds to length of `run_id`.
#' @examples
#'
#' m0 <- nm(run_id = "m0")
#' m0 ## a human readable representation
#'
#' ## nm objects can be put into tibbles to group runs together
#' d <- dplyr::tibble(run_id = c("m1", "m2"))
#' d$m <- nm(d$run_id)
#' d
#'
#' @keywords internal
#' @export
nm <- Vectorize_nm_list(nm_generic, SIMPLIFY = FALSE)


#' Make child nm object from parent
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Child objects inherit attributes of parent but with a new `run_id`. The control
#' file will be inherited too with $TABLEs updated.
#'
#' @param m Parent nm object.
#' @param run_id Character.  New `run_id` to assign to child object.
#' @param type Character (default = `"execute"`). Type of child object.
#' @param parent Optional nm object (default = `nm(NA)`) . Parent object will by
#'   default be `m`, but this argument will force parent to be a different
#'   object.
#' @param silent Logical (default = `FALSE`). Should warn if conflicts detected.
#'
#' @details Specifying `parent` will force parent to be different from `m`. This
#'   is useful in piping when a parent object is modified prior to being used in
#'   the child object.
#'
#' @return An new nm object with modified `parent_*` fields updated to be the
#'   `*` fields of the parent object, `m`.
#'
#' @examples
#' 
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#'
#' m2 <- m1 %>% child("m2")
#' 
#' nm_diff(m2, m1)
#'
#' @export
child <- function(m, run_id = NA_character_, type = "execute", parent = nm(NA), silent = FALSE) {
  UseMethod("child")
}
#' @export
child.nm_generic <- function(m, run_id = NA_character_, type = "execute", parent = nm(NA), silent = FALSE) {
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
  if (is.na(run_id)) {
    if (!is.na(run_id(m))) {
      run_id <- increment_run_id(run_id(m))
      message("child run_id automatically set to: ", run_id)
    }
  }
  if (!is.na(run_id)) m <- m %>% run_id(run_id)

  m <- m %>% ctl_contents(ctl_contents(m),
    update_numbering = TRUE,
    update_dollar_data = FALSE
  )
  if (!type %in% "execute") m <- m %>% type(type)
  m[["ctl_orig"]] <- m[["ctl_contents"]] ## reset ctl_orig

  ## check for file conficts
  if (!is_single_na(m[["ctl_contents"]])) {
    file_conflicts <- intersect(psn_exported_files(mparent), psn_exported_files(m))
    if (length(file_conflicts) > 0) {
      if (!silent) {
        warning("Child file(s) currently in conflict with parent:\n",
          paste(paste0(" ", file_conflicts), collapse = "\n"),
          "\nYou will overwrite parent object outputs if you run now",
          call. = FALSE
        )
      }
    }
  }

  ## warn if ctl_name or run_dir aren't glueable
  relevant_glue_fields <- unlist(m$glue_fields[c("ctl_name", "run_dir")])
  non_glued_glue_fields <- relevant_glue_fields[!grepl("\\{", relevant_glue_fields)]
  if (length(non_glued_glue_fields) > 0) {
    if (!silent) {
      warning("Following parents attributes do not use {glue} fields:\n",
        paste(paste0(" ", names(non_glued_glue_fields)), collapse = "\n"),
        "\nThese fields will be identical to parent and may result in conflicts",
        "\nIf this is unintended, make sure parent object uses {glue} notation for these attributes",
        call. = FALSE
      )
    }
  }

  if (!is.na(parent)) m <- m %>% change_parent(parent)

  m
}
#' @export
child.nm_list <- Vectorize_nm_list(child.nm_generic, SIMPLIFY = FALSE)

increment_run_id <- function(run_id) {
  if (length(run_id) != 1) stop("run_id must be of length 1")
  stop_msg <- paste0("cannot automatically increment ", run_id, ", specify run_id argument explicitly")

  # number at end
  run_id_regex <- "^(\\w*?)([0-9]+)$"
  if (!grepl(run_id_regex, run_id)) stop(stop_msg)

  prefix_part <- gsub("^(\\w*?)([0-9]+)$", "\\1", run_id)
  numeric_part <- gsub("^(\\w*?)([0-9]+)$", "\\2", run_id)
  numeric_part <- as.numeric(numeric_part)

  paste0(prefix_part, numeric_part + 1)
}

#' Change parent object
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' When a `child()` has been created by a modified parent sometimes adoption is
#' the answer.  This will force parenthood to the specified object.
#'
#' @param m An nm object.
#' @param mparent New parent object to child.
#' @param silent Logical (default = `TRUE`). Should warn if conflicts detected.
#'
#' @return An nm object with modified `parent_*` fields.
#' 
#' @seealso [child()]
#'
#' @keywords internal
change_parent <- function(m, mparent, silent = TRUE) {
  UseMethod("change_parent")
}

change_parent.nm_generic <- function(m, mparent, silent = TRUE) {
  m <- m %>% parent_run_id(run_id(mparent))
  m <- m %>% parent_run_in(run_in(mparent))
  m <- m %>% parent_ctl_name(ctl_name(mparent))
  m <- m %>% parent_results_dir(results_dir(mparent))

  m[["ctl_orig"]] <- mparent[["ctl_contents"]] ## reset ctl_orig

  ## check for file conficts
  if (!is_single_na(m[["ctl_contents"]])) {
    file_conflicts <- intersect(psn_exported_files(mparent), psn_exported_files(m))
    if (length(file_conflicts) > 0) {
      if (!silent) {
        warning("new parent file(s) currently in conflict with parent:\n",
          paste(paste0(" ", file_conflicts), collapse = "\n"),
          "\nYou will overwrite parent object outputs if you run now",
          call. = FALSE
        )
      }
    }
  }

  m
}

change_parent.nm_list <- Vectorize_nm_list(change_parent.nm_generic, SIMPLIFY = FALSE)

#' Get parent object of nm object
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Will pull the parent run of an nm object from the run cache.  Run needs to
#' have been executed for this to work.
#'
#' @param m An nm object.
#' @param n Numeric. Generation of parent (default = `1`).
#'
#' @return An nm object.  Will not return parent object, if the parent object
#'   has not been run.
#' 
#' @export
parent_run <- function(m, n = 1L) {
  UseMethod("parent_run")
}
#' @export
parent_run.nm_generic <- function(m, n = 1L) {
  if (n == 0) {
    return(as_nm_generic(nm(NA)))
  }

  hack_m <- m %>%
    run_id(parent_run_id(m)) %>%
    run_in(parent_run_in(m))

  pattern <- hack_m %>%
    unique_run_cache_path() %>%
    basename()
  pattern <- paste0("^", pattern, "$")

  ## sort by most recent

  dir_list <- dir(".cache", pattern = pattern, full.names = TRUE)

  file_info <- file.info(dir_list)

  md5_files <- dir_list[order(file_info$mtime, decreasing = TRUE)]

  ## get proposed parent objects and update
  if (length(md5_files) == 0) {
    return(as_nm_generic(nm(NA)))
  }

  md5_file <- md5_files[1]
  parent_ob <- readRDS(md5_file)$object

  if (n > 1) {
    return(parent_run(parent_ob, n = n - 1))
  }
  parent_ob
}
#' @export
parent_run.nm_list <- Vectorize_nm_list(parent_run.nm_generic, SIMPLIFY = FALSE)


glue_text_nm <- function(m, text) {
  UseMethod("glue_text_nm")
}
glue_text_nm.nm_generic <- function(m, text) {
  stringr::str_glue(text, .envir = m, .na = NULL)
}
glue_text_nm.nm_list <- Vectorize_nm_list(glue_text_nm.nm_generic, SIMPLIFY = TRUE)

replace_tag <- function(m, field) {
  ## this function is rate limiting - use it as little as possible.
  ## only proceed if "raw" field exists
  if (!is.na(m$glue_fields[[field]])) {
    ## start by resetting to raw
    m[[field]] <- glue_text_nm(m, m$glue_fields[[field]])
    # m[[field]] <- stringr::str_glue(m$glue_fields[[field]], .envir = m)
    m[[field]] <- as.character(m[[field]])
  }
  m
}

replace_tags <- function(m, field) {
  ## this function is rate limiting - use it as little as possible.
  ## only proceed if "raw" field exists
  ## this will update all tags
  if (field %in% names(m$glue_fields)) {
    m <- replace_tag(m, field)
  }
  m
}


glue_fields <- function(m) {
  UseMethod("glue_fields")
}

glue_fields.nm_generic <- function(m) m$glue_fields
glue_fields.nm_list <- Vectorize_nm_list(glue_fields.nm_generic, SIMPLIFY = FALSE)

custom_vector_field <- function(m, field, replace) {
  UseMethod("custom_vector_field")
}
custom_vector_field.nm_generic <- function(m, field, replace) {
  if (missing(replace)) {
    if (length(m[[field]]) > 0) {
      return(m[[field]])
    } else {
      return(NA_character_)
    }
  }
  m[[field]] <- replace
  m
}
custom_vector_field.nm_list <- Vectorize_nm_list(custom_vector_field.nm_generic, SIMPLIFY = FALSE, replace_arg = "replace", vectorize.args = c("m", "field"))

#' Convert nm objects to a row-wise tibble/data.frame
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Primarily an internal function. Converts an `nm_list` object to a `tibble`.
#'
#' @param m An nm object.
#'
#' @return A wide format `tibble` with `length(m)` rows where one dimensional
#'   fields are represented as columns.
#' @keywords internal
#' @export
nm_row <- function(m) {
  UseMethod("nm_row")
}
#' @export
nm_row.nm_generic <- function(m) {
  m_orig <- m
  m <- as.list(m)
  eligible_rows <- sapply(m, function(field) {
    length(field) == 1
  })
  d <- dplyr::as_tibble(m[eligible_rows])
  d
}
#' @export
nm_row.nm_list <- function(m) {
  d <- lapply(m, nm_row)
  d <- suppressWarnings(dplyr::bind_rows(d))
  d
}

nm_list2list <- function(m) {
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
#' @param x An environment (default = `.GlobalEnv`) to search
#'   or `data.frame` with (`nm_list` column) or `nm_list`.
#'
#' @return A single `nm_list` object with all model objects in environment `x`.
#'
#' @examples 
#' 
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#'
#' m2 <- m1 %>% child("m2")
#' 
#' m_all <- nm_list_gather()
#' 
#' identical(
#'   m_all %>% subset(run_id(m_all) %in% "m1"),
#'   m1
#' )
#' 
#' @export
nm_list_gather <- function(x = .GlobalEnv) {
  UseMethod("nm_list_gather")
}

#' @export
nm_list_gather.default <- function(x = .GlobalEnv) {
  m <- lapply(x, function(object) {
    if (inherits(object, "nm_list")) object else NA
  })

  m <- m[!is.na(m)]
  m <- do.call(c, m)
  m
}

#' @export
nm_list_gather.nm_list <- function(x = .GlobalEnv) x
