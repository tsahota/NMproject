#' @include utils.R
#' @include Vectorize.R

NULL

#' @rdname is_nm
#' @name is_nm
#'
#' @title Test if object is an nm coercible object
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Mostly internal functions to test object types.
#'
#' @param x Object.
#'
#' @export
is_nm_list <- function(x) {
  if (inherits(x, "nm_list")) {
    return(TRUE)
  }
  if (!inherits(x, "list") & !inherits(x, "environment")) {
    return(FALSE)
  }

  is_valid_subobject <- function(x) {
    # if(is_single_na(x)) return(TRUE) ## na's allowed
    inherits(x, "nm_generic")
  }

  all(sapply(x, is_valid_subobject))
}

#' Coerce object into nm_list
#'
#' This may be relegated to a back end function soon.  Create an nm_list from an
#' nm_generic or list or other object coercible into an nm_list.
#'
#' @param m An object coercible into an nm object.
#' @keywords internal
#' @export
as_nm_list <- function(m) {
  UseMethod("as_nm_list")
}

#' @export
as_nm_list.nm_list <- function(m) {
  m
}
#' @export
as_nm_list.list <- function(m) {
  if (is_nm_list(m)) {
    class(m) <- c("nm_list", "list")
    names(m) <- unique_id(m)
    return(m)
  } else {
    stop("list not coercible to nm_list")
  }
}
#' @export
as_nm_list.nm_generic <- function(m) {
  m <- list(m)
  as_nm_list.list(m)
}

#' @rdname is_nm
#'
#' @export

is_nm_generic <- function(x) {
  inherits(x, "nm_generic")
}

#' Convert nm object to nm_generic
#'
#' Used where methods for nm_list don't exist or aren't appropriate.
#'
#' @param m An nm object.
#'
#' @keywords internal
#' @export
as_nm_generic <- function(m) {
  UseMethod("as_nm_generic")
}
#' @export
as_nm_generic.nm_generic <- function(m) m
#' @export
as_nm_generic.nm_list <- function(m) {
  if (length(m) > 1) stop("cannot coerce more than one object to nm_generic")
  m <- m[[1]]
  if (!inherits(m, "nm_generic")) stop("could not coerce to nm_generic")
  m
}

#' @export
c.nm_list <- function(...) {
  ## try append
  basic_list <- lapply(list(...), function(ob) {
    class(ob) <- "list"
    ob
  })
  basic_list <- do.call(c, basic_list)
  as_nm_list(basic_list)
  #
  # basic_list <- lapply(list(...), '[[', i = 1)
  # as_nm_list(basic_list)
}

#' @export
c.nm_generic <- function(...) {
  basic_list <- list(...)
  class(basic_list) <- c("nm_list", "list")
  # as_nm_list(basic_list)
  basic_list
}

#' @export
"[.nm_list" <- function(x, i, j, ...) {
  class(x) <- "list"
  val <- NextMethod()
  class(val) <- c("nm_list", "list")
  # val <- as_nm_list(val)
  val
}



#' @export
unique.nm_list <- function(x, incomparables = FALSE, ...) {
  class(x) <- "list"
  val <- NextMethod()
  class(val) <- c("nm_list", "list")
  # val <- as_nm_list(val)
  val
}

#' @export
is.na.nm_generic <- function(x) is.na(run_id(x))
#' @export
is.na.nm_list <- function(x) is.na(run_id(x))

#' @export
print.nm_generic <- function(x, ...) {
  x <- printable_nm_generic(x)

  str_ob <- utils::capture.output(utils::str(x, ...))
  str_ob <- gsub(
    "(.*?)\"\\.{3}\\[(NA.*)\\].*",
    paste0("\\1", crayon::underline("\\2")), str_ob
  )
  str_ob <- gsub(
    "(.*?)\"\\.{3}\\[(collapsed.*)\\].*",
    paste0("\\1", crayon::green("\\2")), str_ob
  )
  cat(str_ob, sep = "\n")
}

#' @export
print.nm_list <- function(x, ...) {
  for (i in seq_along(x)) {
    x[[i]] <- printable_nm_generic(x[[i]])
  }

  str_ob <- utils::capture.output(utils::str(x, ...))
  ## post str modification
  str_ob <- gsub(":List of.*", "", str_ob)
  str_ob <- gsub(
    "(.*?)\"\\.{3}\\[(NA.*)\\].*",
    paste0("\\1", crayon::underline("\\2")), str_ob
  )
  str_ob <- gsub(
    "(.*?)\"\\.{3}\\[(collapsed.*)\\].*",
    paste0("\\1", crayon::green("\\2")), str_ob
  )
  cat(str_ob, sep = "\n")
}

printable_nm_generic <- function(x) {
  pretty_empty_fields <- c("ctl_contents", "data_path", "cmd")
  pretty_empty_fill_f <- c("based_on", "data_path", "cmd")

  ## included even if NA
  minimum_fields <- c("run_id", pretty_empty_fields)

  for (field in names(x)) {
    if (!field %in% minimum_fields) {
      if (is_single_na(x[[field]])) x[[field]] <- NULL
    }
  }

  ## if ctl_contents is NA, remove ctl_name
  if (is_single_na(x[["ctl_contents"]])) {
    x[["ctl_name"]] <- NULL
    x[["glue_fields"]][["ctl_name"]] <- NULL
  }

  for (j in seq_along(pretty_empty_fields)) {
    pretty_empty_field <- pretty_empty_fields[j]
    pretty_empty_f <- pretty_empty_fill_f[j]
    if (is_single_na(x[[pretty_empty_field]])) {
      x[[pretty_empty_field]] <-
        paste0("...[NA - fill with ", pretty_empty_f, "()]...")
    }
  }

  collapse_fields <- c("ctl_contents", "ctl_orig")
  for (field in collapse_fields) {
    ## special handling of these two
    if (field %in% names(x)) {
      if (length(x[[field]]) > 1) {
        x[[field]] <- paste0("...[collapsed - view with ", field, "()]...")
      }
    }
  }
  # remove all raw fields from output
  remove_fields <- c("glue_fields")
  for (field in remove_fields) x[[field]] <- NULL

  x
}
