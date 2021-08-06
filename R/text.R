ctl_list2 <- function(r) {
  UseMethod("ctl_list2")
}

ctl_list2.ctl_list <- function(r) r

ctl_list2.character <- function(r) {
  if (length(r) == 1) {
    ctl <- trimws(readLines(r), which = "right")
  } else {
    ctl <- r
  }
  ctl_nm2r(ctl)
}

ctl_list2.nm_generic <- function(r) r[["ctl_contents"]]

ctl_list2.nm_list <- Vectorize_nm_list(ctl_list2.nm_generic, SIMPLIFY = FALSE)

#' @rdname target
#' @name target
#'
#' @title Target part of control object for further modification
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Intended mostly for internal use.  Used with [text()] to target control file
#' modifications to specific NONMEM subroutines
#'
#' @param m An nm object.
#' @param dollar Character. Name of subroutine to target.
#' @param lines Optional character.  Assignment of lines.
#' 
#' @return An nm object with modified `target` field.
#' @keywords internal
#' @export
target <- function(m, dollar, lines) {
  UseMethod("target")
}
#' @export
target.nm_generic <- function(m, dollar, lines) {
  if (missing(dollar)) {
    if (length(m[["target"]]) > 0) {
      return(m[["target"]])
    } else {
      return(NA_character_)
    }
  }

  # if(!missing(dollar) & !missing(lines)){
  #  stop("can't have both 'dollar' and 'lines' arguments")
  # }
  dollar <- toupper(dollar)

  dollar_text <- gsub("\\$", "", dollar)
  m <- m %>% custom_1d_field(field = "target", replace = dollar_text)
  # m <- m %>% custom_vector_field(field = "target", replace = list(dollar_text))

  # if(!missing(dollar)){
  #  dollar_text <- gsub("\\$","",dollar)
  #  m <- m %>% custom_1d_field(field = "target_dollar", replace = dollar_text)
  # }

  # if(!missing(lines)){
  #  m <- m %>% custom_vector_field(field = "target_lines", replace = list(lines))
  # }


  m
}
#' @export
target.nm_list <- Vectorize_nm_list(target.nm_generic, replace_arg = "dollar")
# target.nm_list <- Vectorize_nm_list(target.nm_generic, SIMPLIFY = FALSE, replace_arg = "dollar")

#' @rdname target
#'
#' @export
untarget <- function(m, dollar) {
  UseMethod("untarget")
}
#' @export
untarget.nm_generic <- function(m, dollar) {
  m[["target"]] <- NA_character_
  m
}
#' @export
untarget.nm_list <- Vectorize_nm_list(untarget.nm_generic, SIMPLIFY = FALSE)

get_target_text <- function(m) {
  ## m is nm_generic
  ctl <- ctl_contents(m)
  target <- target(m) # m[["target"]]
  if (!is.na(target)) {
    if (target %in% names(ctl)) text <- ctl[[target]] else text <- NA_character_
  } else {
    text <- ctl_character(ctl)
  }
  text <- as.character(text)
  class(text) <- c("nm_ctl_text", class(text))
  text
}

#' @export
print.nm_ctl_text <- function(x, ...) {
  cat(paste0(format(seq_along(x), width = 3), "| ", x), sep = "\n")
}

set_target_text <- function(m, text) {
  ## m is nm_generic
  ctl <- ctl_contents(m)
  target <- target(m) # m[["target"]]
  if (!is.na(target)) {
    # if(append) text <- c(ctl[[target]],"",text)
    ctl[[target]] <- setup_dollar(text, paste0("$", target), add_dollar_text = FALSE)
    m <- m %>% ctl_contents_simple(ctl)
  } else {
    # if(append) text <- c(ctl_character(ctl),"",text)
    text <- ctl_list2(text)
    # text <- ctl_nm2r(text)
    m[["ctl_contents"]] <- text
  }
  m
}

#' @importFrom graphics text
#' @export
text.nm_generic <- function(x, text, append = FALSE, after = character(), add_dollar_text = FALSE, ...) {
  m <- x
  current_text <- get_target_text(m)
  if (missing(text)) {
    return(current_text)
  }

  text <- paste(text, collapse = "\n")
  text <- strsplit(text, split = "\n")[[1]]
  text <- trimws(text)

  if (length(after)) append <- TRUE
  if (append) {
    if (!length(after)) text <- c(current_text, text)

    if (length(after)) {
      matches <- grep(after, current_text)
      if (!length(matches)) stop("cannot find 'after'")
      matches <- matches[1]
      text <- append(current_text, text, after = matches)
    }
  }


  if (is.na(target(m))) {
    # stop("not developed yet")
  } else {
    text <- setup_dollar(text, paste0("$", target(m)), add_dollar_text = add_dollar_text)
  }
  m <- m %>% set_target_text(text)
  m
}
#' @export
text.nm_list <- Vectorize_nm_list(text.nm_generic, SIMPLIFY = FALSE, vectorize.args = c("x"))
