is_single_na <- function(x) if (length(x) == 1) is.na(x) else FALSE

#' Test if full path
#'
#' @param x String giving file/path name.
#' @return `TRUE` only when path starts with ~, /, \\ or X: (i.e. when x is a full path), FALSE otherwise.
#' @keywords internal
is_full_path <- function(x) grepl("^(~|/|\\\\|([a-zA-Z]:))", x, perl = TRUE)


#' Check if git is available on command line
#' @keywords internal
git_cmd_available <- Sys.which("git") != ""

#' Pipe operator
#'
#' See `magrittr::[\%>\%][magrittr::pipe]` for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' @importFrom magrittr %<>%
#' @export
magrittr::"%<>%"

#' @importFrom magrittr %T>%
#' @export
magrittr::"%T>%"

#' @importFrom magrittr %$%
#' @export
magrittr::"%$%"

#' @importFrom rlang .data
#' @export
rlang::.data


#' Function pipe for nm objects
#' 
#' @description
#'
#' `r lifecycle::badge("experimental")`
#' 
#' Pipe an nm object object to a list of functions.  Although this enables
#' multiple NONMEM runs to be handled simulataneously, it does make your code
#' less readable.
#' 
#' @param lhs An nm object.
#' @param rhs A list of functions.  Must be same length as `lhs`.
#' 
#' @return A modified nm object.
#' 
#' @seealso [child()] for creating multiple child NONMEM objects
#' 
#' @examples 
#' 
#' \dontrun{
#' mWT <- m2 %>% child(c("m3", "m4", "m5", "m6", "m7", "m8")) %f>% 
#' list(
#'  . %>% add_cov(param = "CL", cov = "WT", state = "power"),
#'  . %>% add_cov(param = "CL", cov = "WT", state = "power0.75"),
#'  . %>% add_cov(param = "V", cov = "WT", state = "power"),
#'  . %>% add_cov(param = "V", cov = "WT", state = "power1"),
#'  . %>% add_cov(param = "CL", cov = "WT", state = "power") %>%
#'    add_cov(param = "V", cov = "WT", state = "power"),
#'  . %>% add_cov(param = "CL", cov = "WT", state = "power0.75") %>%
#'   add_cov(param = "V", cov = "WT", state = "power1")
#' )
#' 
#' mWT %>% run_nm() %>% wait_finish()
#' 
#' mWT %>% summary_wide()
#' 
#' }
#' 
#' @export

`%f>%` <- function(lhs, rhs) {
  if(length(lhs) != length(rhs)) {
    stop("for 'lhs %f>% rhs' the length of lhs and rhs must be the same")
  }
  as_nm_list(lapply(seq_along(lhs), function(i) rhs[[i]](lhs[[i]])))
}

#' Compute path relative to reference
#'
#' @param path Path of desired directory or file.
#' @param relative_path Path of directory to travel from.
#'
#' @keywords internal
#' @export

relative_path <- function(path, relative_path) {
  file.exists_path <- file.exists(path)
  file.exists_relative_path <- file.exists(relative_path)

  if (!file.exists_path) path <- file.path(getwd(), path) ## if doesn't exist assume relative path
  if (!file.exists_relative_path) relative_path <- file.path(getwd(), relative_path)

  mainPieces <- strsplit(normalizePath(path, mustWork = FALSE, winslash = "/"), .Platform$file.sep, fixed = TRUE)[[1]]
  refPieces <- strsplit(normalizePath(relative_path, mustWork = FALSE, winslash = "/"), .Platform$file.sep, fixed = TRUE)[[1]]

  # if(!file.exists_path) unlink(path)
  # if(!file.exists_relative_path) unlink(relative_path)

  shorterLength <- min(length(mainPieces), length(refPieces))

  last_common_piece <- max(which(mainPieces[1:shorterLength] == refPieces[1:shorterLength]), 1)

  dots <- refPieces[!seq_along(refPieces) %in% 1:last_common_piece]
  dots <- rep("..", length(dots))

  mainPieces <- mainPieces[!seq_along(mainPieces) %in% 1:last_common_piece]

  relativePieces <- c(dots, mainPieces)
  do.call(file.path, as.list(relativePieces))
}

relative_path <- Vectorize(relative_path, USE.NAMES = FALSE)

file.exists2 <- function(x) { ## only true is file exists and is not a directory
  if (!file.exists(x)) {
    return(FALSE)
  }
  !file.info(x)$isdir
}

#' List directories
#'
#' Wrapper around `list.dirs()` but includes `maxdepth` and pattern arguments and
#' removes `full.names` argument, always return full names.
#'
#' @param path Same as `list.dirs()`.
#' @param full.names Same as `list.dirs()`.
#' @param recursive Same as `list.dirs()`.
#' @param maxdepth Integer (default = `1`) maximum depth to search.
#' @param pattern Optional character (default is missing) regex pattern match on directory
#'   name.
#'
#' @keywords internal
list_dirs <- function(path = ".", full.names = TRUE, recursive = FALSE, maxdepth = 1, pattern) {
  dirs <- list()
  dirs[[1]] <- list.dirs(path, full.names = TRUE, recursive = FALSE)

  missing_pattern <- missing(pattern)

  return_ready <- function(dirs) { ## prep for return character vector
    dirs <- unlist(dirs)
    if (!missing_pattern) dirs <- dirs[grepl(pattern, basename(dirs))]
    if (!full.names) dirs <- basename(dirs)
    dirs
  }

  if (maxdepth == 1 | !recursive) {
    return(return_ready(dirs))
  }

  i <- 2
  while (i <= maxdepth) {
    # for(i in 2:maxdepth){
    current_dirs <- sapply(dirs[[i - 1]], function(path) {
      list.dirs(path, full.names = TRUE, recursive = FALSE)
    })
    current_dirs <- unlist(current_dirs)
    names(current_dirs) <- NULL

    ## breakpoint if no more dirs
    if (length(current_dirs) == 0) {
      return(return_ready(dirs))
    }

    dirs[[i]] <- current_dirs
    i <- i + 1
  }

  return(return_ready(dirs))
}



#' Logical flag for detecting if R session is on RStudio
#'
#' @keywords internal
#' @export
is_rstudio <- function() Sys.getenv("RSTUDIO") == "1"

## experimental - goes against dplyr, maybe delete if not useful
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  condition <- condition %in% TRUE
  .data[condition, ] <- .data[condition, ] %>% dplyr::mutate(...)
  .data
}

gsub_in_brackets <- function(pattern, replacement, x) {
  x <- gsub("\\(", "~(", x)
  x <- gsub("\\)", ")~", x)
  x <- paste0(x, collapse = "\n")
  x <- strsplit(x, split = "~")[[1]]
  x[grepl("^\\(.*\\)", x)] <-
    gsub(pattern, replacement, x[grepl("^\\(.*\\)", x)])
  x <- paste(x, collapse = "")
  x <- paste(x, " ") ## added to make sure final \n doesn't shorten vector
  x <- strsplit(x, split = "\n")[[1]]
  x[length(x)] <- trimws(x[length(x)])
  x
}

gsub_out_brackets <- function(pattern, replacement, x) {
  x <- gsub("\\(", "~(", x)
  x <- gsub("\\)", ")~", x)
  x <- paste0(x, collapse = "\n")
  x <- strsplit(x, split = "~")[[1]]
  x[!grepl("^\\(.*\\)", x)] <-
    gsub(pattern, replacement, x[!grepl("^\\(.*\\)", x)])
  x <- paste(x, collapse = "")
  x <- paste(x, " ") ## added to make sure final \n doesn't shorten vector
  x <- strsplit(x, split = "\n")[[1]]
  x[length(x)] <- trimws(x[length(x)])
  x
}

na.locf <- function(x) {
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v) + 1]
}


#' Wait for statement to be TRUE
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Will block R console until an expression evaluates to be `TRUE`.
#'
#' @param x Boolean expression to evaluate.
#' @param timeout Numeric. Maximum time (in seconds) to wait.
#' @param interval Numeric. Number of seconds (default=`1`) to wait before
#'   rechecking.
#'
#' @seealso [wait_finish()].
#'
#' @examples
#'
#' \dontrun{
#'
#' ## the following are identical
#'
#' wait_finish(m1)
#' wait_for(is_finished(m1))
#' }
#'
#' @export
wait_for <- function(x, timeout = NULL, interval = 1) {
  x <- substitute(x)
  start.time <- Sys.time()
  diff.time <- 0
  while (!eval(x, envir = parent.frame())) {
    diff.time <- difftime(Sys.time(), start.time, units = "secs")
    if (!is.null(timeout)) {
      if (diff.time > timeout) {
        warning(paste("timed out waiting for\n", x, sep = ""))
        return(invisible(FALSE))
      }
    }
    Sys.sleep(1)
  }
  invisible(TRUE)
}
