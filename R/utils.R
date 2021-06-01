is_single_na <- function(x) if(length(x) == 1) is.na(x) else FALSE

#' Test if full path
#'
#' @param x String giving file/path name.
#' @return `TRUE` only when path starts with ~, /, \\ or X: (i.e. when x is a full path), FALSE otherwise.
#' @examples
#' \dontrun{
#' is_full_path('file.text.ext')
#' is_full_path('/path/to/file.text.ext')
#' }
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
magrittr::'%<>%'

#' @importFrom magrittr %T>%
#' @export
magrittr::'%T>%'

#' @importFrom magrittr %$%
#' @export
magrittr::'%$%'

#' @importFrom rlang .data
#' @export
rlang::.data

#' Compute path relative to reference
#'
#' @param path Path of desired directory or file.
#' @param relative_path Path of directory to travel from.
#'
#' @keywords internal
#' @export

relative_path <- function(path, relative_path){
  
  file.exists_path <- file.exists(path)
  file.exists_relative_path <- file.exists(relative_path)
  
  if(!file.exists_path) path <- file.path(getwd(), path)  ## if doesn't exist assume relative path
  if(!file.exists_relative_path) relative_path <- file.path(getwd(), relative_path)
  
  mainPieces <- strsplit(normalizePath(path, mustWork = FALSE, winslash = "/"), .Platform$file.sep, fixed=TRUE)[[1]]
  refPieces <- strsplit(normalizePath(relative_path, mustWork = FALSE, winslash = "/"), .Platform$file.sep, fixed=TRUE)[[1]]
  
  #if(!file.exists_path) unlink(path)
  #if(!file.exists_relative_path) unlink(relative_path)
  
  shorterLength <- min(length(mainPieces), length(refPieces))
  
  last_common_piece <- max(which(mainPieces[1:shorterLength] == refPieces[1:shorterLength]),1)
  
  dots <- refPieces[!seq_along(refPieces) %in% 1:last_common_piece]
  dots <- rep("..", length(dots))
  
  mainPieces <- mainPieces[!seq_along(mainPieces) %in% 1:last_common_piece]
  
  relativePieces <- c(dots, mainPieces)
  do.call(file.path, as.list(relativePieces))
}

relative_path <- Vectorize(relative_path, USE.NAMES = FALSE)


#' Logical flag for detecting if R session is on rstudio not
#' 
#' @keywords internal
#' @export
is_rstudio <- function() Sys.getenv("RSTUDIO") == "1"

