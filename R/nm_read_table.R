#' @importFrom methods setAs

suppressWarnings(
  suppressMessages(
    methods::setAs("character", "dummy.numeric", function(from) as.numeric(from))
  )
)

#' Fast read of NONMEM output table
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Reads in $TABLE outputs rapidly.  [output_table()] is a higher level function
#' for reading output files and combining with input datasets.
#'
#' @param file File argument from `utils::read.table()`.
#' @param ... Other arguments to be passed to `utils::read.table()`.
#'
#' @return A `data.frame` from a relevant $TABLE output file.
#' @seealso [output_table()]
#' @export

nm_read_table <- function(file, ...) {
  if (!file.exists(file)) {
    usethis::ui_stop("The file {usethis::ui_path(file)} could not be found")
  }
  tmp <- suppressWarnings(utils::read.table(file, fill = T, colClasses = "dummy.numeric", ...))
  return(tmp[stats::complete.cases(tmp[, sapply(tmp, function(i) !all(is.na(i)))]), ])
}
