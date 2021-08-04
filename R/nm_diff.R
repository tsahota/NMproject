#' Compute diff between two NONMEM runs
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' The easiest way to use this function is via the "view diff" RStudio 'Addin'.
#'
#' NMproject's control file manipulation functions (e.g. [subroutine()])
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
#' @return A `diff` object.
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
#' }
#' @export
nm_diff <- function(m, ref_m, format = "raw") {
  requireNamespace("diffobj", quietly = TRUE)

  if (missing(ref_m)) {
    old_ctl <- as.character(ctl_character(
      as_nm_generic(m)[["ctl_orig"]]
    ))
  } else {
    if (inherits(ref_m, "nm_list") | inherits(ref_m, "nm_generic")) {
      old_ctl <- as.character(ctl_character(ctl_contents(as_nm_generic(ref_m))))
    } else {
      if (is.character(ref_m)) {
        if (length(ref_m) > 1) old_ctl <- ref_m
        if (length(ref_m) == 0) stop("ref_m should not be length 0")
        if (length(ref_m) == 1) {
          if (file.exists(ref_m)) {
            old_ctl <- readLines(ref_m)
          }
        }
      } else {
        stop("don't know how to handle ref_m")
      }
    }
  }
  new_ctl <- as.character(ctl_character(ctl_contents(as_nm_generic(m))))
  # "ansi256"
  dff <- diffobj::diffChr(old_ctl, new_ctl, format = format)
  dff <- as.character(dff)

  if (grepl("No visible differences between objects", dff[1])) {
    dff <- character()
  }
  cat(dff, sep = "\n")

  invisible(dff)
}
