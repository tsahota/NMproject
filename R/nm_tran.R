update_dollar_data <- function(ctl_name, new_data_name) {
  if (is_single_na(ctl_name)) {
    return(NA)
  }
  ctl <- ctl_character(ctl_name)
  ctl <- gsub("^(\\s*\\$DATA\\s*)[^ ]+(.*)$", paste0("\\1", new_data_name, "\\2"), ctl)
  ctl
}

#' Run NMTRAN step of a NONMEM job
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This is the function behind the "nm_tran" RStudio 'Addin', which is the
#' recommended way to use this functionality.  Highlight your code (e.g see
#' examples below for a code segment), and then open the "nm_tran" RStudio 'Addin'.
#'
#' Useful especially on grid infrastructures where it may take a while for
#' NONMEM to start return control file and dataset errors. Runs initial NMTRAN
#' step of NONMEM in a temporary directory where control file and dataset checks
#' are performed. Stops before running NONMEM.
#'
#' @param x An nm object.
#'
#' @return The same `x` object is returned, called for side effects.
#'
#' @seealso [run_nm()], [nm_tran_command()] for configuration.
#'
#' @examples
#'
#' \dontrun{
#'
#' ## highlight the code below and use the "nm_tran" RStudio 'Addin'
#'
#' m1 <- new_nm(run_id = "m1",
#'              based_on = "staging/Models/ADVAN2.mod",
#'              data_path = "DerivedData/data.csv") %>%
#'   cmd("execute {ctl_name} -dir={run_dir}") %>%
#'   fill_input() %>%
#'   init_theta(init = c(-2, 0.5, 1)) %>%
#'   init_sigma(init = c(0.1, 0.1)) %>%
#'   run_nm()
#' }
#' @export
nm_tran <- function(x) UseMethod("nm_tran")

#' @export
nm_tran.default <- function(x) {
  if (is.null(nm_tran_command())) stop("nm_tran not set up, see ?nm_tran_command")

  tempdir0 <- basename(tempdir()) ## make temporary directory in current directory
  dir.create(tempdir0)
  on.exit(unlink(tempdir0, recursive = TRUE, force = TRUE))
  file.copy(x, tempdir0) ## copy_control file
  data_path <- file.path(dirname(x), data_name(x))
  file.copy(data_path, tempdir0) ## copy dataset
  dataset.name <- basename(data_path)
  suppressMessages({
    ctl_text <- update_dollar_data(file.path(tempdir0, basename(x)), dataset.name)
    write(ctl_text, file.path(tempdir0, basename(x)))
  })
  message("running NMTRAN on ", x)

  nm_tran_command <- nm_tran_command()
  cmd <- stringr::str_glue(nm_tran_command, .envir = list(ctl_name = basename(x)), .na = NULL)
  ## if non-glue - append the control file name
  if (cmd == nm_tran_command) cmd <- paste(cmd, "<", basename(x))

  system_nm(cmd, dir = tempdir0, wait = TRUE)
}

#' @export
nm_tran.nm_generic <- function(x) {
  xtmp <- x %>% run_in(file.path(run_in(x), "temp"))
  xtmp %>% write_ctl(force = TRUE)
  nm_tran.default(ctl_path(xtmp))
  invisible(x)
}
#' @export
nm_tran.nm_list <- Vectorize_nm_list(nm_tran.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)
