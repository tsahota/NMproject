#' Check NMproject installation
#'
#' This function uses `testthat` to test whether pre-requisites are met.  It
#' first checks for availability of PsN via `system_nm()`, this is a requirement
#' for NMproject.  Subsequent checks are not mandatory, but recommended for full
#' functionality - these comprise a check to see if NMTRAN checks are configured
#' and a check to see if code completion has been set up.
#'
#' @return Logical `TRUE` or `FALSE` indicating whether the tests have succeeded
#'   or not.  A test failure does not necessarily mean that NMproject
#'   incorrectly configured.  Test messages will say whether they are needed or
#'   just recommended to pass.
#' @export

check_installation <- function() {
  if (!requireNamespace("testthat")) stop("install testthat")
  
  testthat::test_that("Required configuration", {
    
    testthat::expect(psn_check(),
                     "PsN is unavailable.  This is required functionality for
NMproject.  The comamnd system_nm(\"psn --version\") command should succeed.
Ensure PsN is installed: 'https://uupharmacometrics.github.io/PsN/'.
If PsN is installed ensure `system_nm()` is correctly configured.
See ?system_nm for help.")
    
    nm_tran_cmd <- nm_tran_command()
    nm_tran_test <- FALSE
    if (!is.null(nm_tran_cmd)) {
      if (length(nm_tran_cmd) > 0) {
        if (is.character(nm_tran_cmd)) {
          if (nchar(nm_tran_cmd) > 0) nm_tran_test <- TRUE
        }
      }
    }
  })
  
  testthat::test_that("Recommended configuration", {
    
    testthat::expect(nm_tran_test, 
                     "NMTRAN is not configured.  This may be because NONMEM is
not installed.  If NONMEM is installed and you are working in non-grid 
environment (e.g. laptop/desktop) then don't worry about this test failing.  
However if working in a grid environment, it is highly recommended that this 
check succeed as without it NMTRAN checks will not work.  NMTRAN checks provide 
fast feedback of control file and dataset errors.  See ?nm_tran_command for 
instructions")
    
    testthat::expect(check_code_completion(),
                     "Code completion has not been configured.
It is recommended to configure this as it helps greatly with NMproject
scripting.  It is not mandatory though for NMproject to work.
See ?setup_code_completion for help.")
  })
  
}


check_code_completion <- function(snippet_path = find_snippet_path()) {
  
  if (!is_rstudio()) return(FALSE)
  if (!interactive()) return(FALSE)
  
  template_path <- system.file("extdata", "r.snippets", package = "NMproject")
  
  snippet_exists <- file.exists(snippet_path)
  if (!snippet_exists) return(FALSE)
  ## check to see if modification is needed.
  template_contents <- readLines(template_path)
  snippet_contents <- readLines(snippet_path)
  
  last_line_blank <- snippet_contents[length(snippet_contents)]
  last_line_blank <- grepl("^\\s*$", last_line_blank)
  if (!last_line_blank) snippet_contents <- c(snippet_contents, "")
  
  last_line_blank <- template_contents[length(template_contents)]
  last_line_blank <- grepl("^\\s*$", last_line_blank)
  if (!last_line_blank) template_contents <- c(template_contents, "")
  
  matching_snippets <- sapply(c("new_nm", "child"), function(snippet_name) {
    to_index <- snippet_index(snippet_name, snippet_contents)
    from_index <- snippet_index(snippet_name, template_contents)
    identical(snippet_contents[to_index], template_contents[from_index])
  })
  
  all(matching_snippets)
}
