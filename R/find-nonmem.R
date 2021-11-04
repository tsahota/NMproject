#' Test if psn is available locally
#'
#' This is mainly for testing.  PsN doesn't have to be available locally for NMproject to work.  As long as it is available from `system_nm()`
#'
#' @return Logical `TRUE` or `FALSE`
#'
#' @seealso `psn_check()`
#'
#' @keywords internal
#' @export
psn_available <- function() Sys.which("psn") != ""

#' Tests if psn is accessible
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' NMproject needs PsN installed to function.  If PsN is not installed.
#'
#' @param fail_if_false Logical (default = `TRUE`). Should the function return
#'   an error message if `system_nm()` is not available.
#'
#' @return Logical TRUE or FALSE.
#'
#' @examples
#'
#' psn_check(fail_if_false = FALSE)
#'
#' @keywords internal
#' @export

psn_check <- function(fail_if_false = TRUE) {

  system_nm_available(fail_if_false = fail_if_false)

  ## check psn

  psn_nm_versions <- try(system_nm_intern("psn --version"), silent = TRUE)

  if (inherits(psn_nm_versions, "try-error")) {
    if (fail_if_false) {
      usethis::ui_oops("{usethis::ui_code('system_nm(\"psn --version\")')} command fails.  Ensure PsN is installed: {usethis::ui_path('https://uupharmacometrics.github.io/PsN/')}")
    }
    return(FALSE)
  }

  if (length(psn_nm_versions) == 0) {
    if (fail_if_false) {
      usethis::ui_oops("{usethis::ui_code('system_nm(\"psn --version\")')} command fails to return a result.  Ensure PsN is installed: {usethis::ui_path('https://uupharmacometrics.github.io/PsN/')}")
    }
    return(FALSE)
  }

  return(TRUE)

}

#' Test integrity of system_nm
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' `system_nm()` needs to be able to submit commands to the terminal for
#' NMproject to function.  This function tests whether meaningful output can be
#' produced and is only intended for diagnostic purposes.
#'
#' @param fail_if_false Logical (default = `FALSE`). Should the function return
#'   an error message if `system_nm()` is not available.
#'
#' @return Logical `TRUE` or `FALSE`.
#'
#' @examples
#'
#' system_nm_available(fail_if_false = FALSE)
#'
#' @keywords internal
#' @export

system_nm_available <- function(fail_if_false = FALSE) {
  result <- try(system_nm_intern("whoami"), silent = TRUE)

  if (inherits(result, "try-error")) {
    if (fail_if_false) {
      usethis::ui_oops("{usethis::ui_code('system_nm(\"whoami\")')} command fails.  See {usethis::ui_code('?system_nm')} for more information how to configure")
    }
    return(FALSE)
  }

  if (length(result) == 0) {
    if (fail_if_false) {
      usethis::ui_oops("{usethis::ui_code('system_nm(\"whoami\")')} command fails to return a result.  See {usethis::ui_code('?system_nm')} for more information how to configure")
    }
    return(FALSE)
  }

  result <- result[1]

  if (!is.character(result)) {
    if (fail_if_false) {
      usethis::ui_oops("{usethis::ui_code('system_nm(\"whoami\")')} command fails to return a character type.  See {usethis::ui_code('?system_nm')} for more information how to configure")
    }
    return(FALSE)
  }

  if (nchar(result) == 0) {
    if (fail_if_false) {
      usethis::ui_oops("{usethis::ui_code('system_nm(\"whoami\")')} command fails to return a meaningful output.  See {usethis::ui_code('?system_nm')} for more information how to configure")
    }
    return(FALSE)
  }

  return(TRUE)

}

#' @rdname find_nonmem
#' @name find_nonmem
#' @title Find location of NONMEM installation
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Attempts to find location of NONMEM installation directory used by PsN.  Can
#' be useful for finding the location of parafiles etc.
#'
#' @param name Character name of NONMEM installation (according PsN).
#'
#' @details The function `find_nm_install_path()` will attempt to use a locally
#'   available PsN installation to get this information.  If the PsN
#'   installation is on a remote server, this function will not work (it will
#'   return a `NULL`)
#' @return If functions cannot find installation they will return `NULL` without
#'   errors or warning, otherwise they will return the located paths.
#'
#' @export
find_nm_install_path <- function(name = "default") {
  if (!psn_available()) {
    return(NULL)
  }

  nm_versions <- try(system_nm_intern("psn -nm_versions"), silent = TRUE)

  if (inherits(nm_versions, "try-error")) {
    return(NULL)
  }

  nm_version <- nm_versions[grepl(paste0("^", name), nm_versions)]

  if (length(nm_version) != 1) {
    warning("Could not determine path to nonmem from psn -nm_versions.")
    return(NULL)
  }

  ## can now assume version is unique
  path <- gsub(".*\\((.*),.*\\)", "\\1", nm_version)
  if (!file.exists(path)) warning("directory ", path, "doesn't exist")
  path
}

#' @rdname find_nonmem
#' @param warn Logical (default = `TRUE`) to warn if fails to find NMTRAN.exe.
#'
#' @details The function `find_nm_tran_path()` will attempt to use a locally
#'   available PsN installation to get this information.  If the PsN
#'   installation is on a remote server, this function will not work (it will
#'   return a `NULL`). This is normally used to set [nm_tran_command()].  If
#'   this function cannot find installation, you will need to set
#'   [nm_tran_command()], manually.
#'
#' @seealso [nm_tran_command()]
#' @export
find_nm_tran_path <- function(name = "default", warn = TRUE) {
  if (!psn_available()) {
    return(NULL)
  }

  nm_versions <- try(system_nm_intern("psn -nm_versions"), silent = TRUE)

  warn_func <- function() {
    if (warn) {
      warning(
        "Could not determine path to nmtran from psn -nm_versions.\n",
        "To set manually add the following command:\n",
        "  options(nmtran_exe_path=\"path/to/nmtran\")\n",
        "     1. (for this session only) in the console\n",
        "     2. (for this user) to ~/.Rprofile\n",
        paste0("     3. (for all users) to ", file.path(R.home(component = "home"), "etc", "Rprofile.site"))
      )
    }
  }

  if (inherits(nm_versions, "try-error")) {
    warn_func()
    return(NULL)
  }

  nm_version <- nm_versions[grepl(paste0("^", name), nm_versions)]

  if (length(nm_version) != 1) {
    warn_func()
    return(NULL)
  }

  ## can now assume version is unique
  path <- gsub(".*\\((.*),.*\\)", "\\1", nm_version)
  tr_path <- file.path(path, "tr")
  if (file.exists(path)) {
    path <- dir(tr_path, pattern = "NMTRAN.exe", ignore.case = TRUE, full.names = TRUE)
  } else { ## guess NMTRAN.exe
    path <- file.path(tr_path, "NMTRAN.exe")
  }
  path
}

#' NONMEM version info
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Gets version information about the NONMEM installation, PsN installation and
#' compilers.  Can be useful for documentation purposes.
#'
#' @return Returns list with version info for NONMEM, PsN, perl and
#' fortran compiler (only gfortran currently).
#'
#' @export

NONMEM_version <- function() {

  ## scrape version info
  psn_nm_versions <- try(system_nm_intern("psn -nm_versions"), silent = TRUE)
  if (inherits(psn_nm_versions, "try-error")) {
    warning("can't find nonmem installation")
    return(NULL)
  }

  psn_nm_version <- psn_nm_versions[grepl("default is", psn_nm_versions)]
  psn_nm_version <- basename(gsub("^.* (/.*)$", "\\1", psn_nm_version))

  if (length(psn_nm_version) == 0) psn_nm_version <- "not found"

  nm_compiler_version <- system_nm_intern("gfortran --version")[1]
  if (length(nm_compiler_version) == 0) {
    nm_compiler_version <- "not found"
  }

  psn_version <- system_nm_intern("psn --version")
  if (length(psn_version) == 0) psn_version <- "not found"

  perl_version <- system_nm_intern("perl --version")
  perl_version <- perl_version[grepl("This is", perl_version)]
  perl_version <- gsub("^.*\\((v.*)\\).*$", "\\1", perl_version)
  if (length(perl_version) == 0) perl_version <- "not found"

  c(
    NONMEM = psn_nm_version,
    compiler = nm_compiler_version,
    psn = psn_version,
    perl = perl_version
  )
}
