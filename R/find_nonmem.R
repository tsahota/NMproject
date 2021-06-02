psn_available <- function() Sys.which("psn") != ""

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
#' @details The function `find_nm_install_path()` will attempt to use a locally available PsN
#'   installation to get this information.  If the PsN installation is on a
#'   remote server, this function will not work (it will return a `NULL`)
#' @return If functions cannot find installation they will return `NULL` without
#'   errors or warnings
#'
#' @export
find_nm_install_path <- function(name = "default"){
  
  if(!psn_available()) return(NULL)
  
  nm_versions <- try(system_nm("psn -nm_versions", intern = TRUE, ignore.stderr = TRUE, wait = TRUE),
                     silent = TRUE)
  if(inherits(nm_versions, "try-error")) {
    return(NULL)
  }
  
  nm_version <- nm_versions[grepl(paste0("^", name), nm_versions)]
  
  if(length(nm_version) != 1){
    warning("Could not determine path to nonmem from psn -nm_versions.")
    return(NULL)
  }
  
  ## can now assume version is unique
  path <- gsub(".*\\((.*),.*\\)", "\\1", nm_version)
  if(!file.exists(path)) warning("directory ", path, "doesn't exist")
  path
}

#' @rdname find_nonmem
#' @param warn Logical (default = `TRUE`) to warn if fails to find NMTRAN.exe
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
find_nm_tran_path <- function(name = "default", warn = TRUE){
  
  if(!psn_available()) return(NULL)
  
  nm_versions <- try(system_nm("psn -nm_versions", intern = TRUE, ignore.stderr = TRUE, wait = TRUE), 
                     silent = TRUE)
  
  warn_func <- function(){
    if(warn) warning("Could not determine path to nmtran from psn -nm_versions.\n",
                     "To set manually add the following command:\n",
                     "  options(nmtran_exe_path=\"path/to/nmtran\")\n",
                     "     1. (for this session only) in the console\n",
                     "     2. (for this user) to ~/.Rprofile\n",
                     paste0("     3. (for all users) to ",file.path(R.home(component = "home"), "etc", "Rprofile.site")))
  }
  
  if(inherits(nm_versions, "try-error")) {
    warn_func()
    return(NULL)
  }
  
  nm_version <- nm_versions[grepl(paste0("^", name), nm_versions)]
  
  if(length(nm_version) != 1){
    warn_func()
    return(NULL)
  }
  
  ## can now assume version is unique
  path <- gsub(".*\\((.*),.*\\)", "\\1", nm_version)
  tr_path <- file.path(path, "tr")
  if(file.exists(path)){
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

NONMEM_version <- function(){
  
  ## scrape version info
  psn_nm_versions <- try(system_nm("psn -nm_versions", intern = TRUE, ignore.stderr = TRUE, wait = TRUE),
                         silent = TRUE)
  if(inherits(psn_nm_versions, "try-error")) {
    warning("can't find nonmem installation")
    return(NULL)
  }
  
  psn_nm_version <- psn_nm_versions[grepl("default is", psn_nm_versions)]
  psn_nm_version <- basename(gsub("^.* (/.*)$","\\1", psn_nm_version))
  
  if(length(psn_nm_version) == 0) psn_nm_version <- "not found"
  
  nm_compiler_version <- system_nm("gfortran --version", intern = TRUE, ignore.stderr = TRUE, wait = TRUE)[1]
  if(length(nm_compiler_version) == 0) {
    nm_compiler_version <- "not found"
  }
  
  psn_version <- system_nm("psn --version", intern = TRUE, ignore.stderr = TRUE, wait = TRUE)
  if(length(psn_version) == 0) psn_version <- "not found"
  
  perl_version <- system_nm("perl --version", intern = TRUE, ignore.stderr = TRUE, wait = TRUE)
  perl_version <- perl_version[grepl("This is", perl_version)]
  perl_version <- gsub("^.*\\((v.*)\\).*$","\\1",perl_version)
  if(length(perl_version) == 0) perl_version <- "not found"
  
  c(NONMEM = psn_nm_version,
    compiler = nm_compiler_version,
    psn = psn_version,
    perl = perl_version)
  
}
