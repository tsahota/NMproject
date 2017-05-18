.onLoad <- function(libname, pkgname){
  set_nm_opts()
}

.sso_env <- new.env(parent=emptyenv())
.sso_env$wait <- FALSE
