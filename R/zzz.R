.onLoad <- function(libname, pkgname){
  if(!requireNamespace("tidyproject",quietly = TRUE))
    stop("tidyproject needed for this function to work. Please install it.",
         call. = FALSE)
  x <- lapply("tidyproject",library,character.only = TRUE,warn.conflicts=FALSE)
  set_nm_opts()
}

if(!exists(".sso_env")){
  .sso_env <- new.env(parent=emptyenv())
  .sso_env$wait <- FALSE
  .sso_env$run_overwrite <- FALSE
}

