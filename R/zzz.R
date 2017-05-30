.onLoad <- function(libname, pkgname){
  if(!requireNamespace("tidyproject",quietly = TRUE))
    stop("tidyproject needed for this function to work. Please install it.",
         call. = FALSE)
  x <- lapply("tidyproject",library,character.only = TRUE)
  #library(tidyproject,character.only = TRUE)
  set_nm_opts()
}

.sso_env <- new.env(parent=emptyenv())
.sso_env$wait <- FALSE
.sso_env$run_overwrite <- FALSE
