.onLoad <- function(libname, pkgname){
  if(!requireNamespace("tidyproject",quietly = TRUE,lib.loc = libname))
    stop("tidyproject needed for this function to work. Please install it.",
         call. = FALSE)
  
  ## load tidyproject
  x <- try(lapply("tidyproject",library,character.only = TRUE,warn.conflicts = FALSE, lib.loc = libname), silent = TRUE)
  if(inherits(x, "try-error"))
    x <- lapply("tidyproject",library,character.only = TRUE,warn.conflicts = FALSE)
  
  set_nm_opts()
}

if(!exists(".sso_env")){
  .sso_env <- new.env(parent=emptyenv())
}

if(!exists(".db")){
  .db <- new.env(parent=emptyenv())
}
