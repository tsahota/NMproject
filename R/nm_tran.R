
update_dollar_data <- function(ctl_name,new_data_name){
  if(is_single_na(ctl_name)) return(NA)
  ctl <- ctl_character(ctl_name)
  ctl <- gsub("^(\\s*\\$DATA\\s*)[^ ]+(.*)$",paste0("\\1",new_data_name,"\\2"),ctl)
  ctl
}

#' Run NMTRAN step only
#'
#' Runs initial part of NONMEM where control file and dataset checks are performed.
#' Stops before running NONMEM.  Useful especially on grid infrastructures where 
#' it may take a while for NONMEM to start return ctl/dataset errors
#'
#' @param x nm object
#' @details
#' Requires \code{options("nmtran_exe_path")} to be set.
#' @export
nm_tran <- function(x) UseMethod("nm_tran")

#' @export
nm_tran.default <- function(x){
  
  if(is.null(nm_tran_command())) stop("nm_tran not set up, see ?nm_tran_command")
  
  tempdir0 <- basename(tempdir()) ## make temporary directory in current directory
  dir.create(tempdir0) ; on.exit(unlink(tempdir0,recursive=TRUE,force = TRUE))
  file.copy(x,tempdir0) ## copy_control file
  data_path <- file.path(dirname(x), data_name(x))
  file.copy(data_path,tempdir0) ## copy dataset
  dataset.name <- basename(data_path)
  suppressMessages({
    ctl_text <- update_dollar_data(file.path(tempdir0,basename(x)),dataset.name)
    write(ctl_text, file.path(tempdir0,basename(x)))
  })
  message("running NMTRAN on ", x)
  
  nm_tran_command <- nm_tran_command()
  cmd <- stringr::str_glue(nm_tran_command, .envir = list(ctl_name= basename(x)), .na = NULL)
  ## if non-glue - append the control file name
  if(cmd == nm_tran_command) cmd <- paste(cmd, "<", basename(x))
  
  system_nm(cmd, dir=tempdir0, wait=TRUE)
}

#' @export
nm_tran.nm_generic <- function(x){
  xtmp <- x %>% run_in(file.path(run_in(x), "temp"))
  xtmp %>% write_ctl(force = TRUE)
  nm_tran.default(ctl_path(xtmp))
  invisible(x)
}
#' @export
nm_tran.nm_list <- Vectorize_nm_list(nm_tran.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)
