show_file <- function(file_name){
  if(.Platform$OS.type=="windows")
    file.show(file_name) else
      if(exists("file.show")) file.show(file_name) else
        utils::file.edit(file_name)
}

#' Show an uneditable version of the lst file
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Opens a read-only version of the NONMEM control file for browsing.
#' 
#' @param r An nm object.
#' @seealso [show_ctl()].
#' @export
show_out <- function(r){
  UseMethod("show_out")
}

#' @export
show_out.nm_generic <- function(r) {
  out_file <- file.path(run_in(r), lst_path(r))
  #out_file <- nm_output_path(r, extn = "lst")
  show_file(out_file)
}

#' @export
show_out.nm_list <- show_out.nm_generic


#' Show an uneditable version of the control file
#'
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Opens a read-only version of the NONMEM control file for browsing.
#'
#' @param r An nm object.
#' @seealso [show_out()].
#' @export
show_ctl <- function(r) {
  UseMethod("show_ctl")
}

#' @export
show_ctl.nm_generic <- function(r) {
  rtmp <- r %>% ctl_name(paste0(ctl_name(r), ".tmp"))
  rtmp %>% write_ctl(force = TRUE)
  file.show(ctl_path(rtmp))
}
#' @export
show_ctl.nm_list <- show_ctl.nm_generic
