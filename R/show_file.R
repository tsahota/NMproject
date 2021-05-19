#' Show an uneditable version of the lst file
#'
#' @param r nm object
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
#' @param r nm object
#' @export
show_ctl <- function(r) {
  UseMethod("show_ctl")
}

#' @export
show_ctl.nm_generic <- function(r) {
  rtmp <- r %>% run_in(file.path(run_in(r), "temp"))
  r %>% write_ctl(force = TRUE)
  file.show(ctl_path(r))
}
#' @export
show_ctl.nm_list <- show_ctl.nm_generic
