#' Kill cluster job
#'
#' Can be useful in conjuction with a modified [system_nm()].  Requires setting `"kill_job"` option.
#'
#' @param m An nm object.
#' 
#' @details The option `"kill_job"` needs to be defined as a function that kills
#'   a cluster job.
#' 
#' @seealso [system_nm()], [job_info()]
#' 
#' @keywords internal
#' @export

kill_job <- function(m){
  getOption("kill_job")(m)
}

#' Get job information (if it exists)
#'
#' Can be useful for storing job numbers with a modified [system_nm()].
#'
#' @param m An nm object.
#' @param text Optional character to set job_info.
#' 
#' @seealso [system_nm()]
#' 
#' @keywords internal
#' @export
job_info <- function(m, text){
  UseMethod("job_info")  
}

#' @export
job_info.nm_generic <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "job_info") else custom_1d_field(m, "job_info", as.character(text))
}

#' @export
job_info.nm_list <- Vectorize_nm_list(job_info.nm_generic)

