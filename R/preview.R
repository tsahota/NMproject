#' Preview code_library file
#' @param name character indicating script in code_library to preview
#' @export
preview <- function(name) {
  ## preview files in code_library
  if(length(name)>1) stop("can only preview one file at a time")
  if (is_full_path(name)) {
    if (!file.exists(name)) 
      stop("file not found")
    file.show(name)
    return()
  }
  d <- code_library(extn = ".*", viewer = FALSE, silent = TRUE, return_info = TRUE, 
                    fields = c())
  if (!name %in% d$NAME) 
    stop("file not found in code_library")
  if (length(which(d$NAME %in% name)) > 1) 
    stop("Matched more than one file with that name.\n Try preview() again with full path")
  pos <- match(name, d$NAME)
  path <- file.path(d$FOLDER[pos], d$NAME[pos])
  file.show(path)
}
