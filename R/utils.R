#' compute path relative to reference
#' 
#' @param path character
#' @param relative_path character
#' 
#' @export

relative_path <- function (path, relative_path){
  
  file.exists_path <- file.exists(path)
  file.exists_relative_path <- file.exists(relative_path)
  
  if(!file.exists_path) path <- file.path(getwd(), path)  ## if doesn't exist assume relative path
  if(!file.exists_relative_path) relative_path <- file.path(getwd(), relative_path)
  
  mainPieces <- strsplit(normalizePath(path, mustWork = FALSE), .Platform$file.sep, fixed=TRUE)[[1]]
  refPieces <- strsplit(normalizePath(relative_path, mustWork = FALSE), .Platform$file.sep, fixed=TRUE)[[1]]
  
  #if(!file.exists_path) unlink(path)
  #if(!file.exists_relative_path) unlink(relative_path)
  
  shorterLength <- min(length(mainPieces), length(refPieces))
  
  last_common_piece <- max(which(mainPieces[1:shorterLength] == refPieces[1:shorterLength]),1)
  
  dots <- setdiff(refPieces,refPieces[1:last_common_piece])
  dots <- rep("..", length(dots))
  
  mainPieces <- setdiff(mainPieces,mainPieces[1:last_common_piece])
  
  relativePieces <- c(dots, mainPieces)
  do.call(file.path, as.list(relativePieces))
}

is_single_na <- function(x) if(length(x) == 1) is.na(x) else FALSE
