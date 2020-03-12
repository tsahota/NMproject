#' #' @export
#' class_factor <- function(x = character(), levels, labels = levels, ...){
#'   if(missing(x) | missing(levels) | missing(labels))
#'     stop("Need x, levels, and label arguments")
#'   labels <- paste0(levels, ":", labels)
#'   f <- factor(..., labels)
#'   class(f) <- c("class_factor", class(f))
#'   f
#' }
#' 
#' is_class_factor <- function(x) {
#'   inherits(x, "class_factor")
#' }
#' 
#' cov_continuous <- function(d, col_names){
#'   if(missing(col_names))
#'     return(names(d)[sapply(d, is_cov_continuous)])
#'   
#'   for(i in col_names){
#'     class(d[[i]]) <- c("cov_continuous", class(d[[i]]))
#'   }
#'   d
#' }
#' 
#' cov_categorical <- function(d, col_names){
#'   if(missing(col_names))
#'     return(names(d)[sapply(d, is_cov_categorical)])
#'   
#'   for(i in col_names){
#'     class(d[[i]]) <- c("cov_categorical", class(d[[i]]))
#'   }
#'   d
#' }
#' 
#' is_cov_continuous <- function(x) inherits(x, "cov_continuous")
#' 
#' is_cov_categorical <- function(x) inherits(x, "cov_categorical")
