#' @include utils.R
#' @include Vectorize.R

NULL

#' Test if object is an nm_list object
#' 
#' @param x object
#' 
#' @export
is_nm_list <- function(x){
  if(inherits(x, "nm_list")) return(TRUE)
  if(!inherits(x, "list") & !inherits(x, "environment")) return(FALSE)
  
  is_valid_subobject <- function(x){
    #if(is_single_na(x)) return(TRUE) ## na's allowed
    inherits(x, "nm_generic")
  }
  
  all(sapply(x, is_valid_subobject))
}

#' Coerce object into nm_list
#' 
#' @param m nm object
#' @export
as_nm_list <- function(m){
  UseMethod("as_nm_list")
}

#' @export
as_nm_list.nm_list <- function(m){
  m
}
#' @export
as_nm_list.list <- function(m){
  if(is_nm_list(m)){
    class(m) <- c("nm_list", "list")
    names(m) <- unique_id(m)
    return(m)
  } else {
    stop("list not coercible to nm_list")
  }
}
#' @export
as_nm_list.nm_generic <- function(m){
  m <- list(m)
  as_nm_list.list(m)
}

#' Test if object is an nm_generic object
#' 
#' @param x object
#' 
#' @export

is_nm_generic <- function(x){
  inherits(x, "nm_generic")
}

#' Convert nm object to nm_generic
#'
#' mainly an internal function to be used where methods for nm_list don't exist
#' or aren't appropriate
#'
#' @param m nm object
#'
#' @export
as_nm_generic <- function(m){
  UseMethod("as_nm_generic")
}
#' @export
as_nm_generic.nm_generic <- function(m) m
#' @export
as_nm_generic.nm_list <- function(m){
  if(length(m)>1) stop("cannot coerce more than one object to nm_generic")
  m <- m[[1]]
  if(!inherits(m, "nm_generic")) stop("could not coerce to nm_generic")
  m
}

#' @export
c.nm_list <- function(...){
  ## try append
  basic_list <- lapply(list(...), function(ob){
    class(ob) <- "list"
    ob
  })
  basic_list <- do.call(c, basic_list)
  as_nm_list(basic_list)
  # 
  # basic_list <- lapply(list(...), '[[', i = 1)
  # as_nm_list(basic_list)
}

#' @export
c.nm_generic <- function(...){
  basic_list <- list(...)
  class(basic_list) <- c("nm_list", "list")
  #as_nm_list(basic_list)
  basic_list
}

#' @export
'[.nm_list' <- function(x, i, j, ...) {
  class(x) <- "list"
  val <- NextMethod()
  class(val) <- c("nm_list", "list")
  #val <- as_nm_list(val)
  val
}



#' @export
unique.nm_list <- function(x, incomparables = FALSE, ...) {
  class(x) <- "list"
  val <- NextMethod()
  class(val) <- c("nm_list", "list")
  #val <- as_nm_list(val)
  val
}

## experimental - goes against dplyr, maybe delete if not useful
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  condition <- condition %in% TRUE
  .data[condition, ] <- .data[condition, ] %>% dplyr::mutate(...)
  .data
}

gsub_in_brackets <- function(pattern, replacement, x){
  x <- gsub("\\(", "~(", x)
  x <- gsub("\\)", ")~", x)
  x <- paste0(x, collapse = "\n")
  x <- strsplit(x, split = "~")[[1]]
  x[grepl("^\\(.*\\)",x)] <- 
    gsub(pattern, replacement, x[grepl("^\\(.*\\)",x)])
  x <- paste(x, collapse = "")
  x <- paste(x, " ")  ## added to make sure final \n doesn't shorten vector
  x <- strsplit(x, split = "\n")[[1]]
  x[length(x)] <- trimws(x[length(x)])
  x
}

gsub_out_brackets <- function(pattern, replacement, x){
  x <- gsub("\\(", "~(", x)
  x <- gsub("\\)", ")~", x)
  x <- paste0(x, collapse = "\n")
  x <- strsplit(x, split = "~")[[1]]
  x[!grepl("^\\(.*\\)",x)] <- 
    gsub(pattern, replacement, x[!grepl("^\\(.*\\)",x)])
  x <- paste(x, collapse = "")
  x <- paste(x, " ")  ## added to make sure final \n doesn't shorten vector
  x <- strsplit(x, split = "\n")[[1]]
  x[length(x)] <- trimws(x[length(x)])
  x
}

na.locf <- function(x) {
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v)+1]
}

