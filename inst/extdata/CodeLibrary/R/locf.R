#' Last observation carried forward
#'
#' @param x vector with NAs to replace
#' @param only.forwards if `TRUE` (default = `FALSE`) will not not fill in
#'   leading NAs before first non-NA.
#'
#' @export

locf <- function(x,only.forwards=FALSE){
  L <- !is.na(x)
  if(only.forwards) c(x[1], x[L])[cumsum(L)+1] else c(x[L][1], x[L])[cumsum(L)+1]
}
