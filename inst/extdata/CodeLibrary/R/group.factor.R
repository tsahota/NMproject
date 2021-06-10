#' Group a set of values into equally sized groups
#' 
#' Useful if wanting to group columsn like `ID` into groups
#' 
#' @param x vector to group
#' @param n group size
#' 
#' @export

group.factor <- function(x,n){
  GRP <- factor(x,levels=unique(x))
  levels(GRP) <- ((1:length(unique(x))-0.1) %/% n) + 1
  GRP
}
