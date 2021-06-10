#' replace -99 with NA in dataset
#' 
#' @param d dataset
#' 
#' @export

convert.to.na <- function(d,replace=-99){
  names <- names(d)[which(sapply(seq_along(d),function(i)replace %in% d[,i]))]
  d[names][d[names] == replace] <- NA
  d
}
