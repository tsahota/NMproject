#' Fast read of NONMEM output table
#'
#' @param file file argument - see ?read.table help for details
#' @param ... other arguments to be passed to read.table()
#' @export

read.table.nm <- function(file,...){
  suppressWarnings(suppressMessages(setAs("character","dummy.numeric", function(from) as.numeric(from))))
  tmp <- suppressWarnings(read.table(file, fill=T, colClasses="dummy.numeric",...))
  return(tmp[complete.cases(tmp[,sapply(tmp,function(i) !all(is.na(i)))]),])
}
