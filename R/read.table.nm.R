suppressWarnings(suppressMessages(methods::setAs("character","dummy.numeric", function(from) as.numeric(from))))

#' Fast read of NONMEM output table
#'
#' @param file file argument - see ?read.table help for details
#' @param ... other arguments to be passed to read.table()
#' @export

nm_read_table <- function(file,...){
  tmp <- suppressWarnings(utils::read.table(file, fill=T, colClasses="dummy.numeric",...))
  return(tmp[stats::complete.cases(tmp[,sapply(tmp,function(i) !all(is.na(i)))]),])
}
