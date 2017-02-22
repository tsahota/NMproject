## Description: read.table for NONMEM output
## Run interactively: FALSE
## Key words: function

## Example use: read.table.nm("out.csv",skip=1,header=TRUE,sep=",",na=".")

suppressWarnings(suppressMessages(setAs("character","dummy.numeric", function(from) as.numeric(from))))

read.table.nm <- function(x,...){
  tmp <- suppressWarnings(read.table(x, fill=T, colClasses="dummy.numeric",...))
  return(tmp[complete.cases(tmp[,sapply(tmp,function(i) !all(is.na(i)))]),])
}
