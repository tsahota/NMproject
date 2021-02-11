## Description: locf function
## Run interactively: FALSE
## Key words: locf, function

locf <- function(x,only.forwards=FALSE){
  L <- !is.na(x)
  if(only.forwards) c(x[1], x[L])[cumsum(L)+1] else c(x[L][1], x[L])[cumsum(L)+1]
}
