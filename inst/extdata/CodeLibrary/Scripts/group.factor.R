## Description: creates group variable
## Run interactively: FALSE
## Key words: function

group.factor <- function(x,n){
  GRP <- factor(x,levels=unique(x))
  levels(GRP) <- ((1:length(unique(x))-0.1) %/% n) + 1
  GRP
}