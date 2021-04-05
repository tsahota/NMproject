## Author: klgk669
## First created: 2016-09-16
## Description: replace -99 with NA in dataset
## Run interactively: FALSE
## Keywords: function

########################################
## load packages and source functions here


########################################
## main script here

convert.to.na <- function(d,replace=-99){
  names <- names(d)[which(sapply(seq_along(d),function(i)replace %in% d[,i]))]
  d[names][d[names] == replace] <- NA
  d
}
