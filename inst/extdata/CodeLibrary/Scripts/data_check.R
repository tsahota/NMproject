## Author: klgk669
## First created: 2017-10-30
## Description: Basic NONMEM data checks
## Keywords: 

########################################
## load packages and source functions here

library(NMprojectAZ)

########################################
## main script here

data_check <- function(d){
  
  result <- do_test("First column numeric"=is.numeric(d[,1]))

  result <- do_test("no spaces/commas in text vars"={
    res <- paste(names(d)[sapply(d,function(i) any(grepl("\\s|,",i)))],collapse=", ")
    if(length(res)==0) return("TRUE") else return(res)
  },append = result)

  result <- do_test("no NONMEM reserved variables present"={
    illegal_names <- names(d)[names(d) %in% c("EXP","DEXP","LOG","RES","WRES","PRED","IPRED","PHI")]
    res <- length(illegal_names)==0
    if(res) return("TRUE") else
    return(paste(illegal_names,collapse = ", "))
  },append = result)
  
  result
  
}

