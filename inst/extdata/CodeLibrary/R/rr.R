## Author: Tarjinder Sahota (klgk669) First created: 2016-02-03 Description: PsN's
## runrecord function Depends on: wait.for.R Run interactively: FALSE Keywords:
## function, psn

######################################## load packages and source functions here

source("Scripts/wait.for.R")

######################################## main script here

rr <- function(arg) {
    message("This function is very slow and not very nice. Sorry...")
    unlink("AAruninfo.txt")
    system3(paste0("runrecord ", arg))
    wait.for(file.exists("AAruninfo.txt"))
    d <- read.table("AAruninfo.txt", skip = 4, header = TRUE, sep = ";", row.names = NULL)
    names(d) <- names(d)[-1]
    d[, length(d)] <- NULL
    d
}
