## Copied from staging/Scripts/macro.R
##  (2019-12-30 21:25:31) by klgk669
## Author: klgk669
## First created: 2017-10-19
## Description: Macro script
## Keywords:

## Run with Rscript Scripts/macro.R from main directory

########################################
## load packages and source functions here

#library(NMprojectAZ)

#source("Scripts/nm.log.R")
#Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

file_time <- format(Sys.time(), "%Y-%m-%d-%H-%M")
input <- "Scripts/test_report.Rmd"
output_file <- paste0("Results/test_report", file_time, ".pdf")

rmarkdown::render(input = input, output_file = output_file)

if(!file.exists(output_file)) stop("no output file. knitr failed?")

dir.create("~/test_reports", showWarnings = FALSE)

file.copy(file.path(output_file), "~/test_reports")

message("report produced: ", file.path("~/test_reports", basename(output_file)))
