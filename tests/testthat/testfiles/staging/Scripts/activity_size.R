## Copied from staging/Scripts/activity_size.R
##  (2020-01-07 19:55:26) by klgk669
library(dplyr)

## updated every Monday morning

report_path <- "/projects/qcp/scpqcpusage_report.txt"

d <- read.table(report_path,sep = "\t",stringsAsFactors = FALSE)

names(d) <- c("size", "path")

d$size <- as.numeric(gsub("M","",d$size))

d$unit <- "MB"

d <- as_tibble(d)

d <- d %>% arrange(desc(size))

is_activity <- function(path){
  all(c("Models", "Scripts", "SourceData") %in% dir(path))
}

owner <- function(path){
  file.info(file.path(path, "Results"))$uname
}

d$is_activity <- sapply(d$path, is_activity)

d_overall <- d[grepl("/qcp$", d$path), ]

d_QCP_MODELLING <- d[grepl("/qcp/QCP_MODELING$", d$path), ]

d_TA <- d[grepl("/qcp/QCP_MODELING/[^/]+$", d$path), ]

## largest activities

da <- d %>% filter(is_activity)

da$owner <- sapply(da$path, owner)
da %>% select(-is_activity) %>% View


