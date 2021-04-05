## Author: klgk669
## First created: 2018-03-14
## Description: covariate summary tables
## Keywords: 

## example use:
# d %>% get_static_data %>%
#   group_by(STUDYTXT) %>% 
#   summarise(BWT_kg = summarise_continuous(BWT),
#             percentageFemale = summarise_binary(SEX),
#             RACE = summarise_categorical(RACETXT))

## output:
# STUDYTXT       BWT_kg      percentageFemale RACE                                 
# <fctr>         <chr>       <chr>            <chr>                                
# STUDY1         72 (56-110) 39.58%           white:89.6%, black:0%, asian:10.4%, …
# STUDY2         61 (48-86)  35.14%           white:0%, black:0%, asian:100%, othe…
# STUDY3         63 (53-73)  0%               white:0%, black:0%, asian:100%, othe…
# STUDY4         57 (46-81)  61.67%           white:0%, black:0%, asian:100%, othe…

########################################
## load packages and source functions here

library(NMprojectAZ)
library(dplyr)
library(tidyr)

########################################
## main script here

get_static_data <- function(data,by=c("ID")){
  static_covs <- data %>% group_by_(by) %>% summarise_all(function(i)length(unique(i))) %>%
    select_if(function(x) all(x == 1)) %>% names
  dstat <- select(d,c(by,static_covs)) %>% unique()
  dstat <- ungroup(dstat)
  dstat
}

summarise_continuous <- function(x){
  q <- signif(quantile(x,probs=c(0.05,0.5,0.95),na.rm=TRUE),2)
  paste0(q[2]," (",q[1],"-",q[3],")")
}

summarise_binary <- function(x){
  p <- length(which(x %in% 1))/length(x)
  paste0(100*signif(p,4),"%")
}

summarise_categorical <- function(x){
  tab <- table(x)
  sum(tab)
  percs <- 100*signif(tab/sum(tab),3)
  paste0(paste0(names(tab),":",percs,"%"),collapse=", ")
}