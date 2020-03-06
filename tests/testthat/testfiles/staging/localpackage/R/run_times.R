## Copied from /home/klgk669/test27/test23/Scripts/run_times.R
##  (2018-11-30 08:16:26) by klgk669
require(ggplot2)
require(dplyr)
require(lubridate)

## NOTE:
## Script assumes you are in a directory with a subdirectory called
## "Models" where NONMEM is run.  Otherwise change path_to_models below
## It also assumes at the bottom of the scripts, there's a "Results"
## subdirectory.

args <- commandArgs(trailingOnly = TRUE)
if(length(args) == 0) stubs <- "run6_" else stubs <- args

get_job_info_list <- function(joblist){
  cmd <- paste("sacct -j",joblist,"--format=JobID,JobName,submit,start,end,Eligible,Nnodes,NCPUS,Timelimit")
  
  if(require(NMprojectAZ)){
    d <- system_nm(cmd, intern = TRUE)  
  } else {
    orig_dir <- getwd()
    setwd(path_to_models)
    on.exit(setwd(orig_dir))
    d <- system(cmd, intern = TRUE)
    setwd(orig_dir)
  }
  
  dtop <- d[1]
  dbottom <- d[3:length(d)]
  dbottom <- dbottom[grepl("^[0-9]+\\s",dbottom)]
  d <- c(dtop,dbottom)
  
  d <- read.table(text = d, header = TRUE)
  d <- d %>% filter(grepl("^[0-9]+$",JobID))    
  d <- d %>% dplyr::rename(job = JobID, model = JobName, nCPU = NCPUS, Nnodes = NNodes)
  
  d$Start <- ymd_hms(d$Start)
  d$End <- ymd_hms(d$End)
  d$Submit <- ymd_hms(d$Submit)
  d$Timelimit <- as.character(d$Timelimit)
  d$Timelimit_day <- 0
  has_day <- grepl("^[0-9]+-", d$Timelimit)
  d$Timelimit_day[has_day] <- as.numeric(gsub("^([0-9]+)-.*","\\1",d$Timelimit[has_day]))
  d$Timelimit_time <- hms(gsub("^[0-9]+-","",d$Timelimit))
  d$Timelimit <- days(d$Timelimit_day) + d$Timelimit_time
  d$Timelimit <- period_to_seconds(d$Timelimit)/60
  d$Timelimit_day <- NULL
  d$Timelimit_time <- NULL
  
  d$month <- month(d$Start, label = TRUE)
  d$day <- wday(d$Start, label = TRUE)
  d$is_weekend <- d$day %in% c("Sat","Sun")
  d$time_of_day <- hour(d$Start)
  d$working_hours <- d$time_of_day >= 8 | d$time_of_day <= 18
  d$weeks_ago <- -floor(difftime(today(),d$Start, units = "weeks"))
  d$weeks_ago_txt <- paste(d$weeks_ago,"wks ago")
  d$days_ago <- -floor(difftime(today(),d$Start, units = "days"))
  d$days_ago_txt <- paste(d$days_ago,"days ago")
  
  d$Qtime=(as.numeric(d$Start)-as.numeric(d$Submit))/60
  d$Rtime=(as.numeric(d$End)-as.numeric(d$Start))/60
  d$Ttime <- d$Qtime+d$Rtime
  
  d
    
}

summarise_runs <- function(stubs = "run6_"){
  path_to_models <- "Models"
  
  psn.files <- list.files(path_to_models,pattern="psn")
  jobs<-gsub(".out","",gsub("psn-","",psn.files))
  joblist <- paste(jobs, collapse = ",")
  
  d <- get_job_info_list(joblist)
  dl <- reshape2::melt(d,measure.vars=c("Qtime","Rtime","Ttime"))
  ## filter here for particular run types.
  dl <- dl %>% filter(grepl(stubs, model))
  
  pl <- list()
  
  common_layers0 <- function(p){
    p <- p +  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x)) +
      theme_bw() +
      annotation_logticks(sides="l") +
      labs(y="Time (min)")
    
    p + labs(caption = paste(Sys.info()['user'],
                             ifelse(tidyproject::is_rstudio(),rstudioapi::getSourceEditorContext()$path,""),
                             Sys.time(),
                             sep = " | "))
    
  }
  
  p <- ggplot(dl,aes(nCPU,value,color=day))+
    geom_point()+ 
    geom_line() +
    facet_grid(weeks_ago_txt~variable)
  p <- p %>% common_layers0()
  pl[[length(pl)+1]] <- p
  
  p <- ggplot(dl,aes(time_of_day,value,color=day))+
    geom_point()+ 
    facet_grid(weeks_ago_txt~variable) 
  p <- p %>% common_layers0()
  pl[[length(pl)+1]] <- p
  
  p <- ggplot(dl,aes(days_ago,value,color=day))+  theme_bw()+
    geom_point()+ 
    facet_grid(weeks_ago_txt~variable)
  p <- p %>% common_layers0()
  pl[[length(pl)+1]] <- p
  
  p <- ggplot(dl,aes(day,value,color=working_hours))+  theme_bw()+
    geom_point()+ facet_grid(weeks_ago_txt~variable)
  p <- p %>% common_layers0()
  pl[[length(pl)+1]] <- p
  
  p <- ggplot(dl,aes(nCPU,value,color=factor(Timelimit)))+  theme_bw()+
    geom_point()+ 
    geom_line() +
    facet_grid(weeks_ago_txt~variable)
  p <- p %>% common_layers0()
  pl[[length(pl)+1]] <- p
  
  p <- ggplot(dl,aes(nCPU,value,color=factor(Nnodes)))+  theme_bw()+
    geom_point()+ 
    facet_grid(weeks_ago_txt~variable)
  p <- p %>% common_layers0()
  pl[[length(pl)+1]] <- p
  
  pdf_name <- paste0("Results/run_times",stubs,Sys.info()["user"],".pdf" )
  pdf(pdf_name, width = 7, height = 5)
  print(pl)
  dev.off()
  if(tidyproject::is_rstudio()) file.show(pdf_name)
}

if(length(args) > 0) summarise_runs(stubs)