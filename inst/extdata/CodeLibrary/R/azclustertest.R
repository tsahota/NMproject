## Author: Tarjinder Sahota (klgk669)
## First created: 2016-01-29
## Description: AZ cluster testing
## Depends on: wait.for.R
## Keywords: script, package test
## Run interactively: TRUE

## To run:
## new_project("~/clusterTest")
## setwd("~/clusterTest")
## copy_script("azclustertest.R")
## source("Scripts/azclustertest.R")


########################################
## load packages and source functions here

library(NMproject,lib.loc = "ProjectLibrary")
source("Scripts/wait.for.R")

########################################
## main script here

## AZ infrastructure specific

run2 <- function(cmd){
  ctl <- gsub("^.+(run[0-9]+\\.mod).+$","\\1",cmd)
  copy_control("run0.mod",ctl,overwrite = TRUE)
  m <- nm(cmd)
  r <- run(m,force=TRUE,overwrite = TRUE,intern=TRUE)
  if(m$env=="cluster"){
    job.no <- as.numeric(gsub("Submitted batch job ","",r[grepl("Submitted batch job",r)]))
    if(length(job.no)==0) {
      warning("couldn't find job number, returning NA...")
      job.no <- NA
    }
    #Sys.sleep(3)
    #wait.for(!is_job_running(m))
  } else job.no <- NA
  #res <- res(m$run.no)
  data.frame(cmd,job.no,run=m$run.no,time=Sys.time())
  #data.frame(cmd,job.no,res,time=Sys.time())
}

res <- function(run.no){
  lst.name <- file.path(getOption("models.dir"),run.no,"NM_run1","psn.lst")
  if(!file.exists(lst.name)) return("NO LST")
  obj <- system_cmd(paste("grep '#OBJV'",lst.name),intern=TRUE)
  res <- NA
  if(length(obj)==0) res <- "NO OBJ" else
    res <- gsub("[^0-9\\.]","",obj)
  res
}

copy_control("/opt/az/icondevsolutions/nonmem/standard.7.3.0/run/CONTROL5","run0.mod")
file.copy("/opt/az/icondevsolutions/nonmem/standard.7.3.0/run/THEOPP","Models")

disp <- function(run.no) system_cmd(paste0("tail Models/",run.no,"/NM_run1/psn.lst"))

p <- list()

p[[length(p)+1]] <- run2("qpsn -c auto -t 1000 -- execute run1.mod -dir=1")
p[[length(p)+1]] <- run2("execute run2.mod -dir=2")
p[[length(p)+1]] <- run2("qpsn -t 1000 -- execute run3.mod -dir=3")
p[[length(p)+1]] <- run2("qpsn -c auto -t 1000 -r 1000 -- execute run4.mod -dir=4")
p[[length(p)+1]] <- run2("qpsn -c auto -t 5000 -r 1000 -- execute run5.mod -dir=5")
p[[length(p)+1]] <- run2("qpsn -c auto -t 1000 -r 1000 -- execute run6.mod -dir=6")
p[[length(p)+1]] <- run2("qpsn -c auto -t 1000 -r 2000 -- execute run7.mod -dir=7")
p[[length(p)+1]] <- run2("qpsn -c auto -t 5000 -r 1000 -- execute run8.mod -dir=8")
p[[length(p)+1]] <- run2("qpsn -t 1000 -- execute run9.mod -dir=9")
p[[length(p)+1]] <- run2("qpsn -c 2 -t 1000 -- execute run10.mod -dir=10")
p[[length(p)+1]] <- run2("qpsn -c 3 -t 1000 -- execute run11.mod -dir=11")
p[[length(p)+1]] <- run2("qpsn -c 6 -t 1000 -- execute run12.mod -dir=12")
p[[length(p)+1]] <- run2("qpsn -c 7 -t 1000 -- execute run13.mod -dir=13")
p[[length(p)+1]] <- run2("qpsn -c 7 -t 1000 -- execute run14.mod -dir=14")
p[[length(p)+1]] <- run2("qpsn -c 8 -t 1000 -- execute run15.mod -dir=15")
p[[length(p)+1]] <- run2("qpsn -c 10 -t 1000 -- execute run16.mod -dir=16")

Sys.sleep(20)

p2 <- do.call(rbind,p)
p2$res <- sapply(p2$run,res)

save(p2,file="Results/p2.csv")

write.table(p2,file=file.path("Results","results.csv"),sep=",",col.names=TRUE,row.names = FALSE)
