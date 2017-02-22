## Author: Tarjinder Sahota (klgk669)
## First created: 2016-01-29
## Description: AZ cluster testing
## Depends on: wait.for.R
## Run interactively: TRUE
## Keywords: script, package test

## To run:
## new_project("~/clusterTest")
## setwd("~/clusterTest")
## copy_script("azclustertest.R")
## source("Scripts/azclustertest.R")


########################################
## load packages and source functions here

library(plyr)
library(dplyr)

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
  d <- data.frame(cmd,job.no,run=m$run.no,time=Sys.time())
  save(d,file=paste0("d",d$run,".R"))
  return(invisible(d))
}

res <- function(run.no){
  lst.name <- file.path(getOption("models.dir"),run.no,"NM_run1","psn.lst")
  #wait.for(file.exists(lst.name),timeout = 30)
  if(!file.exists(lst.name)) return("NO LST")
  obj <- system_cmd(paste("grep '#OBJV'",lst.name),intern=TRUE)
  res <- NA
  if(length(obj)==0) res <- "NO OBJ" else
    res <- gsub("[^0-9\\.]","",obj)
  res
}

list.nodes <- function(job.no){
  if(is.na(job.no)) return(NA)
  if(!file.exists(paste0("Models/hostfile.",job.no))) return("NO HOSTFILE")
  x <- readLines(paste0("Models/hostfile.",job.no))[-1]
  paste(x,collapse=";")
}

get.rng <- function(x){  # internal func
  
  if(!grepl("-",x)) return(x)
  from <- as.numeric(gsub("(.*)-.*","\\1",x))
  to <- as.numeric(gsub(".*-(.*)","\\1",x))
  rng <- as.character(from:to)
  fill.out <- function(x){
    if(nchar(x)==1) return(paste0("00",x))
    if(nchar(x)==2) return(paste0("0",x))
    if(nchar(x)==3) return(x)
  }
  rng <- sapply(rng,fill.out)
  return(rng)
  
}

all.nodes <- function(){
  x <- system_cmd("sinfo -o %N",intern=TRUE)[2]
  prefix <- gsub("(.*)\\[.*\\].*","\\1",x)
  x <- gsub(".*\\[(.*)\\].*","\\1",x)
  x <- strsplit(x,",")[[1]]
  x <- unlist(lapply(x,get.rng))
  names(x) <- NULL
  x
}

is.node.available <- function(x,type=c("idle","idle~")){
  
  d <- system_cmd("sinfo",intern=TRUE)
  d <- lapply(d,strsplit,split=" +")
  d <- t(sapply(d,function(x)x[[1]]))
  
  d <- as.data.frame(d)
  names(d) <- as.character(unlist(d[1,]))
  d <- d[-1,]
  
  avail.nodes <- as.character(d$NODELIST[d$STATE %in% type])
  
  prefix <- gsub("(.*)\\[.*\\].*","\\1",avail.nodes)
  avail.nodes <- gsub(".*\\[(.*)\\].*","\\1",avail.nodes)
  avail.nodes <- strsplit(avail.nodes,",")[[1]]
  
  avail.nodes <- unlist(lapply(avail.nodes,get.rng))
  names(avail.nodes) <- NULL
  #print(avail.nodes)
  x %in% avail.nodes
}

disp <- function(run.no) system_cmd(paste0("tail Models/",run.no,"/NM_run1/psn.lst"))

job.info <- function(job.no) system_cmd(paste("scontrol show -d job",job.no),intern=TRUE)

system_nm("rm -rf *")

copy_control("/opt/az/icondevsolutions/nonmem/standard.7.3.0/run/CONTROL5","run0.mod")
file.copy("/opt/az/icondevsolutions/nonmem/standard.7.3.0/run/THEOPP","Models")

####

d <- data.frame(node=all.nodes())

nodes.list <- all.nodes()

get.available.node <- function(nodes.list,...){
  tmp.node <- sample(nodes.list,1)
  if(is.node.available(tmp.node,...)) return(tmp.node) else 
    return(get.available.node(nodes.list,...))
}

new.ctl.name <- function(){
  mods <- dir(getOption("models.dir"),pattern = "^run[0-9]+\\.mod$")
  run.nos <- sort(as.numeric(gsub("^run([0-9]+)\\.mod$","\\1",mods)))
  paste0("run",run.nos[length(run.nos)]+1,".mod")
}

get.run.no <- function(ctl) gsub("^run([0-9]+)\\.mod$","\\1",ctl)


try.random <- function(...){
  nodes <- sapply(1:5,function(i)get.available.node(nodes.list,...))
  nodes <- paste0("cal-",nodes,collapse=",")
  ctl <- new.ctl.name()
  run.no <- get.run.no(ctl)
  run2(paste0("qpsn2 --nodes ",nodes," -c 11 -t 1000 -- execute ",ctl," -dir=",run.no))
}

try.random.idle(type=c("idle"))

load(file=paste0("d",run.no,".R"))

tmp <- job.info(d$job.no)
tmp <- strsplit(paste(tmp,collapse="")," ")[[1]]
tmp <- tmp[!tmp %in% ""]
n.val <- gsub("([^=]*)=.*","\\1",tmp)
val <- gsub("[^=]*=(.*)","\\1",tmp)
names(val) <- n.val
val[!names(val) %in% c("Command","WorkDir","StdErr","StdOut")]


res(d$run)
list.nodes(d$job.no)

##


##########################


d <- data.frame(node=all.nodes())
d$i <- seq_len(nrow(d))

## find available nodes 

d1 <- ddply(d,"node",function(d){
  node <- d$node
  i <- d$i
  ctl <- paste0("run",i,".mod")
  cmd <- paste0("qpsn2 --nodes cal-",node," -c 11 -t 1000 -- execute ",ctl," -dir=",i)
  if(is.node.available(node)){
    d <- cbind(d,run2(cmd))
  } else {
    d$cmd <- cmd
    d$job.no <- "busy node"
    d$run <- d$i
  }
  d$i <- NULL
  d
})

d1$res <- NA
d1$nodes <- NA

Sys.sleep(10)

d1$res[!d1$job.no %in% "busy node"] <- sapply(d1$run[!d1$job.no %in% "busy node"],res)
d1$nodes[!d1$job.no %in% "busy node"] <- sapply(d1$job.no[!d1$job.no %in% "busy node"],list.nodes)


write.table(d1,file=file.path("Results","results.csv"),sep=",",col.names=TRUE,row.names = FALSE)

