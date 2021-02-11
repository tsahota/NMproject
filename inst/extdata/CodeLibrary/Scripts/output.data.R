## Author: Tarjinder Sahota (klgk669)
## First created: 2015-12-24
## Description: Merge input and output datasets
## Run interactively: FALSE
## Depends on: read.table.nm.R

source("Scripts/read.table.nm.R")

output.data <- function(...) UseMethod("output.data")
output.data.default <- function(run.no,primary.key,models.dir=getOption("models.dir")){
  ## assumes there is an primary.key column(s) in sdtab e.g.
  ##   $TABLE ID TIME IPRED IWRES IRES CWRES NPDE ORD
  ##   FILE=sdtab[run.no] NOPRINT ONEHEADER FORMAT=tF13.4
  if(is.null(models.dir)) models.dir = "."
  
  s <- readLines(file.path(models.dir,paste0("run",run.no,".mod")))
  orig.data <- s[grepl("^\\s*\\$DATA",s)]
  orig.data <- file.path(models.dir,gsub("^\\s*\\$DATA\\s+(\\S+)\\s+.*$","\\1",orig.data))
  
  do <- read.table.nm(file.path(models.dir,paste0("sdtab",run.no)),skip=1,header=TRUE)
  do$SIM <- rep(1:(nrow(do)/length(unique(do$ORD))),each=length(unique(do$ORD)))
  if(max(do$SIM)>1) type <- "sim" else type <- "est"
  # if(type %in% "sim"){
  #   do <- do[,names(do)[names(do) %in%
  #                         c("IPRED","IWRES","IRES",
  #                           "PRED","WRES","RES",
  #                           "CWRES","NPDE","DV","SIM",primary.key)]]
  # } else {
  #   do <- do[,names(do)[names(do) %in%
  #                         c("IPRED","IWRES","IRES",
  #                           "PRED","WRES","RES",
  #                           "CWRES","NPDE","DV",primary.key)]]
  # }
  
  names(do)[names(do) %in% "DV"] <-  "DV_OUT"

  d <- read.csv(orig.data,na=".")
  if(FALSE %in% (primary.key %in% names(d))) stop(paste("need primary key in",orig.data))
  
  common.names <- c(names(do)[!names(do) %in% names(d)],primary.key)
  do <- do[,common.names]
  nd <- names(d)
  
  d <- merge(d,do,all.x=TRUE,by=primary.key)
  d <- d[,c(nd,names(d)[!names(d) %in% nd])]
  if(type=="est"){
    if(length(primary.key)==1) d <- d[order(d$ORD), ] else
      d <- d[do.call(order,as.list(d[,primary.key])),]
  }
  if(type=="sim"){
    if(length(primary.key)==1) d <- d[order(d$SIM,d$ORD), ] else
      d <- d[do.call(order,append(d$SIM,as.list(d[,primary.key]))),]
  }
  d
}

output.data.nmexecute <- function(r,primary.key,models.dir=getOption("models.dir"))
  output.data.default(run.no=r$run.no,
                      primary.key=primary.key,
                      models.dir=models.dir)
