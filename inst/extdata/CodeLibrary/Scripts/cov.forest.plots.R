## Author: klgk669
## First created: 2016-08-27
## Description: Forests plots full covariate model
## Run interactively: FALSE
## Keywords: forest plot covariate

########################################
## load packages and source functions here

library(NMproject)
library(ggplot2)

########################################
## main script here

cov.plots <- function(run.no,pars=c("CL","V","KA")){
  
  d0 <- run_record(run.no)
  
  ## get covariates for parameters in pars
  dc <- d0[grepl(paste0("(",paste(pars,collapse="|"),").+"),d0$Parameter),]
  
  dc$CHNG <- 100*as.numeric(gsub("(.+) \\(.+","\\1",dc[[names(dc)[5]]]))
  dc$SE <- 100*as.numeric(gsub(".+ \\((.+)\\)","\\1",dc[[names(dc)[5]]]))
  
  dc$UPP <- dc$CHNG+qnorm(0.975)*dc$SE
  dc$LOW <- dc$CHNG+qnorm(0.025)*dc$SE
  
  p <- list()
  
  p[[length(p)+1]] <- ggplot(dc,aes(x=Parameter,y=CHNG)) + theme_bw()+
    geom_point() + geom_hline(yintercept=0) + geom_errorbar(aes(ymin=LOW,ymax=UPP)) +
    scale_y_continuous("% change in parameter (0=no change)",limits=c(-100,100)) +
    coord_flip() 
  
  pdf(paste0("Results/Cov.plot",run.no,".pdf"))
  print(p)
  dev.off()
  
}