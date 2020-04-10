## Copied from staging/localpackage/R/gof_ggplot2.R
##  (2019-12-30 21:25:28) by klgk669
## Author: klgk669
## First created: 2019-12-28
## Description: 
## Keywords: 

########################################
## load packages and source functions here

library(NMproject)

########################################
## main script here

##' @export
gof_ggplot2 <- Vectorize_nm_list(function(m){
  wait_finish(m)
  # d <- output_table(m) ## read in combined output 
  
  ###############
  ## include plotting code here
  
  ## assumes existance of "plots" directory in main working directory (plots.dir)
  
  ## assumes the existance of an sdtab $TABLE file e.g.
  ##   $TABLE ID TIME IPRED IWRES IRES CWRES NPDE
  ##   FILE=sdtab[run.no] NOPRINT ONEHEADER FORMAT=tF13.4
  
  d <- NMproject::output_table(m)
  
  library(ggplot2)
  
  pl <- list()
  p <- ggplot(d,aes_string(x="PRED",y="DV")) + theme_bw() +
    geom_abline(slope=1)+
    geom_point()
  pl[[length(pl)+1]] <- p
  pl[[length(pl)+1]] <- p + scale_x_log10() + scale_y_log10()
  
  p <- ggplot(d,aes_string(x="IPRED",y="DV")) + theme_bw() +
    geom_abline(slope=1)+
    geom_point()
  pl[[length(pl)+1]] <- p
  pl[[length(pl)+1]] <- p + scale_x_log10() + scale_y_log10()
  
  maxCWRES <- max(abs(d$CWRES),na.rm=TRUE)
  p <- ggplot(d,aes_string(x="PRED",y="CWRES")) + theme_bw() +
    geom_hline(yintercept=0)+
    geom_point() + scale_y_continuous(limits=c(-1.05*maxCWRES,1.05*maxCWRES))
  pl[[length(pl)+1]] <- p
  
  p <- ggplot(d,aes_string(x="TIME",y="CWRES")) + theme_bw() +
    geom_hline(yintercept=0)+
    geom_point() + scale_y_continuous(limits=c(-1.05*maxCWRES,1.05*maxCWRES))
  pl[[length(pl)+1]] <- p
  
  maxNPDE <- max(abs(d$NPDE),na.rm=TRUE)
  p <- ggplot(d,aes_string(x="PRED",y="NPDE")) + theme_bw() +
    geom_hline(yintercept=0)+
    geom_point() + scale_y_continuous(limits=c(-1.05*maxNPDE,1.05*maxNPDE))
  pl[[length(pl)+1]] <- p
  
  p <- ggplot(d,aes_string(x="TIME",y="NPDE")) + theme_bw() +
    geom_hline(yintercept=0)+
    geom_point() + scale_y_continuous(limits=c(-1.05*maxNPDE,1.05*maxNPDE))
  pl[[length(pl)+1]] <- p
  
  plg <- gridExtra::marrangeGrob(pl[-c(2,4)], ncol = 2, nrow = 3)
  
  ###############
  ## return plotting object here
  return(plg)
  
}, SIMPLIFY = FALSE)
