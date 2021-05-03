## Description: Function template: ggplot GOF
## Instructions: source() this file, then launch on completed model object
## Author:
## Depends on:
## Key words: function, template

gof_ggplot <- function(m){

  wait_finish(m)  ## ensures m is finished
  d <- output_table_first(m)  ## reads in all table files

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



  pdf(file.path(results_dir(m1), paste0("gof_ggplot_run_",run_id(m),".pdf")))
  print(pl)
  dev.off()

}
