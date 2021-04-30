## Description: Function template: Xpose GOF
## Instructions: source() this file, then launch on completed model object
## Run interactively: FALSE
## Key words: function, template
## E.g. for sdtab37, run with gof1(37)

gof_xpose <- function(m){

  wait_finish(m)  ## ensures m is finished

  ## assumes the existance of an sdtab $TABLE file e.g.
  ##   $TABLE ID TIME IPRED IWRES IRES CWRES NPDE
  ##   FILE=sdtab[run.no] NOPRINT ONEHEADER FORMAT=tF13.4


  library(xpose)

  xpdb <- xpose_data(runno = run_id(m), dir = run_in(m))

  pdf(file.path(results_dir(m1), paste0("gof_xpose_run_",run_id(m),".pdf")))

  print(dv_vs_pred(xpdb))
  print(dv_vs_ipred(xpdb))
  print(pred_vs_idv(xpdb))
  print(ipred_vs_idv(xpdb))
  
  dev.off()


}
