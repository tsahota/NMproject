## Description: Function to wait for condition
## Example use:  wait.for(file.exists("run1.con.lst"))
## Run interactively: FALSE
## Key words: function

wait.for <- function(x,timeout=NULL){
  x <- substitute(x)
  start.time <- Sys.time()
  diff.time <- 0
  while (!eval(x,env=parent.frame())){
    diff.time <- difftime(Sys.time(),start.time,units="secs")
    if(!is.null(timeout))if(diff.time > timeout) stop(paste("timed out waiting for\n",x,sep=""))
    Sys.sleep(1)
  }
}
