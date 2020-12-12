## Copied from staging/localpackage/R/AUC.R
##  (2020-04-03 18:18:14) by klgk669
## Copied from /home/klgk669/poppk_20180926_pk-eff-ae-interim2/Scripts/AUC.R
##  (2019-01-23 14:32:02) by klgk669
## Copied from /home/klgk669/poppk_20180522_pk-eff-ae_cut28Feb2018/Scripts/AUC.R
##  (2018-10-11 12:58:58) by klgk669
## Copied from /projects/qcp/QCP_MODELING/PMXcodelibrary/R/AUC.R
##  (2018-08-15 15:26:51) by klgk669
## Description: Basic NCA AUC function
## Key words: NCA, AUC, function
## Author: Tarj Sahota, Peter Lawrence
## Run interactively: FALSE

AUC <- function(time, conc, loq=0,method=c("linuplogdown","linuplogdowntmax","linear"))
{
  method <- match.arg(method)
  trap.log.rule <- function(x,y){ ## custom function for log down
    y1 <- y[-length(y)]
    y2 <- y[-1]
    sum(diff(x)*(y1-y2)/(log(y1)-log(y2)))      
  }
  ok = !is.na(time) & !is.na(conc) & conc >= loq
  
  time <- time[ok]
  conc <- conc[ok]
  
  if(method=="linear") return(Hmisc::trap.rule(time, conc))
  if(method=="linuplogdowntmax"){
    tmax <- time[which.max(conc)]
    return(Hmisc::trap.rule(time[time<=tmax],conc[time<=tmax]) +
             trap.log.rule(time[time>=tmax],conc[time>=tmax]))
  }
  if(method=="linuplogdown"){
    up.diffs <- which(diff(conc)>=0)
    down.diffs <- which(diff(conc)<0)
    auc <- 0
    if(length(up.diffs)>0){
      linup <- sum(sapply(up.diffs,function(i){
        Hmisc::trap.rule(time[i:(i+1)],conc[i:(i+1)])
      }))
      auc <- auc + linup
    }
    if(length(down.diffs)>0){
      logdown <- sum(sapply(down.diffs,function(i){
        trap.log.rule(time[i:(i+1)],conc[i:(i+1)])
      }))
      auc <- auc + logdown
    }
    return(auc)
  }
}
