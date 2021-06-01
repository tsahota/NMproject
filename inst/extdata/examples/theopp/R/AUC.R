#' @title Calculate AUC
#'
#' @description Computes AUC by trapezoidal rule.
#'
#'   The style of documentation is from `roxygen2`.  It is how package
#'   documentation is created and may be of interest for those users looking to
#'   turn their functions into shareable packages.  The learning curve is fairly
#'   large though so if this isn't a strong interest, don't worry about
#'   including these kinds of comments for your functions.  You can leave them
#'   undocumented or document them in a more informal way.  Documenting your
#'   functions in this style is optional but it will enable use of `?AUC` to
#'   allow yourself and others to browse documentation and make it easy to
#'   incorporate your function into a package at a later date if you believe it
#'   will be useful to others.
#'
#'   Also available is the `testthat` package.  If you want you can construct
#'   automated unit tests for this function.
#'
#'   All of this is optional though, you can just leave functions undocumented
#'   if you want.
#'
#' @author Tarj Sahota, Peter Lawrence
#'
#' @param time Numeric vector of times to integrate over.
#' @param conc Numeric vector of concentrations to integrate.
#' @param loq Numeric value (default = 0).  Conc values below this will be set
#'   to 0.
#' @param method Character indicating calculation method of trapezoidal area.
#'
#' @return Will return a single number for the AUC.  This function is most
#'   useful using `dplyr`.
#'
#' @seealso [NMproject::ppc_data()]
#' @examples
#'
#' \dontrun{
#'
#' AUC(d$TIME, d$DV)  ## return AUC number
#'
#' ## data.frame with AUC value for each ID (one row per ID)
#' d %>% group_by(ID) %>%
#'   summarise(AUC = AUC(TIME, DV),
#'             CMAX = max(DV),
#'             TMAX = TIME[which.max(DV)])
#'
#' ## Add exposure columns to a dataset
#' d %>% group_by(ID) %>%
#'   mutate(AUC = AUC(TIME, DV),
#'             CMAX = max(DV),
#'             TMAX = TIME[which.max(DV)])
#'
#' }
#' @export

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
