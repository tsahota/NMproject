## Author: klgk669
## First created: 2016-09-16
## Description: metrumrg snap function 
## Depends on: 
## Run interactively: FALSE
## Keywords: function

########################################
## load packages and source functions here

########################################
## main script here

snap <- function (x, rule = 1, left = TRUE, ...) 
{
  stopifnot(is.numeric(x), is.numeric(rule), is.finite(rule))
  if (!length(x)) 
    return(x)
  rule <- sort(unique(rule))
  if (length(rule) == 1) {
    stopifnot(rule > 0)
    lo <- min(x, na.rm = TRUE)
    hi <- max(x, na.rm = TRUE)
    lo <- (lo%/%rule) * rule - rule
    hi <- (hi%/%rule) * rule + rule
    rule <- seq(from = lo, to = hi, by = rule)
  }
  lt <- findInterval(x, rule)
  rt <- findInterval(x, rule) + 1
  lt[lt == 0] <- 1
  rt[rt > length(rule)] <- length(rule)
  lt <- rule[lt]
  rt <- rule[rt]
  fun <- match.fun(if (left) 
    "<="
    else "<")
  closer <- ifelse(fun(abs(x - lt), abs(rt - x)), lt, rt)
  closer
}
