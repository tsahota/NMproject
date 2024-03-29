#' Create dosing times
#' 
#' @param dtimes Dosing times.
#' @param cycle_length Length of cycle.
#' @param ncycles Number of cycles.
#' @param max_time Maximum time.
#' @param make_hours Default=`TRUE`.
#' 
#' @author Nuria Buil Bruna
#' @examples 
#' \dontrun{
#' To generate Monday, Thursday dosing for 7 weeks:
#' rep_times(dtimes=c(1,4), cycle_length = 7, ncycles=10)
#' OR
#' rep_times(dtimes=c(1,4), cycle_length = 7, max_time = 70)
#' 
#' Intermittent 3-on, 4-off dosing
#' inter_times(on=3, off=4, tau= 1, max_time = 70)
#' in hours:
#' inter_times(on=3, off=4, tau= 24, max_time = 168*4)
#' }
#' @export
#' @name dosing-times

rep_times <- function(dtimes,cycle_length,ncycles,max_time,make_hours=TRUE,...){
  if(missing(ncycles)) ncycles <- max_time/cycle_length
  
  #base_times <- seq(0,cycle_length*ncycles,cycle_length) # with this you end up with one cycle more than you want - modified it below 
  base_times <- seq(0,(cycle_length*ncycles-cycle_length),cycle_length) 

  d <- expand.grid(base_times=base_times,dtimes=dtimes)
  d.dose <- sort(d$dtimes + d$base_times)
  if(make_hours==T) d.dose <- d.dose*24-24 # this will make first dose at t=0 h

}

#' @rdname dosing-times
#' @param on on/off regimen times
#' @param off on/off regimen times
#' @param tau dosing interval
#' @param ... parameters passed to `rep_times()`
#' @export

inter_times <- function(on,off,tau,...){
  dtimes <- seq(0,on*tau,tau)
  dtimes <- dtimes[-length(dtimes)]
  cycle_length <- (on+off)*tau
  rep_times(dtimes=dtimes,cycle_length=cycle_length,...)
}

