## Description: PK simulation deSolve
## Run interactively: TRUE
## Keywords: deSolve, template, script

library(manipulate)  ## for interactivity : Requires Rstudio
library(ggplot2)     ## for plotting

############################

## POPULATION PARAMETERS/VARIABLES
p <- c(CL=250,V2=3100,V3=2900,Q=336,KA=0.75)
om <- diag(c(0.1, 0.1,0.1, 0.1, 0.1))

## GENERATE INDIVIDUAL PARAMETERS/VARIABLES
n.ind <- 1
params <- MASS::mvrnorm(n.ind, mu = log(p),Sigma = om)  ## multivarite log-normal distribution
params <- exp(params)
if(n.ind %in% 1) params <- t(params)  ## bug fix for n.ind = 1

## loop through matrix row by row (i.e. by ID) output data.frame as you go
out <- plyr::ldply(1:nrow(params),function(id){
  
  ## define compartment names and specify initial values
  yini <- c(Adepot = 0, Ablood = 0, Aperiph = 0)
  
  dose <- 40
  tau <- 24
  end <- 7*tau
  
  ## Define dosing events for bolus doses longitudinally (like in nonmem)
  dose.events <- data.frame(var="Adepot",time=seq(0,end,by=tau),value=dose,method="add")
  
  ## Define function for ODEs
  pkeq <- function(t, y, p) {with(as.list(p),{  
    
    ## define any infusions here
    infusion <- 0  ## ifelse(t%%24<2,10,0) = Every 24 hrs a 2hr infustion at rate of 10
    
    dy1 <- - KA* y[1]
    dy2 <- infusion + KA* y[1] - (CL/V2) *y[2] - (Q/V2)*y[2] + (Q/V3)*y[3]
    dy3 <- (Q/V2)*y[2] - (Q/V3)*y[3]
    list(c(dy1, dy2, dy3))
    
  })}  
  
  ## observation times
  times <- seq(0,end,0.5)
  
  outi <- deSolve::ode(func = pkeq, times = times,
                       y = yini, parms = params[id,],events=list(data=dose.events))
  outi <- as.data.frame(outi)
  outi$Cblood <- outi$Ablood/params[id,"V2"]
  outi$ID <- id
  outi
})

## create any plot you want, below I do a vpc type plot.
ds <- plyr::ddply(out,"time",summarise,
                  low = quantile(Cblood,probs=0.025),
                  mid = quantile(Cblood,probs=0.5),
                  hi = quantile(Cblood,probs=0.975))

## plot the results
xbreaks <- c(seq(0,56,by=7),seq(56,168,by=7*4))  
print(ggplot(ds,aes(x=time,y=mid)) + geom_line() + theme_bw() + 
        geom_ribbon(aes(ymin=low,ymax=hi),fill="orange",alpha=0.4) +
        scale_x_continuous("Time (hrs)")+#,breaks=xbreaks) +
        scale_y_continuous("Plasma concentration (units)"))

