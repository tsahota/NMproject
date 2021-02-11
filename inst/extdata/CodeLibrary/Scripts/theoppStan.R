## Description: Stan PK (Rbloggers 19/04/15)
## Run interactively: TRUE
## Keywords: Stan, script, template

########################################
## load packages and source functions here

library(ggplot2)
library(rstan)

########################################
## main script here

Theoph$dose <- Theoph$Dose*Theoph$Wt
datain <-  list(
  Time=Theoph$Time,
  conc=Theoph$conc,
  n=nrow(Theoph),
  subject =c(1:nlevels(Theoph$Subject))[Theoph$Subject],
  nsubject = nlevels(Theoph$Subject),
  dose=unique(subset(Theoph,, c(Subject,dose)))$dose
)

smodel <- '
data {
  int <lower=0> n;
  int <lower=0> nsubject;
  int <lower=1,upper=nsubject>  subject[n];
  real  dose[nsubject];
  real  Time[n];
  real  conc[n];
}
parameters {
  real <lower=0> sdr;
  real <lower=0> kas[nsubject];
  real <lower=0> kes[nsubject];
  real <lower=0> CLs[nsubject];
  real lke;
  real lka;
  real <lower=0> CL;
  real kesd;
  real kasd;
  real CLsd;
}
transformed parameters {
  real pred[n];
  real <lower=0> c0star[nsubject];
  for (i in 1:nsubject) {
    c0star[i] <- dose[i]*((kes[i]*kas[i])/CLs[i])/
      (kas[i]-kes[i]);
  }
  for (i in 1:n) {
    pred[i] <- c0star[subject[i]]*
      (exp(-kes[subject[i]]*Time[i])-exp(-kas[subject[i]]*Time[i]));
  }
}
model {
  conc ~ normal(pred,sdr);
  kes ~ lognormal(lke,kesd);
  kas ~ lognormal(lka,kasd);
  lke ~ uniform(-3,3);
  lka ~ uniform(-3,3);
  CL ~ uniform(0.01,300);
  CLs ~ normal(CL,CLsd);
  kesd ~ uniform(0.01,2);
  kasd ~ uniform(0.01,2);
  CLsd ~ uniform(0.01,10);
}
generated quantities {
  real Ka;
  real Ke;
  Ka <- exp(lka);
  Ke <- exp(lke);
}'

fstan <- stan(model_code = smodel, 
              data = datain, 
              pars=c('CL','kes','kas','CLs',
                      'c0star','Ka','Ke'),
              init= function() {
                list(lka = rnorm(1,log(2),.3),
                     kas=   rnorm(12,2,.2), 
                     lke=   rnorm(1,log(.08),.01),
                     kes=   rnorm(12,.08,.01),
                     CLs = rnorm(12,1,.1),
                     CL = rnorm(1,1,.1),
                     kesd=runif(1,0.04,0.1),
                     kasd=runif(1,0.2,2),
                     sdr= runif(1,1,3),
                     CLsd=runif(1,.1,1)
                )})
fstan

stansampls <- extract(fstan,c('CL','Ke','Ka'))
Time <- seq(1,max(datain$Time))

la2 <- sapply(1:nrow(stansampls$CL),
              function(i) {
                CL <- stansampls$CL[i]
                Ka <- stansampls$Ka[i]
                Ke <- stansampls$Ke[i]
                c0star <- mean(datain$dose)*((Ke*Ka)/CL)/(Ka-Ke)
                y<- c0star* (exp(-Ke*Time)-exp(-Ka*Time))
              })

res2 <- data.frame(
  Time=Time,
  Conc=apply(la2,1,mean),
  C05=apply(la2,1,FUN=function(x) quantile(x,0.05)),
  C95=apply(la2,1,FUN=function(x) quantile(x,0.95)))

ggplot(res2, aes(x=Time)) +
  geom_line(aes(y=Conc))+
  scale_y_log10('theophylline (mg/L)')+
  geom_line(aes(y=conc,x=Time,col=Subject),alpha=1,data=Theoph)+
  geom_ribbon(aes(ymin=C05, ymax=C95),alpha=.2)+
  theme(legend.position='bottom')
