## Author: klgk669
## First created: 2016-11-09
## Description: Binomial regression meta-analysis in R
## Depends on: 
## Run interactively: TRUE
## Keywords: binomial, meta-analysis, script, template

########################################
## load packages and source functions here

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

#############################
## Generate some fake data

d1 <- data.frame(TRT=1,
                 nr=c(0.1,0.3,0.6),
                 N=c(10,20,60))

d2 <- data.frame(TRT=2,
                 nr=c(0.2,0.3,0.8,0.6,0.88),
                 N=c(10,20,40,20,30))

d <- rbind(d1,d2)

## make some fake quantiles
d$UPP <- qbinom(0.9,size=d$N,prob=d$nr)/d$N
d$LOW <- qbinom(0.1,size=d$N,prob=d$nr)/d$N
d$STUDY <- 1:nrow(d)
d$TRT <- factor(d$TRT)
d$estimated <- "raw data"

p <- ggplot(d,aes(x=STUDY,y=nr,colour=TRT)) +theme_bw() +
  geom_point() + geom_errorbar(aes(ymin=LOW,ymax=UPP)) +
  coord_flip()

#########################################
## binomial regression

fit <- glm(nr~TRT,data=d,weights=N,family=binomial)
fit
print(summary(fit)) ## See statistical test.

pred <- predict(fit,se.fit = TRUE)


## combine with observed data
dp <- d
dp$PRED <- plogis(pred$fit)
dp$PRED.LOW <- plogis(pred$fit-1.96*pred$se.fit)
dp$PRED.UPP <- plogis(pred$fit+1.96*pred$se.fit)
dp <- select(dp,TRT,PRED,PRED.LOW,PRED.UPP) %>% unique()
names(dp) <- c("TRT","nr","LOW","UPP")
dp$estimated <- "estimated"
dp$STUDY <- paste0("TRT",dp$TRT)
d <- merge(d,dp,all=TRUE)

p <- ggplot(d,aes(x=STUDY,y=nr,colour=TRT)) +theme_bw() +
  geom_point() + geom_errorbar(aes(ymin=LOW,ymax=UPP,linetype=estimated)) +
  coord_flip()
print(p)

