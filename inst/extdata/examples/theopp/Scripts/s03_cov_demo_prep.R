## Copied from staging/Scripts/s03_cov_demo_prep.R
##  (2020-12-12 14:44:54) by tarjinde
## Copied from staging/Scripts/prep_cov_test.R
##  (2020-04-22 19:27:40) by klgk669
## Copied from staging/Scripts/cov_test.R
##  (2020-01-07 19:55:27) by klgk669
## Copied from staging/Scripts/nm.log2.R
##  (2019-12-30 21:25:32) by klgk669
## Author: klgk669
## First created: 2019-12-10
## Description: Model development script
## Keywords:

########################################
## load packages and source functions here

library(future)
future::plan("future::multiprocess", workers = 2)
library(NMproject)
library(dplyr)
load_localpackage()

########################################
## main script here

## Use existing model to generate ETAs - produce fake covariates that are
##  correlated and then try to recapitulate them

c0 <- readRDS("Results/m1.RDS")

## read in the outputs

dc <- output_table_first(c0)

dc <- dc %>% select(ID, ETA1:ETA3) %>% unique() %>%
  ## exponentiate to be linearly related to params
  mutate_at(c("ETA1", "ETA2" ,"ETA3"), exp)

## define true relationships here
set.seed(1)
sd_all <- 0.1
dc$LIN1 <- rnorm(n = nrow(dc),
               mean = dc$ETA1 + 2, sd = sd_all)
dc$LIN2 <- rnorm(n = nrow(dc),
               mean = dc$ETA2 + 2, sd = sd_all)
dc$LIN3 <- rnorm(n = nrow(dc),
               mean = dc$ETA3 + 2,sd = sd_all)
dc$RND1 <- rnorm(n = nrow(dc),
                 mean = 0,sd = sd_all)
dc$RND2 <- rnorm(n = nrow(dc),
                 mean = 0, sd = sd_all)
dc$RND3 <- rnorm(n = nrow(dc),
                 mean = 0, sd = sd_all)
dc$PW1 <- rnorm(n = nrow(dc),
                 mean = dc$ETA1**0.75, sd = sd_all)
dc$PW2 <- rnorm(n = nrow(dc),
                 mean = dc$ETA2**0.75, sd = sd_all)
dc$PW3 <- rnorm(n = nrow(dc),
                 mean = dc$ETA3**0.75, sd = sd_all)

dc$BN1 <- rbinom(n = nrow(dc),
                 prob = dc$ETA1/max(dc$ETA1), size = 1)

## prevent covariates coming out negative
force_floor <- function(x, floor = 0.01) {
  x[x < floor] <- floor
  x
}

dc <- dc %>% mutate_at(c("LIN1", "LIN2", "LIN3",
                         "RND1", "RND2", "RND3",
                         "PW1", "PW2", "PW3"),
                       force_floor)

## plot these as a sanity check
library(ggplot2)

dc1 <- dc %>% select(ETA1, LIN1, PW1) %>%
  tidyr::gather(key = "cov", value = "value", LIN1, PW1)

ggplot(dc1, aes(x = ETA1, y = value)) + theme_bw() +
         geom_point(aes(colour = cov))

dc2 <- dc %>% select(ETA2, LIN2, PW2) %>%
  tidyr::gather(key = "cov", value = "value", LIN2, PW2)

ggplot(dc2, aes(x = ETA2, y = value)) + theme_bw() +
  geom_point(aes(colour = cov))

dc3 <- dc %>% select(ETA3, LIN3, PW3) %>%
  tidyr::gather(key = "cov", value = "value", LIN3, PW3)

ggplot(dc3, aes(x = ETA3, y = value)) + theme_bw() +
  geom_point(aes(colour = cov))

## happy with plots, select final covs
dcf <- dc %>% select(ID, LIN1:BN1)

## merge with data and create new dataset
input_data(c0) %>% merge(dcf) %>%
  write_derived_data("THEOPcov1")

