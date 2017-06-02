## Description: Example log of NONMEM runs
## Run interactively: TRUE
## Key words: script, template

########################################
## load packages and source functions here

library(NMproject)
source("Scripts/gof1.R")

########################################
## main script here

## Initial model
m1 <- nm("execute run1.mod -dir=1")
#nm_tran(m1) will run NMTRAN check on control stream
run(m1)
#shiny_nm() will
gof1(m1$run_id)

## Same model for demo purposes
m2 <- nm("execute run2.mod -dir=2")
run(m2)
gof1(m2$run_id)

## Bootstrap
m1boot <- nm("bootstrap run1.mod -samples=10 -dir=1boot")
run(m1boot)

## VPC
m1vpc <- nm("vpc run1.mod -samples=50 -dir=1vpc")
run(m1vpc)

## SSE
m1sse <- nm("sse run1.mod -samples=5 -dir=1sse")
run(m1sse)

## SCM
m1scm <- nm("scm run1.mod -config_file=run1.scm -dir=1scm")
run(m1scm)


