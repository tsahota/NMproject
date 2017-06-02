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
m1 <- nm("qpsn -c auto -t 3000 -- execute run1.mod -dir=1")
#nm_tran(m1) ## will run NMTRAN check on control stream
run(m1)
#shiny_nm() ## will open GUI
gof1(m1$run_id)

## Same model for demo purposes
#copy_control("run1.mod","run2.mod") ## create control file from existing
m2 <- nm("qpsn -c auto -t 3000 -- execute run2.mod -dir=2")
run(m2)
gof1(m2$run_id)

## Bootstrap
m1boot <- nm("qpsn -c auto -t 3000 -- bootstrap run1.mod -samples=10 -dir=1boot")
run(m1boot)

## VPC
m1vpc <- nm("qpsn -c auto -t 3000 -- vpc run1.mod -samples=50 -dir=1vpc")
run(m1vpc)

## SSE
m1sse <- nm("qpsn -c auto -t 3000 -- sse run1.mod -samples=5 -dir=1sse")
run(m1sse)

## SCM
m1scm <- nm("qpsn -c auto -t 3000 -- scm run1.mod -config_file=run1.scm -dir=1scm")
run(m1scm)


