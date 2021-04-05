## Description: Example log of NONMEM runs (AZ specific)
## Run interactively: TRUE
## Key words: script, template

########################################
## load packages and source functions here

library(NMprojectAZ)
source("Scripts/gof_xpose.R")

########################################
## main script here

interactive_mode(TRUE)

## Initial run  
m1 <- nm("qpsn -c auto -t 3000 -- execute run1.mod -dir=1")
run(m1)
## NOTE: track run with shiny_nm()
gof_xpose(m1)  ## post processing on model 1

## NOTE: create new control with copy_control("run1.mod","run2.mod")
# m2 <- nm("qpsn -c auto -t 3000 -- execute run2.mod -dir=2")
# run(m2)
# gof_xpose(m2)

## examples for other psn commands
# m1boot <- nm("qpsn -c auto -t 3000 -- bootstrap run1.mod -samples=10 -dir=1boot")
# run(m1boot)
# 
# m1vpc <- nm("qpsn -c auto -t 3000 -- vpc run1.mod -samples=50 -dir=1vpc")
# run(m1vpc)
# 
# m1scm <- nm("qpsn -c auto -t 3000 -- scm run1.mod -config_file=run1.scm -dir=1scm")
# run(m1scm)
#
# m1sse <- nm("qpsn -c auto -t 3000 -- sse run1.mod -samples=5 -dir=1sse")
# run(m1sse)
