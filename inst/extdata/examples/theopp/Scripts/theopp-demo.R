## Description: Example log of NONMEM runs
## Run interactively: TRUE
## Key words: script, template

########################################
## load packages and source functions here

library(NMproject)
source("Scripts/gof_xpose.R")

########################################
## main script here

interactive_mode(TRUE)

## Initial model
m1 <- nm("execute run1.mod -dir=1 -nmfe_options='-prdefault'")
## NOTE: run NMTRAN only with nm_tran(m1)
run(m1)
## NOTE: track run with shiny_nm()
gof_xpose(m1)

## Same model for demo purposes
## NOTE: create new control with copy_control("run1.mod","run2.mod")
m2 <- nm("execute run2.mod -dir=2 -nmfe_options='-prdefault'")
run(m2)
gof_xpose(m2)

## Bootstrap
m1boot <- nm("bootstrap run1.mod -samples=10 -dir=1boot -nmfe_options='-prdefault'")
run(m1boot)

## VPC
m1vpc <- nm("vpc run1.mod -samples=50 -dir=1vpc -nmfe_options='-prdefault'")
run(m1vpc)

## SSE
m1sse <- nm("sse run1.mod -samples=5 -dir=1sse -nmfe_options='-prdefault'")
run(m1sse)

## SCM
m1scm <- nm("scm run1.mod -config_file=run1.scm -dir=1scm -nmfe_options='-prdefault'")
run(m1scm)


