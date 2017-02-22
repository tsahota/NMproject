## Author: Tarjinder Sahota (klgk669)
## First created: 2016-01-29
## Description: NONMEM testing suite
## Depends on: wait.for.R
## Run interactively: FALSE
## Keywords: script, package test

########################################
## load packages and source functions here

library(NMproject,lib.loc = "ProjectLibrary")

source("Scripts/wait.for.R")

########################################
## main script here

## m1 = CONTROL5 on cluster
## m2 = CONTROL5 on head node

copy_control("/opt/az/icondevsolutions/nonmem/standard.7.3.0/run/CONTROL5","run1.mod")
file.copy("/opt/az/icondevsolutions/nonmem/standard.7.3.0/run/THEOPP","Models")

m1 <- nm("qpsn -c auto -t 1000 -- execute run1.mod -dir=1")
run(m1)

copy_control("run1.mod","run2.mod")
m2 <- nm("execute run2.mod -dir=2")
run(m2,force=TRUE)

copy_control("run1.mod","run3.mod")
m3 <- nm("qpsn -t 1000 -- execute run3.mod -dir=3")
run(m3)

copy_control("run1.mod","run4.mod")
m4 <- nm("qpsn -c auto -t 1000 -r 1000 -- execute run4.mod -dir=4")
run(m4)

copy_control("run1.mod","run5.mod")
m5 <- nm("qpsn -c auto -t 5000 -r 1000 -- execute run5.mod -dir=5")
run(m5)

## need to figure out what files are created in finished runs (successful and failed)
##


