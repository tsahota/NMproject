## Description: Test NMproject functionality
## Run interactively: TRUE
## Keywords: script, package test

########################################
## load packages and source functions here

library(NMproject)

source("Scripts/gof1.R")
source("Scripts/wait.for.R")

########################################
## main script here

## test script
new_script("emptyscript.R")

## setup NONMEM control files
copy_control("runtest.mod","run1.mod")  ## needs data2.csv
copy_file("misc/data2.csv","DerivedData")
copy_control("run1.mod","run2.mod")

########################################
## Test basic NONMEM usage

nm_tran("Models/run1.mod")
system_nm("qpsn -m -c auto -t 3000 -- execute run1.mod -dir=1") ## assumes runXX.mod format
#monitor()
#squeue()
#status(1)
#track_iter(1)
#wait.for(is_lst_finished(1))
#gof1(1,"Models")
#coef.ext("run1.ext")

########################################
## Test R style NONMEM model development.

m2 <- nm("qpsn -m -c auto -t 3000 -- execute run2.mod -dir=2")
nm_tran(m2)
run(m2)
monitor(m2)
squeue()
status(m2)
plot_iter(m2$psn.ext)

wait.for(is_lst_finished(m2))
summary(m2)
gof1(m2$run.no,"Models")
coef(m2)

run_record(1,2)
