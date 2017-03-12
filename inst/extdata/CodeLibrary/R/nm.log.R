## Description: Example log of NONMEM runs Depends on: gof1.R Run interactively: TRUE
## Key words: script, template

################################### If NM control stream directory != project base directory, need: setwd(...full path
################################### to model directory...)  and scripts.dir <- ...relative path of scripts directory
################################### from model directory...

######################################## load packages and source functions here

library(NMproject, lib.loc = "ProjectLibrary")

source("Scripts/wait.for.R")
source("Scripts/gof1.R")

######################################## main script here

m1 <- nm("qpsn -m -c auto -t 3000 -- execute run1.mod -dir=1")
run(m1)
wait.for(!is_job_running(m1))
run_record(1)
gof1(m1$run.no)

m2 <- nm("qpsn -m -c auto -t 3000 -- execute run2.mod -dir=2")
run(m2)
wait.for(!is_job_running(m2))
run_record(2)
gof1(m2$run.no)
coef(m2)

m2s <- nm("qpsn -m -c auto -t 3000 -- scm run3.scm -dir=3")
