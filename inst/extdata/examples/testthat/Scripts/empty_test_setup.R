## Copied from staging/Scripts/empty_test_setup.R
##  (2020-02-27 13:26:18) by klgk669
## Copied from staging/Scripts/nm.log2.R
##  (2020-02-05 17:07:24) by klgk669
## Copied from staging/Scripts/nm.log2.R
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
library(NMprojectAZ)
library(dplyr)
devtools::load_all("~/NMproject/")
load_localpackage()

#module_cmd("module load psn")
#module_cmd("module load psn/4.8.1-foss-2017a")
#module_cmd("module load psn/4.4.8-foss-2017a-gfortran-5.2-b1")
module_cmd("module load psn/4.9.0-foss-2017a-gfortran-5.2-test")

#module_cmd("module use /opt/scp/unsupported/PSN_NONMEM/opt/scp/modules/all; module load psn/4.9.0-foss-2017a-gfortran-5.2")


## resolution information

level <- 1  ## NOTE Uncomment this if running directly
#if(!exists("level")) stop("need 'level' to be defined (either 1 or 2)", call. = FALSE)

if(level == 2) {
  rerun_base <- TRUE
  rep_test_n <- 100
  core_by <- 1
  do_bootstrap <- TRUE
}

if(level == 1) {
  rerun_base <- FALSE
  rep_test_n <- 10
  core_by <- 3
  do_bootstrap <- FALSE
}

########################################
## main script here

## setup_nm_demo("testfiles")

## single core tests
## control streams for runs 1 to 6 are in staging area

ds <- tibble(run_id = 1:6)  ## runs 1 to 6 to be run single core

ds$m <- nm(run_id = ds$run_id) %>%
  ctl_contents("staging/Models/run{run_id}.mod") %>%
  #data_path("DerivedData/THEOPP.csv") %>%
  cmd("qpsn -t 100 -r 1000 -- execute run{run_id}.mod -dir={run_dir}")

#nm_tran(ds$m)

m1default <- ds$m[1] %>% child("1default") %>%
  run_in("Models/moduletest") #%>%
#run_nm()


m1new %<-% {
  m1default %>% child(run_id = "1new") %>%
    run_in("Models/moduletestb1") #%>%
  #run_nm()
}

## direct psn test
m1psn %<-% {
  m1new %>% child(run_id = "1psn") %>%
    cmd("execute run{run_id}.mod -dir={run_id}") %>%
    run_in("Models/moduletest") #%>%
  #run_nm()
}

## qnmfe test
m1qnmfe <- {
  ds$m[1] %>% child(run_id = "1qnmfe") %>%
    run_in(file.path(run_in(.), "qnmfe")) %>%
    cmd("qnmfe -t 100 run{run_id}.mod run{run_id}.lst") %>%
    output_location("run{run_id}.lst") #%>%
  #run_nm(force = TRUE)
}

####################################
## repeat run6 100s of times

d6 <- tibble(rep = 1:rep_test_n)
d6$run_id <- paste0("6_", d6$rep)

d6$m <- ds$m[6] %>% child(run_id = d6$run_id) %>%
  run_in("Models/reptest") %>%
  cmd("qpsn -c auto -t 3000 -r 1000 -- execute run{run_id}.mod -dir={run_dir}")

#nm_tran(d6$m[1:2])

# d6 %<-% {
#   d6 %>% mutate(m = m %>% run_nm() %>%
#                   wait_finish(timeout = 120*60) ## 120 mins
#   )
# }

m1boot <- {
  ds$m[1] %>% child(run_id = "m1boot", type = "bootstrap") %>%
    cmd("qpsn -c 5 -t 1000 -r 1000 -- bootstrap run{run_id}.mod -samples=50 -threads=25 -dir={run_dir}")
}


####### summary test

runs <- c(ds$m,
          m1psn,
          m1qnmfe,
          d6$m[1],
          m1boot)

runs %>% write_ctl()

## write commands
paste("cd", run_in(runs), ";", cmd(runs)) %>%
  cat(file = "run_commands.txt", sep = "\n")

proj_name <- basename(getwd())

system_nm(
  paste0("cd .. ; tar -czvf ", proj_name, ".tar.gz ", proj_name, "/"),
  dir = getwd()
)

