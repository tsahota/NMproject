## Author: klgk669
## First created: 2019-12-10
## Description: pop pk demo
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
module_cmd("module load psn/4.4.8-foss-2017a-gfortran-5.2-b1")

########################################
## main script here

read_derived_data("DerivedData/THEOPcov1.csv") %>%
  mutate(EVID = as.numeric(!is.na(AMT)),
         MDV = EVID) %>%
  write_derived_data("THEOPcov2")

c1 <- nm2(run_id = "c1") %>%
  ctl("staging/Models/run1.mod") %>%
  cmd("qpsn -t 100 -r 1000 -- execute {ctl_name} -dir={run_dir}") %>%
  data_path("DerivedData/THEOPcov2.csv") %>%
  gsub_ctl("FORMAT=.*$", "") %>%
  fill_input() %>%
  gsub_ctl("NPDE", "NPDE MDV EVID")

## TODO: add MDV and EVID into sdtab

#nm_tran(c1)

system.time(
  c1 <- c1 %>% run_nm() %>% wait_finish(timeout = 5*60)
)

## need gofs for this.

## TODO: modify this to use 2 dollar problems

# ; Problem no. 2
# $PROBLEM    Model simulations
# $INPUT      ID DOSE DV SCR AGE SEX CLASS WT ACE MED1 MED2 TAD TIME
#             CLCR AMT SS II EVID
# $DATA      mx19_2.csv IGNORE=@ REWIND
# $MSFI      msf001
# $SIMULATION (221287) ONLYSIM NSUB=20                          ; Small sim for illustration only
# $TABLE      ID DV IPRED DOSE AMT TIME TAD EVID SEX CLCR AGE WT
# ONEHEADER NOPRINT NOAPPEND FILE=simtab001         ; Simulation table

es1 <- c1 %>% child(run_id = "es1") %>%
  gsub_ctl("SIGL=9", "SIGL=9 MSFO=msfes1") %>%
  text("
$PROBLEM simulation
$INPUT ID AMT TIME DV WT LIN1 LIN2 LIN3 RND1 RND2 RND3 PW1 PW2 PW3 BN1 EVID MDV
$DATA ../DerivedData/THEOPcov2.csv IGNORE=@ REWIND
$MSFI      msfes1
$SIMULATION (221287) ONLYSIM SUBPR=20 TRUE=FINAL
$TABLE      ID AMT TIME DV WT LIN1 LIN2 LIN3 RND1 RND2 RND3 PW1 PW2 PW3 BN1 EVID MDV IPRED
   ONEHEADER NOPRINT NOAPPEND FILE=simtabes1
       ", append = TRUE)


nm_tran(es1)

system.time(
  es1 <- es1 %>% run_nm() %>% wait_finish(timeout = 5*60)
)

plot_job_run_times_nm(c(c1, es1))

## use new xpose

library(xpose)

xpdb <- xpose_data(runno = 'c1', dir = "Models")
xpdb
summary(xpdb, problem = 1)
dv_vs_ipred(xpdb)
ind_plots(xpdb, page = 1)
xpdb %>% 
  vpc_data() %>% 
  vpc()
eta_distrib(xpdb, labeller = 'label_value')
prm_vs_iteration(xpdb, labeller = 'label_value')

xpdb <- xpose_data(runno = 'es1', dir = "Models")
xpdb
summary(xpdb, problem = 1)
dv_vs_ipred(xpdb)
ind_plots(xpdb, page = 1)
xpdb %>% 
  vpc_data() %>% 
  vpc()
eta_distrib(xpdb, labeller = 'label_value')
prm_vs_iteration(xpdb, labeller = 'label_value')
