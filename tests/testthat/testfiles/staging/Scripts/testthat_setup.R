## Author: klgk669
## First created: 2020-04-11
## Description: 
## Keywords: 

########################################
## load packages and source functions here

library(future)
future::plan("future::multiprocess", workers = 2)
library(NMprojectAZ)
library(dplyr)
devtools::load_all("~/NMproject/")
load_localpackage()

########################################
## main script here

m1 <- nm(run_id = "m1") %>%
  prior_ctl("staging/Models/run1.mod") %>%
  cmd("execute {ctl_name} -dir={run_dir}") %>%
  run_nm()

m2 <- m1 %>% child(run_id = "m2") %>%
  subroutine(advan = 4, trans = 1) %>%
  run_nm()

unlink("~/NMproject/tests/testthat/testfiles/Models/", recursive = TRUE, force = TRUE)
file.copy("Models", "~/NMproject/tests/testthat/testfiles/", recursive = TRUE)

unlink("~/NMproject/tests/testthat/testfiles/DerivedData/", recursive = TRUE, force = TRUE)
file.copy("DerivedData", "~/NMproject/tests/testthat/testfiles/", recursive = TRUE)

unlink("~/NMproject/tests/testthat/testfiles/.cache/", recursive = TRUE, force = TRUE)
file.copy(".cache", "~/NMproject/tests/testthat/testfiles/", recursive = TRUE)

unlink("~/NMproject/tests/testthat/testfiles/staging/", recursive = TRUE, force = TRUE)
file.copy("staging", "~/NMproject/tests/testthat/testfiles/", recursive = TRUE)
