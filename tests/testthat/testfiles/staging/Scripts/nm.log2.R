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

if(!exists("module_load_cmd")){
  module_load_cmd <- "module load psn"
}

if(!exists("level")){
  level <- 1
}

module_cmd(module_load_cmd)
#module_cmd("module load psn")
#module_cmd("module load psn/4.8.1-foss-2017a")
#module_cmd("module load psn/4.4.8-foss-2017a-gfortran-5.2-b1")
#module_cmd("module load psn/4.9.0-foss-2017a-gfortran-5.2-test")

## resolution information
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
  cmd("qpsn -t 59 -r 1000 -- execute run{run_id}.mod -dir={run_dir}")

#nm_tran(ds$m)

# ds$m <- ds$m %>% run_nm()
# ds$m <- ds$m %>% update_parameters()
# ds$m <- ds$m %>% run_nm()

if(rerun_base){
  ds %<-% {
    ds %>% mutate(
      m = m %>% run_nm() %>%
        update_parameters() %>%
        run_nm() %>%
        wait_finish(timeout = 20*60) ## wait max of 20 mins
    )
  }
} else {
  ds %<-% {
    ds %>% mutate(
      m = m %>% run_nm() %>%
        wait_finish(timeout = 20*60) ## wait max of 20 mins
    )
  }
}

summary(ds$m) %>%
  select(run_id, status, ofv:cond_num)

## gof plot of first run
pl <- ds$m[1] %>% gof_ggplot2()

pl2 <- plot_job_run_times_nm(ds$m)
pl2[6]

ds$m <- ds$m %>%
  nmsave_plot(gof_ggplot2(.), plot_name = "gof_{run_id}.pdf") %>%
  nmsave_plot(gof_ggplot2(.), plot_name = "gof_{run_id}.png")

ds$m[1] <- ds$m[1] %>%
  nmsave_plot(pl2[6], plot_name = "run_times_1:6.png")

ds$m[1] <- ds$m[1] %>%
  nmsave_plot(pl2[[6]], plot_name = "run_times_1:6.png")


# module_cmd("module load psn")
# m1default <- ds$m[1] %>% child("1default") %>%
#   run_in("Models/moduletest") %>%
#   run_nm()
#
#
# m1new %<-% {
#   m1default %>% child(run_id = "1new") %>%
#     run_in("Models/moduletestb1") %>%
#     run_nm()
# }
#
# ## direct psn test
# m1psn %<-% {
#   m1new %>% child(run_id = "1psn") %>%
#     cmd("execute run{run_id}.mod -dir={run_id}") %>%
#     run_nm()
# }
#
# ## qnmfe test
# m1qnmfe %<-% {
#   m1new %>% child(run_id = "1qnmfe") %>%
#     run_in(file.path(run_in(.), "qnmfe")) %>%
#     cmd("qnmfe -t 100 run{run_id}.mod run{run_id}.lst") %>%
#     output_location("run{run_id}.lst") %>%
#     run_nm()
# }
#
# ## nmfe test in own directory
# m1nmfe %<-% {
#   m1new %>% child(run_id = "1nmfe") %>%
#     run_in(file.path(run_in(.), "nmfe")) %>%
#     cmd("nmfe73 run{run_id}.mod run{run_id}.lst") %>%
#     output_location("run{run_id}.lst") %>%
#     run_nm()
# }

####################################
## repeat run6 100s of times

d6 <- tibble(rep = 1:rep_test_n)
d6$run_id <- paste0("6_", d6$rep)

d6$m <- ds$m[6] %>% child(run_id = d6$run_id) %>%
  run_in("Models/reptest") %>%
  cmd("qpsn -c auto -t 59 -r 1000 -- execute run{run_id}.mod -dir={run_dir}")

#nm_tran(d6$m[1:2])

d6 %<-% {
  d6 %>% mutate(m = m %>% run_nm() %>%
                  wait_finish(timeout = 120*60) ## 120 mins
  )
}

####### summary test

s6 <- summary(d6$m)

status_table(d6$m)

s6 %>% select(run_id, status, ofv:cond_num) %>% head(10)

p62 <- plot_job_run_times_nm(d6$m)
p62[[6]]

d6$m[1] %>%
  nmsave_table(status_table(d6$m), "d6_run_status.csv") %>%
  nmsave_plot(p62[[6]], "d6_run_times.png")


####################################
## repeat run6 with different numbers of cores

dc %<-% {
  tibble(cores = seq(1,36, by = core_by)) %>%
    mutate(m = ds$m[6] %>% child(run_id = cores) %>%
             cmd("qpsn -c {run_id} -t 1000 -r 1000 -- execute run{run_id}.mod -dir={run_dir}") %>%
             run_in("Models/coretest") %>%
             run_nm() %>%
             wait_finish(timeout = 120*60) ## 2 hour
    )
}

sc <- summary(dc$m)

status_table(dc$m)

sc %>% select(run_id, status, ofv:cond_num) %>% head(5)

pl <- plot_job_run_times(jobs = job_info(dc$m),
                         run_in = run_in(dc$m))


## print pl[[6]]

dc$m[1] %>%
  nmsave_table(status_table(dc$m), "dc_run_status.csv") %>%
  nmsave_plot(pl[[6]], "dc_run_times.png")


###################################

## run a bootstrap

if(do_bootstrap){
  m1boot %<-% {
    ds$m[1] %>% child(run_id = "m1boot", type = "bootstrap") %>%
      cmd("qpsn -c 5 -t 1000 -r 1000 -- bootstrap run{run_id}.mod -samples=50 -threads=25 -dir={run_dir}") %>%
      run_nm() %>%
      wait_finish(timeout = 120*60) ## 2 hour
  }

  status_table(m1boot)
}


##########################################################


#system("ls Models/psn-* | xargs grep -l -E '(Not restarting this model)|(Fatal error)|(No nonmem execution)'",
#       intern = TRUE)

