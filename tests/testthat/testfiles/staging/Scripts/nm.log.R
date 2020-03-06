## Author: klgk669
## First created: 2017-10-19
## Description: Model development script
## Keywords:

########################################
## load packages and source functions here
{

  library(NMprojectAZ)
  load_localpackage()

  #module_cmd("module load nonmem-standard/7.3.0-foss-2017a-gfortran-5.2 psn/4.4.8-foss-2017a-gfortran-5.2")

  #library(future)
  #plan(multisession, workers = 3)

  ########################################
  ## main script here

  interactive_mode(FALSE)
  run_jobs <- TRUE

  ## single core tests

  m1 <- nm("qpsn -t 3000 -r 1000 -- execute run1.mod -dir=1")
  m2 <- nm("qpsn -t 3000 -r 1000 -- execute run2.mod -dir=2")
  m3 <- nm("qpsn -t 3000 -r 1000 -- execute run3.mod -dir=3")
  m4 <- nm("qpsn -t 3000 -r 1000 -- execute run4.mod -dir=4")
  m5 <- nm("qpsn -t 3000 -r 1000 -- execute run5.mod -dir=5")

  ## run these with:
  # run_nm(c(m1,m2,m3,m4,m5))

  ####################################
  ## repeat run6 100s of times

  dm <- tibble(rep = 1:600)
  dm$run_in <- "Models/reptest"
  dm$ctl <- paste0("run6_",dm$rep,".mod")

  copy_control("Models/run6.mod", dm$ctl, overwrite = TRUE, dest_dir = dm$run_in)

  dm$m <- nm(paste0("qpsn -c auto -t 3000 -r 1000 -- execute ",dm$ctl," -dir=6_",dm$rep), run_in = dm$run_in)

  dm$m <- run_nm(dm$m)  ## launch in separate process - Q package, use checksums to decide to repeat = store in db

  dm$ofv <- ofv(dm$m)
  dm$AIC <- AIC(dm$m)
  dm$BIC <- BIC(dm$m)
  dm$cond_num <- cond_num(dm$m)

  dm$is_finished <- is_finished(dm$m)

  dm$status <- status(dm$m)

  table(dm$status)

  ####################################
  ## repeat run6 with different numbers of cores

  dc <- tibble(cores = 1:36)
  dc$run_in <- "Models/coretest"

  copy_control("Models/run6.mod", paste0("run6c_",dc$cores,".mod"), overwrite = TRUE, dest_dir = dc$run_in)

  dc$m <- nm(paste0("qpsn -c ",dc$cores," -t 3000 -r 1000 -- execute run6c_",dc$cores,".mod -dir=6c_",dc$cores), run_in = dc$run_in)

  dc$m <- run_nm(dc$m)

}

{
  nmq8 <- nm("qpsn -c auto -t 3000 -r 1000 -- execute runnmq8.mod -dir=nmq8")
  nmex6 <- nm("qpsn -c auto -t 3000 -r 1000 -- execute runnmex6.mod -dir=nmex6")

  run_nm(nmq8)
  run_nm(nmex6)
  wait_for_finished(nmq8,nmex6)
}

###################################

if(0){  ## additional testing

  ##PKPD
  m7 <- nm("qpsn -c auto -t 3000 -r 1000 -- execute run7.mod -dir=7")

  ##binary
  m8 <- nm("qpsn -c auto -t 3000 -r 1000 -- execute run8.mod -dir=8")

  m2boot <- nm("qpsn -t 3000 -r 1000 -- bootstrap run2.mod -threads=10 -samples=10 -dir=2boot")

  m2boot2 <- nm("qpsn -t 3000 -c 2 -r 1000 -- bootstrap run2.mod -threads=10 -samples=10 -dir=2boot2")
  m2boot3 <- nm("qpsn -t 3000 -r 1000 -- bootstrap run2.mod -threads=10 -samples=10 -dir=2boot3")

}

##########################################################

{
  if(run_jobs) system.time(run_nm(m1,m2,m3,m4,m5,initial_timeout=1000))

  #if(run_jobs) system.time(do.call(NMproject::run,c(m,initial_timeout=1000)))

  #if(run_jobs) system.time(run(m7,m8))

  #if(run_jobs) system.time(NMproject::run(m2boot))
  #if(run_jobs) system.time(NMproject::run(m2boot2))

  res1 <- test_execute(m1)
  res2 <- test_execute(m2)
  res3 <- test_execute(m3)
  res4 <- test_execute(m4)
  res5 <- test_execute(m5)

  #res <- lapply(m,test_execute)

  #res7 <- test_execute(m7)
  #res8 <- test_execute(m8)

  save(list = ls(pattern = "res"),file = "Results/res.Rdata")

}

system("ls Models/psn-* | xargs grep -l -E '(Not restarting this model)|(Fatal error)|(No nonmem execution)'",
       intern = TRUE)

