context("db & plot_iter works")

proj_name <- "test_nmproject"
require(tidyproject)

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE,force = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE,force = TRUE)
}

test_that("db & plot_iter works",{
  currentwd <- getwd()
  make_project(proj_name)
  on.exit({
    setwd(currentwd)
    cleanup(proj_name)
  })

  setwd(proj_name)

  testloc <- file.path(currentwd,"testfiles")
  
  file.copy(file.path(testloc,"."),".",recursive = TRUE)

  ### end boiler plate
  ############################

  m1 <- nm("qpsn -m -c auto -t 3000 -- execute run1.mod -dir=1")

  ctl <- ctl_character(m1)
  expect_true(inherits(ctl_character(ctl_list(m1)),"ctl_character"))
  expect_true(inherits(ctl_character(ctl_list(m1$ctl)),"ctl_character"))
  expect_true(inherits(ctl_character(ctl_list(ctl)),"ctl_character"))
  thetasR <- param_info(m1)
  expect_true(inherits(thetasR,"data.frame"))
  thetasNM <- theta_r2nm(thetasR)
  expect_true(inherits(thetasNM,"nm.theta"))

  ## old way: copy_control 
  copy_control("run1.mod","run3.mod")
  expect_true(file.exists("Models/run3.mod"))
  
  ## new way
  build_modfile({
    ctl <- m1 %>% update_parameters() %>% new_ctl("4") %>% write_ctl %>%
      add_cov(param = "K", cov = "WT")
  })

  expect_true(any(grepl("KWT-DEFINITION START", ctl_character(ctl))))
  
  ctl_orig <- ctl %>% remove_cov(param = "K", cov = "WT")
  
  expect_true(!any(grepl("KWT-DEFINITION START", ctl_character(ctl_orig))))

  m3 <- nm("qpsn -m -c auto -t 3000 -- execute run3.mod -dir=3")
  m4 <- nm("qpsn -m -c auto -t 3000 -- execute run4.mod -dir=4")

  runs <- show_runs()

  expect_true(inherits(runs,"data.frame"))
  expect_true(nrow(runs)==3)
  m4 <- nm("qpsn -m -c auto -t 3000 -- execute run4.mod -dir=4")

  expect_true(nrow(show_runs())==3) ## should still be three runs

  file.copy("Models/run4.mod","Models/1/run4.mod")
  m4 <- nm("qpsn -m -c auto -t 3000 -- execute run4.mod -dir=4",run_in="Models/1")
  expect_true(nrow(show_runs())==4)

  expect_error(nm("qpsn -m -c auto -t 3000 -- execute run3.mod -dir=4"))

  copy_control("run1.mod","run5.mod")
  ctl <- readLines("Models/run5.mod")
  ctl <- gsub("sdtab5","sdtab1",ctl) ## create an output conflict
  write(ctl,"Models/run5.mod")
  expect_error(nm("qpsn -m -c auto -t 3000 -- execute run5.mod -dir=5"))

  p <- plot_iter(m1,trans = FALSE)
  expect_true(inherits(p,"ggplot"))

  p <- plot_iter(m1)
  expect_true(inherits(p,"ggplot"))


})
