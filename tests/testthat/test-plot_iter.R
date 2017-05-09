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

  file.copy(file.path(testloc,"."),"Models",recursive = TRUE)
  file.copy("Models/2/NM_run1/catab2","Models/.")
  file.copy("Models/2/NM_run1/sdtab2","Models/.")
  file.copy("Models/2/NM_run1/cotab2","Models/.")
  file.copy("Models/2/NM_run1/patab2","Models/.")

  ### end boiler plate
  ############################

  m2 <- nm("qpsn -m -c auto -t 3000 -- execute run2.mod -dir=2")

  copy_control("run2.mod","run3.mod")
  copy_control("run2.mod","run4.mod")

  m3 <- nm("qpsn -m -c auto -t 3000 -- execute run3.mod -dir=3")
  m4 <- nm("qpsn -m -c auto -t 3000 -- execute run4.mod -dir=4")

  runs <- show_runs()

  expect_true(inherits(runs,"data.frame"))
  expect_true(nrow(runs)==3)
  m4 <- nm("qpsn -m -c auto -t 3000 -- execute run4.mod -dir=4")

  expect_true(nrow(show_runs())==3) ## should still be three runs

  file.copy("Models/run4.mod","Models/2/run4.mod")
  m4 <- nm("qpsn -m -c auto -t 3000 -- execute run4.mod -dir=4",run_in="Models/2")
  expect_true(nrow(show_runs())==4)

  expect_error(nm("qpsn -m -c auto -t 3000 -- execute run3.mod -dir=4"))

  copy_control("run2.mod","run5.mod")
  ctl <- readLines("Models/run5.mod")
  ctl <- gsub("sdtab5","sdtab2",ctl) ## create an output conflict
  write(ctl,"Models/run5.mod")
  expect_error(nm("qpsn -m -c auto -t 3000 -- execute run5.mod -dir=5"))

  p <- plot_iter(m2,trans = FALSE)
  expect_true(inherits(p,"ggplot"))

  p <- plot_iter(m2)
  expect_true(inherits(p,"ggplot"))


})
