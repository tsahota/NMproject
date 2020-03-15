context("run and post")

proj_name <- "test_nmproject"
require(tidyproject)

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE,force = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE,force = TRUE)
}

test_that("run and post",{
  currentwd <- getwd()
  make_project(proj_name)
  on.exit({
    setwd(currentwd)
    cleanup(proj_name)
  })

  testfilesloc <- file.path(currentwd,"testfiles")
  setwd(proj_name)
  file.copy(file.path(testfilesloc,"."),".",recursive = TRUE)
  
  ## end boiler plate
  ############################
  
  #m1 <- nm("qpsn -m -c auto -t 3000 -- execute run1.mod -dir=1")
  
  ## dataset procesing
  
  m1 <- nm(run_id = "m1") %>%
    ctl("staging/Models/run1.mod") %>%
    cmd("execute {ctl_name} -dir={run_dir}") %>%
    run_nm()
  
  m2 <- m1 %>% child(run_id = "m2") %>%
    subroutine(advan = 4, trans = 1) %>%
    run_nm()
  
  mlist <- c(m1, m2)

  ## running
  
  expect_message(mlist %>% run_nm())
  
  ## post processing

  
  expect_true(inherits(rr(mlist), "data.frame"))
  expect_true(inherits(summary_wide(mlist), "data.frame"))
  expect_true(inherits(summary_long(mlist), "data.frame"))
  
  expect_true(inherits(ofv(mlist), "numeric"))
  
  do <- output_table(m1) %>% dplyr::first()
  expect_true(inherits(do, "data.frame"))
  expect_true("INNONMEM" %in% names(do))

  p <- plot_iter(m1,trans = FALSE)
  expect_true(inherits(p,"ggplot"))

  p <- plot_iter(m1)
  expect_true(inherits(p,"ggplot"))


})
