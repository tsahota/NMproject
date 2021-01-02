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
  file.rename("cache", ".cache")
  
  ## end boiler plate
  ############################
  
  #m1 <- nm("qpsn -m -c auto -t 60 -- execute run1.mod -dir=1")
  
  ## dataset procesing
  
  m1 <- nm(run_id = "m1") %>%
    based_on("staging/Models/run1.mod") %>%
    cmd("execute {ctl_name} -dir={run_dir}")

  stopifnot(in_cache(m1))  ## no point in continue if cache broken
  
  m1 <- m1 %>% run_nm()
  
  m2 <- m1 %>% child(run_id = "m2") %>%
    subroutine(advan = 4, trans = 1) %>%
    run_nm()
  
  ## to update cache files run the following lines.
  # dir(file.path(testfilesloc,"cache"), full.names = TRUE) %>% unlink()
  # file.copy(".cache/.", file.path(testfilesloc,"cache"), recursive = TRUE)
  # dir(file.path(testfilesloc,"Models"), full.names = TRUE) %>% unlink()
  # file.copy("Models/.", file.path(testfilesloc,"Models"), recursive = TRUE)
  
  mlist <- c(m1, m2)

  ## running
  
  expect_message(mlist %>% run_nm())
  
  ## post processing

  cw <- coef_wide(mlist[1])
  cl <- coef_long(mlist[1])

  expect_true(inherits(cw, "data.frame"))
  expect_true(nrow(cw) > 0)
  
  expect_true(inherits(cl, "data.frame"))
  expect_true(nrow(cl) > 0)
  
  #rr2(mlist)
  status_table(mlist) 

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
  
  ## parameter updates and initial value setting
  itheta0 <- init_theta(m1)
  m1 <- m1 %>% update_parameters()
  itheta1 <- init_theta(m1)
  
  expect_true(!identical(itheta0, itheta1))
  expect_false(in_cache(m1))
  
  ## last thing to do: clean up run
  expect_true(all(file.exists(psn_exported_files(m1)[[1]])))
  wipe_run(m1)
  expect_true(all(!file.exists(psn_exported_files(m1)[[1]])))


})
