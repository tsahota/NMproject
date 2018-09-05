context("status")

proj_name <- "test_nmproject"
require(tidyproject)

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE,force = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE,force = TRUE)
}

test_that("status",{
  currentwd <- getwd()
  make_project(proj_name)
  on.exit({
    setwd(currentwd)
    cleanup(proj_name)
  })

# <<<<<<< HEAD
#   testfilesloc <- file.path(currentwd,"testfiles")
# =======
# >>>>>>> parent of a83e851... improved tests
  setwd(proj_name)

  testloc <- file.path(currentwd,"testfiles")

  file.copy(file.path(testloc,"."),".",recursive = TRUE)

  ### end boiler plate
  ############################

  m1 <- nm("qpsn -m -c auto -t 3000 -- execute run1.mod -dir=1")

  expect_error(run(m1)) ## already run so this should fail
  expect_output(print(m1),"List of") ## does object print using str

  wait_default(FALSE)
  expect_false(getOption("wait"))
  wait_default(TRUE)
  expect_true(getOption("wait"))

  expect_error(wait_for_finished(m1),NA)

  st <- status(m1)
  expect_true(inherits(st,"list"))

  sum0 <- run_summary(m1)
  expect_true(inherits(sum0,"data.frame"))

  rt <- run_table()
  expect_true(inherits(rt,"data.frame"))

  d <- check_session(check_rstudio = FALSE)
  expect_true(inherits(d,"data.frame"))

  r <- extract_nm(1)
  expect_true(inherits(r,"nmexecute"))


  res <- run_record0(m1,coef.func = coef.nm)
  expect_true(inherits(res,"data.frame"))
  expect_true(nrow(res)>1)
  res <- run_record(m1)
  expect_true(inherits(res,"data.frame"))
  expect_true(nrow(res)>1)

  expect_true(file.exists(m1$run_dir))
  clean_run(m1)
  expect_true(!file.exists(m1$run_dir))

  get_PMX_code_library("testCodeLibrary",
                       config_file="test_config.R")

  expect_true(length(readLines("test_config.R"))>0)

})
