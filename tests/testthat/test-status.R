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

  st <- status(m2)
  expect_true(inherits(st,"data.frame"))
  expect_true(any(st$RESULT))

  run_type <- run_type(m2$run_dir)
  expect_true(run_type == "execute")

  sum0 <- run_summary(m2)
  expect_true(inherits(sum0,"data.frame"))

  rt <- run_table()
  expect_true(inherits(rt,"data.frame"))

  d <- check_session()
  expect_true(inherits(d,"data.frame"))

  r <- extract_nm(1)
  expect_true(inherits(r,"nmexecute"))

  res <- run_record0(m2,coef.func = coef.nm)
  expect_true(inherits(res,"data.frame"))
  expect_true(nrow(res)>1)
  res <- run_record(m2)
  expect_true(inherits(res,"data.frame"))
  expect_true(nrow(res)>1)

})
