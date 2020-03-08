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

  testfilesloc <- file.path(currentwd,"testfiles")
  setwd(proj_name)
  file.copy(file.path(testfilesloc,"."),".",recursive = TRUE)

  ### end boiler plate
  ############################

  ds <- tibble::tibble(run_id = 1)
  
  ds$m <- nm(run_id = ds$run_id) %>%
    ctl("staging/Models/run{run_id}.mod") %>%
    #data_path("DerivedData/THEOPP.csv") %>%
    cmd("execute run{run_id}.mod -dir={run_dir}")
  
  expect_message(ds$m %>% run_nm())
  
  m1 <- ds$m[1]
  
  st <- status(m1)
  expect_true(inherits(st, "character"))
  
  d <- check_session(check_rstudio = FALSE)
  expect_true(inherits(d,"data.frame"))

  res <- rr(m1)
  expect_true(inherits(res,"data.frame"))
  expect_true(nrow(res)>1)
  

})
