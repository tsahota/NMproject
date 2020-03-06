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
  ds <- tibble::tibble(run_id = 1)
  
  ds$m <- nm2(run_id = ds$run_id) %>%
    ctl("staging/Models/run{run_id}.mod") %>%
    #data_path("DerivedData/THEOPP.csv") %>%
    cmd("execute run{run_id}.mod -dir={run_dir}")
  
  expect_message(ds$m %>% run_nm())
  
  d <- input_data(ds$m[1])

  write_derived_data_extra(d, "data")

  expect_true(file.exists("DerivedData/data.csv"))
  expect_true(file.exists("DerivedData/data.RDS"))

  d <- read_derived_data_extra("data")
  expect_true(inherits(d, "data.frame"))

  ## post processing

  m1 <- ds$m[1]
  
  expect_true(inherits(ofv(ds$m), "numeric"))
  expect_true(!is.na(ofv(m1)))
  
  do <- output_table(m1) %>% dplyr::first()
  expect_true(inherits(do, "data.frame"))
  expect_true("INNONMEM" %in% names(do))

  p <- plot_iter(m1,trans = FALSE)
  expect_true(inherits(p,"ggplot"))

  p <- plot_iter(m1)
  expect_true(inherits(p,"ggplot"))


})
