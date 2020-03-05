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

  testfilesloc <- file.path(currentwd,"testfiles")
  setwd(proj_name)
  file.copy(file.path(testfilesloc,"."),".",recursive = TRUE)
  
  ## end boiler plate
  ############################
  
  #m1 <- nm("qpsn -m -c auto -t 3000 -- execute run1.mod -dir=1")
  
  ## dataset procesing

  d <- input_data(m1)

  write_derived_data(d, "data")

  expect_true(file.exists("DerivedData/data.csv"))
  expect_true(file.exists("DerivedData/data.RDS"))

  d <- "temp"
  d <- read_derived_data("data")
  expect_true(inherits(d, "data.frame"))

  ## post processing

  expect_true(inherits(ofv(m1),"numeric"))

  do <- output_table(m1)
  expect_true(inherits(do, "data.frame"))
  expect_true("INNONMEM" %in% names(do))

  expect_true(inherits(omega_matrix(m1), "matrix"))

  p <- plot_iter(m1,trans = FALSE)
  expect_true(inherits(p,"ggplot"))

  p <- plot_iter(m1)
  expect_true(inherits(p,"ggplot"))


})
