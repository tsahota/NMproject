require("tidyproject")
context("nm object basic use")

proj_name <- "test_nmproject"

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE,force = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE,force = TRUE)
}

test_that("nm object basic use",{
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
  
  ## empty object
  #mempty <- nm2()
  #expect_true(length(mempty) == 0) 
  ## Currently no empty object
  ##  - do only if there is a need
  
  ## NA object
  mNA <- nm2(NA)
  expect_true(length(mNA) == 1)
  expect_true(is.na(mNA))
  
  m1 <- nm2(run_id = "m1")
  m1 <- m1 %>% cmd("qpsn -t 100 -c auto -- execute {ctl_name} -dir={run_dir}")
  m1 <- m1 %>% ctl("staging/Models/run1.mod")
  
  expect_identical(ctl_path(m1), "Models/runm1.mod")
  ## file doesn't exist yet
  expect_true(!file.exists(ctl_path(m1)))
  
  ## NOTE: cannot test manual edit (blocks console)
  
  expect_true(
    any(grepl("EXP\\(THETA",
              m1 %>% dollar("PK") %>% dplyr::first()
    ))
  )
  
  #m1 %>% dollar("SUB")
  expect_true(advan(m1) == 2)
  expect_true(trans(m1) == 1)
  

  suppressWarnings({
    m1 <- m1 %>% subroutine(advan = 2, trans = 2)
  })
  
  #expect_true(trans(m1) == 2)
  
  
})

