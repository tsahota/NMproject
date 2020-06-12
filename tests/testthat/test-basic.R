context("basic NM project setup")

proj_name <- "test_nmproject"
require(tidyproject)

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE,force = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE,force = TRUE)
}

test_that("Project has basic functionality",{

  currentwd <- getwd()
  make_project(proj_name)
  on.exit({
    setwd(currentwd)
    cleanup(proj_name)
  })

  setwd(proj_name)
  
  set_nm_opts()
  
  cmd_test <- system_nm("echo test", intern = TRUE)
  
  #expect_true(identical(cmd_test, "test"))

  options(code_library_path=c(system.file("extdata/CodeLibrary",package="NMproject")))
  expect_true(length(getOption("code_library_path"))>0)
  
  #browser()
  setup_nm_demo(overwrite = TRUE)
  expect_true(file.exists("DerivedData/THEOPP.csv"))

})


test_that("set up",{

  currentwd <- getwd()
  make_project(proj_name)
  on.exit({
    setwd(currentwd)
    cleanup(proj_name)
  })

  setwd(proj_name)

  set_nm_opts()

  tmp <- system_nm("echo test",intern=TRUE, wait=TRUE)
  expect_true(length(tmp)>0 & "character" %in% class(tmp))

  old.opt <- getOption("nmtran_exe_path")
  options(nmtran_exe_path=NULL)
  expect_error(nm_tran("run1.mod"))
  options(nmtran_exe_path=old.opt)

})
