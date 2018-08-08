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

  options(code_library_path=c(system.file("extdata/CodeLibrary",package="NMproject")))
  expect_true(length(getOption("code_library_path"))>0)

  clib <- code_library(viewer=FALSE,silent=TRUE)
  expect_true("character" %in% class(clib))

  expect_true(any(grepl("ADVAN2.mod",clib)))

  preview("NONMEM/ADVAN2.mod")

  info <- info_scripts(code_library(viewer=FALSE,silent=TRUE),viewer = FALSE)
  expect_true("data.frame" %in% class(info))

  copy_control("NONMEM/ADVAN2.mod","run1.mod")

  expect_true("run1.mod" %in% dir(getOption("models.dir")))

  copy_control("run1.mod","run2.mod")

  expect_true("run2.mod" %in% dir(getOption("models.dir")))

  dn <- data_name("Models/run1.mod")

  expect_true(length(dn)==1)
  expect_true(class(dn)=="character")

  update_dollar_data("Models/run1.mod","new.data.csv")

  dn <- data_name("Models/run1.mod")
  expect_true(dn=="new.data.csv")

  setup_nm_demo(overwrite = TRUE)
  expect_true(file.exists("DerivedData/THEOPP.csv"))
  expect_true(file.exists(file.path(getOption("scripts.dir"),"theopp-demo.R")))

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
  expect_true(getOption("model_file_stub")=="run")

  tmp <- system_nm("echo test",intern=TRUE, wait=TRUE)
  expect_true(length(tmp)>0 & "character" %in% class(tmp))

  old.opt <- getOption("nmtran_exe_path")
  options(nmtran_exe_path=NULL)
  expect_error(nm_tran("run1.mod"))
  options(nmtran_exe_path=old.opt)

})
