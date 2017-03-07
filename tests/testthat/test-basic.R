context("basic NM project setup")

proj_name <- "test_nmproject"
require(TidyProject)

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE)
}

test_that("Project has basic functionality",{

  currentwd <- getwd()
  make_project(proj_name)
  on.exit({
    setwd(currentwd)
    cleanup(proj_name)
  })

  setwd(proj_name)

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

})

