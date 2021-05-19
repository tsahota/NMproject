context("basic NM project setup")

proj_name <- "test_nmproject"
proj_path <- file.path(tempdir(), proj_name)

test_that("Project has basic functionality",{

  currentwd <- getwd()
  nm_create_analysis_project(proj_path)
  on.exit({
    setwd(currentwd)
    unlink(proj_path, recursive = TRUE, force = TRUE)
  })
  setwd(proj_path)

  NMproject:::set_nm_opts()
  
  cmd_test <- system_nm("echo test", intern = TRUE)
  
  #expect_true(identical(cmd_test, "test"))

  options(code_library_path=c(system.file("extdata/CodeLibrary",package="NMproject")))
  expect_true(length(getOption("code_library_path"))>0)
  
  setup_nm_demo()
  expect_true(file.exists("SourceData/THEOPP.csv"))

})


test_that("set up",{

  currentwd <- getwd()
  nm_create_analysis_project(proj_path)
  on.exit({
    setwd(currentwd)
    unlink(proj_path, recursive = TRUE, force = TRUE)
  })
  setwd(proj_path)

  NMproject:::set_nm_opts()

  tmp <- system_nm("echo test",intern=TRUE, wait=TRUE)
  expect_true(length(tmp)>0 & "character" %in% class(tmp))

  old.opt <- getOption("nmtran_exe_path")
  options(nmtran_exe_path=NULL)
  expect_error(nm_tran("run1.mod"))
  options(nmtran_exe_path=old.opt)

  expect_error(nm("execute run1.mod -dir=m1"))
  
  d <- code_library(viewer = FALSE, return_info = TRUE)
  expect_true(nrow(d) > 10)
  
  ls_code_library("Models/ADVAN2.mod") %>%
    stage()
  
  expect_true(file.exists("staging/Models/ADVAN2.mod"))
  
  TVK_files <- ls_code_library() %>% 
    search_raw("TVK")
  
  expect_true(length(TVK_files) > 0)
  
})
