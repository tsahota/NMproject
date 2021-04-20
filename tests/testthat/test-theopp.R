context("run and post")

proj_name <- "test_nmproject"
proj_path <- file.path(tempdir(), proj_name)

test_that("run and post",{
  
  currentwd <- getwd()
  nm_create_analysis_project(proj_path)
  on.exit({
    setwd(currentwd)
    unlink(proj_path, recursive = TRUE, force = TRUE)
  })

  
  testfilesloc <- file.path(currentwd, "theopp")
  zip_file <- file.path(currentwd, "theopp.zip")
  unzip(zip_file)
  
  setwd(proj_path)

  file.copy(file.path(testfilesloc,"."), ".", recursive = TRUE)
  file.rename("cache", ".cache")
  
  unlink(testfilesloc, recursive = TRUE)
  
  ## end boiler plate
  ############################
  
  options(nm.force_render = TRUE)

  ## run all scripts
  overwrite_behaviour("skip")
  
  # if(!all(c(".Rprofile", "OpenProject.Rproj") %in% dir(all.files = TRUE)))
  #   stop(paste("files missing: ", paste(dir(all.files = TRUE), collapse = ",")))
  
  expect_true(run_all_scripts())
  
  res_files <- dir("Results", pattern = "\\.html", full.names = TRUE)
  expect_true(length(res_files) > 0)
  expect_true(min(file.info(res_files)$size) > 1e5)
  
  script_res_files <- dir("Scripts", pattern = "\\.html", full.names = TRUE)
  expect_true(length(script_res_files) > 0)
  expect_true(min(file.info(script_res_files)$size) > 1e5)
  
  all_temp_files <- ls_tempfiles()
  expect_true(length(all_temp_files) == 0) ## shouldn't be any tempfiles in zip
  
})
