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

  testfilesloc <- file.path(currentwd, "theopp")
  zip_file <- file.path(currentwd, "theopp.zip")
  unzip(zip_file)
  
  setwd(proj_name)

  file.copy(file.path(testfilesloc,"."), ".", recursive = TRUE)
  file.rename("cache", ".cache")
  
  unlink(testfilesloc, recursive = TRUE)
  
  ## end boiler plate
  ############################
  
  options(nm.force_render = TRUE)

  ## run all scripts
  overwrite_behaviour("skip")
  
  if(!all(c(".Rprofile", "OpenProject.Rproj") %in% dir(all.files = TRUE)))
    stop(paste("files missing: ", paste(dir(all.files = TRUE), collapse = ",")))
  
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
