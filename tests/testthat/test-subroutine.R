debug <- TRUE
proj_name <- "test_nmproject"
proj_path <- file.path(tempdir(), proj_name)

test_that("manual_edit", {
  
  skip_if_not(rmarkdown::pandoc_available("1.12.3"))
  
  currentwd <- getwd()
  if (file.exists(proj_path)) unlink(proj_path, recursive = TRUE, force = TRUE)
  with_temp_git_config(nm_create_analysis_project(proj_path))
  on.exit({
    setwd(currentwd)
    unlink(proj_path, recursive = TRUE, force = TRUE)
  })
  
  
  testfilesloc <- file.path(currentwd, "theopp")
  zip_file <- file.path(currentwd, "theopp.zip")
  unzip(zip_file)
  
  setwd(proj_path)
  
  file.copy(file.path(testfilesloc, "."), ".", recursive = TRUE)
  file.rename("cache", ".cache")
  
  unlink(testfilesloc, recursive = TRUE)
  
  ## end boiler plate
  ############################
  
  run_all_scripts(index = 1:2)
  
  m1 <- readRDS("Results/m1.RDS")
  
  ## check all conversions work without errors
  mt <- .available_advans %>%
    mutate(m = m1 %>% child(run_id = .data$label) %>% 
             subroutine(advan = .data$advan,
                        trans = .data$trans))
  
  ## check advans and trans are the way they should be
  expect_equal(advan(mt$m), mt$advan)
  expect_equal(trans(mt$m), mt$trans)  
  
  ### advan 5 conversion
  m1a5 <- m1 %>% subroutine(advan = 5)
  expect_true(any(grepl("K1T2", text(m1a5)[[1]])))
  
  ### diff tests
  expect_true(length(nm_diff(m1, m1a5)) > 0)
  expect_true(length(nm_diff(m1, text(m1a5)[[1]])) > 0)
  
  ### advan 13 conversion
  m1a13 <- m1 %>% subroutine(advan = 13)
  expect_true(any(grepl("K1T2", text(m1a13)[[1]])))
  expect_true(any(grepl("\\$DES", text(m1a13)[[1]])))
  
  m1reverse <- m1a13 %>% subroutine(advan = 2)
  
  
})
