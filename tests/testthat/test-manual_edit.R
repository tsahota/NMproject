debug <- TRUE
proj_name <- "test_nmproject"
proj_path <- file.path(tempdir(), proj_name)

test_that("manual_edit", {
  currentwd <- getwd()
  if (file.exists(proj_path)) unlink(proj_path, recursive = TRUE, force = TRUE)
  nm_create_analysis_project(proj_path)
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
  
  run_all_scripts(index = 1)
  
  m1 <- new_nm(run_id = "m1",
               based_on = "staging/Models/ADVAN2.mod",
               data_path = "DerivedData/data.csv") %>%
    fill_input() %>%
    init_theta(init = c(0.5, -2.5, -0.5)) %>%
    init_sigma(init = c(0.1, 0.1))
  
  #browser()
  
  ## make a patch 
  
  # p3 <- m1 %>% child("p3")  %>%
  #   apply_manual_edit("tarjinde-2021-08-14-17-06-44")
  # 
  # p4 <- m1 %>% child("p4") %>%
  #   ## break the patch
  #   gsub_ctl("Label", "sdlfkjsldfjk") %>%
  #   apply_manual_edit("tarjinde-2021-08-14-17-06-44")
  
  
  
  
})
