debug <- TRUE
proj_name <- "test_nmproject"
proj_path <- file.path(tempdir(), proj_name)

test_that("manual_edit", {
  
  skip_if_not(rmarkdown::pandoc_available("1.12.3"))
  
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
  
  m1s_temp <- m1 %>% child("m1s_temp") %>%  
    update_parameters(m1) %>%
    apply_manual_edit("tarjinde-2021-08-15-16-55-38")
  
  ## test that the manual edit made a $SIM
  
  expect_false(m1 %>% dollar("SIM") %>% {.[[1]][1]} %>% {grepl("^\\$SIM", .)})
  expect_true(m1s_temp %>% dollar("SIM") %>% {.[[1]][1]} %>% {grepl("^\\$SIM", .)})
  
  ## induce a merge conflict and check we get an error
  expect_error(
    suppressWarnings(
      m1 %>% child("m1s_temp") %>%  
        update_parameters(m1) %>%
        gsub_ctl("ISAMPLE=300", "ISAMPLE=600") %>% 
        apply_manual_edit("tarjinde-2021-08-15-16-55-38")
    )
  )
  
})
