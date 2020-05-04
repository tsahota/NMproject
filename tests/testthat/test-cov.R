require("tidyproject")
context("covariate modelling")

proj_name <- "test_nmproject"

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE,force = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE,force = TRUE)
}

test_that("covariate modelling",{
  currentwd <- getwd()
  make_project(proj_name)
  on.exit({
    setwd(currentwd)
    cleanup(proj_name)
  })
  
  testfilesloc <- file.path(currentwd,"testfiles")
  setwd(proj_name)
  file.copy(file.path(testfilesloc,"."),".",recursive = TRUE)
  file.rename("cache", ".cache")
  
  ## end boiler plate
  ############################
  
  m1 <- nm(run_id = "m1") %>%
    prior_ctl("staging/Models/run1.mod") %>%
    cmd("execute {ctl_name} -dir={run_dir}") %>%
    run_nm()
  
  d <- input_data(m1)
  ## change to have no missing WT
  d <- d %>% 
    dplyr::group_by(ID) %>%
    dplyr::mutate(WT = na.omit(WT),
                  COV1 = rnorm(1),
                  COV2 = rbinom(1, 1, 0.5))
  
  write_derived_data(d, "THEOPPCOV")
  
  suppressWarnings({
    m0 <- m1 %>% child(run_id = "m0") %>%
      data_path("DerivedData/THEOPPCOV.csv") %>%
      fill_input()
  })
  
  dtest <- test_relations()
  
  dtest <- dtest %>%
    test_relations(param = "K",
                   cov = c("WT", "COV1"), 
                   state = "linear",
                   continuous = TRUE) %>%
    test_relations(param = c("K", "KA"),
                   cov = c("COV2"), 
                   state = "linear",
                   continuous = FALSE)
  
  d_f1 <- m0 %>% covariate_step_setup(run_id = "m0_f1",
                                      run_in = "Models/m0_cov",
                                      dtest = dtest,
                                      direction = "forward")
  
  d_f1$m %>% dollar("PK")
  
  ## none should be NA objects
  expect_true(length(which(is.na(d_f1$m))) == 0)
  
  ## TODO: run this and do post processing
  
  ## take a winner
  m0_f1 <- d_f1$m[1]
  
  d_f2 <- m0_f1 %>% covariate_step_setup(run_id = "m0_f2",
                                         run_in = "Models/m0_cov",
                                         dtest = dtest,
                                         direction = "forward")
  
  is.na(d_f2$m) ## one should be NA
  expect_true(length(which(is.na(d_f2$m))) == 1)
  
  ## take a winner
  m0_f2 <- d_f2$m[2]
  
  d_b1 <- m0_f2 %>% covariate_step_setup(run_id = "m0_b1",
                                         run_in = "Models/m0_cov",
                                         dtest = dtest,
                                         direction = "backward")
  
  expect_true(nrow(d_b1) == 2)

  
})

