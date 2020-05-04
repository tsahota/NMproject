require("tidyproject")
context("nm object basic use")

proj_name <- "test_nmproject"

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE,force = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE,force = TRUE)
}

test_that("nm object basic use",{
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
  
  ## empty object
  #mempty <- nm()
  #expect_true(length(mempty) == 0) 
  ## Currently no empty object
  ##  - do only if there is a need
  
  ## NA object
  mNA <- nm(NA)
  expect_true(length(mNA) == 1)
  expect_true(is.na(mNA))
  
  ## pick a different run_id to post processing tests
  
  m1 <- nm(run_id = "mod1")
  
  expect_output(print(m1))
  
  m1 <- m1 %>% cmd("qpsn -t 100 -c auto -- execute {ctl_name} -dir={run_dir}")
  m1 <- m1 %>% prior_ctl("staging/Models/run1.mod")
  
  expect_identical(ctl_path(m1), "Models/runmod1.mod")
  ## file doesn't exist yet
  expect_true(!file.exists(ctl_path(m1)))
  
  ## NOTE: cannot test manual edit (blocks console)
  
  expect_true(
    any(grepl("EXP\\(THETA",
              m1 %>% dollar("PK") %>% dplyr::first()
    ))
  )
  
  expect_true(is.character(status(m1)))
  
  m2 <- m1 %>% child(run_id = "mod2")
  
  ## parent hasn't been run
  expect_true(is.na(parent_run(m2)))
  
  dtemp <- overlapping_outputs(c(m1,m2))
  expect_true(nrow(dtemp) == 0)
  
  expect_identical(ctl_name(m2), "runmod2.mod")
  
  ## table renumbering
  tabs <- psn_exported_files(m2) %>% dplyr::first()  
  expect_true(
    any(grepl("sdtabmod2", tabs)) & ## contains sdtabm2
      !any(grepl("sdtabmod1", tabs)) ## doesn't contain sdtabm1
  )
  
  m2 <- m2 %>% 
    run_in("Models/base")
  
  expect_identical(ctl_path(m2), "Models/base/runmod2.mod")
  
  expect_true(file.exists(data_path(m2)))
  
  ## read in data
  d <- input_data(m2)
  expect_true(is.data.frame(d))
  
  nrow(d)
  d2 <- d %>% dplyr::filter(ID < 12)
  nrow(d2)
  expect_true(nrow(d) > nrow(d2))
  
  d2$DUMMY <- 1
  
  d2 %>% write_derived_data("data")
  
  expect_true(file.exists("DerivedData/data.csv"))

  d2r <- read_derived_data("data")
  expect_identical(d2, d2r)
  
  m2 <- m2 %>% data_path("DerivedData/data.csv")
  
  expect_identical(data_path(m2), "DerivedData/data.csv")
  
  suppressWarnings({
    m2 <- m2 %>% fill_input()
  })
  
  expect_true( ## dummy present
    any(
      grepl("DUMMY",
            m2 %>% dollar("INPUT") %>% .[[1]])
      )
  )

  m2 <- m2 %>% ignore("ID > 10")
  d3 <- input_data(m2, filter = TRUE)
  expect_true(nrow(d2) > nrow(d3))
  
  #############################
  #m1 %>% dollar("SUB")
  expect_true(advan(m1) == 2)
  expect_true(trans(m1) == 1)
  
  suppressWarnings({
    m1trans2 <- m1 %>% 
      subroutine(advan = 2, trans = 2)
  })
  
  diff_fails <- FALSE
  if(requireNamespace("diffobj", quietly = TRUE)){
    dff <- nm_diff(m1, m1trans2)
    diff_fails <- !inherits(dff, "Diff")    
  }
  expect_false(diff_fails)

  expect_true(trans(m1trans2) == 2)
  
  itheta_m1 <- init_theta(m1)
  log_K <- itheta_m1$init[itheta_m1$name == "K"]
  log_V <- itheta_m1$init[itheta_m1$name == "V"]
  
  itheta_m1trans2 <- init_theta(m1trans2)
  log_CL <- itheta_m1trans2$init[itheta_m1trans2$name == "CL"]
  
  ## CL = K*V
  ## log_CL = log_K + log_V
  ## tol = 0.01 for numerical precision
  expect_true(abs(log_CL - log_K - log_V) < 0.01)
  
  ds <- available_advans %>%
    dplyr::filter(advan %in% c(2,4)) %>%
    dplyr::mutate(
      m = m1 %>% child(run_id = label) %>%
        subroutine(advan = advan, trans = trans)
    )
  
  expect_true(inherits(ds, "data.frame"))
  
  dollar_subs <- ds$m[-1] %>% dollar("SUB") %>% unlist()
  names(dollar_subs) <- NULL
  
  expect_true(all.equal(
    dollar_subs,
    paste0("$SUB ADVAN", ds$advan[-1], " TRANS", ds$trans[-1])
  ))

  ## use of stringr pipe
  mdummy <- m1 %ns>% stringr::str_replace("THETA", "DUMMY")
  
  expect_true( ## dummy present
    any(
      grepl("DUMMY",
            mdummy %>% dollar("PK") %>% .[[1]])
    )
  )
  
})

