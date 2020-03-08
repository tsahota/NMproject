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
  
  m1 <- nm(run_id = "m1")
  
  expect_output(print(m1))
  
  m1 <- m1 %>% cmd("qpsn -t 100 -c auto -- execute {ctl_name} -dir={run_dir}")
  m1 <- m1 %>% ctl("staging/Models/run1.mod")
  
  expect_identical(ctl_path(m1), "Models/runm1.mod")
  ## file doesn't exist yet
  expect_true(!file.exists(ctl_path(m1)))
  
  ## NOTE: cannot test manual edit (blocks console)
  
  expect_true(
    any(grepl("EXP\\(THETA",
              m1 %>% dollar("PK") %>% dplyr::first()
    ))
  )
  
  expect_true(is.character(status(m1)))
  
  browser()
  
  m2 <- m1 %>% child(run_id = "m2")
  
  ## parent hasn't been run
  expect_true(is.na(parent(m2)))
  
  dtemp <- overlapping_outputs(c(m1,m2))
  expect_true(nrow(dtemp) == 0)
  
  expect_identical(ctl_name(m2), "runm2.mod")
  
  ## table renumbering
  tabs <- psn_exported_files(m2) %>% dplyr::first()  
  expect_true(
    any(grepl("sdtabm2", tabs)) & ## contains sdtabm2
      !any(grepl("sdtabm1", tabs)) ## doesn't contain sdtabm1
  )
  
  m2 <- m2 %>% 
    run_in("Models/base")
  
  expect_identical(ctl_path(m2), "Models/base/runm2.mod")
  
  expect_true(file.exists(data_path(m2)))
  
  ## read in data
  d <- input_data(m2)
  expect_true(is.data.frame(d))
  
  nrow(d)
  d2 <- d %>% dplyr::filter(ID < 12)
  nrow(d2)
  expect_true(nrow(d) > nrow(d2))
  
  d2$DUMMY <- 1
  
  d2 %>% write_derived_data_extra("data")
  
  expect_true(file.exists("DerivedData/data.csv"))
  
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
    m1 <- m1 %>% subroutine(advan = 2, trans = 2)
  })
  
  #expect_true(trans(m1) == 2)
  
  
})

