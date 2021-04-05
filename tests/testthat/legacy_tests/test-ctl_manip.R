context("ctl manipulation")

proj_name <- "test_nmproject"
require(tidyproject)

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE,force = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE,force = TRUE)
}

test_that("ctl manipulation",{
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
  
  #m1 <- nm("qpsn -m -c auto -t 3000 -- execute run1.mod -dir=1")
  
  ## dataset procesing
  
  m1 <- nm(run_id = "m1") %>%
    based_on("staging/Models/run1.mod") %>%
    cmd("execute {ctl_name} -dir={run_dir}") %>%
    run_nm()
  
  ## make a fake dataset with more variables and see if IGNORE works
  
  d <- input_data(m1)
  
  for(i in 1:20){
    d[[paste0("DUM", i)]] <- 1:nrow(d)
  }
  
  for(i in 21:30){
    d[[paste0("DUM", i)]] <- paste0("TEXT", 1:nrow(d))
  }
  
  d %>% write_derived_data("dummy_data.csv")
  
  m2 <- m1 %>% child("m2") %>%
    data_path("DerivedData/dummy_data.csv") %>%
    fill_input(rename = c("NDUM1" = "DUM1"), drop = "DUM2")
    
  ## test fill_input
  dollar_text <- m2 %>% dollar("INPUT")
  dollar_text <- dollar_text[[1]]
    
  expect_true(any(grepl("DUM2=DROP", dollar_text)))  ## dropped
  expect_true(any(grepl("DUM21=DROP", dollar_text))) ## character
  expect_true(!any(grepl("\\bDUM1\\b", dollar_text))) ## no DUM1
  expect_true(any(grepl("NDUM1", dollar_text))) ## character
  
  ## test ignore
  
  data_text <- m2 %>% ignore("DUM4<5, DUM3>9") %>% 
    dollar("DATA")
  data_text <- data_text[[1]]
    
  expect_true(any(grepl("DUM4\\.LT\\.5", data_text)))
  expect_true(any(grepl("DUM3\\.GT\\.9", data_text)))

  ## make a difficult $DATA  
  m2 <- m2 %>% target("DATA") %>% 
    text(c("IGNORE=(DUM4.LT.3,DUM5.LT.3, DUM6.LT.3",
           "DUM7.LT.3)",
           "IGNORE=DUM8.LT.3,DUM9.LT.3, DUM10.LT.3"),append = TRUE)
  
  m2 %>% dollar("DATA")
  
  extracted_ignore <- m2 %>% data_ignore_char()
  
  expect_true(grepl("DUM4", extracted_ignore))
  expect_true(grepl("DUM6", extracted_ignore))
  expect_true(grepl("DUM7", extracted_ignore))
  expect_true(grepl("DUM8", extracted_ignore))
  expect_true(grepl("DUM10", extracted_ignore))

  
})
