test_that("child", {
  exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
  m1 <- new_nm(run_id = "m1", 
               based_on = file.path(exdir, "Models", "ADVAN2.mod"),
               data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
  
  expect_error(m1 %>% child())
  
  m1 <- m1 %>% gsub_ctl("tabm1", "tab")  ## cause table name conflict
  
  expect_silent(m2 <- m1 %>% child("m2"))
  
  m3 <- m2 %>% child("m3", parent = m1)
  expect_equal(parent_run_id(m3), "m1")
  
  expect_error(m3 <- m2 %>% child("m2", parent = m1))
  
})
