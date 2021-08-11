test_that("new_nm", {
  exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
  m1 <- new_nm(run_id = "m1", 
               based_on = file.path(exdir, "Models", "ADVAN2.mod"),
               data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
  
  expect_error(
    m1 <- new_nm(run_id = "m1", 
               based_on = "Models/ADVAN2.mod",
               data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
  )

  
})
