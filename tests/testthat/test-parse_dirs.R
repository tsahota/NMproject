test_that("parse_dirs", {
  
  ## default
  res <- parse_dirs("Scripts, Models, Results, SourceData, DerivedData")
  expect_true(identical(res, list(
    scripts = "Scripts",
    models = "Models",
    results = "Results",
    source_data = "SourceData",
    derived_data = "DerivedData"
  )))

  ## extra directory  
  res <- parse_dirs("Scripts, Models, Results, SourceData, DerivedData, extra")
  expect_true(identical(res, list(
    scripts = "Scripts",
    models = "Models",
    results = "Results",
    source_data = "SourceData",
    derived_data = "DerivedData",
    "extra"
  )))

  ## no derived data  
  res <- parse_dirs("Scripts, Models, Results, SourceData, ")
  expect_true(identical(res, list(
    scripts = "Scripts",
    models = "Models",
    results = "Results",
    source_data = "SourceData"
  )))
  
  ## no derived data - no trailing comma
  res <- parse_dirs("Scripts, Models, Results, SourceData")
  expect_true(identical(res, list(
    scripts = "Scripts",
    models = "Models",
    results = "Results",
    source_data = "SourceData"
  )))
  
  ## Models dir = base dir
  res <- parse_dirs("Scripts,., Results, SourceData, ")
  expect_true(identical(res, list(
    scripts = "Scripts",
    models = ".",
    results = "Results",
    source_data = "SourceData"
  )))  
  ## No results dir
  res <- parse_dirs("Scripts, Models, , SourceData, ")
  expect_true(identical(res, list(
    scripts = "Scripts",
    models = "Models",
    source_data = "SourceData"
  )))
  
})
