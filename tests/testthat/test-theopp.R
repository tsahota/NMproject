context("run and post")

proj_name <- "test_nmproject"
proj_path <- file.path(tempdir(), proj_name)

test_that("run and post",{
  
  currentwd <- getwd()
  nm_create_analysis_project(proj_path)
  on.exit({
    setwd(currentwd)
    unlink(proj_path, recursive = TRUE, force = TRUE)
  })

  
  testfilesloc <- file.path(currentwd, "theopp")
  zip_file <- file.path(currentwd, "theopp.zip")
  unzip(zip_file)
  
  setwd(proj_path)

  file.copy(file.path(testfilesloc,"."), ".", recursive = TRUE)
  file.rename("cache", ".cache")
  
  unlink(testfilesloc, recursive = TRUE)
  
  ## end boiler plate
  ############################
  
  options(nm.force_render = TRUE)

  ## run all scripts
  overwrite_behaviour("skip")
  
  # if(!all(c(".Rprofile", "OpenProject.Rproj") %in% dir(all.files = TRUE)))
  #   stop(paste("files missing: ", paste(dir(all.files = TRUE), collapse = ",")))
  
  expect_true(run_all_scripts())
  
  res_files <- dir("Results", pattern = "\\.html", full.names = TRUE)
  expect_true(length(res_files) > 0)
  expect_true(min(file.info(res_files)$size) > 1e5)
  
  script_res_files <- dir("Scripts", pattern = "\\.html", full.names = TRUE)
  expect_true(length(script_res_files) > 0)
  expect_true(min(file.info(script_res_files)$size) > 1e5)
  
  ## additional tests
  
  all_temp_files <- ls_tempfiles()
  expect_true(length(all_temp_files) == 0) ## shouldn't be any tempfiles in zip
  
  m1 <- readRDS("Results/m1.RDS")
  
  expect_true(length(nm_list_gather(environment())) == 1)
  
  m1 <- m1 %>% simple_field(test_field = 3)
  expect_true(simple_field(m1, test_field) == 3)
  
  m1 <- m1 %>% ignore("ID > 10")
  df <- input_data(m1, filter = TRUE)
  d <- input_data(m1)
  
  expect_true(nrow(df) > 0 & nrow(df) < nrow(d))
  
  io <- m1 %>% init_omega()
  
  io <- io %>% block(c(2,3))
  m1 <- m1 %>% init_omega(io[[1]])
  
  om_text <- m1 %>% dollar("OMEGA") %>% dplyr::first()
  
  expect_true(any(grepl("BLOCK", om_text)))
  
  io <- io %>% unblock(c(2,3))
  m1 <- m1 %>% init_omega(io[[1]])
  
  unlink(file.path(run_dir_path(m1), "NMout.RDS"))
  
  m1 <- readRDS("Results/m1.RDS")
  d <- output_table_first(m1)
  expect_true(nrow(d) > 0)
  
  d <- d %>% append_nonmem_var(m1, "K")
  expect_true(!is.null(d$K))
  
  ## can't test job_stats as xmls are removed
  #d <- job_stats(m1)
  
  
})
