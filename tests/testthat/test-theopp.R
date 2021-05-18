context("run and post")

debug <- TRUE
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
  
  res <- tryCatch(run_all_scripts(), error = function(e){
    on.exit({
      dump.frames(to.file = TRUE)
      debug_zip_file <- paste0(proj_path, ".zip")
      setwd("..")
      unlink(debug_zip_file)
      utils::zip(basename(debug_zip_file), proj_name)
      file.copy(debug_zip_file, file.path(currentwd, basename(debug_zip_file)))
      setwd(currentwd)
    })
    stop(e)
  })
  
  expect_true(res)
  
  res_files <- dir("Results", pattern = "\\.html", full.names = TRUE)
  expect_true(length(res_files) > 0)
  expect_true(min(file.info(res_files)$size) > 1e5)
  
  script_res_files <- dir("Scripts", pattern = "\\.html", full.names = TRUE)
  expect_true(length(script_res_files) > 0)
  expect_true(min(file.info(script_res_files)$size) > 1e5)
  
  ## additional tests
  
  all_temp_files <- ls_tempfiles()
  ## apart from some exceptions shouldn't be any tempfiles in zip
  expect_true(length(all_temp_files) < 10) 
  
  m1 <- readRDS("Results/m1.RDS")
  
  expect_true(length(nm_list_gather(environment())) == 1)
  
  m1 <- m1 %>% simple_field(test_field = 3)
  expect_true(simple_field(m1, test_field) == 3)
  
  m1 <- m1 %>% ignore("ID > 10")
  df <- input_data(m1, filter = TRUE)
  d <- input_data(m1)
  
  expect_true(nrow(df) > 0 & nrow(df) < nrow(d))

  ##################
  ## omega block/unblock test - can delete this, it's in demo
  io <- m1 %>% init_omega()
  io <- io %>% block(c(2,3))
  m1 <- m1 %>% init_omega(io)
  om_text <- m1 %>% dollar("OMEGA") %>% dplyr::first()
  expect_true(any(grepl("BLOCK", om_text)))
  
  io <- m1 %>% init_omega()
  io <- io %>% unblock(c(2,3)) ## this isn't working
  m1 <- m1 %>% init_omega(io)
  om_text <- m1 %>% dollar("OMEGA") %>% dplyr::first()
  expect_true(!any(grepl("BLOCK", om_text)))
  ###################
  
  unlink(file.path(run_dir_path(m1), "NMout.RDS"))
  
  m1 <- readRDS("Results/m1.RDS")
  expect_true(is_successful(m1))
  
  om_matrix <- m1 %>% omega_matrix() %>% dplyr::first()
  expect_true(inherits(om_matrix, "matrix"))
  expect_true(nrow(om_matrix) > 0)
  expect_true(nrow(om_matrix) == ncol(om_matrix))
  
  d <- output_table_first(m1)
  expect_true(nrow(d) > 0)
  
  d <- d %>% append_nonmem_var(m1, "K")
  expect_true(!is.null(d$K))
  
  
  m1 <- readRDS("Results/m1.RDS")
  m1 <- m1 %>% insert_dollar("DES", "
  $DES
  DADT(1) = ...
  ","PK")
  expect_true(any(grepl("\\$DES", text(m1)[[1]])))
  
  m1 <- m1 %>% delete_dollar("DES")
  expect_false(any(grepl("\\$DES", text(m1)[[1]])))
  
  m1 <- readRDS("Results/m1.RDS")
  coef_wide(m1)
  expect_true(is.numeric(ofv(m1)))
  expect_true(is.numeric(AIC(m1)))
  expect_true(is.numeric(BIC(m1)))
  expect_true(is.numeric(cond_num(m1)))
  
  m1 <- m1 %>% remove_parameter("K")
  
  m1 <- readRDS("Results/m1.RDS")
  ## shouldn't be any temp files for m1 in zip
  expect_true(length(ls_tempfiles(m1)) == 0)
  
  clean_run(m1) ## remove non-temp
  wipe_run(m1)  ## remove all
  expect_true(!file.exists(run_dir_path(m1)))
  
  ## can't test job_stats as xmls are removed
  #d <- job_stats(m1)
  
  
})
