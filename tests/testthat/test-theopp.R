debug <- TRUE
proj_name <- "test_nmproject"
proj_path <- file.path(tempdir(), proj_name)

test_that("run and post", {
  
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

  options(nm.force_render = TRUE)

  ## run all scripts
  overwrite_behaviour("skip")
  
  res <- tryCatch(run_all_scripts(), error = function(e) {
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

  #################################
  ## basic check on output file sizes

  res_files <- dir("Results", pattern = "\\.html", full.names = TRUE)
  expect_true(length(res_files) > 0)
  expect_true(min(file.info(res_files)$size) > 1e5)

  script_res_files <- dir("Scripts", pattern = "\\.html", full.names = TRUE)
  expect_true(length(script_res_files) > 0)
  expect_true(min(file.info(script_res_files)$size) > 1e5)

  #######################################
  ## additional tests on the directory, created objects
  ## and other functions

  all_temp_files <- ls_tempfiles()
  ## apart from some exceptions shouldn't be any tempfiles in zip
  expect_true(length(all_temp_files) < 10)

  m1 <- readRDS("Results/m1.RDS")

  expect_true(length(nm_list_gather(environment())) == 1)
  expect_true(length(nm_list_gather(m1)) == 1)

  expect_true(length(cached_object(m1)) == 1)

  print(as_nm_generic(m1))
  gfs <- glue_fields(m1)
  expect_true(length(gfs) > 0)

  ## custom_vector_field tests
  dummy_list <- list(a = 1, b = 1:2)
  m1 <- m1 %>% custom_vector_field("test field", dummy_list)
  extracted_list <- m1 %>%
    custom_vector_field("test field") %>%
    dplyr::first()
  expect_identical(dummy_list, extracted_list)

  ## simple field test
  m1 <- m1 %>% simple_field(test_field = 3)
  expect_true(simple_field(m1, test_field) == 3)

  ## input data test
  dataset <- data_name(ctl_path(m1))
  expect_true(file.exists(file.path(run_in(m1), dataset)))
  d <- input_data(m1)

  ## $IGNORE/input_data test
  m1 <- m1 %>% ignore("ID > 10")
  df <- input_data(m1, filter = TRUE)
  d <- input_data(m1)
  expect_true(nrow(df) > 0 & nrow(df) < nrow(d))

  ## exclude outliers test
  dexcl <- d %>% filter(ID < 3)
  dnew <- d %>% exclude_rows(dexcl)
  expect_true(max(dnew$ID[dnew$EXCL %in% 1]) < 3)

  ## $THETA/$OMEGA/$SIGMA test
  it <- m1 %>%
    init_theta() %>%
    dplyr::first()
  m1 <- m1 %>% init_theta(it)

  m1 <- readRDS("Results/m1.RDS")
  expect_true(identical(0.5, it$init[it$name %in% "KA"]))

  it <- m1 %>%
    init_theta(init = c(KA = 1)) %>%
    init_theta() %>%
    dplyr::first()
  expect_true(identical(1, it$init[it$name %in% "KA"]))

  it <- m1 %>%
    init_theta(init = rnorm(init, mean = init, sd = 0.3)) %>%
    init_theta() %>%
    dplyr::first()
  expect_true(!identical(0.5, it$init[it$name %in% "KA"]))

  expect_error(m1 %>% init_omega(init = c(FAKENAME = 1)))

  io <- m1 %>%
    init_omega(init = c(IIV_KA = 1)) %>%
    init_omega() %>%
    dplyr::first()
  expect_true(identical(1, io$init[io$name %in% "IIV_KA"]))

  io <- m1 %>%
    init_omega(init = runif(init, min = init / 2, max = init * 2)) %>%
    init_omega() %>%
    dplyr::first()
  expect_true(!identical(0.1, io$init[io$name %in% "IIV_KA"]))

  is <- m1 %>%
    init_sigma(init = c("prop error" = 0.3)) %>%
    init_sigma() %>%
    dplyr::first()
  expect_true(identical(0.3, is$init[is$name %in% "prop error"]))

  ##################
  ## omega block/unblock test - can delete this, it's in demo
  io <- m1 %>% init_omega()
  io <- io %>% block(c(2, 3))
  m1 <- m1 %>% init_omega(io)
  om_text <- m1 %>%
    dollar("OMEGA") %>%
    dplyr::first()
  expect_true(any(grepl("BLOCK", om_text)))

  io <- m1 %>% init_omega()
  io <- io %>% unblock(c(2, 3)) ## this isn't working
  m1 <- m1 %>% init_omega(io)
  om_text <- m1 %>%
    dollar("OMEGA") %>%
    dplyr::first()
  expect_true(!any(grepl("BLOCK", om_text)))
  ###################

  ## output file tests

  unlink(file.path(run_dir_path(m1), "NMout.RDS"))

  m1 <- readRDS("Results/m1.RDS")
  expect_true(is_successful(m1))

  ## omega matrix test
  om_matrix <- m1 %>%
    omega_matrix() %>%
    dplyr::first()
  expect_true(inherits(om_matrix, "matrix"))
  expect_true(nrow(om_matrix) > 0)
  expect_true(nrow(om_matrix) == ncol(om_matrix))

  ## output file tests
  d <- output_table_first(m1)
  expect_true(nrow(d) > 0)

  d <- d %>% append_nonmem_var(m1, "K")
  expect_true(!is.null(d$K))

  m1 <- readRDS("Results/m1.RDS")
  coef_wide(m1)
  expect_true(is.numeric(ofv(m1)))
  expect_true(is.numeric(AIC(m1)))
  expect_true(is.numeric(BIC(m1)))
  expect_true(is.numeric(cond_num(m1)))

  ## ctl manipulation tests
  m1 <- readRDS("Results/m1.RDS")
  m1 <- m1 %>% insert_dollar("DES", "
  $DES
  DADT(1) = ...
  ", "PK")
  expect_true(any(grepl("\\$DES", text(m1)[[1]])))

  m1 <- m1 %>% delete_dollar("DES")
  expect_false(any(grepl("\\$DES", text(m1)[[1]])))

  m1 <- readRDS("Results/m1.RDS")

  m1 <- m1 %>% rename_parameter("DUMPARAM" = "K")
  expect_true(any(grepl("\\bDUMPARAM\\b", m1 %>% dollar("PK"))))
  m1 <- m1 %>% remove_parameter("DUMPARAM")
  expect_true(!any(grepl("\\bDUMPARAM\\b", m1 %>% dollar("PK"))))

  new_tol <- m1 %>%
    advan(13) %>%
    tol(12) %>%
    tol()
  expect_true(new_tol %in% 12)

  ## subroutine tests
  m1 <- readRDS("Results/m1.RDS")

  ### advan 5 conversion
  m1a5 <- m1 %>% subroutine(advan = 5)
  expect_true(any(grepl("K1T2", text(m1a5)[[1]])))

  ### diff tests
  expect_true(length(nm_diff(m1, m1a5)) > 0)
  expect_true(length(nm_diff(m1, text(m1a5)[[1]])) > 0)

  ### advan 13 conversion
  m1a13 <- m1 %>% subroutine(advan = 13)
  expect_true(any(grepl("K1T2", text(m1a13)[[1]])))
  expect_true(any(grepl("\\$DES", text(m1a13)[[1]])))

  m1reverse <- m1a13 %>% subroutine(advan = 2)

  ## cache tests
  m1 <- readRDS("Results/m1.RDS")
  cache_history(m1)
  cache_current(m1)

  ## covariate tests
  m1 <- readRDS("Results/m1.RDS")

  expect_true(nrow(test_relations()) == 0)
  expect_error(m1 %>% add_cov(param = "DUMMYPARAM", cov = "WT"))
  expect_error(m1 %>% add_cov(param = "K", cov = "DUMMY"))
  expect_error(m1 %>% add_cov(param = "K", cov = "WT", state = "2", additional_state_text = c("2" = "nonsense")))

  ## rmd to vignettes
  rmd_to_vignettes()
  expect_true(length(dir("vignettes", pattern = "\\.Rmd$")) >= 5)

  ## shouldn't be any temp files for m1 in zip
  expect_true(length(ls_tempfiles(m1)) == 0)

  ## clean up tests
  clean_run(m1) ## remove non-temp
  wipe_run(m1) ## remove all
  expect_true(!file.exists(run_dir_path(m1)))

  clear_cache()
  expect_true(!file.exists(".cache"))
})
