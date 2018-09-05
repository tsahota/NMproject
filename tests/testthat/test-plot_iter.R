context("db & plot_iter works")

proj_name <- "test_nmproject"
require(tidyproject)

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE,force = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE,force = TRUE)
}

test_that("db & plot_iter works",{
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
  
  m1 <- nm("qpsn -m -c auto -t 3000 -- execute run1.mod -dir=1")
  
  m1again <- nm("qpsn -m -c auto -t 3000 -- execute run1.mod -dir=1")
  
  expect_true(identical(m1, m1again))
  
  expect_true(is_finished(m1)) ## m1 has been pre-run
  
  expect_true(inherits(job_info(m1), "character"))
  
  expect_true(inherits(tail_lst(m1), "character"))
  
  ## test to see if ctl_character and ctl_list
  ctl <- ctl_character(m1)
  expect_true(inherits(ctl_character(ctl_list(m1)),"ctl_character"))
  expect_true(inherits(ctl_character(ctl_list(m1$ctl)),"ctl_character"))
  expect_true(inherits(ctl_character(ctl_list(ctl)),"ctl_character"))
  thetasR <- param_info(m1)
  expect_true(inherits(thetasR,"data.frame"))
  thetasNM <- theta_r2nm(thetasR)
  expect_true(inherits(thetasNM,"nm.theta"))

  ## old way: copy_control
  copy_control("run1.mod","run3.mod")
  expect_true(file.exists("Models/run3.mod"))


  data_name <- get_data_name(m1)
  expect_true(file.exists(from_models(data_name)))

  build_run({
    suppressWarnings({
      ctl <- m1 %>% update_parameters() %>% new_ctl("4") %>% write_ctl %>%
        update_dollar_input(rename = c("WT" = "BWT")) %>%
        update_dollar_data(data_name) %>%
        update_ignore("TIME>2") %>%
        update_sizes("PD = 200") %>%
        add_cov(param = "K", cov = "WT")
    })
  })

  ctl <- ctl %>% gsub_ctl("THEOPP\\.csv", "THEOFILENAME")

  expect_true(any(grep("THEOFILENAME", as.character(ctl))))

  ctl <- ctl %>% gsub_ctl("THEOFILENAME","THEOPP.csv")

  expect_true(!any(grep("THEOFILENAME", as.character(ctl))))
  expect_true(any(grep("THEOPP", as.character(ctl))))


  build_run({
    suppressWarnings({
      ctl <- m1 %>% update_parameters() %>% new_ctl("4") %>% write_ctl %>%
        update_dollar_input() %>%
        add_cov(param = "K", cov = "WT")      
    })
  })

  expect_true(any(grepl("KWT-DEFINITION START", ctl_character(ctl))))

  suppressWarnings({
    ctl_orig <- ctl %>% remove_cov(param = "K", cov = "WT")
  })

  expect_true(!any(grepl("KWT-DEFINITION START", ctl_character(ctl_orig))))

  build_run({
    ctl <- m1 %>% change_to_sim %>% change_seed(98765)
  })

  expect_true(any(grepl("\\$SIM", rem_comment(ctl_character(ctl)))))
  expect_true(any(grepl("98765", rem_comment(ctl_character(ctl)))))

  m3 <- nm("qpsn -m -c auto -t 3000 -- execute run3.mod -dir=3")
  m4 <- nm("qpsn -m -c auto -t 3000 -- execute run4.mod -dir=4")

  runs <- show_runs()

  expect_true(inherits(runs,"data.frame"))
  expect_true(nrow(runs)==3)
  m4 <- nm("qpsn -m -c auto -t 3000 -- execute run4.mod -dir=4")

  expect_true(nrow(show_runs())==3) ## should still be three runs

  file.copy("Models/run4.mod","Models/1/run4.mod")
  m4 <- nm("qpsn -m -c auto -t 3000 -- execute run4.mod -dir=4",run_in="Models/1")
  expect_true(nrow(show_runs())==4)

  expect_error(nm("qpsn -m -c auto -t 3000 -- execute run3.mod -dir=4"))

  copy_control("run1.mod","run5.mod")
  ctl <- readLines("Models/run5.mod")
  ctl <- gsub("sdtab5","sdtab1",ctl) ## create an output conflict
  write(ctl,"Models/run5.mod")
  expect_error(nm("qpsn -m -c auto -t 3000 -- execute run5.mod -dir=5"))

  m1scm <- nm("scm run1.mod -config_file=run1.scm -dir=1scm -nmfe_options='-prdefault'")

  m1scm_again <- nm("scm run1.mod -config_file=run1.scm -dir=1scm -nmfe_options='-prdefault'")

  expect_true(identical(m1scm, m1scm_again))

  ## dataset procesing

  d <- get_data(m1)

  write_derived_data(d, "data")

  expect_true(file.exists("DerivedData/data.csv"))
  expect_true(file.exists("DerivedData/data.RData"))

  d <- "temp"
  d <- read_derived_data("data")
  expect_true(inherits(d, "data.frame"))

  ## post processing

  expect_true(inherits(ofv(m1),"numeric"))

  do <- nm_output(m1)
  expect_true(inherits(do, "data.frame"))
  expect_true("INNONMEM" %in% names(do))

  expect_true(inherits(omega_matrix(m1), "matrix"))

  p <- plot_iter(m1,trans = FALSE)
  expect_true(inherits(p,"ggplot"))

  p <- plot_iter(m1)
  expect_true(inherits(p,"ggplot"))


})
