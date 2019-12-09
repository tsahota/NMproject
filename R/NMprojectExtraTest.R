if(0){
  
  ## use case
  
  m0 <- nm2() 
  m0
  cmd(m0)  ## NA
  run_id(m0)  ## "m0"
  run_in(m0)
  run_dir(m0)
  type(m0)
  ctl(m0)
  target(m0)

  db <- nm_db$new("pk.RDS")
    
  m1 <- nm2(run_id = "m1")
  db$register(m1)
  m1 <- m1 %>% cmd("qpsn -t 100 -c auto -- execute run{run_id}.mod -dir={run_dir}")
  m1 <- m1 %>% ctl("tests/testthat/testfiles/Models/run1.mod")
  db$refresh()
  
  db$save()
  
  rm(m1)
  rm(db)
  exists("m1")  ## FALSE
  exists("db")  ## FALSE
  db <- load_db()
  db$repopulate()
  exists("m1")  ## TRUE
  exists("db")  ## TRUE
  
  ## following should be via shiny
  #m1 <- m1 %>% ctl("/projects/qcp/QCP_MODELING/ONC/azd6094/poppk_20181203_pk-eff-ae-interim3/Models/runm2.mod")


  
  ## equivalent of old code
  # m1 <- nm("qpsn -t 100 -c auto -- execute runm1.mod -dir=m1")
  # run_nm(m1)
  # 
  # m2 <- nm("qpsn -t 100 -c auto -- execute runm2.mod -dir=m2")
  # run_nm(m2)

  ## equivalent in new code
  # m1 <- nm(run_id = "m1") %>%
  #   cmd("qpsn -t 100 -c auto -- execute run{run_id}.mod -dir={run_dir}")
  # run_nm(m1old)
  #
  # m2 <- m1 %>% child(run_id = "m2")
  # run_nm(m2)
  
  ## always specify data_path, control file, assume ctl is out of date.
  ##  but if there is no data_path, ctl shouldn't update data_path.
  
    
  m2 <- m1 %>% child(run_id = "m2")
  db$register(m2)
  
  cmd(m2)
  ctl_path(m2)
  m2 %>% dollar("TABLE")
  text(m2)
  parent_run_id(m2)

  db$refresh()
  m2 %>% parent(db)
  
  
  
  m2boot <- m2 %>% child(type = "bootstrap") %>%
    cmd("qpsn -t 100 -c auto -- {type} --samples=50 run{run_id}.mod -dir={run_id}")
  
  ## target certain aspects of the control stream in an intermediate step
  
  ## Assign data
  m2 %>% dollar("DATA")
  m2 %>% dollar("INPUT")
  m2 %>% ignore()
  
  m2 <- m2 %>% data_path("tests/testthat/testfiles/DerivedData/THEOPP.csv")
  m2 <- m2 %>% input() #rename = c("WT" = "BWT"))
  #m2 <- m2 %>% ignore("BWT>1000")
  
  m2 %>% dollar("DATA")
  m2 %>% dollar("INPUT")
  
  ## modify $PK  
  m2 %>% dollar("$PK")
  
  m2 %>% target("$PK") %>% text(c("",
                                "TVCL = THETA(1)",
                                "blah de blah")) %>% dollar("PK") ## works
  
  m2 %>% target("$PK") %>% text("
                                TVCL = THETA(1)
                                blah de blah") %>% dollar("PK") ## works
  m2 %>% target("$PK") %>% text("additional params
                                additional lines", append = TRUE) %>% dollar("PK")
  
  
  m2 %>% ignore("TAD>30, EXCL>0") %>% dollar("$DATA") 
  m2 %>% ignore("TAD>30, EXCL>0") %>% ignore()
  m2 %>% delete_dollar("$PK") %>% ctl
  
  m2 %>% target("$PK") %>% gsub_ctl("KA", "TLEKSJSFL")
  
  m2 %>% new_dollar("$MODEL", "sdflksjf", after_dollar = "$SUB")
  
  ## following requires separate SUBROUTINES not to be lumped
  m2 %>% target("$EST", contains = "IMP\\b", nth = 2) %>% uncomment()
  m2 %>% target("$EST", contains = "IMP\\b") %>% comment()
  
  m2 <- m2 %>% do_manual_edit()
  
  ## patch functionality
  patch_name <- "patchm21"
  if(!patch_exists(m2, patch_name)){
    patch <- m2 %>% do_manual_edit(patch)
  }
  m2 %>% apply_patch_if_needed(patch)
  
  nm_tran(m2)
  m2 <- m2 %>% run_nm  ## run asynchronously
  
  advan(m2)
  trans(m2)
  
  m3 <- m2 %>% child(run_id = "m3") %>%
    subroutine(advan = 4, trans = 1)
  db$register(m3)
  
  m3 <- m3 %>% run_nm() %>%
    update_parameters() %>%
    run_nm()
  
  rr(c(m2,m3))
  
  library(dplyr)
  
  d <- available_advans %>% 
    filter(advan %in% c(2,4))
  d$m <- m2 %>% child(run_id = d$label)
  d$m <- d$m %>% subroutine(advan = d$advan, trans = d$trans)
  d$m <- d$m %>% ctl_path("Models/m2_adtrans/run{run_id}.mod")
  d$m[d$advan %in% 2] <- d$m[d$advan %in% 2] %>%
    cmd("qpsn -t 59 -c auto -- execute run{run_id}.mod -dir={run_id}")
  
  
  nm_tran(d$m)
  d$m <- d$m %>% run_nm()
  
  rr(d$m)
  
  ds <- nm_row(d$m)
  ds$m <- d$m
  ds$ofv <- ofv(d$m)
  ds$AIC <- AIC(d$m)
  ds$BIC <- BIC(d$m)
  ds$cond_num <- cond_num(ds$m)
  
  d$m <- d$m %>% update_parameters %>% run_nm
  

  library(dplyr)
  
  d <- dplyr::tibble(cores = 1:36) %>%
    mutate(m = m2 %>% child(run_id = cores),
           m = m %>% ctl_path("Models/m2_coretest/run{run_id}.mod"),
           m = m %>% cmd("qpsn -t 100 -c {run_id} -- execute run{run_id}.mod -dir={run_dir}"))
  db$register(d$m)
  
  
  m2cores <- m2 %>% 
    child(run_id=1:36) %>%
    ctl_path("Models/m2_coretest/run{run_id}.mod") %>%  ## run in subdirectory to be clean & tidy
    cmd("qpsn -t 100 -c {run_id} -- execute run{run_id}.mod -dir={run_dir}")
    
  d$m <- d$m %>% data_path(data_path(m2))
  d$m <- d$m %>% ctl_path("Models/m2_coretest/run{run_id}.mod")
  d$m <- d$m %>% cmd("qpsn -t 100 -c {run_id} -- execute run{run_id}.mod -dir={run_dir}")
  
  d$m <- d$m %>% 
    run_nm() %>%
    update_parameters() %>%
    run_nm() %>%
    gof()
  
  d <- dplyr::tribble(
    ~CMT, ~logDV, ~dataset,
    1,       TRUE,  "data.csv",
    1,       FALSE,  "data.csv",
    2,       TRUE,  "dataSMALL.csv",
    2,       FALSE,  "dataSMALL.csv"
  )
  
  d$m <- m2 %>% child(run_id = 1:nrow(d))
  d$m <- d$m %>% ctl_path("Models/baseModelTest/run{run_id}.mod")
  ctl_path(d$m)
  cmd(d$m)
  
  d$m[d$CMT %in% 1] <- d$m[d$CMT %in% 1] %>% 
    cmd("qpsn -t 59 -c auto -- execute run{run_id}.mod -dir={run_id}")
  cmd(d$m)
  d$m[d$CMT %in% 2] <- d$m[d$CMT %in% 2] %>% subroutine(advan = 4, trans = 1)
  d$m <- d$m %>% data_path(d$dataset)
  
  d$m[!duplicated(d$CMT)] %>% dollar("PK")
  d$m[!duplicated(d$dataset)] %>% dollar("DATA")
  
  library(stringr)
  m2 %>% target("PK") %s>% 
    str_replace("TVV", "TVV2") %s>%
    str_to_lower() %>% text
  
  
  m2 %>% advan()
  m2 %>% trans()
  
  m2 %>% model_type(advan = 4, trans = 4)
  
  
  
  
  
  ## need interface for lower level parameter changes
  
  ## dollar_theta could refer to
  ##  getting initial values
  ##  setting initial values
  ##  setting lower(/upper) bounds
  ##  getting lower(/upper) bounds
  ## final_thetas could refer to GETs like above
  
  ## Use cases:
  ##  Getting final ests for a simulation
  ##    need to get vector of thetas and a FULL omega block
  ##    look into what mrgsolve needs
  
  
  ## use data.frame instead?  fixed_err_nm2r_extra?
  ## would need convenience functions to interact with it.
  ##  use case 1: insert a new parameter
  m2 %>% insert_mixed_effect(name = "Q",
                             after = "KA",  ## how?
                             type = c("log_mu",   ## default
                                      "logit_mu",
                                      "normal_mu", 
                                      "fixed_theta"),
                             theta_init = 3,
                             theta_lower = NA,  ## default
                             theta_upper = NA,  ## default
                             theta_FIX = FALSE, ## default
                             omega_init = 0.1,
                             omega_FIX = TRUE)
  
  ## KA may have covariates - but that's OK
  ## find theta (in data.frame), MU and ETA (from PK block)
  ##  increment by one to get new things.
  
  ## what about omega blocks?  Do this in separate function
  
  m2 %>% create_omega_block(parameters = c("CL", "V", "K"))
  ## default = all
  
  
  
  m2 %>% remove_mixed_effect(name = "V")
  ## may have reverse dependencies (e.g. S2 = V, K12 = CL/V)
  ## do not try to fix this - this requires user decision making
  
  dollar_theta(m2)  ## return column vec of thetas
  
  m2 %>% dollar_theta(parameter = "CL", init = 3)
  ## changes (0, 2)  to (0, 3)
  
  m2 %>% dollar_theta(dollar_theta(m0))  ## sets 
  
  dollar_omega(m2)
  ## return list of omega blocks - or a vector if all blocks are size 1
  ##  or a large omega block?
  
  m2 %>% dollar_omega(dollar_omega(m0))  ## sets 
  m2 %>% dollar_omega(parameter = "CL", init = 0.1)
  
  
  iomega <- raw_omega_init(m1)
  iomega$init[3] <- 0.2
  m1 <- m1 %>% raw_omega_init(iomega)
  
  ## need to map parameters from one parameterisation to 
  ##  another
  
  ## how?
  
  ## some parameters are exactly the same
  ## some are equivalent, but named differently
  ## some are completely different.
  
  
  
  ## multiple ways to access elements of nm_list
  
  d$m$m4
  d$m[4]
  d$m[[4]]
  d$m[run_id(d$m) %in% "m4"]
  
  
  ## TODO: code library (tidyproject)
  ##    organisation
  ##    funs to extract files + projects
  
}

