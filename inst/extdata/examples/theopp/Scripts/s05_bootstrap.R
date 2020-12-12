## Copied from staging/Scripts/s04_bootstrap.R
##  (2020-04-22 19:27:41) by klgk669
## Author: kpfn434
## First created: 2020-04-15
## Description:
## Keywords:

########################################
## load packages and source functions here

#.libPaths(c("/projects/qcp/packages/dev/installed/", .libPaths()))
library(NMproject)
# devtools::load_all("~/tidyproject/")
# devtools::load_all("~/NMproject/")
# devtools::load_all("~/tidyprojectAZ/")
# devtools::load_all("~/NMproject/")

module_cmd("module load psn/4.4.8-foss-2017a-gfortran-5.2-b1")

# install.packages("rsample")
library(dplyr)
library(rsample)
########################################
## main script here

skip_data_creation <- TRUE

### Create boostraps dataset
# Stratify on STUDYID - to make sure we keep same HV/Pts, sampling scheme, metabolite...

c1 <- readRDS("Results/base_model_pk.RDS")  ## get base model from cov_test.Rmd
dd <- input_data(c1)


## Need to Stratify on STUDYID, PPI and ECOG
# PPI is either 0 vs 1+2
# ECOG is 0+1 vs 2+3
dd <-dd %>%
  mutate(PPICOV = ifelse(PPI%in%0,0,1),
         ECOGCOV = ifelse(!ECOG%in%c(2,3),0,2),
         STRATA = paste(STUDYID,PPICOV,ECOGCOV,sep="_")) %>%
  select(-PPICOV,-ECOGCOV)


dd_id <- dd %>%
  distinct(NMSEQSID, STRATA)

set.seed(123)

## create large set of resamples (to enable simulation to grow without ruining seed)
dboots_all <- rsample::bootstraps(dd_id, 1000, strata = "STRATA")

## create subset to write to disk
dboots <- dboots_all[1:500,] # datasets created
dboots$run_id <- 1:nrow(dboots)

boot_to_csv <- function(rsplit, run_id, folder){
  csv_name <- file.path(folder, paste0(run_id, ".csv"))
  if(skip_data_creation) return(csv_name)

  dd_boot <- analysis(rsplit) %>%
    mutate(ID = 1:nrow(.)) %>%
    inner_join(dd)

  dir.create(folder, showWarnings = FALSE, recursive = TRUE)

  write.csv(dd_boot, file = csv_name, quote = FALSE, col.names = TRUE, row.names = FALSE, na = ".")

  return(csv_name)
}

## write bootstrap datasets
## TODO: this needs caching (too slow)
dboots <- dboots %>%
  group_by(run_id) %>%
  mutate(
    csv_name = boot_to_csv(splits, run_id, "DerivedData/bootstrap_datasets")
  )


## for speed create one object which is close to the bootstrap replicates then create children
dm1_boot <- dboots[1:150,] %>%
  ungroup() %>%
  mutate(
    m = c1 %>%
      data_path(csv_name[1]) %>%
      fill_input() %>%
      ## don't change run_id or run_in before child() to retain parent
      child(run_id) %>%
      data_path(csv_name) %>%
      run_in("Models/m1_boot")
  )

dm518b_boot$m %<>% run_nm()

## 95% CI on all param
dres <- coef(dm518b_boot$m[status(dm518b_boot$m)%in%"finished"]) %>%
  bind_rows()%>%
  group_by(parameter) %>%
  summarise(low=quantile(FINAL,probs=0.025),
            med=median(FINAL),
            upp=quantile(FINAL,probs=0.975))

dres %>% View()
