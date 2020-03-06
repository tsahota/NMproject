## Author: klgk669
## First created: 2019-12-10
## Description: PPC demo
## Keywords:

########################################
## load packages and source functions here

library(future)
future::plan("future::multiprocess", workers = 2)
library(NMprojectAZ)
library(dplyr)
devtools::load_all("~/NMproject/")
load_localpackage()

module_cmd("module load psn")
#module_cmd("module load psn/4.4.8-foss-2017a-gfortran-5.2-b1")

########################################
## main script here

idEXPstat <- function(d){ ## example statistic function
  ## arg = nonmem dataset data.frame 
  ## return data.frame with statistic column
  d %>% group_by(ID) %>% filter(is.na(AMT)) %>%
    summarise(
      AUC = AUC(time = TIME, conc = DV),
      CMAX = max(DV, na.rm = TRUE),
      TMAX = TIME[which.max(DV)]
    ) %>%
    tidyr::gather(key = "type", value = "statistic", -ID)
}

EXPstat <- function(d){ ## example statistic function
  ## arg = nonmem dataset data.frame 
  ## return data.frame with statistic column
  d %>% group_by(ID) %>% filter(is.na(AMT)) %>%
    summarise(
      AUC = AUC(time = TIME, conc = DV),
      CMAX = max(DV, na.rm = TRUE),
      TMAX = TIME[which.max(DV)]
    ) %>% 
    ungroup() %>%
    summarise_at(c("AUC", "CMAX", "TMAX"), median, na.rm = TRUE) %>%
    tidyr::gather(key = "type", value = "statistic")
}


c1 <- readRDS("Results/base_model_pk.RDS")  ## get base model from cov_test.Rmd

d <- tibble(sim = 1:10)
d$run_id <- paste0("s", d$sim)

d$m <- c1 %>% child(run_id = d$run_id) %>%
  update_parameters(c1) %>%
  run_in("Models/ppc_c1") %>%
  convert_to_simulation(seed = d$sim)

d <- {
  d %>% mutate(m = m %>% run_nm() %>% wait_finish())
}

## nmsave_plot cannot handle multiple page reports - do traditional pdf() method


p <- d$m %>% ppc_plots(idEXPstat, factor(ID), type)

c1 %>% nmsave_multiplot(list(p), "base_ppc_idEXPstat.pdf")


dppc <- d$m %>% ppc_data(idEXPstat)
p1 <- dppc %>% ppc_whisker_plot(factor(ID), type)
p2 <- dppc %>% ppc_histogram_plot(factor(ID), type)
pdf(file.path(results_dir(c1), "base_ppc_idEXPstat.pdf"))
list(p1, p2)
dev.off()

dppc <- d$m %>% ppc_data(EXPstat)
p1 <- dppc %>% ppc_whisker_plot(type)
p2 <- dppc %>% ppc_histogram_plot(type)
pdf(file.path(results_dir(c1), "base_ppc_EXPstat.pdf"))
list(p1, p2)
dev.off()



