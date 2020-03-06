## Author: klgk669
## First created: 2017-10-20
## Description:
## Keywords:

########################################
## main script here

test_nm <- function(r){

  status_ob <- status(r)

  status <- status_ob$status

  job_info <- job_info(r)

  results <- do_test(cmd = r$cmd,
                     status = status,
                     job_no = job_info,
                     silent = TRUE)

  if(!all(status_ob$sub_run_status != "finished")) {

    d <- get_job_info_list(job_info)

    raw_res <- read.csv(file.path(r$run_dir,paste0("raw_results_run",r$run_id,".csv")))
    run_time <- raw_res$model_run_time
    run_time <- as.character(run_time)
    run_time <- lubridate::hms(run_time)

    results <- rbind(results,
                     do_test(NMtime = as.character(run_time),silent = TRUE))

    if(nrow(d) == 1){
      results <- rbind(results,
                       do_test(Rtime = as.character(lubridate::duration(d$Rtime, units = "min")),
                               Qtime = as.character(lubridate::duration(d$Qtime, units = "min")),
                               nCPU = d$nCPU,
                               silent = TRUE))

    }

    psn_out_path <- file.path(r$run_in, paste0("psn-", job_info,
                                               ".out"))

    p <- readLines(psn_out_path)

    error_type <- "None detected"
    if(any(grepl("Not restarting this model",p))) error_type <- "Not restarting this model"
    if(any(grepl("Fatal error",p))) error_type <- "Fatal error"
    if(any(grepl("No nonmem execution",p))) error_type <- "No nonmem execution"

    results <- rbind(results,
                     do_test(errors = error_type,silent = TRUE))


  }

  results

}

test_execute <- function(r){

  results <- test_nm(r)

  status_ob <- status(r)
  if(!all(status_ob$sub_run_status != "finished")){
    res1 <- coef(r)
    res1$FINAL[res1$Parameter=="OBJ"]

    results <- rbind(results,do_test(OFV = res1$FINAL[res1$Parameter=="OBJ"],silent = TRUE))
  }

  ####################################################
  ## do post processing plots and check size

  #############
  ## basic gof
  gof_ggplot(r)

  results <- rbind(results,
                   do_test(gof_pdf_size = file.size(
                     paste0("Results/gof.ggplot.run.",r$run_id,".pdf")
                   ),silent = TRUE))

  #############
  ## plot iter
  pdf(paste0("Results/plot_iter",r$run_id,".pdf"))
  plot_iter(r)
  dev.off()

  results <- rbind(results,
                   do_test(plot_iter_pdf_size = file.size(
                     paste0("Results/plot_iter",r$run_id,".pdf")
                   ),silent = TRUE))

  #############
  ####################################################

  results

}
