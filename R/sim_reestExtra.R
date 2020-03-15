#' generate paths for simulation runs
#' 
#' @param dsc tibble::tibble with experimental factors to vary
#' @param start_dir directory name within which all runs should take place.

gen_sim_path <- function(dsc,start_dir,include_names = TRUE){
  
  ## run as dsc$location <- gen_sim_path(dsc, ...)
  
  for(i in which(sapply(dsc,is.factor))){
    dsc[,i] <- as.character(dsc[,i])
  }
  sapply(1:nrow(dsc),function(i){
    if(include_names) location <- paste(names(dsc[i,]),as.character(dsc[i,]),sep="_",collapse = .Platform$file.sep)
    if(!include_names) location <- paste(as.character(dsc[i,]),sep="_",collapse = .Platform$file.sep)
    file.path(start_dir,location)
  })
}

split_by_row <- function(data) split(data, row.names(data))

#' @export
by_row <- function(.data,...,.apply_fun=lapply) {
  ## added dots to prevent partial argument matching. Not perfect.
  split_by_row(.data) %>% .apply_fun(...)
}

#' @export
by_row_df <- function(.data, .f, .apply_fun=lapply, ...) {
  ## added dots to prevent partial argument matching. Not perfect.
  .f <- purrr::as_mapper(.f)
  d <- split_by_row(.data) %>% .apply_fun(.f, ...)
  if(length(d) == 0) return(data.frame())
  nm_cols <- names(d[[1]])[sapply(d[[1]], is_nm_list)]
  d <- suppressWarnings(dplyr::bind_rows(d))
  for(nm_col in nm_cols){
    d[[nm_col]] <- as_nm_list(d[[nm_col]])    
  }
  d
}

mark_completed <- function(data, task, name, result, result_character,
                           return = c("logical","character"), RDS_name="progress.RDS"){
  
  ## use on exit points of functions
  
  progress_file_name <- file.path(data$location, RDS_name)
  if(file.exists(progress_file_name)) {
    progress <- readRDS(progress_file_name)
    #  } else progress <- data.frame()
  } else progress <- tibble::tibble()
  ## progress is now defined
  
  if(missing(name)) name <- NA
  if(missing(result_character)) result_character <- NA
  if(missing(result)) result <- NA
  
  d <- tibble::tibble(task, name, result = as.logical(result), result_character = as.character(result_character))
  dmatch <- merge(progress,d[,c("task","name")])
  if(nrow(dmatch) == 0){ ## if this task is new
    progress <- rbind(progress, d)
  } else {
    progress$result[progress$task %in% task & progress$name %in% name] <- as.logical(result)
    progress$result_character[progress$task %in% task & progress$name %in% name] <- as.character(result_character)
  }
  progress <- unique(progress)
  
  saveRDS(progress, file = progress_file_name)
  return <- match.arg(return)
  if(return == "logical") return_col <- "result"
  if(return == "character") return_col <- "result_character"
  get(return_col)
  
}

completed <- function(data, task, name = as.character(NA), return = c("logical","character"), RDS_name="progress.RDS"){
  
  return <- match.arg(return)
  progress_file_name <- file.path(data$location, RDS_name)
  if(return == "logical") return_col <- "result"
  if(return == "character") return_col <- "result_character"
  
  if(!file.exists(progress_file_name)){
    if(return %in% "logical") return(FALSE) 
    if(return %in% "character") return("FALSE")
  }
  
  progress <- readRDS(file = progress_file_name)
  progressi <- progress[progress$task %in% task &
                          progress$name %in% name,]
  if(nrow(progressi)==0) {
    if(return %in% "logical") return(FALSE) 
    if(return %in% "character") return("FALSE")
  }
  progressi[[return_col]]
}

collate_res <- function(data,data_name){
  paths <- file.path(data$location,data_name)
  res <- lapply(paths,function(path){
    if(!file.exists(path)) return(NA)
    return(readRDS(path))
  })
  res
}

collate_data <- function(data, data_name){
  data <- collate_res(data, data_name)
  data <- do.call(rbind,data)
  data
}

collate_m <- function(data, data_name = "m.RDS"){
  m <- collate_res(data, data_name)
  m <- do.call(c, m)
  m
}

run_nm_batch <- function(m, threads = 10, ...){
  runs_remaining <- seq_along(m)
  while(length(runs_remaining) > 0){
    n_to_take <- min(threads, length(runs_remaining))
    runs_to_run <- runs_remaining[seq_len(n_to_take)]
    sub_m <- lapply(runs_to_run, function(x) m[[x]])
    run_nm(sub_m, ...)
    runs_remaining <- setdiff(runs_remaining, runs_to_run)
    do.call(wait_for_finished, sub_m)
  }
  Sys.sleep(10) ## this shouldn't be needed, soemthing weird going on with wait_for_finished?
}

get_progress <- function(data, RDS_name = "progress.RDS"){
  RDS_name <- file.path(data$location, RDS_name)
  if(!file.exists(RDS_name)) progress <- tibble::tibble() else progress <- readRDS(RDS_name)
  merge(data,progress)
}


#' Prepare forward covariate step
#' 
#' @param base nm object
#' @param run_id run_id 
#' @param run_in run_in
#' @param dtest tibble::tibble with testing relations
#' @param direction character. "forward" (default) or "backward"
#' @param ... additional arguments passed to add_cov
#' 
#' @return tibble::tibble dtest with appended columns 

covariate_step_setup <- function(base, run_id, run_in = getOption("models.dir"), dtest, direction = c("forward", "backward"), ...){
  
  direction <- match.arg(direction)
  
  ## check dtest
  
  if(!inherits(dtest, "data.frame")) stop("dtest needs to be a tibble of tested relations - use tested_relations()")
  
  required_cols <- c("cov", "param", "state", "continuous")
  if(direction == "backward")
    required_cols <- c("cov", "param")
  
  for(col in required_cols) {
    if(!col %in% names(dtest)) stop("need a ",col," column in dtest")
  }
  
  ## check no covariates are in dataset and have no missing values
  dinput <- NMproject::input_data(base)
  test_covs <- unique(dtest$cov)
  if(any(!test_covs %in% names(dinput))){
    stop("cannot find covariates:\n ",
         paste(unique(test_covs[!test_covs %in% names(dinput)]), collapse = ","),
         "\nin dataset")
  }
  incomplete_cases <- names(dinput)[!stats::complete.cases(t(dinput))]
  if(any(test_covs %in% incomplete_cases)){
    stop("the following covariates contain missing values:\n ",
         paste(unique(test_covs[test_covs %in% incomplete_cases]), collapse = ","),
         "\nstopping to be safe. Impute missing values in input dataset")
  }
  
  len_unique_cols <- sapply(dinput[, test_covs], function(i) length(unique(i)))
  if(any(len_unique_cols <= 1)){
    stop("the following covariates only have a single value:\n ",
         paste(test_covs[len_unique_cols <= 1], collapse = ","),
         "\nstopping to be safe. Impute missing values in input dataset")
  }
  
  dsc <- dtest
  
  #start_dir <- file.path(getOption("models.dir"), paste0(run_id(base),"_",label))
  # start_dir <- run_in(base)
  # if(length(start_dir) > 1) stop("non unique parents", call. = FALSE)
  # if(length(start_dir) < 1) stop("can't find parent run in location", call. = FALSE)
  start_dir <- file.path(run_in, run_id)
  
  if(direction == "forward"){
    dsc$location <- gen_sim_path(dsc[,c("param","cov","state")],
                                 start_dir, include_names = FALSE)
    
  } else {
    dsc$location <- gen_sim_path(dsc[,c("param","cov")],
                                 start_dir, include_names = FALSE)
  }
  
  ## store included relation
  ##   control stream text - prefer this.
  
  run_ids <- paste(run_id, dsc$param, dsc$cov, dsc$state, sep = "_")
  
  dsc$m <- base %>% child(run_id = run_ids)
  
  if(direction == "forward") {
    dsc$m <- try(add_cov(ctl = dsc$m,
                         param = dsc$param,
                         cov = dsc$cov,
                         state = dsc$state,
                         continuous = dsc$continuous,
                         ...), silent = TRUE)
  } else {
    dsc$m <- try(remove_cov(ctl = dsc$m,
                            param = dsc$param,
                            cov = dsc$cov,
                            ...), silent = TRUE)
    
    dsc <- dsc[!is.na(dsc$m), ] ## don't want covariates that aren't there
    dsc <- dsc[!duplicated(paste(dsc$param, dsc$cov)), ] ## dont want duplicate states
    dsc$state <- NULL
    dsc$continuous <- NULL
  }
  
  dsc$m <- dsc$m %>% run_in(dsc$location)
  dsc$m <- dsc$m %>% results_dir(dsc$location)
  
  dsc
  
}

#' @export
bind_covariate_results <- function(dsc, nm_col = "m", parameters = "new"){
  
  dsum <- summary_wide(dsc[[nm_col]], parameters = parameters, trans = FALSE)
  
  dsc <- dsc[, names(dsc)[!names(dsc) %in% names(dsum)]]
  
  dsc %>%
    dplyr::bind_cols(dsum) %>% 
    dplyr::arrange(.data$p_chisq)
}

stamp_completed <- function(data, d){
  task <- "run completed"
  if(!completed(data, "object created")) {
    mark_completed(data, task, result = FALSE)
    return()
  }
  
  m <- readRDS(file.path(data$location, "m.RDS"))
  mark_completed(data, task, result = is_finished(m))
}

process_outputs <- function(data, d){
  task <- "results saved"
  if(!completed(data, "run completed")) {
    mark_completed(data, task, result = FALSE)
    return()
  }
  m <- readRDS(file.path(data$location, "m.RDS"))
  out <- try(process_output(m, dorig = d))
  if(inherits(out, "try-error")){
    mark_completed(data, task, result = FALSE)
    return()
  }
  mark_completed(data, task, result = TRUE)
}

covariates_define_old <- function(dcov, cov, continuous){
  
  if(missing(cov)) return(tibble::tibble())
  if(length(continuous) > 1) stop("continous can only be TRUE/FALSE")
  dcov_new <- tibble::tibble(cov, continuous)
  if(!missing(dcov)) dcov_new <- dplyr::bind_rows(dcov, dcov_new)
  return(dcov_new)
  
}

#' @export
covariates_define <- function(d, continuous, categorical, log_transform_plot = c()){

  if(!inherits(d, "grouped_df"))
    stop("d must be data.frame grouped_by ID using dplyr::group_by()")
  
  by <- attributes(d)$vars
  
  if(missing(continuous) & missing(categorical)) 
    return(tibble::tibble())
  
  cov <- c(continuous, categorical)
  continuous <- cov %in% continuous
  
  dcov <- tibble::tibble(cov, continuous)
  ## gather information on these covariates
  
  dtemp <- d[, c(by, dcov$cov)] %>%
    dplyr::group_by_(by) %>% 
    dplyr::summarise_all(function(i)length(unique(i)))
  
  static <- sapply(dcov$cov, function(i) all(dtemp[[i]] %in% 1))
  
  dcov$static <- static
  dcov$log_transform_plot <- dcov$cov %in% log_transform_plot
  dcov$by <- by
  
  return(dcov)
  
}

#' @export
test_relations <- function(dtest, param, cov, state, continuous){
  
  if(missing(param)) return(tibble::tibble())
  if(length(continuous) > 1) stop("continous can only be TRUE/FALSE")
  dtest_new <- expand.grid(param = param, cov = cov, state = state, stringsAsFactors = FALSE)
  dtest_new <- tibble::as_tibble(dtest_new)
  dtest_new$continuous <- continuous
  if(!missing(dtest)) dtest_new <- dplyr::bind_rows(dtest, dtest_new)
  return(dtest_new)
  
}

identify_cov_relations <- function(r){
  ctl <- ctl_list(r)
  pk_block <- ctl$PK
  
  cov_relations <- pk_block[grepl("^;;;\\s*\\S+-DEFINITION START", pk_block)]
  
  cov_relations <- gsub("^;;;\\s*(\\S+)-DEFINITION.*","\\1",cov_relations)
  
  pars <- pk_block[grepl("^\\s*\\S+\\s=", pk_block)]
  
  pars <- gsub("^\\s*(\\S+)\\s=.*","\\1",pars)
  
  #d <- get_data(r)
  d <- input_data(r)
  
  dcomb <- expand.grid(param = pars, cov=names(d))
  possible_cov_relations <- paste0(dcomb$param,dcomb$cov)
  
  dcomb[possible_cov_relations %in% cov_relations, ]
  
}

## given param and cov

#' @export
psn_style_scm <- function(base, run_in, dtest,
                          alpha_forward = 0.05, alpha_backward = 0.01){
  
  base_run_id <- run_id(m)
  m <- base
  fi <- 1
  bi <- 1
  while(TRUE){ ## foward step
    run_id <- paste0(base_run_id, "_", fi)
    d <- m %>% covariate_step_setup(run_id = run_id,
                                    run_in = run_in,
                                    dtest = dtest,
                                    direction = "forward")
    
    d <- d$m %>% run_nm() %>% wait_finish()
    
    d <- d %>% bind_covariate_results()
    
    ## most significant is the first - pick that
    if(d$p_chisq[1] > alpha_forward) break
    
    m <- d$m[1]
    fi <- fi + 1
  }
  
  forward_run_id <- run_id
  
  while(TRUE){
    run_id <- paste0(forward_run_id, "_", bi)
    d <- m %>% covariate_step_setup(run_id = run_id,
                                    run_in = run_in,
                                    dtest = dtest,
                                    direction = "backward")
    
    d$m <- d$m %>% run_nm() %>% wait_finish()
    
    d <- d %>% bind_covariate_results()
    
    if(d$p_chisq[1] < alpha_backward) break
    
    m <- d$m[1] ## most sig
  }
  m
}

#' process data from PPC for plotting
#' 
#' @param r nm object
#' @param FUN statistic function with NONMEM dataset as arg and
#'   returns data.frame with a column "statistic"
#' @param statistic character (default = "statistic") name of statistic column
#'   returned by FUN
#' @param additional arguments passed to dplyr::mutate for data manipulaion prior
#'   to statistic (FUN) calculation
#' @export

ppc_data <- function(r, FUN, statistic = "statistic", ...){
  
  if(length(unique(data_path(r))) > 1) stop("non-unique datasets")
  
  dorig <- input_data(r[1])
  dsims <- output_table(r)
  
  dsims <- lapply(seq_along(dsims), function(i){
    dsims[[i]]$mod_no <- i
    dsims[[i]]
  })
  
  dsims <- dplyr::bind_rows(dsims)
  dsims$DV <- dsims$DV_OUT

  ## two datasets dorig and dsims ready now, apply function
  
  stat_orig <- FUN(dorig)
  if(!inherits(stat_orig, "data.frame")) stop("FUN must return a data.frame", call. = FALSE)
  if(!statistic %in% names(stat_orig)) stop("statistic must be a column of FUN output", call. = FALSE)
  
  names(stat_orig)[names(stat_orig) %in% statistic] <- paste0(statistic, "_true")
  
  ## if ... are present  
  ## useful for modifying data items before stat function is applied
  current_call <- as.list(match.call())
  supplied_args <- names(current_call[-1])
  total_args <- names(formals(ppc_data))
  dots_present <- any(!supplied_args %in% total_args)

  if(dots_present){
    dsims <- dsims %>% dplyr::mutate(...)    
  }
  
  ## apply stat FUN to each sim
  ## apply change to the dataset
  
  stat_sim <- dsims %>% dplyr::group_by(.data$mod_no) %>% 
    tidyr::nest() %>%
    dplyr::mutate(statistic = purrr::map(.data$data, FUN)) %>%
    tidyr::unnest(statistic)
  
  merge(stat_sim, stat_orig)
  
}

