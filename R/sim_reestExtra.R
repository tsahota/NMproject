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

#' @export
run_nm_batch <- function(m, threads = 10, ...){
  runs_remaining <- seq_along(m)
  while(length(runs_remaining) > 0){
    n_to_take <- min(threads, length(runs_remaining))
    runs_to_run <- runs_remaining[seq_len(n_to_take)]
    m_sub <- m[runs_to_run]
    run_nm(m_sub, ...)
    runs_remaining <- setdiff(runs_remaining, runs_to_run)
    if(length(runs_remaining) > 0) wait_finish(m_sub)
  }
  m
}

#' Prepare forward covariate step
#' 
#' @param base nm object
#' @param run_id base run_id to construct run_ids of covariate runs
#' @param run_in character.  See \code{\link{run_in}}
#' @param dtest tibble::tibble with testing relations (from \code{\link{test_relations}})
#' @param direction character. "forward" (default) or "backward"
#' @param ... additional arguments passed to add_cov
#' 
#' 
#' @return tibble::tibble dtest with appended columns 
#' 
#' @seealso \code{\link{test_relations}}, \code{\link{covariate_step_tibble}}, \code{\link{bind_covariate_results}} 
#' 
#' @examples 
#' \dontrun{
#' 
#' dtest <- test_relations(param = c("KA", "K", "V"),
#'                         cov = c("LIN1", "LIN2", "LIN3", "RND1", "RND2", "RND3"), 
#'                         state = c("linear", "power"), 
#'                         continuous = TRUE)
#' dtest <- dtest %>% 
#'              test_relations(param = c("KA", "K", "V"),
#'                             cov = "BN1",
#'                             state = "linear",
#'                             continuous = FALSE)
#'                             
#' ## create tibble of covariate step with model objects as column m
#' dsm1 <- m1 %>% covariate_step_tibble(
#'   run_id = "m1_f1",
#'   dtest = dtest,
#'   direction = "forward"
#' )
#' 
#' ## run all models greedily
#' dsm1$m <- dsm1$m %>% run_nm()
#' 
#' ## extract results and put into tibble
#' dsm1 <- dsm1 %>% bind_covariate_results()
#' 
#' ## sort by BIC (for example) and view
#' dsm1 <- dsm1 %>% arrange(BIC)
#' dsm1
#' 
#' ## check condition number, covariance,... 
#' ## run any diagnostics here
#' 
#' ## when happy with selection, select run for subsequent step
#' 
#' m1_f1 <- dsm1$m[1]   ## select most signifcant BIC
#' # alternative select by relationship
#' m1_f1 <- dsm1 %>%
#'     filter(param = "CL", cov = "BWT", state = "power") %$%
#'     m
#' 
#' ## do next forward step
#' 
#' dsm2 <- m1_f1 %>% covariate_step_tibble(
#'   run_id = "m1_f2",
#'   dtest = dtest,
#'   direction = "forward"
#' )
#' 
#' ## continue ...
#'   
#' }
#' @export

covariate_step_tibble <- function(base, run_id, run_in = getOption("models.dir"), dtest, direction = c("forward", "backward"), ...){
  
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
  dinput <- tibble::tibble(input_data(base))
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
    dsc$m <- add_cov(ctl = dsc$m,
                     param = dsc$param,
                     cov = dsc$cov,
                     state = dsc$state,
                     continuous = dsc$continuous,
                     ...)
  } else {
    dsc$m <- remove_cov(ctl = dsc$m,
                        param = dsc$param,
                        cov = dsc$cov,
                        ...)
    
    dsc <- dsc[!is.na(dsc$m), ] ## don't want covariates that aren't there
    dsc <- dsc[!duplicated(paste(dsc$param, dsc$cov)), ] ## dont want duplicate states
    dsc$state <- NULL
    dsc$continuous <- NULL
  }

  dsc$m <- dsc$m %>% run_in(dsc$location)
  dsc$m <- dsc$m %>% results_dir(dsc$location)
  
  dsc
  
}

#' Add run results into a covariate tibble
#' 
#' @param dsc a covariate tibble (see \code{\link{covariate_step_tibble}})
#' @param nm_col character (default = "m"). name of column to store nm objects
#' @param parameters character (default = "new").  Passed to \code{\link{summary_wide}}
#' 
#' @seealso \code{\link{covariate_step_tibble}}
#' 
#' @examples 
#' \dontrun{
#' ## create tibble of covariate step with model objects as column m
#' dsm1 <- m1 %>% covariate_step_tibble(
#'   run_id = "m1_f1",
#'   dtest = dtest,
#'   direction = "forward"
#' )
#' 
#' ## run all models greedily
#' dsm1$m <- dsm1$m %>% run_nm()
#' 
#' ## extract results and put into tibble
#' dsm1 <- dsm1 %>% bind_covariate_results()
#' }
#' @export
bind_covariate_results <- function(dsc, nm_col = "m", parameters = "new"){
  
  dsum <- summary_wide(dsc[[nm_col]], parameters = parameters, trans = FALSE)
  
  dsc <- dsc[, names(dsc)[!names(dsc) %in% names(dsum)]]
  
  dsc %>%
    dplyr::bind_cols(dsum) %>% 
    dplyr::arrange(.data$p_chisq)
}

#' Generate tibble of relations to test
#'
#' @param dtest (optional) existing dtest to append (from an previous use test_relations())
#' @param param character. Name of parameter
#' @param cov character. Name of covariate
#' @param state numeric or character. Number/name of state (see details)
#' @param continuous logical (default = TRUE). is covariate continuous?
#'
#' @details 
#' available states (see also \code{\link{add_cov}}):
#' "2" or "linear":
#'   PARCOV= ( 1 + THETA(1)*(COV - median))
#' 
#' "3" or "hockey-stick":
#'   IF(COV.LE.median) PARCOV = ( 1 + THETA(1)*(COV - median))
#'   IF(COV.GT.median) PARCOV = ( 1 + THETA(2)*(COV - median))
#'                     
#' "4" or "exponential":
#'    PARCOV= EXP(THETA(1)*(COV - median))
#' 
#' "5" or "power":
#'    PARCOV= ((COV/median)**THETA(1))
#'    
#' "power1":
#'    PARCOV= ((COV/median))
#'    
#' "power0.75":
#'    PARCOV= ((COV/median)**0.75)
#' 
#' "6" or "log-linear":
#'    PARCOV= ( 1 + THETA(1)*(LOG(COV) - log(median)))
#'    
#' @seealso \code{\link{add_cov}}, \code{\link{covariate_step_tibble}} 
#'    
#' @examples 
#' 
#' \dontrun{
#' dtest <- test_relations(param = c("KA", "K", "V"),
#'                         cov = c("LIN1", "LIN2", "LIN3", "RND1", "RND2", "RND3"), 
#'                         state = c("linear", "power"), 
#'                         continuous = TRUE)
#' dtest <- dtest %>% 
#'              test_relations(param = c("KA", "K", "V"),
#'                             cov = "BN1",
#'                             state = "linear",
#'                             continuous = FALSE)
#' 
#' }
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
    d <- m %>% covariate_step_tibble(run_id = run_id,
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
    d <- m %>% covariate_step_tibble(run_id = run_id,
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

#' PPC functions: process data from simulation and plot
#' 
#' @param r nm object (simulation)
#' @param FUN statistic function with NONMEM dataset as arg and
#'   returns data.frame with a column "statistic"
#' @param ... additional arguments for FUN
#' @param pre_proc function to mutate dataset prior to compute statistics
#' @param max_mod_mo integer. Maximum model number to read (set low for debugging)
#' @param DV character (default = "DV")
#' @param statistic character (default = "statistic") name of statistic column
#'   returned by FUN
#' @param group,var1,var2 grouping variables for plotting
#'   
#' @seealso \code{\link{nm_render}}
#' @examples 
#' \dontrun{
#' 
#' idEXPstat <- function(d, ...){ ## example individual statistic function
#'  ## arg = nonmem dataset data.frame
#'  ## return data.frame with statistic column
#'   d %>% group_by(ID, ...) %>% filter(is.na(AMT)) %>%
#'     summarise(
#'       AUC = AUC(time = TIME, conc = DV),
#'       CMAX = max(DV, na.rm = TRUE),
#'       TMAX = TIME[which.max(DV)]
#'     ) %>%
#'     tidyr::gather(key = "exposure", value = "statistic", AUC:TMAX) %>%
#'     ungroup()
#'}
#'
#'EXPstat <- function(d, ...){ ## example summary statistic function
#'  ## arg = nonmem dataset data.frame
#'  ## return data.frame with statistic column
#'  d %>% idEXPstat(...) %>%  ## reuse idEXPstat for individual stats
#'    ## summarise over study and any other variables (...)
#'    group_by(exposure, ...) %>%
#'    summarise(
#'      median = median(statistic, na.rm = TRUE),
#'      cv = 100*sd(statistic, na.rm = TRUE)/mean(statistic, na.rm = TRUE)
#'    ) %>%
#'    tidyr::gather(key = "type", value = "statistic", median:cv)
#'}
#' 
#' dppc <- m1s %>% ppc_data(EXPstat)
#' 
#' dppc %>% ppc_whisker_plot()
#' dppc %>% ppc_forest_plot()
#' 
#' }
#' @rdname ppc
#' @export

ppc_data <- function(r,  FUN, ..., pre_proc = identity, max_mod_no = NA, DV = "DV", statistic = "statistic"){
  
  if(!"..." %in% names(formals(FUN))) stop("FUN must have ... in arguments")
  if(length(names(formals(FUN))) < 2) stop("FUN must have at least two arguments (a data.frame and ...")
  if(length(unique(data_path(r))) > 1) stop("non-unique datasets")
  
  dorig <- input_data(r[1], filter = TRUE)
  dsims <- output_table(r) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(.data$INNONMEM)
  
  total_rows <- nrow(dorig)
  
  nsim <- nrow(dsims)/nrow(dorig)
  
  dsims$mod_no <- rep(1:nsim, each = total_rows)
  
  if(!is.na(max_mod_no)) {
    dsims <- dsims[dsims$mod_no <= max_mod_no, ]
    nsim <- nrow(dsims)/nrow(dorig)
  }
  
  dsims <- dsims[, c("DV_OUT", "mod_no")]
  names(dsims)[1] <- DV
  
  dorig2 <- dorig
  dorig2[[DV]] <- NULL
  
  dorig2$ORD <- 1:nrow(dorig2)
  dsims$ORD <- 1:nrow(dorig2)
  
  nrow(dsims)
  dsims <- dplyr::right_join(dorig2, dsims)
  nrow(dsims)
  
  ## two datasets dorig and dsims ready now, apply function
  dorig <- pre_proc(dorig)
  stat_orig <- FUN(dorig, ...)
  if(!inherits(stat_orig, "data.frame")) stop("FUN must return a data.frame", call. = FALSE)
  if(!statistic %in% names(stat_orig)) stop("statistic must be a column of FUN output", call. = FALSE)
  
  names(stat_orig)[names(stat_orig) %in% statistic] <- paste0(statistic, "_true")
  
  ## if ... are present  
  ## useful for modifying data items before stat function is applied
  current_call <- as.list(match.call())
  supplied_args <- names(current_call[-1])
  total_args <- names(formals(ppc_data))
  dots_present <- any(!supplied_args %in% total_args)
  
  # if(dots_present){
  #    dsims <- dsims %>% dplyr::mutate(...)    
  #  }
  
  ## apply stat FUN to each sim
  ## apply change to the dataset
  # browser()
  # stat_tmp <- FUN(dsims %>%
  #                   filter(mod_no %in% 1), ...)
  # 
  # dtmp <- dsims %>%
  #   filter(mod_no %in% 1) %>% 
  #   dplyr::group_by(.data$mod_no) %>% 
  #   pre_proc() %>%
  #   tidyr::nest() %>%
  #   dplyr::mutate(statistic = purrr::map(.data$data, FUN,...)) %>%
  #   tidyr::unnest(statistic)
  
  
  stat_sim <- dsims %>% dplyr::group_by(.data$mod_no) %>% 
    pre_proc() %>%
    tidyr::nest() %>%
    dplyr::mutate(statistic = purrr::map(.data$data, FUN,...)) %>%
    tidyr::unnest(statistic)
  
  stat_sim$data <- NULL
  
  merge(stat_sim, stat_orig)
  
}

