#' @export
add_cov.nm_generic <- function(ctl, param, cov, state = 2, continuous = TRUE,
                               time_varying, additional_state_text, id_var = "ID",
                               force = FALSE, force_TV_var = FALSE,
                               init, lower, upper){
  
  m <- ctl
  ctl <- ctl_contents(m)
  param <- as.character(param)
  cov <- as.character(cov)
  state <- as.character(state)
  continuous <- as.logical(continuous)
  
  if("PK" %in% names(ctl)) dol_PK <- "PK" else dol_PK <- "PRED"
  
  PK_section <- rem_comment(ctl[[dol_PK]])
  
  data <- suppressMessages(input_data(m, filter = TRUE))
  
  if(!cov %in% names(data)) {
    if(force) {
      warning("can't find ",cov," in data", call. = FALSE)
    } else {
      stop("can't find ",cov," in data", call. = FALSE)
    }
  }
  
  if(any(is.na(data[[cov]]))) {
    if(force) {
      warning("missing values in ",cov," detected", call. = FALSE)
    } else {
      stop("missing values in ",cov," detected", call. = FALSE)
    }
  }
  
  if(length(unique(data[[cov]])) > 5 & !continuous)
    warning(length(unique(data[[cov]])), " unique values for ", cov, " found. are you sure it's categorical?",
            call. = FALSE)
  
  if(length(unique(data[[cov]])) <= 1)
    warning(length(unique(data[[cov]])), " unique values for ", cov, " found. are you sure about this?",
            call. = FALSE)
  
  if(missing(time_varying)){
    max_levels <- max(tapply(data[[cov]], data[[id_var]], function(x) length(unique(x))), na.rm = TRUE)
    if(max_levels > 1) time_varying <- TRUE else time_varying <- FALSE
  }
  
  if(force_TV_var){
    tvparam <- paste0("TV",param)
  } else {
    if(time_varying){
      tvparam <- param
    } else {
      ## try TV param if exists
      if(any(grepl(paste0("\\bTV",param,"\\b"), PK_section)))
        tvparam <- paste0("TV",param)
    }
  }
  
  if(!any(grepl(paste0("\\bTV",param,"\\b"), PK_section)))
    stop("cant find parameter in control file", call. = FALSE)
  
  existing_param_rel <- any(grepl(paste0("\\b",tvparam,"COV"), PK_section))
  existing_param_cov_rel <- any(grepl(paste0("\\b",tvparam,cov), PK_section))
  if(existing_param_cov_rel) {
    return(as_nm_generic(nm(NA)))
    #stop("covariate relation already exists, cannot add", call. = FALSE)
  }
  
  param_info <- param_info(ctl)
  theta_n_start <- max(param_info$N) + 1
  
  relation_start_txt <- paste0(";;; ",tvparam,"-RELATION START")
  relation_end_txt <- paste0(";;; ",tvparam,"-RELATION END")
  
  definition_start_txt <- paste0(";;; ",tvparam,cov,"-DEFINITION START")
  definition_end_txt <- paste0(";;; ",tvparam,cov,"-DEFINITION END")
  
  if(!existing_param_rel){
    par_relation_text <- paste0(tvparam,"COV=",tvparam,cov)
    
    ## insert at beginning
    ctl[[dol_PK]] <- c(ctl[[dol_PK]][1],"",
                       relation_start_txt,
                       par_relation_text,
                       relation_end_txt,
                       ctl[[dol_PK]][-1])
    
    tv_definition_row <- which(grepl(paste0("^\\s*",tvparam,"\\s*="), rem_comment(ctl[[dol_PK]])))
    dont_count <- which(grepl(paste0("^\\s*",tvparam,"\\s*=.*\\b",tvparam), rem_comment(ctl[[dol_PK]])))
    tv_definition_row <- setdiff(tv_definition_row, dont_count)
    if(length(tv_definition_row) > 1) stop("can't find unique TV parameter definition in $PK")
    if(length(tv_definition_row) == 0) stop("can't find TV parameter definition in $PK")
    
    ctl[[dol_PK]] <- c(ctl[[dol_PK]][1:tv_definition_row],"",
                       paste0(tvparam," = ", tvparam,"COV*",tvparam),
                       ctl[[dol_PK]][(tv_definition_row+1):length(ctl[[dol_PK]])])
    
  }
  
  if(existing_param_rel){
    ctl[[dol_PK]] <- gsub(paste0(tvparam,"COV="),
                          paste0(tvparam,"COV=",tvparam,cov,"*"),ctl[[dol_PK]])
  }
  
  ## use state to get the relationship in there.
  if(!missing(additional_state_text)) {
    param_cov_text <- param_cov_text(param=tvparam,cov=cov,state = state,
                                     data = data,
                                     theta_n_start = theta_n_start,
                                     continuous = continuous,
                                     additional_state_text = additional_state_text)
  } else {
    param_cov_text <- param_cov_text(param=tvparam,cov=cov,state = state,
                                     data = data,
                                     theta_n_start = theta_n_start,
                                     continuous = continuous)
  }
  
  ctl[[dol_PK]] <- c(ctl[[dol_PK]][1],"",
                     definition_start_txt,
                     param_cov_text,
                     definition_end_txt,
                     ctl[[dol_PK]][-1])
  
  ## add thetas
  n_add_thetas <- attr(param_cov_text, "n")
  if(n_add_thetas > 0){
    
    if(missing(init)){
      init <- rep("0.0001", n_add_thetas)
      if(state == 3 | state == "power") {
        init <- rep(0.8, n_add_thetas)
      }
    }
    
    if(missing(lower)){
      lower <- rep(-1, n_add_thetas)
    }
    
    if(missing(upper)){
      upper <- rep(5, n_add_thetas)
    }
    
    
    if(any(lower > init)) stop("lower bound > initial estimate")
    if(any(upper < init)) stop("upper bound < initial estimate")
    
    if(n_add_thetas == 1) {
      theta_lines <- paste0("$THETA  (",lower,",",init,",",upper,") ; ",tvparam, cov, state)
    } else {
      theta_lines <- paste0("$THETA  (",lower,",",init,",",upper,") ; ",tvparam, cov, state,"_",seq_len(n_add_thetas))
    }
    ctl$THETA <- c(ctl$THETA,theta_lines)
  }
  
  m <- m %>% ctl_contents_simple(ctl)
  
}

#' @export
add_cov.nm_list <- Vectorize_nm_list(add_cov.nm_generic, SIMPLIFY = FALSE)

#' @export
remove_cov.nm_generic <- function(ctl, param, cov, state = 2, continuous = TRUE,
                                  time_varying, additional_state_text, id_var = "ID"){
  
  m <- ctl
  ctl <- ctl_contents(m)
  param <- as.character(param)
  cov <- as.character(cov)
  state <- as.character(state)
  continuous <- as.logical(continuous)
  
  if("PK" %in% names(ctl)) dol_PK <- "PK" else dol_PK <- "PRED"
  
  PK_section <- rem_comment(ctl[[dol_PK]])
  
  data <- suppressMessages(input_data(m, filter = TRUE))
  
  if(any(is.na(data[[cov]]))) warning("missing values in ",cov," detected")
  
  if(missing(time_varying)){
    max_levels <- max(tapply(data[[cov]], data[[id_var]], function(x) length(unique(x))), na.rm = TRUE)
    if(max_levels > 1) time_varying <- TRUE else time_varying <- FALSE
  }
  
  if(time_varying){
    tvparam <- param
  } else {
    tvparam <- paste0("TV",param)
  }
  
  existing_param_rel <- which(grepl(paste0("\\b",tvparam,"COV"), PK_section))
  existing_param_cov_rel <- which(grepl(paste0("\\b",tvparam,cov), PK_section))
  
  ## remove parm vs specific cov code
  match_start <- grep(paste0(";;; ",tvparam,cov,"-DEFINITION START"),ctl[[dol_PK]])
  match_end <- grep(paste0(";;; ",tvparam,cov,"-DEFINITION END"),ctl[[dol_PK]])
  if(length(match_start) == 0 | length(match_end) == 0){
    return(as_nm_generic(nm(NA)))
    #stop("can't find cov definition code - did you add with add_cov()?")
  }
  
  ctl_matched <- ctl[[dol_PK]][match_start:match_end]
  theta_match <- gregexpr("THETA\\([0-9]+\\)", ctl_matched)
  
  thetas <- lapply(seq_along(theta_match), function(i){
    matchi <- theta_match[[i]]
    ctl_matchedi <- ctl_matched[i]
    if(length(matchi) == 1)
      if(matchi %in% -1)
        return(NULL)
    sapply(seq_along(matchi), function(j){
      matchij <- matchi[j]
      len <- attr(matchi, "match.length")[j]
      return(substr(ctl_matchedi, matchij, matchij+len-1))
    })
  })
  thetas <- unlist(thetas)
  theta_n <- gsub("THETA\\(([0-9]+)\\)","\\1", thetas)
  theta_n <- sort(as.numeric(theta_n))
  reduce_thetas <- length(theta_n)
  if(reduce_thetas > 0){
    ctl_char <- ctl_character(ctl)
    
    next_theta <- max(theta_n)+1
    keep_going <- TRUE
    while(keep_going){
      next_text_to_match <- paste0("THETA\\(",next_theta,"\\)")
      next_text_to_replace <- paste0("THETA\\(",next_theta-reduce_thetas,"\\)")
      if(!any(grepl(next_text_to_match, ctl_char))){
        keep_going <- FALSE
      } else {
        ctl_char <- gsub(next_text_to_match, next_text_to_replace, ctl_char)
        next_theta <- next_theta + 1
      }
    }
    ctl <- ctl_list(ctl_char)
  }
  
  ctl[[dol_PK]] <- ctl[[dol_PK]][setdiff(seq_along(ctl[[dol_PK]]), match_start:match_end)]
  
  ## adjust/remove parm vs any cov code
  
  match_start <- grep(paste0(";;; ",tvparam,"-RELATION START"),ctl[[dol_PK]])
  match_end <- grep(paste0(";;; ",tvparam,"-RELATION END"),ctl[[dol_PK]])
  if(length(match_start) == 0 | length(match_end) == 0)
    stop("can't find cov relation code - did you add with add_cov()?")
  
  rel_section <- ctl[[dol_PK]][match_start:match_end]
  
  rel_index_all <- grep(paste0(tvparam,"COV=",tvparam,cov), rel_section)
  unique_rel_match <- grep(paste0(tvparam,"COV=",tvparam,cov,"$"), rel_section)
  if(length(unique_rel_match) > 1)
    stop("can't identify unique cov relation code line- did you add with add_cov()?")
  if(length(unique_rel_match) == 1){ ## only covariate on this param
    ctl[[dol_PK]] <- ctl[[dol_PK]][setdiff(seq_along(ctl[[dol_PK]]), match_start:match_end)]
    match_param_rel <- grep(paste0("\\b",tvparam, "COV\\b"), rem_comment(ctl[[dol_PK]]))
    if(length(match_param_rel) != 1)
      stop("can't identify parameter modification line- did you add with add_cov()?")
    ctl[[dol_PK]] <- ctl[[dol_PK]][setdiff(seq_along(ctl[[dol_PK]]), match_param_rel)]
  }
  if(length(unique_rel_match) == 0){ ## (maybe - test this) other covariates
    if(length(rel_index_all) > 1)
      stop("can't identify unique cov relation code line- did you add with add_cov()?")
    
    text_match_at_start <- paste0("^(",tvparam,"COV=)",tvparam,cov,"\\*(.+)")
    match_at_start <- grep(text_match_at_start,ctl[[dol_PK]])
    
    text_match_after_start <- paste0("^(",tvparam,"COV=.+)\\*",tvparam,cov,"(.*)")
    match_after_start <- grep(text_match_after_start,ctl[[dol_PK]])
    
    if(length(match_at_start) + length(match_after_start) != 1)
      stop("couldn't identify cov relation in relation code line- did you add with add_cov()?")
    
    if(length(match_at_start)){
      ctl[[dol_PK]] <- gsub(text_match_at_start,"\\1\\2",ctl[[dol_PK]])
    }
    
    if(length(match_after_start)){
      ctl[[dol_PK]] <- gsub(text_match_after_start,"\\1\\2",ctl[[dol_PK]])
    }
    
  }
  
  matched_theta <- grep(paste0("\\$THETA\\s.*;.*",tvparam, cov), ctl$THETA)
  #if(length(matched_theta) == 0)
  #  stop("can't find $THETA entry to remove- did you add with add_cov()?")
  
  ctl$THETA <- ctl$THETA[setdiff(seq_along(ctl$THETA), matched_theta)]
  
  m <- m %>% ctl_contents_simple(ctl)
}

#' @export
remove_cov.nm_list <- Vectorize_nm_list(remove_cov.nm_generic, SIMPLIFY = FALSE)


#' Produce dataset for covariate forest plotting
#'
#' @param m nm object
#' @param covariate_scenarios data.frame. need names "cov", "value" and (optional) "text"
#' 
#' @examples 
#' \dontrun{
#' 
#' dcov <- input_data(m1, filter = TRUE)
#' dcov <- dcov[!duplicated(dcov$ID), ]
#' covariate_scenarios <- bind_rows(
#'   tibble(cov = "HEALTHGP", value = c(0, 1)),
#'   tibble(cov = "HEPATIC", value = unique(dcov$HEPATIC[dcov$HEPATIC > -99])),
#'   tibble(cov = "BWTIMP", value = c(50, 80, 120)),
#'   tibble(cov = "ECOG", value = c(0,1,2,3)),
#'   tibble(cov = "BEGFRIMP", value = quantile(dcov$BEGFR[dcov$BEGFR > -99])),
#'   tibble(cov = "RACE", value = c(1,2),text=c("white","black")),
#'   tibble(cov = "PPI", value = c(0,1)),
#'   tibble(cov = "H2RA", value = c(0,1))
#' )
#' 
#' dplot <- cov_forest_data(m1, covariate_scenarios = covariate_scenarios)
#' cov_forest_plot(dplot)
#' 
#' }
#' 
#' @export
cov_forest_data <- function(m, covariate_scenarios){
  if(!is_finished(m)) wait_finish(m)
  # d <- nm_output(m) ## read in combined output
  
  ## assume m is nm_list
  m <- as_nm_generic(m)
  
  ###############
  ## include plotting code here
  dpar <- coef(m, trans = FALSE)
  dpar$name <- coef(m)$parameter
  
  dpar$SE[is.na(dpar$SE)] <- 0
  
  dpar$lower <- dpar$FINAL - 1.96*dpar$SE
  dpar$upper <- dpar$FINAL + 1.96*dpar$SE
  
  PK_text <- ctl_contents(m)$PK
  PK_text_R <- nonmem_code_to_r(PK_text)
  
  par_covs <- PK_text[grepl(";;; .*-DEFINITION START", PK_text)]
  par_covs <- gsub(";;; (.*)-.*", "\\1", par_covs)
  
  pars <- PK_text[grepl(";;; .*-RELATION START", PK_text)]
  pars <- gsub(";;; (.*)-.*", "\\1", pars)
  
  dd <- input_data(m, filter = TRUE)
  
  ## to be used later in evaluation of R expressions
  dpar_mid <- dpar$FINAL
  names(dpar_mid) <- dpar$parameter
  
  dpar_low <- dpar$lower
  names(dpar_low) <- dpar$parameter
  
  dpar_upp <- dpar$upper
  names(dpar_upp) <- dpar$parameter
  
  dd[is.na(dd)] <- 0
  dd_first <- dd[1,]
  
  dpar_df <- as.data.frame(as.list(dpar_mid))
  dd_first <- cbind(dd_first, dpar_df)
  d <- lapply(seq_along(par_covs), function(i){
    
    par_cov <- par_covs[i]
    
    potential_pars <- sapply(pars, function(par) grepl(par, par_cov))
    potential_pars <- pars[potential_pars]
    
    matches <- unlist(lapply(potential_pars, function(potential_par){
      grep(paste0(potential_par, "COV\\s*=\\s*.*", par_cov), PK_text_R)
    }))
    
    if(length(matches) != 1) stop("can't get param value for ", par_cov, call. = FALSE)
    
    PK_matched_row <- PK_text_R[matches]
    par <- gsub("^(.*)COV\\s*.*$", "\\1", PK_matched_row)
    
    #par <- sapply(pars, function(par) grepl(paste0("^", par), par_cov))
    #par <- pars[par]
    #if(length(par) != 1) stop("can't get param value for ", par_cov, call. = FALSE)
    
    cov <- gsub(paste0(par,"(.*)"), "\\1", par_cov)
    
    if(!any(grepl(paste0(par_cov, "\\s*\\="), PK_text_R)))
      stop("can't find TVPARCOV= rows")
    
    # ## use code before TVPARCOV lines to compute derived covariates
    # prior_indicies <- seq_len(min(grep(paste0(par_cov, "\\s*\\="), PK_text_R))-1)
    # prior_code <- PK_text_R[prior_indicies]
    # prior_code <- c(prior_code, cov)
    # prior_exprs <- parse(text = prior_code)
    
    
    #theta_lines <- PK_text_R[seq_len(max(grep(paste0(par_cov, "\\s*\\="), PK_text_R)))]
    theta_lines <- PK_text_R[grepl(paste0(par_cov, "\\s*\\="), PK_text_R)]
    theta_lines <- c(theta_lines, par_cov)
    exprs <- parse(text = theta_lines)
    
    
    ## redefine data covariates with 
    ## browser()
    dcov_sc <- covariate_scenarios[covariate_scenarios$cov %in% cov, ]
    if(nrow(dcov_sc) == 0)
      stop("couldn't find covariate ", cov, " in covariate scenarios")
    
    dcov_sc_simple <- dplyr::tibble(dcov_sc$value)
    names(dcov_sc_simple) <- cov
    
    dd_first[[cov]] <- NULL
    dd_first <- merge(dd_first, dcov_sc_simple)
    
    #dd_first[[cov]] <- with(dd_first, eval(prior_exprs))
    
    # cov_col <- sapply(seq_len(nrow(dd)), function(j) {
    #   with(dd[j,], eval(prior_exprs))
    # })     
    
    #dd[[cov]] <- cov_col
    #browser()
    #cov_col <- dd[[cov]]
    #cov_col[is.na(cov_col)] <- 0 #stats::na.omit(cov_col)
    
    # categorical <- TRUE
    # if(length(unique(dd[[cov]])) > 10) categorical <- FALSE  ## too many levels = FALSE
    # 
    # if(!all(stats::na.omit(floor(dd[[cov]]) == dd[[cov]]))) categorical <- FALSE  ## not round = FALSE
    
    # print(cov)
    # if(cov=="BWTIMP") browser()
    ### Need to change it to generate quantile based on original BWT
    # without accounting the imputed BWT to the median value
    
    # if(categorical) {
    #   levs <- unique(cov_col) 
    #   lev_text <- paste0(cov,"_",levs)
    # } else {
    #   levs <- stats::quantile(cov_col, probs = c(0.05, 0.5, 0.95))
    #   levs <- signif(levs, 2)
    #   lev_text <- paste0(cov,"_",c("low5","mid","upp95"),"_",levs)
    # }
    
    levs <- dd_first[[cov]]
    if(!"text" %in% names(covariate_scenarios)){
      lev_text <- paste0(cov, "_", levs) 
    } else {
      if("text" %in% names(dcov_sc)) lev_text <- paste0(cov, "_", levs) else
        lev_text <- dcov_sc$text
    }
    
    d <- dplyr::tibble(par, cov, levs, lev_text)
    
    d$mask_mid <- lapply(levs, function(lev){
      d <- dplyr::tibble(lev)
      names(d) <- cov
      cbind(d, as.data.frame(as.list(dpar_mid)))
    })
    
    d$mask_low <- lapply(levs, function(lev){
      d <- dplyr::tibble(lev)
      names(d) <- cov
      cbind(d, as.data.frame(as.list(dpar_low)))
    })
    
    d$mask_upp <- lapply(levs, function(lev){
      d <- dplyr::tibble(lev)
      names(d) <- cov
      cbind(d, as.data.frame(as.list(dpar_upp)))
    })
    
    d$mid <- sapply(seq_along(levs), function(i){
      with(d$mask_mid[[i]], eval(exprs))
    })
    
    d$low <- sapply(seq_along(levs), function(i){
      with(d$mask_low[[i]], eval(exprs))
    })
    
    d$upp <- sapply(seq_along(levs), function(i){
      with(d$mask_upp[[i]], eval(exprs))
    })
    
    return(d)
    
  })
  
  d <- dplyr::bind_rows(d)
  d
  
}

#' Plot covariate forest plots
#'
#' Uses `ggplot2` to take outputs from [cov_forest_data()]
#' and display a forest plot
#'
#' @param d data.frame from [cov_forest_data()]
#'
#' @return a `ggplot2` forest plot
#' @seealso [cov_forest_data()]
#' @examples 
#' \dontrun{
#' 
#' dcov <- input_data(m1, filter = TRUE)
#' dcov <- dcov[!duplicated(dcov$ID), ]
#' covariate_scenarios <- bind_rows(
#'   tibble(cov = "HEALTHGP", value = c(0, 1)),
#'   tibble(cov = "HEPATIC", value = unique(dcov$HEPATIC[dcov$HEPATIC > -99])),
#'   tibble(cov = "BWTIMP", value = c(50, 80, 120)),
#'   tibble(cov = "ECOG", value = c(0,1,2,3)),
#'   tibble(cov = "BEGFRIMP", value = quantile(dcov$BEGFR[dcov$BEGFR > -99])),
#'   tibble(cov = "RACE", value = c(1,2),text=c("white","black")),
#'   tibble(cov = "PPI", value = c(0,1)),
#'   tibble(cov = "H2RA", value = c(0,1))
#' )
#' 
#' dplot <- cov_forest_data(m1, covariate_scenarios = covariate_scenarios)
#' cov_forest_plot(dplot)
#' 
#' }
#' 
#' @export

cov_forest_plot <- function(d){
  requireNamespace("ggplot2")
  
  ggplot2::ggplot(d, ggplot2::aes_string(x = "mid", y = "lev_text")) + ggplot2::theme_bw() +
    ggplot2::geom_rect(ggplot2::aes(ymin = -Inf, ymax = Inf, xmin = 1-0.2, xmax = 1+0.2), 
                       colour = "grey100") +
    ggplot2::geom_point() +
    ggplot2::geom_errorbarh(ggplot2::aes_string(xmin = "low", xmax = "upp"), height = 0.1) + 
    ggplot2::geom_vline(xintercept = 1, color='black', linetype='dashed') +
    ggplot2::facet_grid(par~., scales = "free_y", space = "free") +
    ggplot2::scale_y_discrete("") +
    ggplot2::scale_x_continuous("effect size \n", breaks = seq(floor(min(d$low)), ceiling(max(d$upp)), 0.1))
}

#' Include NONMEM variables in output table
#'
#' Experimental function - designed to be used in situation where you don't want
#' to rerun NONMEM, but need a variable in the dataset that wasn't specified in
#' $TABLE
#'
#' @param output_table output from output_table()
#' @param r nm object
#' @param var character. name of variable to compute (needs to be defined in
#'   $PK/$PRED)
#'
#' @export
append_nonmem_var <- function(output_table, r, var){
  r <- as_nm_generic(r)
  do <- output_table#(r)
  
  dc <- coef(r, trans = FALSE)
  
  ctl <- ctl_contents(r)
  
  pk_dollar <- ifelse("PK" %in% names(ctl), "PK", "PRED")
  
  pk_block <- ctl[[pk_dollar]]
  
  pk_block <- nonmem_code_to_r(pk_block)
  
  pk_block_param <- parse(text = c(pk_block, var))
  
  wide_coef <- dc %>%
    dplyr::select(.data$parameter, .data$FINAL) %>%
    tidyr::spread(key = "parameter", value = "FINAL")
  
  #dos <- do[!duplicated(paste(do$ID, do[[cov]])), ]
  
  varcol <- try(with(wide_coef,
                     sapply(1:nrow(do), function(i){
                       with(do[i,], eval(pk_block_param))})), silent = TRUE)
  
  if(!inherits(varcol, "try-error")) do[[var]] <- varcol
  
  do
}


#' Plot relationship between a parameter and covariate
#' 
#' @param r nm object
#' @param param character. Name of parameter
#' @param cov character. Name of covariate
#' @param ... additional arguments passed to dplyr::mutate()
#' @param categorical logical (default = FALSE)
#' @param plot_tv logical
#' 
#' @details the mutate statement is to add variables not included in original $TABLE
#' 
#' @export
param_cov_diag <- function(r, param, cov, ..., categorical = FALSE, plot_tv = TRUE){
  
  requireNamespace("ggplot2")
  r <- as_nm_generic(r)
  
  tvparam <- paste0("TV",param)
  do <- output_table(r)
  
  ## want scatter of posthoc values
  ## + pred line from formula
  
  ## want pred values on there too
  
  dc <- coef(r, trans = FALSE)
  
  ctl <- ctl_contents(r)
  
  pk_dollar <- ifelse("PK" %in% names(ctl), "PK", "PRED")
  
  pk_block <- ctl[[pk_dollar]]
  
  pk_block <- nonmem_code_to_r(pk_block, eta_to_0 = FALSE)
  
  pk_block_param <- parse(text = c(pk_block, param))
  pk_block_tvparam <- parse(text = c(pk_block,tvparam))
  
  wide_coef <- dc %>% dplyr::select(.data$parameter, .data$FINAL) %>%
    tidyr::spread(key = "parameter", value = "FINAL")
  
  dos <- do[!duplicated(paste(do$ID, do[[cov]])), ]
  
  parcol <- try(with(wide_coef,
                     sapply(1:nrow(dos), function(i){
                       with(dos[i,], eval(pk_block_param))})), silent = TRUE)
  
  if(!inherits(parcol, "try-error")) dos[[param]] <- parcol
  
  tvcol <- try(with(wide_coef,
                    sapply(1:nrow(dos), function(i){
                      with(dos[i,], eval(pk_block_tvparam))})), silent = TRUE)
  
  if(!inherits(tvcol, "try-error")) dos[[tvparam]] <- tvcol else
    plot_tv <- FALSE
  
  if(categorical) dos[[cov]] <- factor(dos[[cov]])
  
  dos <- substitute(dos %>% dplyr::mutate(...)) %>% eval
  
  p <- ggplot2::ggplot(dos, ggplot2::aes_string(x = cov, y = param)) + ggplot2::theme_bw()
  if(!categorical){
    p <- p + ggplot2::geom_point()
    if(plot_tv) p <- p + ggplot2::geom_smooth(ggplot2::aes_string(y = tvparam), colour = "red", se = FALSE)
  } else {
    p <- p + ggplot2::geom_boxplot()
    p <- p + ggplot2::coord_flip()
    if(plot_tv) p <- p + ggplot2::geom_point(ggplot2::aes_string(y = tvparam), colour = "red", size = 2)
  }
  
  #if(!missing(facet)) p <- p + facet_wrap(facet)
  p
  
}

#' Plot correlation between two covariates
#' 
#' @param d dataset with covariates
#' @param cov vector of length 2 for covariate names
#' @param continuous logical vector of length 2 for whether cov is continuous or not
#' @param log_transform_plot should plot be log transformed or not
#' @param dcov_info option dataframe with covariate information
#' @param by character (default = "ID") variable to split over
#' 
#' @export
cov_cov_plot <- function(d,
                         cov,
                         continuous,
                         log_transform_plot = rep(FALSE, length(cov)),
                         dcov_info,
                         by = "ID"){
  
  cov1 <- cov[1]
  cov2 <- cov[2]
  
  if(!missing(dcov_info)){
    dcov <- rbind(
      dcov_info[dcov_info$cov %in% cov1, ],
      dcov_info[dcov_info$cov %in% cov2, ]
    )
  } else {
    continuous1 <- continuous[1]
    continuous2 <- continuous[2]
    log_transform_plot1 <- log_transform_plot[1]
    log_transform_plot2 <- log_transform_plot[2]
    dcov <- dplyr::tibble(cov = c(cov1, cov2),
                           continuous = c(continuous1, continuous2),
                           log_transform_plot = c(log_transform_plot1, log_transform_plot2))
  }
  
  important_row_contents <-
    do.call(paste, d[, c(by, dcov$cov)])
  
  dplot <- d[!duplicated(important_row_contents), ]
  
  #dplot <- d[, c(by, dcov$cov)] %>% unique()
  
  if(max(table(dplot[[by]])) > 1)
    warning("time varying cov detected, taking first only")
  
  dplot <- dplot[!duplicated(dplot[[by]]), ]
  
  if(all(dcov$continuous)){
    p <- ggplot2::ggplot(dplot, ggplot2::aes_string(x = cov1, y = cov2))
    p <- p + ggplot2::theme_bw()
    p <- p + ggplot2::geom_point()
    p <- p + ggplot2::geom_smooth(method = "lm")
    
    if(dcov$log_transform_plot[dcov$cov %in% cov1])
      p <- p + ggplot2::scale_x_log10()
    
    if(dcov$log_transform_plot[dcov$cov %in% cov2])
      p <- p + ggplot2::scale_y_log10()
  }
  
  if(xor(dcov$continuous[1], dcov$continuous[2])){
    cov1 <- dcov$cov[!dcov$continuous]
    cov2 <- dcov$cov[dcov$continuous]
    dplot[[cov1]] <- as.factor(dplot[[cov1]])
    p <- ggplot2::ggplot(dplot, ggplot2::aes_string(x = cov1, y = cov2))
    p <- p + ggplot2::theme_bw()
    p <- p + ggplot2::geom_boxplot()
    
    if(dcov$log_transform_plot[dcov$cov %in% cov2])
      p <- p + ggplot2::scale_y_log10()
  }
  
  if(all(!dcov$continuous)){
    p <- ggplot2::ggplot(dplot, ggplot2::aes_string(x = cov1))
    p <- p + ggplot2::theme_bw()
    p <- p + ggplot2::geom_bar()
    p <- p + ggplot2::facet_wrap(cov2)
  }
  
  p
  
}
