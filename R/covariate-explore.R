
#' Produce dataset for covariate forest plotting
#'
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' The main workhorse for computing uncertainty quantiles of covariate effects
#' in different subpopulations.
#' 
#' @param m An nm object.
#' @param covariate_scenarios A `data.frame`. Need columns `cov`, `value`
#'   and (optional) `text`.  See details for more information.
#'   
#' @details The column `cov` in `covariate_scenarios` refers to covariate
#'   variables in the dataset. The column `value` refers to covariate values of
#'   importance.  Typically these will be quantiles of continuous variables and
#'   categories (for categorical covariates). The column `text` is option but is
#'   a labelling column for [cov_forest_plot()] to adjust how the covariate
#'   scenarios are printed on the axis
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
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#'
#' Uses `ggplot2` to take outputs from [cov_forest_data()]
#' and display a forest plot.
#'
#' @param d A `data.frame` from [cov_forest_data()].
#'
#' @return A `ggplot2` forest plot.
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
#' @description 
#' 
#' `r lifecycle::badge("experimental")`
#'
#' This is designed to be used in situations where you don't want to rerun
#' NONMEM, but need a variable defined in the control file.  This will parse the
#' `$PK`/`$PRED`` and compute it as an additional row in R.  Safest way is to
#' just rerun the model with the variable in $TABLE, but this is for those who
#' are too time constrained.  It is advisable to QC the output.
#'
#' @param output_table Output from [output_table()].
#' @param r An nm object.
#' @param var Character. Name of variable to extract (needs to be defined in
#'   $PK/$PRED).
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
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Plots posthoc parameter-covariate relationships from NONMEM run.
#' 
#' @param r An nm object.
#' @param param Character. Name of parameter.
#' @param cov Character. Name of covariate.
#' @param ... Additional arguments passed to [dplyr::mutate()].
#' @param categorical Logical (default = `FALSE`).
#' @param plot_tv Logical.
#'
#' @details The mutate statement is to add variables not included in original
#'   $TABLE.
#'   
#' @return A `ggplot2` plot object.
#'   
#' @export
param_cov_diag <- function(r, param, cov, ..., categorical = FALSE, plot_tv = TRUE){
  
  if(!requireNamespace("ggplot2")) stop("install ggplot2")
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
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Useful for exploratory plots.
#' 
#' @param d Dataset with covariates.
#' @param cov Vector of length 2 for covariate names.
#' @param continuous Logical vector of length 2 for whether cov is continuous or
#'   not.
#' @param log_transform_plot Should plot be log transformed or not.
#' @param dcov_info Optional `data.frame` with covariate information.
#' @param by Character (default = `"ID"`) variable to split over.
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
