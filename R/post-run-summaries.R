#' Run record
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Displays the transformed parameters of a completed or running model.
#' Normally used inside of a diagnostic template, but can be useful for quickly
#' seeing parameter estimates of several models.
#'
#' @param m An nm object.
#' @param trans Logical. If `TRUE` (default) will transform using control file
#'   $THETA/OMEGA conventions.
#'
#' @section THETA transformations using `trans = TRUE`:
#' 
#' Where FINAL is the reported estimate in the returned `tibble`, and RSE% is
#' the reported standard error (where applicable) and \eqn{\theta} and \eqn{se(\theta)}
#' are the NONMEM reported values of parameters and standard errors, respectively
#' 
#' \describe{
#'     \item{LOG or LOGODDS:}{
#'       \eqn{FINAL = exp(\theta), RSE% = 100*\sqrt(exp^(se(\theta)) - 1)}
#'     }
#'     \item{RATIO:}{
#'       \eqn{FINAL = \theta, RSE% = 100*se(\theta) / \theta}
#'     }
#'     \item{LOGIT:}{
#'       \eqn{FINAL = 100 * 1 / (1 + exp(-\theta))}
#'     }
#' }
#' 
#' @section OMEGA transformations using `trans = TRUE`:
#' 
#' Where FINAL is the reported estimate in the returned `tibble`, and RSE% is
#' the reported standard error (where applicable) and \eqn{\omega^2} and \eqn{se(\omega^2)}
#' are the NONMEM reported values of parameters and standard errors, respectively
#'   
#' \describe{
#'     \item{LOG:}{
#'       \eqn{FINAL = 100*\sqrt(exp^(\omega^2) - 1), RSE% = 100*(se(\omega^2)/\omega^2)/2}
#'     }
#'  }
#' 
#' @section SIGMA transformations using `trans = TRUE`:
#' 
#' Where FINAL is the reported estimate in the returned `tibble`, and RSE% is
#' the reported standard error (where applicable) and \eqn{\sigma^2} and \eqn{se(\sigma^2)}
#' are the NONMEM reported values of parameters and standard errors, respectively
#' 
#' \describe{
#'     \item{}{
#'       \eqn{FINAL = \sqrt \omega^2, RSE% = 100*se(\omega^2) / \omega^2}
#'     }
#' }
#'
#' @seealso [nm_render()]
#' @examples
#' \dontrun{
#'
#' rr(m1)
#'
#' ## compare m1 and m2
#'
#' rr(c(m1, m2))
#' }
#' @export
rr <- function(m, trans = TRUE) {
  UseMethod("rr")
}

#' @export
rr.nm_list <- function(m, trans = TRUE) {
  d <- coef(m, trans = trans)
  d <- do.call(rbind, d)
  if (nrow(d) == 0) {
    return(data.frame())
  }
  d$file <- NULL
  d$unit[is.na(d$unit)] <- ""
  d$SEunit[is.na(d$SEunit)] <- ""
  if ("trans" %in% d$trans) d$trans[is.na(d$trans)] <- "" ## optional item
  d <- d[, names(d)[!names(d) %in% c("EVALUATION", "EST.NO", "EST.NAME")]]
  d$Estimate <- NA
  d$Estimate[d$parameter != "OBJ"] <- paste0(signif(d$FINAL[d$parameter != "OBJ"], 3), " (", signif(d$SE[d$parameter != "OBJ"], 3), d$SEunit[d$parameter != "OBJ"], ")")
  d$Estimate[d$parameter == "OBJ"] <- round(d$FINAL[d$parameter == "OBJ"], 3)
  d <- d[, names(d)[!names(d) %in% c("SE", "FINAL")]]
  d <- reshape2::dcast(
    data = d,
    stats::as.formula(paste(
      paste(names(d)[!names(d) %in% c("run_name", "Estimate")], collapse = " + "),
      "~ run_name"
    )),
    value.var = "Estimate"
  )
  ## fix ordering of columns so it's same as m - dcast ruins it
  non_matches <- names(d)[!seq_along(names(d)) %in% match(unique_id(m), names(d))]
  matches <- unique_id(m[!is.na(m)])
  matches <- matches[matches %in% names(d)]

  d <- d[, c(non_matches, matches)]

  d <- d[order(d$type, d$parameter), ]
  d$SEunit <- NULL
  names(d) <- gsub("execute:", "", names(d))
  tmp <- sapply(d, is.factor)
  d[tmp] <- lapply(d[tmp], as.character)
  d
}

rr.nm_generic <- function(m, trans = TRUE) {
  rr(as_nm_list(m), trans = trans)
}

#' @rdname coef_widelong
#' @name coef_widelong
#' @title Extract parameter values
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Pulls parameters, standard errors, OFVs and condition numbers out of ext
#' files, applies transformations.  This function is useful when numeric values
#' are needed. `rr` is easier to read, however it returns characters.  A wide
#' and long format is available via two different functions.
#'
#' @param m An nm object.
#' @param trans Logical (default = `TRUE`). Transform parameters using comments
#'   in $THETA/$OMEGA/$SIGMA.
#'   
#' @section THETA transformations using `trans = TRUE`:
#' 
#' Where FINAL is the reported estimate in the returned `tibble`, and RSE% is
#' the reported standard error (where applicable) and \eqn{\theta} and \eqn{se(\theta)}
#' are the NONMEM reported values of parameters and standard errors, respectively
#' 
#' \describe{
#'     \item{LOG or LOGODDS:}{
#'       \eqn{FINAL = exp(\theta), RSE% = 100*\sqrt(exp^(se(\theta)) - 1)}
#'     }
#'     \item{RATIO:}{
#'       \eqn{FINAL = \theta, RSE% = 100*se(\theta) / \theta}
#'     }
#'     \item{LOGIT:}{
#'       \eqn{FINAL = 100 * 1 / (1 + exp(-\theta))}
#'     }
#' }
#' 
#' @section OMEGA transformations using `trans = TRUE`:
#' 
#' Where FINAL is the reported estimate in the returned `tibble`, and RSE% is
#' the reported standard error (where applicable) and \eqn{\omega^2} and \eqn{se(\omega^2)}
#' are the NONMEM reported values of parameters and standard errors, respectively
#'   
#' \describe{
#'     \item{LOG:}{
#'       \eqn{FINAL = 100*\sqrt(exp^(\omega^2) - 1), RSE% = 100*(se(\omega^2)/\omega^2)/2}
#'     }
#'  }
#' 
#' @section SIGMA transformations using `trans = TRUE`:
#' 
#' Where FINAL is the reported estimate in the returned `tibble`, and RSE% is
#' the reported standard error (where applicable) and \eqn{\sigma^2} and \eqn{se(\sigma^2)}
#' are the NONMEM reported values of parameters and standard errors, respectively
#' 
#' \describe{
#'     \item{}{
#'       \eqn{FINAL = \sqrt \omega^2, RSE% = 100*se(\omega^2) / \omega^2}
#'     }
#' }
#'
#' @return `data.frame` of extracted model parameter values. `coef_wide()`
#'   returns a `data.frame` in wide format. Vector valued objects `m`, will be
#'   stacked vertically with one row per run.  `coef_long()` returns a
#'   `data.frame` in long format. Vector valued objects `m`, will be stacked
#'   horizontally.
#'
#' @seealso [rr()]
#' @export
coef_wide <- function(m, trans = TRUE) {
  d <- coef(m, trans = trans)
  d <- lapply(seq_along(d), function(i) {
    d <- d[[i]]
    if (nrow(d) == 0) {
      return(d)
    }
    d$par_no <- seq_len(nrow(d))
    d$m_no <- i
    d
  })
  d <- do.call(rbind, d)
  if (nrow(d) == 0) {
    return(data.frame())
  }
  d$file <- NULL
  d$unit[is.na(d$unit)] <- ""
  d$SEunit[is.na(d$SEunit)] <- ""
  if ("trans" %in% d$trans) d$trans[is.na(d$trans)] <- "" ## optional item
  d <- d[, names(d)[!names(d) %in% c("EVALUATION", "EST.NO", "EST.NAME")]]

  d <- d[grepl("THETA|OMEGA|SIGMA", d$type), ]

  d <- d[order(paste(d$m_no, d$par_no, d$key)), ]
  d$m_no <- NULL
  # d$par_no <- NULL
  d$run_name <- gsub("execute:", "", d$run_name)

  tmp <- sapply(d, is.factor)
  d[tmp] <- lapply(d[tmp], as.character)

  d
}

#' @rdname coef_widelong
#' @export
coef_long <- function(m, trans = TRUE) {
  d <- coef(m, trans = trans)
  d <- lapply(seq_along(d), function(i) {
    d <- d[[i]]
    if (nrow(d) == 0) {
      return(d)
    }
    d$par_no <- seq_len(nrow(d))
    d$m_no <- i
    d
  })
  d <- do.call(rbind, d)
  if (nrow(d) == 0) {
    return(data.frame())
  }
  d$file <- NULL
  d$unit[is.na(d$unit)] <- ""
  d$SEunit[is.na(d$SEunit)] <- ""
  if ("trans" %in% d$trans) d$trans[is.na(d$trans)] <- "" ## optional item
  d <- d[, names(d)[!names(d) %in% c("EVALUATION", "EST.NO", "EST.NAME")]]

  d <- d[grepl("THETA|OMEGA|SIGMA", d$type), ]

  d <- d %>% tidyr::gather(key = "key", value = "estimate", .data$FINAL:.data$SE)

  d <- d[order(paste("m", d$m_no, "p", d$par_no, d$key)), ]
  d$m_no <- NULL
  # d$par_no <- NULL
  d$run_name <- gsub("execute:", "", d$run_name)

  tmp <- sapply(d, is.factor)
  d[tmp] <- lapply(d[tmp], as.character)

  d
}

#' @importFrom stats coef
#' @export
stats::coef

#' @export
coef.nm_generic <- function(object, trans = TRUE, ...) {
  if (!is_finished(object)) {
    return(invisible(data.frame()))
  }

  ext_file_path <- object %>% nm_output_path("ext")

  d <- coef_ext0(ext_file_path)
  if (nrow(d) == 0) {
    return(data.frame())
  }

  d$run_name <- gsub("execute\\.", "\\1", unique_id(object))
  if (!unique(d$is_final)) d$run_name <- paste0(d$run_name, "*")
  d$is_final <- NULL
  if (!trans) {
    return(d)
  }

  p <- param_info2(object)

  d0 <- d[, names(d)[!names(d) %in% "unit"]]
  d1 <- p[, c("name", "parameter", "unit", "trans")]

  d <- merge(d0, d1, all.x = TRUE, by = "parameter")
  d$name[is.na(d$name)] <- as.character(d$parameter)[is.na(d$name)]
  d$name <- factor(d$name, levels = d$name)
  d$trans_unit <- d$unit
  d$transSEunit <- d$SEunit
  ## transformations
  d$FINAL.TRANS <- d$FINAL
  d$SE.TRANS <- d$SE

  th <- d$type %in% "THETA"
  om <- d$type %in% "OMEGAVAR"
  sg <- d$type %in% "SIGMA"

  ## RATIO data
  d$SE.TRANS[d$trans %in% "RATIO" & th] <- 100 * d$SE[d$trans %in% "RATIO" & th] / d$FINAL[d$trans %in% "RATIO" & th]
  d$transSEunit[d$trans %in% "RATIO" & th] <- "%"
  ## LOG
  d$FINAL.TRANS[d$trans %in% c("LOG", "LOGODDS") & th] <- exp(d$FINAL[d$trans %in% c("LOG", "LOGODDS") & th])
  d$SE.TRANS[d$trans %in% c("LOG", "LOGODDS") & th] <- 100 * sqrt((exp(d$SE[d$trans %in% c("LOG", "LOGODDS") & th]^2) - 1))
  d$transSEunit[d$trans %in% c("LOG", "LOGODDS") & th] <- "%"
  ## LOGIT
  if ("LOGIT" %in% d$trans) {
    d$FINAL.TRANS[d$trans %in% "LOGIT" & th] <- 100 * 1 / (1 + exp(-d$FINAL[d$trans %in% "LOGIT" & th]))
    d$trans_unit[d$trans %in% "LOGIT" & th] <- "%"
    # delt <- lapply(which(d$trans %in% "LOGIT"),function(i){
    #   par <- c(logit=d$FINAL[i])
    #   separ <- c(logit=d$SE[i])
    #   car::deltaMethod(par,"1/(1+exp(-logit))",vcov.=separ^2)
    # })
    # delt <- do.call(rbind,delt)
    # d$SE.TRANS[d$trans %in% "LOGIT"] <- 100*delt$SE
  }
  ## OMEGA

  ## https://www.cognigen.com/nmusers/2008-February/0811.html
  ## delta method:
  ## FINAL = E[OM^2]
  ## SE = SE(OM^2)
  ## f = sqrt
  ## SE(OM) ~= SE(OM^2)/(2*sqrt(E[OM^2]))
  ## SE(OM) ~= SE/(2*sqrt(FINAL))
  ## E(OM) ~= sqrt(E[OM^2]) = sqrt(FINAL)
  ## RSE(OM) = SE(OM) / (2* E(OM))

  d$SE.TRANS[d$trans %in% "LOG" & om] <- 100 * (d$SE[d$trans %in% "LOG" & om] / d$FINAL[d$trans %in% "LOG" & om]) / 2
  d$FINAL.TRANS[d$trans %in% "LOG" & om] <- 100 * sqrt(exp(d$FINAL[d$trans %in% "LOG" & om]) - 1)
  d$trans_unit[d$trans %in% "LOG" & om] <- "CV%"
  d$transSEunit[d$trans %in% "LOG" & om] <- "%"
  ## COV
  d$trans[d$type %in% "OMEGACOV"] <- "COV" ## temp code
  # if("COV" %in% d$trans){
  #   omx <- gsub("^OMEGA\\.([0-9]+)\\.([0-9]+)\\.","\\1",d$parameter[d$trans %in% "COV"])
  #   omy <- gsub("^OMEGA\\.([0-9]+)\\.([0-9]+)\\.","\\2",d$parameter[d$trans %in% "COV"])
  #   omx <- paste0("OMEGA.",omx,".",omx,".")
  #   omy <- paste0("OMEGA.",omy,".",omy,".")
  #   sdx <- sqrt(d$FINAL[match(omx,d$parameter)])
  #   sdy <- sqrt(d$FINAL[match(omy,d$parameter)])
  #   d$FINAL.TRANS[d$trans %in% "COV"] <- d$FINAL[d$trans %in% "COV"]/(sdx*sdy)
  #   d$trans_unit[d$trans %in% "COV"] <- "CORR.COEF"
  #   ## COV[X,Y]/(SD[X]*SD[Y])
  #   ## know SE(COV[X,Y]) and SE[SDX^2] and SE[SDY^2]
  #   ## Need covariance matrix between these though - from .cov file.
  #   ## SQRT(VAR(COV[X,Y]/(SD[X]*SD[Y])))
  #   cov.file <- object$output$psn.cov
  #   dc <- utils::read.table(cov.file,skip=1,header = TRUE)
  #   for(i in seq_along(which(d$trans %in% "COV"))){
  #     ## loop through each COV variable and generate absolute SE
  #     names.c <- c(omx[i],omy[i],as.character(d$parameter[d$trans %in% "COV"][i]))
  #     names.c <- d$parameter[d$parameter %in% names.c] ## reorder
  #     names.c2 <- gsub("\\.([0-9]+)\\.([0-9]+)\\.","(\\1,\\2)",names.c)
  #
  #     ## same order as names.c - important
  #     vcov <- dc[match(names.c2,dc$NAME),as.character(names.c)]
  #     rownames(vcov) <- names(vcov)
  #     vcov <- as.matrix(vcov)
  #
  #     pmean <- d$FINAL[match(names.c,d$parameter)]  ## may as well recompute FINALs
  #     names(pmean) <- d$name[match(names.c,d$parameter)]
  #
  #     formula.i <- paste0(names.c[3],"/(sqrt(",names.c[1],")*sqrt(",names.c[2],"))")
  #     #tmp <- car::deltaMethod(pmean,formula.i,vcov.=vcov)
  #     #d$SE.TRANS[d$trans %in% "COV"][i] <- tmp$SE
  #   }
  #
  # }

  ## SIGMA
  d$SE.TRANS[d$type %in% "SIGMA"] <- 100 * (d$SE[d$type %in% "SIGMA"] / d$FINAL[d$type %in% "SIGMA"]) / 2
  d$FINAL.TRANS[d$type %in% "SIGMA"] <- sqrt(d$FINAL[d$type %in% "SIGMA"])
  d$trans_unit[d$type %in% "SIGMA"] <- "SD"
  d$transSEunit[d$type %in% "SIGMA"] <- "%"

  ## get names back to what they should be
  d$FINAL <- d$FINAL.TRANS
  d$FINAL.TRANS <- NULL
  d$SE <- d$SE.TRANS
  d$SE.TRANS <- NULL
  d$unit <- d$trans_unit
  d$trans_unit <- NULL
  d$SEunit <- d$transSEunit
  d$transSEunit <- NULL
  d$parameter <- d$name
  d$name <- NULL
  d
}

#' @export
coef.nm_list <- function(object, trans = TRUE, ...) {
  d <- lapply(object, coef, trans = trans)
  # do.call(rbind, d)
  d
}

#' Find an output file associated with a run
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This is primarily a backend function used to identify output file paths
#' associated with nm objects.
#'
#' @param m An nm object.
#' @param extn Character. Name of extension.
#' @param file_name Optional character. Name of file name.
#'
#' @return The path to the relevant output file of `m`.
#'
#' @examples
#' \dontrun{
#' m %>% nm_output_path("ext") ## path to ext file
#' }
#' @export
nm_output_path <- function(m, extn, file_name) {
  UseMethod("nm_output_path")
}

#' @export
nm_output_path.nm_generic <- function(m, extn, file_name) {
  lst_file <- lst_path(m)
  if (!missing(extn)) {
    current_extn <- tools::file_ext(lst_file)
    out_file <- gsub(paste0("\\.", current_extn, "$"), paste0(".", extn), lst_file)
  }
  if (!missing(file_name)) {
    out_file <- file.path(dirname(lst_file), file_name)
  }
  file.path(run_in(m), out_file)
}

#' @export
nm_output_path.nm_list <- Vectorize_nm_list(nm_output_path.nm_generic, SIMPLIFY = TRUE)

#' @export
summary.nm_list <- function(object, ref_model = NA, parameters = c("none", "new", "all"), keep_m = FALSE, ...) {
  d <- rr_row(object)
  d <- d %>% dplyr::select(
    .data$run_id,
    .data$m,
    .data$parent_run_id,
    .data$parent_run_in,
    .data$data_path
  )
  cat("reading outputs...")
  d$coef_obs <- coef(d$m) ## slowest step - crashes
  cat("done\n", append = TRUE)
  cat("summarising...")
  d$status <- status(d$m)

  n_parameters_fun <- function(coef) {
    if (!"type" %in% names(coef)) {
      return(NA)
    }
    coef <- coef[grepl("THETA|OMEGA|SIGMA", coef$type), ]
    nrow(coef)
  }

  # browser()
  #
  # f <- function(x) {
  #   ans <- list(x)
  #   return(ans)
  #   parent_run(x)
  # }
  #
  # tmp <- d %>% dplyr::group_by(.data$parent_run_id, .data$parent_run_in) %>%
  #   dplyr::mutate(
  #     parent = list(parent_run(.data$m[[1]]))
  #   )

  d <- d %>%
    dplyr::group_by(.data$parent_run_id, .data$parent_run_in) %>%
    dplyr::mutate(
      parent = list(parent_run(.data$m[[1]])),
      parent_coef_obs = coef.nm_list(.data$parent[1]),
      n_params = sapply(.data$coef_obs, n_parameters_fun),
      parent_n_params = n_parameters_fun(.data$parent_coef_obs[[1]])
    ) %>%
    dplyr::group_by(.data$data_path) %>% ## nobs reads data - only once per data_path
    dplyr::mutate(nobs = nobs(.data$m[1])) %>%
    dplyr::group_by(.data$parent_run_id, .data$parent_run_in) %>%
    dplyr::mutate(
      status = status(.data$m),
      ofv = ofv(.data$coef_obs),
      dofv = .data$ofv - ofv(.data$parent_coef_obs[[1]]),
      df = .data$n_params - .data$parent_n_params,
      p_chisq =
        ifelse(.data$df >= 0,
          1 - stats::pchisq(-.data$dofv, df = .data$df),
          1 - stats::pchisq(.data$dofv, df = -.data$df)
        ),
      AIC = .data$ofv + 2 * .data$n_params,
      BIC = .data$ofv + log(.data$nobs) * .data$n_params,
      ref_cn = cond_num(.data$parent_coef_obs[[1]]),
      cond_num = cond_num(.data$coef_obs)
    )
  d$coef_obs <- NULL
  d$parent_coef_obs <- NULL
  d$parent <- as_nm_list(d$parent)

  parameters <- match.arg(parameters)
  if (parameters != "none") {
    ## for each row, compute rr(d$m[i]) and rr(d$parent[i])
    ## remove nm_list classes - they screw up in dplyr

    ds <- split(d, seq_len(nrow(d)))

    ds <- lapply(ds, function(d) {

      # rri <- rr(c(d$parent,d$m), ...)
      # rri <- rri[grepl("THETA|OMEGA|SIGMA", rri$type), ]
      #
      # index <- !rri$unit %in% "" & !is.na(rri$unit)
      # rri$parameter[index] <-
      #   paste0(rri$parameter[index], " (", rri$unit[index], ")")
      #
      # if("trans" %in% names(rri)){
      #   index <- !rri$trans %in% "" & !is.na(rri$trans)
      #   rri$parameter[index] <-
      #     paste0(rri$parameter[index], " (", rri$trans[index],")")
      # }

      rri <- rr2(c(d$parent, d$m), ...)
      rri$type <- NULL
      rri$unit <- NULL
      rri$SEunit <- NULL
      rri$trans <- NULL
      rri$par_no <- NULL
      rri$key <- NULL

      if (ncol(rri) < 2) {
        return(d)
      }
      if (ncol(rri) == 2) {
        names(rri)[-1] <- c("m")
      }
      if (ncol(rri) == 3) {
        names(rri)[-1] <- c("parent", "m")
      }
      if (ncol(rri) > 3) browser() # stop("stop something wrong, debug")

      if (parameters == "new") {
        param_names <- rri$parameter[!grepl("se_", rri$parameter) &
          !is.na(rri$m)]

        parent_param_names <- rri$parameter[!grepl("se_", rri$parameter) &
          !is.na(rri$parent)]

        new_param_names <- param_names[!param_names %in% parent_param_names]

        se_param_names <- paste0("se_", new_param_names)

        rri <- rri[rri$parameter %in% c(new_param_names, se_param_names), ]
        if (nrow(rri) == 0) {
          return(d)
        }

        # rri$parameter[is.na(rri$parent)]
        #
        # rri <- rri[is.na(rri$parent), ]
        # rri <- rri[!is.na(rri$m), ]
      }

      if (inherits(try(t(rri$m)), "try-error")) browser()

      pars_to_add <- dplyr::as_tibble(t(rri$m))
      names(pars_to_add) <- rri$parameter

      dplyr::bind_cols(d, pars_to_add) # %>%
      # mutate(m = nm_list2list(m),
      #        parent = nm_list2list(parent))
    })

    ## nm_lists screw up in dplyr...
    ds <- lapply(ds, function(x) {
      x %>%
        dplyr::mutate(
          m = nm_list2list(.data$m),
          parent = nm_list2list(.data$parent)
        )
    })

    d <- suppressWarnings(dplyr::bind_rows(ds))
    d$m <- as_nm_list(d$m)
    d$parent <- as_nm_list(d$parent)
  }

  #############################
  ## remove columns that we dont want
  ##  Note: may need reinsert them if they ever are needed in reverse dependencies

  d <- d %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -.data$data_path,
      -.data$parent,
      -.data$parent_run_id,
      -.data$parent_run_in,
      -.data$parent_n_params,
      -.data$n_params
    )

  if (!keep_m) d$m <- NULL

  #############################


  cat("done", append = TRUE)
  d <- d %>% dplyr::ungroup()
  d
}

#' @export
summary.nm_generic <- function(object, ref_model = NA, parameters = c("none", "new", "all"), keep_m = FALSE, ...) {
  summary(object = as_nm_list(object), ref_model = ref_model, parameters = parameters, keep_m = keep_m, ...)
}

rr_row <- function(m) {
  d <- nm_row(m)
  d$m <- m
  d
}

#' @rdname nm_summary
#' @name nm_summary
#' @title Generate a summary of NONMEM results
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Get wide (or a long) `tibble` showing summary results.
#'
#' @param ... Arguments passed to [summary()], usually a vector of nm object +
#'   options.
#' @param include_fields Character vector of nm object fields to include as
#'   columns in the output. Default is empty.
#' @param parameters Character. Either `"none"` (default), `"new"`, or `"all"`
#'   indicating whether parameter values should be included in the summary
#'   `tibble`.  Specifying `"new"` means that only parameters that aren't in the
#'   `parent` run are included in outputs.  This is useful if wanting to know
#'   the value of an added parameter but not all the parameters (e.g. in a
#'   covariate analysis).
#' @param m Logical (default = `TRUE`). Should model object be included as the
#'   `m` column.
#' @param trans Logical (default = `TRUE`). Should parameters be transformed in
#'   accordance with $THETA/$OMEGA/$SIGMA comments.  This is only valid if
#'   `parameters` is `"new"` or `"all`.
#' @return A wide format `tibble` with run results.
#'   
#' @export
summary_wide <- function(..., include_fields = character(), parameters = c("none", "new", "all"), m = TRUE, trans = TRUE) {
  parameters <- match.arg(parameters)
  d <- summary(..., parameters = parameters, trans = trans)
  m_obj <- c(...)
  if (m) d$m <- m_obj
  for (field in include_fields) {
    d[[field]] <- get_simple_field(m_obj, !!field)
  }
  d
}

#' @rdname nm_summary
#' @return A long format `tibble` with run results coerced to `character` form.
#' @export
summary_long <- function(..., parameters = c("none", "new", "all")) {
  parameters <- match.arg(parameters)
  ds <- summary(..., parameters = parameters, keep_m = TRUE)
  m <- ds$m
  ds$m <- NULL
  d <- t(ds)
  dnames <- row.names(d)
  d <- dplyr::as_tibble(d)
  names(d) <- gsub("execute:", "", unique_id(m))
  d <- d %>% dplyr::mutate_all(trimws)
  dcol <- dplyr::tibble("field" = dnames)
  d <- dplyr::bind_cols(dcol, d)
  d
}

#' Plot $COV matrix
#'
#' Plots the correlation plot from the $COV NONMEM output.
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' @param r An nm object.
#' @param trans Logical (default = TRUE).  Applies the transformations specified
#'   in $THETA/$OMEGA/$SIGMA comments before plotting.
#'
#' @return A `ggplot2` object with parameter correlations.
#'
#' @export

covariance_plot <- function(r, trans = TRUE) {
  dc <- nm_output_path(r, extn = "cor") %>%
    nm_read_table(header = TRUE, skip = 1)

  names(dc)[1] <- "Var1"
  dc$Var1 <- names(dc)[-1]

  n_ests <- nrow(dc) / length(unique(dc$Var1))

  dc$EST.NO <- rep(1:n_ests, each = length(unique(dc$Var1)))
  dc <- dc %>% dplyr::filter(.data$EST.NO == max(.data$EST.NO))
  dc$EST.NO <- NULL

  dc <- dc %>% tidyr::gather(key = "Var2", value = "value", -.data$Var1)

  dc$Var1 <- factor(dc$Var1)
  dc$Var2 <- factor(dc$Var2)

  if (trans) {
    dp <- param_info(r)
    current_levels <- levels(dc$Var1)
    dl <- data.frame(cl = current_levels)
    dl$ORD <- 1:nrow(dl)
    dp <- dp[, c("name", "parameter")]
    names(dp)[2] <- "cl"
    dl <- merge(dp, dl, all = TRUE)
    dl$new_names <- dl$cl
    dl$new_names[!is.na(dl$name)] <- paste0(
      dl$name[!is.na(dl$name)], " (",
      dl$cl[!is.na(dl$name)], ")"
    )
    levels(dc$Var1) <- dl$new_names
    levels(dc$Var2) <- dl$new_names
  }

  dc <- dc %>% dplyr::filter(!.data$value %in% 0)
  dc <- dc %>% dplyr::filter(as.numeric(.data$Var1) > as.numeric(.data$Var2)) ## lower corner
  dc$label <- round(dc$value, 2)

  p <- ggplot2::ggplot(dc, ggplot2::aes_string(x = "Var1", y = "Var2", fill = "value")) +
    ggplot2::theme_bw() +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low = "blue", high = "red", mid = "white",
      midpoint = 0, limit = c(-1, 1), space = "Lab",
      name = "Correlation"
    ) +
    ggplot2::geom_text(ggplot2::aes_string(label = "label")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0))

  p
}

#' Get OMEGA matrix from run
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Obtain in matrix form the OMEGA matrix.  This is primarily to feed into other
#' packages such as `mrgsolve`.
#'
#' @param r An nm object.
#' 
#' @return A `matrix` object.
#'
#' @examples
#'
#' \dontrun{
#'
#' ## matrix of initial estimates
#' m1 %>% omega_matrix()
#'
#' ## matrix of final estimates
#' m1 %>%
#'   update_parameters() %>%
#'   omega_matrix()
#' }
#'
#' @export
omega_matrix <- function(r) {
  dc <- coef(r, trans = FALSE)
  dc <- dc[dc$type %in% c("OMEGAVAR", "OMEGACOV"), ]
  dc <- dc[, c("parameter", "FINAL")]
  dc$ROW <- as.numeric(gsub("OMEGA\\.([0-9]+)\\..*", "\\1", dc$parameter))
  dc$COL <- as.numeric(gsub("OMEGA\\.[0-9]+\\.([0-9]+).*", "\\1", dc$parameter))
  dc <- dc[order(dc$ROW, dc$COL), ]
  max_size <- max(c(dc$ROW, dc$COL))
  dc <- dc[, c("FINAL", "ROW", "COL")]
  dc_mirror <- dc
  dc_mirror$COLOLD <- dc_mirror$COL
  dc_mirror$ROWOLD <- dc_mirror$ROW
  dc_mirror$COL <- dc_mirror$ROWOLD
  dc_mirror$ROW <- dc_mirror$COLOLD
  dc_mirror$COLOLD <- NULL
  dc_mirror$ROWOLD <- NULL

  dc <- rbind(dc, dc_mirror)
  dc <- unique(dc)

  d_all <- expand.grid(ROW = 1:max_size, COL = 1:max_size)
  d_all <- merge(dc, d_all, all = TRUE)
  d_all$FINAL[is.na(d_all$FINAL)] <- 0
  d_all <- d_all[order(d_all$ROW, d_all$COL), ]

  matrix(d_all$FINAL, nrow = max_size)
}

omega_matrix <- Vectorize(omega_matrix, vectorize.args = list("r"), SIMPLIFY = FALSE)


ext2coef <- function(extout, file_name) {
  ## raw function to generate parameter table from ext.file.

  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("reshape2 needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  d <- extout
  if (nrow(d) == 0) {
    return(data.frame())
  }

  has_final_est <- "FINAL" %in% d$TYPE
  if (has_final_est) {
    cond_num <- d$THETA1[d$TYPE %in% "CONDNUM" & d$EST.NO %in% max(d$EST.NO)]
    d <- d[d$TYPE %in% c("FINAL", "SE"), ]
    d <- d[d$EST.NO %in% max(d$EST.NO), ]
  } else {
    cond_num <- numeric()
    d <- utils::tail(d, 1)
  }

  d <- d[, c(
    names(d)[grepl("THETA|SIGMA|OMEGA", names(d))],
    c("OBJ", "EST.NAME", "EST.NO", "EVALUATION", "TYPE")
  )]


  par.names <- names(d)[match("THETA1", names(d)):match("OBJ", names(d))]

  d <- reshape2::melt(
    data = d, variable.name = "parameter",
    measure.vars = par.names
  )
  if (!"parameter" %in% names(d)) stop("melt has failed - could be due to reshape being loaded. reshape can interfere with reshape2")


  d <- reshape2::dcast(
    data = d,
    stats::as.formula(paste(
      paste(names(d)[!names(d) %in% c("TYPE", "value")], collapse = " + "),
      "~ TYPE"
    )),
    value.var = "value"
  )

  ## messy hard coding - consider refactoring if need more than just eigenvalues
  if (has_final_est & length(cond_num) > 0) {
    dlast <- d[nrow(d), ]
    dlast$parameter <- "CONDNUM"
    dlast$FINAL <- cond_num
    dlast$SE <- 0

    d <- rbind(d, dlast)
  }

  if (!has_final_est) names(d)[names(d) %in% "ITER"] <- "FINAL"

  d <- d[order(d$EST.NO, decreasing = TRUE), ]
  d$file <- file_name

  is.diag.omega <- grepl("OMEGA.([0-9]+\\.)\\1", d$parameter)
  is.omega <- grepl("OMEGA.([0-9]+\\.)+", d$parameter)
  is.off.diag.omega <- is.omega & !is.diag.omega
  d <- d[!(is.off.diag.omega & d$FINAL == 0), ] ## get rid of off diag 0s
  is.diag.sigma <- grepl("SIGMA.([0-9]+\\.)\\1", d$parameter)
  is.sigma <- grepl("SIGMA.([0-9]+\\.)+", d$parameter)
  is.off.diag.sigma <- is.sigma & !is.diag.sigma
  d <- d[!(is.off.diag.sigma & d$FINAL == 0), ] ## get rid of off diag 0s


  is.diag.omega <- grepl("OMEGA.([0-9]+\\.)\\1", d$parameter) ## redefine
  is.omega <- grepl("OMEGA.([0-9]+\\.)+", d$parameter) ## redefine
  is.off.diag.omega <- is.omega & !is.diag.omega ## redefine
  is.diag.sigma <- grepl("SIGMA.([0-9]+\\.)\\1", d$parameter) ## redefine
  is.sigma <- grepl("SIGMA.([0-9]+\\.)+", d$parameter) ## redefine
  is.off.diag.sigma <- is.sigma & !is.diag.sigma ## redefine

  ## sort so that THETAs first, then diagonal OMEGAs, then off diag, then SIGMA, then OBJ

  par.char <- as.character(d$parameter)
  par.order <- c(
    sort(par.char[grepl("THETA", par.char)]),
    sort(par.char[is.diag.omega]),
    sort(par.char[is.off.diag.omega]),
    sort(par.char[grepl("SIGMA", par.char)]),
    "OBJ",
    sort(par.char[grepl("CONDNUM", par.char)])
  )
  if (!identical(sort(par.order), sort(as.character(d$parameter)))) stop("Bug in code. Debug.")
  d$parameter <- factor(d$parameter, levels = par.order)
  d$type <- NA
  d$type[grepl("THETA", par.char)] <- "THETA"
  d$type[is.diag.omega] <- "OMEGAVAR"
  d$type[is.off.diag.omega] <- "OMEGACOV"
  d$type[grepl("SIGMA", par.char)] <- "SIGMA"
  d$type[grepl("OBJ", par.char)] <- "OBJ"
  if (has_final_est) {
    d$type[grepl("CONDNUM", par.char)] <- "CONDNUM"
    d$type <- factor(d$type, levels = c("THETA", "OMEGAVAR", "OMEGACOV", "SIGMA", "OBJ", "CONDNUM"))
  } else {
    d$type <- factor(d$type, levels = c("THETA", "OMEGAVAR", "OMEGACOV", "SIGMA", "OBJ"))
  }
  d <- d[order(d$type), ]
  d$unit <- NA
  d$SEunit <- NA
  if (!"SE" %in% names(d)) {
    namesd <- names(d)
    d$SE <- NA
    final_pos <- grep("FINAL", namesd)
    d <- d[, c(namesd[1:final_pos], "SE", namesd[(final_pos + 1):length(namesd)])]
  }
  d$is_final <- has_final_est
  d
}

coef_ext0 <- function(ext.file) {
  ## raw function to generate parameter table from ext.file.
  extout <- read_ext0(ext.file = ext.file)
  ext2coef(extout, file_name = ext.file)
}

param_info2 <- function(m) {
  p_info <- dplyr::bind_rows(
    raw_init_theta(m),
    raw_init_omega(m),
    raw_init_sigma(m)
  )
  p_info[!is.na(p_info$parameter), ]
}

rr2 <- function(m, trans = TRUE) {
  d <- coef_long(m, trans = trans)

  if (nrow(d) == 0) {
    return(data.frame())
  }

  index <- !d$unit %in% "" & !is.na(d$unit)
  d$parameter[index] <-
    paste0(d$parameter[index], " (", d$unit[index], ")")

  if ("trans" %in% names(d)) {
    index <- !d$trans %in% "" & !is.na(d$trans)
    d$parameter[index] <-
      paste0(d$parameter[index], " (", d$trans[index], ")")
  }

  d$parameter[d$key %in% "SE"] <- paste0("se_", d$parameter[d$key %in% "SE"])
  d <- d %>%
    dplyr::group_by(.data$parameter) %>%
    dplyr::mutate(par_no = max(.data$par_no))

  m_names <- unique(d$run_name)
  d <- d %>% tidyr::spread(key = "run_name", value = "estimate")
  names1 <- names(d)[!names(d) %in% m_names]
  d <- d[, c(names1, m_names)]
  # d <- d[order(d$key, d$par_no), ]
  d <- d[order(d$par_no, d$key), ]
  row.names(d) <- NULL

  d
}
