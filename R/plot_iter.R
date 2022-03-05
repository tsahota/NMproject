#' Get data for iterations vs parameters/OBJ plots
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Used in shiny app and as a backend function.
#'
#' @param r An nm object.
#' @param trans Logical (default = `TRUE`). Should parameter transformations be
#'   performed in accordance with $THETA/$OMEGA/$SIGMA comments
#' @param skip Numeric (default = 0). The number of iterations to skip when
#'   plotting.  For observing stationarity it is often useful to remove the
#'   beginning iterations where the OFV and parameters may move a lot.
#' @param yvar Character (default = `"OBJ"`). Name of variable/parameter to
#'   display.
#'
#' @return Returns `data.frame` suitable for use in [plot_iter_ggplot()].
#'
#' @seealso [plot_iter_ggplot()]
#'
#' @keywords internal
#'
#' @export

plot_iter_data <- function(r, trans = TRUE, skip = 0, yvar = "OBJ") {
  UseMethod("plot_iter_data")
}

#' @export
plot_iter_data.default <- function(r, trans = TRUE, skip = 0, yvar = "OBJ") {

  ext_file_path <- r %>% nm_output_path("ext")
  if (!file.exists(ext_file_path)) usethis::ui_stop("psn.ext file exist (yet)")
  
  d <- read_ext0(ext_file_path)

  d <- d[d$TYPE %in% c("ITER", "BURN"), ]
  d$EST.NAME2 <- paste("$EST", d$EST.NO, ":", d$EST.NAME, sep = " ")

  ####################################
  ## find "estimation" est.no
  ## will this work with FOCE evaluation?
  if (length(unique(d$EST.NO)) > 1) {
    est.no <- max(unique(d$EST.NO[!grepl("Eval", d$EST.NAME)]))
  } else {
    est.no <- max(unique(d$EST.NO))
  }
  ####################################
  d <- d[d$EST.NO %in% est.no, ]

  par.names <- names(d)[2:(match("OBJ", names(d)) - 1)]
  ## remove fixed parameters
  for (i in par.names) if (length(unique(d[, i])) == 1) d[, i] <- NULL

  ## TODO: would like to remove all duplicated IOV ETAs

  ## move up the NBURN to immediately before NITER
  d <- by(d, d$EST.NO, function(d) {
    d <- d[d$ITERATION >= min(d$ITERATION) + skip, ] ## skip first few iterations on non-evaluation methods.

    if (!"BURN" %in% d$TYPE | length(which(d$TYPE %in% "BURN")) == 1) {
      return(d)
    }
    max.burn <- sort(d$ITERATION[d$TYPE %in% "BURN"], decreasing = TRUE)[1]
    max.burn2 <- sort(d$ITERATION[d$TYPE %in% "BURN"], decreasing = TRUE)[2]
    burn.interval <- max.burn - max.burn2
    d$ITERATION[d$TYPE %in% "BURN"] <- d$ITERATION[d$TYPE %in% "BURN"] - max.burn - burn.interval
    d
  })
  d <- do.call(rbind, d)

  par.names <- c("OBJ", names(d)[names(d) %in% par.names])
  
  d <- d %>% tidyr::pivot_longer(par.names, names_to = "variable", values_to = "value")
  #d <- tidyr::gather(d, key = "variable", value = "value", par.names)
  d$variable <- factor(d$variable, levels = par.names)
  
  if (trans) {
    p_info <- param_info2(r)
    p_info <- p_info[, c("name", "parameter", "trans", "unit")]
    p_info$variable <- p_info$parameter
    p_info$parameter <- NULL
    p_info$type <- NA
    p_info$type[grepl("THETA[0-9]+", p_info$variable)] <- "THETA"
    p_info$type[grepl("OMEGA", p_info$variable)] <- "OMEGAVAR"
    p_info$type[grepl("SIGMA", p_info$variable)] <- "SIGMA"
    
    ## similar to coef
    p_info$unit[p_info$trans %in% "LOGIT"] <- "%"
    p_info$unit[p_info$trans %in% "LOG" & p_info$type %in% "OMEGAVAR"] <- "CV%"
    p_info$unit[p_info$type %in% "SIGMA"] <- "SD"
    
    p_info$name <- paste0(p_info$name, " [", p_info$unit, "]")
    
    d <- dplyr::as_tibble(dplyr::left_join(d, p_info))
    d$orig_name <- d$variable
    d$variable[!is.na(d$name)] <- d$name[!is.na(d$name)] 
    
    par_names <- c("OBJ", p_info$name)
    
    d$variable <- factor(d$variable, levels = par_names)
    
    d <- d %>% dplyr::arrange(.data$EST.NO, .data$variable, .data$ITERATION)

    ## similar code to coef
    th <- d$type %in% "THETA"
    om <- d$type %in% "OMEGAVAR"
    sg <- d$type %in% "SIGMA"
    
    ## LOG
    d$value[d$trans %in% "LOG" & th] <- exp(d$value[d$trans %in% "LOG" & th])
    ## LOGIT
    d$value[d$trans %in% "LOGIT" & th] <- 100 * 1 / (1 + exp(-d$value[d$trans %in% "LOGIT" & th]))
    ## OMEGA LOG
    d$value[d$trans %in% "LOG" & om] <- 100 * sqrt(exp(d$value[d$trans %in% "LOG" & om]) - 1)
    ## SIGMA
    d$value[d$type %in% "SIGMA"] <- sqrt(d$value[d$type %in% "SIGMA"])
    
  }
  
  d
}

#' @export
plot_iter_data.nm_list <- function(r, trans = TRUE, skip = 0, yvar = "OBJ") {
  if (length(r) > 1) stop("currently can't do multiple plots at the same time", call. = FALSE)
  plot_iter_data(as_nm_generic(r), trans = trans, skip = skip, yvar = yvar)
}

#' Plot iterations vs parameters/OBJ (ggplot2)
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Used in shiny app and as a backend function.
#'
#' @param d A `data.frame`. Output from [plot_iter_data()].
#' 
#' @return A `ggplot2` object.
#'
#' @seealso [plot_iter_data()]
#' @keywords internal
#' @export

plot_iter_ggplot <- function(d) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  p <- ggplot2::ggplot(d, ggplot2::aes_string(x = "ITERATION", y = "value"))
  if (length(unique(d$TYPE)) == 1) {
    p <- p + ggplot2::geom_line()
  } else {
    p <- p + ggplot2::geom_line(ggplot2::aes_string(colour = "TYPE"))
  }
  p <- p + ggplot2::facet_wrap(~variable, scale = "free")
  p <- p + ggplot2::theme(legend.position = "none")
  p <- p + ggplot2::ggtitle(unique(d$EST.NAME))
  p <- p + ggplot2::theme_bw()
  p
}

#' Plot iterations vs parameters/OBJ
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Non interactive `ggplot2` based version of the OFV/parameter vs iteration
#' plot in shiny run monitor [shiny_nm()].  Used mainly for inclusion in
#' diagnostic reports.
#'
#' @param r An nm object.
#' @param trans Logical (default = `TRUE`). Should parameter transformations be
#'   performed in accordance with $THETA/$OMEGA/$SIGMA comments
#' @param skip Numeric (default = 0). The number of iterations to skip when
#'   plotting.  For observing stationarity it is often useful to remove the
#'   beginning iterations where the OFV and parameters may move a lot.
#' @param yvar Character (default = `"OBJ"`). Name of variable/parameter to
#'   display.
#'
#' @return A `ggplot2` object.
#'
#' @seealso [shiny_nm()], [nm_render()]
#'
#' @export
plot_iter <- function(r, trans = TRUE, skip = 0, yvar = "OBJ") {
  d <- plot_iter_data(r = r, trans = trans, skip = skip, yvar = yvar)
  plot_iter_ggplot(d)
}
