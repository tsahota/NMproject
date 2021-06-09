#' Convert a NONMEM run to a simulation
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Replaces $EST with $SIM.
#'
#' @param m A nm object.
#' @param seed Numeric (default = `12345`). seed value to include in $SIM.
#' @param subpr Numeric (default = `1`). SUBPR value to include in $SIM.
#'
#' @details Will only change $EST/$SIM, therefore it will not be sufficient to
#'   change a categorical estimation control file to simulation. You will likely
#'   need to perform a `manual edit` for categorical data simulation.
#'
#' @examples
#'
#' \dontrun{
#'
#' m2s <- m2 %>%
#'   child(run_id = "m2s") %>%
#'   update_parameters(m2) %>%
#'   convert_to_simulation(subpr = 50) %>%
#'   run_nm()
#'
#' m2s %>% nm_render("Scripts/basic_vpc.Rmd")
#' m2s %>% nm_render("Scripts/basic_ppc.Rmd")
#' }
#'
#' @export
convert_to_simulation <- function(m, seed = 12345, subpr = 1) {
  UseMethod("convert_to_simulation")
}
#' @export
convert_to_simulation.nm_generic <- function(m, seed = 12345, subpr = 1) {
  ## comment out $EST
  old_target <- target(m)
  m <- m %>%
    target("$EST") %>%
    comment_out() %>%
    untarget()
  m <- m %>%
    target("$COV") %>%
    comment_out() %>%
    untarget()
  if (any(grepl("\\$SIM", ctl_contents(m)))) { ## if there is a $SIM
    m <- m %>% uncomment(pattern = "\\$SIM")
  } else { ## if there is NOT a $SIM
    m <- m %>% insert_dollar(dollar = "SIM", "$SIM (1234) ONLYSIM SUBPR=1", after_dollar = "SIGMA")
  }
  m <- m %>% gsub_ctl("(^.*\\$SIM.*)\\([0-9]+\\)(.*$)", paste0("\\1(", seed, ")\\2"))
  m <- m %>% gsub_ctl("(N?SUBPR.*\\=\\s*)[0-9]+", paste0("\\1", subpr))
  m <- m %>% target(old_target)
  m
}
#' @export
convert_to_simulation.nm_list <- Vectorize_nm_list(
  convert_to_simulation.nm_generic,
  SIMPLIFY = FALSE
)


#' PPC functions: process data from simulation and plot
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' @param r An nm object (a simulation run).
#' @param FUN Statistic function accepting a NONMEM dataset `data.frame` as an
#'   argument and returns `data.frame` with a column `"statistic"`.
#' @param ... Additional arguments for `FUN`.
#' @param pre_proc Function to apply to dataset prior to compute statistics.
#' @param max_mod_no Integer. Maximum model number to read (set low for debugging).
#' @param DV Character (default = `"DV"`).
#' @param statistic Character (default = `"statistic"`). Name of statistic column
#'   returned by FUN.
#' @param group,var1,var2 Grouping variables for plotting.
#'
#' @seealso [nm_render()]
#' @examples
#' \dontrun{
#'
#' idEXPstat <- function(d, ...) { ## example individual statistic function
#'   ## arg = nonmem dataset data.frame
#'   ## return data.frame with statistic column
#'   d %>%
#'     group_by(ID, ...) %>%
#'     filter(is.na(AMT)) %>%
#'     summarise(
#'       AUC = AUC(time = TIME, conc = DV),
#'       CMAX = max(DV, na.rm = TRUE),
#'       TMAX = TIME[which.max(DV)]
#'     ) %>%
#'     tidyr::gather(key = "exposure", value = "statistic", AUC:TMAX) %>%
#'     ungroup()
#' }
#'
#' EXPstat <- function(d, ...) { ## example summary statistic function
#'   ## arg = nonmem dataset data.frame
#'   ## return data.frame with statistic column
#'   d %>%
#'     idEXPstat(...) %>% ## reuse idEXPstat for individual stats
#'     ## summarise over study and any other variables (...)
#'     group_by(exposure, ...) %>%
#'     summarise(
#'       median = median(statistic, na.rm = TRUE),
#'       cv = 100 * sd(statistic, na.rm = TRUE) / mean(statistic, na.rm = TRUE)
#'     ) %>%
#'     tidyr::gather(key = "type", value = "statistic", median:cv)
#' }
#'
#' dppc <- m1s %>% ppc_data(EXPstat)
#'
#' dppc %>% ppc_whisker_plot()
#' dppc %>% ppc_forest_plot()
#' }
#' @rdname ppc
#' @export

ppc_data <- function(r, FUN, ..., pre_proc = identity, max_mod_no = NA, DV = "DV", statistic = "statistic") {
  if (!"..." %in% names(formals(FUN))) stop("FUN must have ... in arguments")
  if (length(names(formals(FUN))) < 2) stop("FUN must have at least two arguments (a data.frame and ...")
  if (length(unique(data_path(r))) > 1) stop("non-unique datasets")

  dorig <- input_data(r[1], filter = TRUE)
  dsims <- output_table(r) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(.data$INNONMEM)

  total_rows <- nrow(dorig)

  nsim <- nrow(dsims) / nrow(dorig)

  dsims$mod_no <- rep(1:nsim, each = total_rows)

  if (!is.na(max_mod_no)) {
    dsims <- dsims[dsims$mod_no <= max_mod_no, ]
    nsim <- nrow(dsims) / nrow(dorig)
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
  if (!inherits(stat_orig, "data.frame")) stop("FUN must return a data.frame", call. = FALSE)
  if (!statistic %in% names(stat_orig)) stop("statistic must be a column of FUN output", call. = FALSE)

  names(stat_orig)[names(stat_orig) %in% statistic] <- paste0(statistic, "_true")

  ## if ... are present
  ## useful for modifying data items before stat function is applied
  current_call <- as.list(match.call())
  supplied_args <- names(current_call[-1])
  total_args <- names(formals(ppc_data))
  dots_present <- any(!supplied_args %in% total_args)

  stat_sim <- dsims %>%
    dplyr::group_by(.data$mod_no) %>%
    pre_proc() %>%
    tidyr::nest() %>%
    dplyr::mutate(statistic = lapply(.data$data, FUN, ...)) %>%
    # dplyr::mutate(statistic = purrr::map(.data$data, FUN, ...)) %>%
    tidyr::unnest(statistic)

  stat_sim$data <- NULL

  merge(stat_sim, stat_orig)
}

#' @name ppc
#' @param d Output from [ppc_data()].
#' @export
ppc_whisker_plot <- function(d, group, var1, var2, statistic = "statistic") {
  requireNamespace("ggplot2")
  requireNamespace("rlang")

  do_facet_wrap <- xor(!missing(var1), !missing(var2))
  do_facet_grid <- !missing(var1) & !missing(var2)
  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)
  group <- rlang::enquo(group)
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y = statistic)) +
    ggplot2::aes(x = !!group) +
    ggplot2::theme_bw() +
    ggplot2::stat_summary(
      fun.ymin = function(x) stats::quantile(x, 0.025, na.rm = TRUE),
      fun.ymax = function(x) stats::quantile(x, 0.975, na.rm = TRUE),
      geom = "errorbar"
    ) +
    ggplot2::geom_point(ggplot2::aes_string(y = paste0(statistic, "_true")), colour = "red")

  if (do_facet_wrap) p <- p + ggplot2::facet_wrap(dplyr::vars(!!var1), scales = "free")
  if (do_facet_grid) p <- p + ggplot2::facet_grid(dplyr::vars(!!var1), dplyr::vars(!!var2), scales = "free")

  p
}

#' @name ppc
#' @export
ppc_histogram_plot <- function(d, var1, var2, statistic = "statistic") {
  requireNamespace("ggplot2")
  requireNamespace("rlang")

  do_facet_wrap <- xor(!missing(var1), !missing(var2))
  do_facet_grid <- !missing(var1) & !missing(var2)
  var1 <- rlang::enquo(var1)
  var2 <- rlang::enquo(var2)

  p <- ggplot2::ggplot(d, ggplot2::aes_string(x = statistic)) +
    ggplot2::theme_bw() +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(ggplot2::aes_string(xintercept = paste0(statistic, "_true")), colour = "red")

  if (do_facet_wrap) p <- p + ggplot2::facet_wrap(dplyr::vars(!!var1), scales = "free")
  if (do_facet_grid) p <- p + ggplot2::facet_grid(dplyr::vars(!!var1), dplyr::vars(!!var2), scales = "free")

  p
}
