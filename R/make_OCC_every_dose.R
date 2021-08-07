#' Make an OCC column for NONMEM IOV use
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Creates and OCC column that increments in accordance to specified condition.
#' To be used in a `dplyr::mutate()` statement `dplyr::group_by()`'d by "ID".
#'
#' @param d A data.frame. NONMEM ready input dataset.
#' @param dose_trigger Logical expression for defining a dosing row.
#' @param new_OCC_trigger Logical expression for defining when OCC should
#'   increment.
#'
#' @examples
#' 
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#'
#' d <- input_data(m1)
#'
#' ## OCC increments on every dosing interval with more than 4 samples
#' d %>% make_OCC_every_dose(!is.na(AMT), any(!is.na(DV)))
#' 
#' @export
make_OCC_every_dose <- function(d, dose_trigger, new_OCC_trigger) {
  # Rule for when new occasion is happening
  # whenever we have a dose, if there is a sample after it and before next dose, that dose is considered a new OCC

  ## TODO: walk the ast of new_OCC_trigger
  ## pull out variables, evaluate them to create a mini d

  dorig <- d
  
  new_OCC_trigger <- rlang::enquo(new_OCC_trigger)
  dose_trigger <- rlang::enquo(dose_trigger)

  d <- d %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::mutate(DPERIOD = cumsum(!!dose_trigger)) %>%
    dplyr::group_by(.data$ID, .data$DPERIOD) %>%
    dplyr::mutate(new_OCC = !!new_OCC_trigger)

  ## select temporarly unique DPERIOD and HAS PK SAMPLE for each ID
  tmp <- d %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data$ID, .data$DPERIOD, .data$new_OCC)

  tmp <- tmp %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::mutate(OCC = cumsum(.data$new_OCC))

  d$ROW <- seq_len(nrow(d))
  d <- merge(d, tmp)
  d <- d[order(d$ROW), ]

  ## normalise to start at 1
  d$OCC <- d$OCC - min(d$OCC) + 1

  d[, c(names(dorig), "OCC")]
}
