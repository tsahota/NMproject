#' Experimental function to make an OCC column for NONMEM IOV use
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
#' \dontrun{
#' 
#' d <- input_data(m1)
#' 
#' ## OCC increments on every dosing interval with more than 4 samples
#' d %>% group_by(ID) %>%
#'   mutate(OCC = make_OCC_every_dose(
#'     EVID %in% 1, 
#'     length(which(EVID %in% 0)) > 4
#'    )
#'   )
#' 
#' }
#' @export
make_OCC_every_dose <- function(d, dose_trigger, new_OCC_trigger){
  # Rule for when new occasion is happening
  # whenever we have a dose, if there is a sample after it and before next dose, that dose is considered a new OCC
  
  ## TODO: walk the ast of new_OCC_trigger
  ## pull out variables, evaluate them to create a mini d
  
  new_OCC_trigger <- rlang::enquo(new_OCC_trigger)
  id_group <- rlang::enquo(id_group)
  dose_trigger <- rlang::enquo(dose_trigger)
  
  d <- d %>% dplyr::group_by(!!id_group) %>%
    dplyr::mutate(DPERIOD = cumsum(!!dose_trigger)) %>%
    dplyr::group_by(!!id_group, .data$DPERIOD) %>%
    dplyr::mutate(new_OCC = !!new_OCC_trigger)
  
  ## select temporarly unique DPERIOD and HAS PK SAMPLE for each ID
  tmp <- d %>%
    dplyr::ungroup() %>%
    dplyr::distinct(!!id_group, .data$DPERIOD,.data$new_OCC)
  
  tmp <- tmp %>%
    dplyr::group_by(!!id_group) %>%
    dplyr::mutate(OCC = cumsum(.data$new_OCC))
  
  d$ROW <- seq_len(nrow(d))
  d <- merge(d,tmp)
  d <- d[order(d$ROW), ]
  
  ## normalise to start at 1
  d$OCC <- d$OCC - min(d$OCC) + 1
  
  d$OCC
}
