#' Get Objective Function Value (OFV)
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Extracts OFV from .ext file.
#'
#' @param r An nm object.
#'
#' @examples
#' \dontrun{
#'
#' ofv(m1)
#' }
#' @export
ofv <- function(r) {
  UseMethod("ofv")
}

#' @export
ofv.default <- function(r) {
  if (is_single_na(r)) {
    return(NA)
  }
  stop("don't know how to handle type")
}

#' @export
ofv.data.frame <- function(r) {
  if (nrow(r) == 0) {
    return(NA)
  }
  r$FINAL[r$parameter %in% "OBJ"]
}

#' @export
ofv.list <- function(r) {
  sapply(r, ofv)
}

#' @export
ofv.nm_generic <- function(r) {
  if (!is_finished(r)) {
    return(NA_real_)
  }
  dc <- try(coef.nm_generic(r, trans = FALSE), silent = TRUE)
  if (inherits(dc, "try-error")) {
    return(NA_real_)
  }
  dc$FINAL[dc$parameter %in% "OBJ"]
}
#' @export
ofv.nm_list <- Vectorize_nm_list(ofv.nm_generic)

#' @importFrom stats AIC
#' @export
stats::AIC

#' @export
AIC.data.frame <- function(object, ..., k = 2) {
  if (is_single_na(object)) {
    return(NA)
  }
  params <- object
  params <- params[grepl("THETA|OMEGA|SIGMA", params$type), ]

  n_parameters <- nrow(params)
  ofv(object) + k * n_parameters
}

#' @export
AIC.nm_generic <- function(object, ..., k = 2) {
  if (!is_finished(object)) {
    return(NA_real_)
  }
  if (is_single_na(object)) {
    return(NA_real_)
  }
  params <- try(coef.nm_generic(object), silent = TRUE)
  if (inherits(params, "try-error")) {
    return(NA_real_)
  }
  params <- params[grepl("THETA|OMEGA|SIGMA", params$type), ]

  n_parameters <- nrow(params)
  ofv(object) + k * n_parameters
}
#' @export
AIC.nm_list <- Vectorize_nm_list(AIC.nm_generic)

#' @importFrom stats nobs
#' @export
stats::nobs

#' @export
nobs.nm_generic <- function(object, ...) {
  if (is_single_na(object)) {
    return(NA)
  }
  suppressMessages(
    d <- input_data(object, filter = TRUE)
  )
  if ("AMT" %in% names(d)) {
    d <- d %>% dplyr::filter(is.na(.data$AMT))
  }
  if ("EVID" %in% names(d)) {
    d <- d %>% dplyr::filter(.data$EVID %in% 0)
  }
  if ("MDV" %in% names(d)) {
    d <- d %>% dplyr::filter(!.data$MDV %in% 1)
  }
  nrow(d)
}
#' @export
nobs.nm_list <- Vectorize_nm_list(nobs.nm_generic)

#' @importFrom stats BIC
#' @export
stats::BIC

#' @export
BIC.nm_generic <- function(object, ...) {
  AIC(object, ..., k = log(nobs.nm_generic(object)))
}
#' @export
BIC.nm_list <- Vectorize_nm_list(BIC.nm_generic)

#' Condition number of run
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Extracts condition number from .ext file.
#'
#' @param r An nm object.
#'
#' @seealso [ofv()], [rr()]
#'
#' @examples
#'
#' \dontrun{
#'
#' is_finished(m1) ## should be TRUE
#' cond_num(m1) ## displays condition number
#' }
#'
#' @export
cond_num <- function(r) {
  UseMethod("cond_num")
}

#' @export
cond_num.default <- function(r) {
  if (is_single_na(r)) {
    return(as.numeric(NA))
  }
  if (is.data.frame(r)) {
    dc <- r
    ans <- as.numeric(dc$FINAL[dc$parameter %in% "CONDNUM"])
    if (length(ans) == 0) ans <- as.numeric(NA)
    return(ans)
  }
  stop("don't know how to get cond_num of this")
}

#' @export
cond_num.list <- function(r) {
  sapply(r, cond_num)
}

#' @export
cond_num.nm_generic <- function(r) {
  dc <- try(coef(r, trans = FALSE), silent = TRUE)
  if (inherits(dc, "try-error")) {
    return(as.numeric(NA))
  }
  cond_num(dc)
}

#' @export
cond_num.nm_list <- function(r) {
  cond_nums <- lapply(r, cond_num.nm_generic)
  names(cond_nums) <- NULL
  unlist(cond_nums)
}
