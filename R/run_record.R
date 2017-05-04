#' Run record
#'
#' @export run_record0

run_record0 <- function(..., coef.func = coef_ext0){
  d <- plyr::ldply(list(...),coef.func)
  d$Unit[is.na(d$Unit)] <- ""
  d$SEUnit[is.na(d$SEUnit)] <- ""
  if("trans" %in% d$trans) d$trans[is.na(d$trans)] <- ""   ## optional item
  d <- dplyr::select(d,-EVALUATION, -EST.NO,-EST.NAME)
  d$Estimate <- NA
  d$Estimate[d$Parameter!="OBJ"] <- paste0(signif(d$FINAL[d$Parameter!="OBJ"],3)," (",signif(d$SE[d$Parameter!="OBJ"],3),d$SEUnit[d$Parameter!="OBJ"],")")
  d$Estimate[d$Parameter=="OBJ"] <- round(d$FINAL[d$Parameter=="OBJ"],3)
  d <- tidyr::spread(dplyr::select(d,-SE,-FINAL),file,Estimate)
  d <- dplyr::arrange(d,Type,Parameter)
  d$SEUnit <- NULL
  d
}
