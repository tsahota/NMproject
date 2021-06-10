#' Functions for summarising covariates
#' 
#' 
#' @name covariate-summaries

NULL

#' @describeIn covariates-summaries reduce to one row per ID will all static
#'   covariates
#' @examples 
#' \dontrun{
#' d %>% get_static_data %>%
#'   group_by(STUDYTXT) %>% 
#'   summarise(BWT_kg = summarise_continuous(BWT),
#'             percentageFemale = summarise_binary(SEX),
#'             RACE = summarise_categorical(RACETXT))
#' }
#' @export

get_static_data <- function(data,by=c("ID")){
  static_covs <- data %>% 
    dplyr::group_by_(by) %>% 
    dplyr::summarise_all(function(i) length(unique(i))) %>%
    dplyr::select_if(function(x) all(x == 1)) %>% names
  dstat <- dplyr::select(d,c(by,static_covs)) %>% unique()
  dstat <- dplyr::ungroup(dstat)
  dstat
}

#' @describeIn covariate-summaries summarise continuous covariates
#' @param x covariate vector/column
#' @export

summarise_continuous <- function(x){
  q <- signif(quantile(x,probs=c(0.05,0.5,0.95),na.rm=TRUE),2)
  paste0(q[2]," (",q[1],"-",q[3],")")
}

#' @describeIn covariate-summaries summarise binary covariates
#' @export

summarise_binary <- function(x){
  p <- length(which(x %in% 1))/length(x)
  paste0(100*signif(p,4),"%")
}

#' @describeIn covariate-summaries summarise categorical covariates
#' @export

summarise_categorical <- function(x){
  tab <- table(x)
  sum(tab)
  percs <- 100*signif(tab/sum(tab),3)
  paste0(paste0(names(tab),":",percs,"%"),collapse=", ")
}
