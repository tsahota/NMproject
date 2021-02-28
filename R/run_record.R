ext2coef <- function(extout,file_name){
  ## raw function to generate parameter table from ext.file.

  if(!requireNamespace("reshape2", quietly = TRUE))
    stop("reshape2 needed for this function to work. Please install it.",
         call. = FALSE)

  d <- extout
  if(nrow(d) == 0) return(data.frame())

  has_final_est <- "FINAL" %in% d$TYPE
  if(has_final_est){
    cond_num <- d$THETA1[d$TYPE %in% "CONDNUM" & d$EST.NO %in% max(d$EST.NO)]
    d <- d[d$TYPE %in% c("FINAL","SE"),]
    d <- d[d$EST.NO %in% max(d$EST.NO), ]
  } else {
    cond_num <- numeric()
    d <- utils::tail(d,1)
  }

  d <- d[,c(names(d)[grepl("THETA|SIGMA|OMEGA",names(d))],
            c("OBJ","EST.NAME","EST.NO","EVALUATION","TYPE"))]


  par.names <- names(d)[match("THETA1",names(d)):match("OBJ",names(d))]

  d <- reshape2::melt(data = d, variable.name = "parameter",
                      measure.vars = par.names)
  if(!"parameter" %in% names(d)) stop("melt has failed - could be due to reshape being loaded. reshape can interfere with reshape2")


  d <- reshape2::dcast(data = d,
                       stats::as.formula(paste(paste(names(d)[!names(d) %in% c("TYPE","value")],collapse=" + "),
                                               "~ TYPE")),
                       value.var = "value")

  ## messy hard coding - consider refactoring if need more than just eigenvalues
  if(has_final_est & length(cond_num) > 0){
    dlast <- d[nrow(d),]
    dlast$parameter <- "CONDNUM"
    dlast$FINAL <- cond_num
    dlast$SE <- 0

    d <- rbind(d,dlast)
  }

  if(!has_final_est) names(d)[names(d) %in% "ITER"] <- "FINAL"

  d <- d[order(d$EST.NO,decreasing = TRUE),]
  d$file <- file_name

  is.diag.omega <- grepl("OMEGA.([0-9]+\\.)\\1",d$parameter)
  is.omega <- grepl("OMEGA.([0-9]+\\.)+",d$parameter)
  is.off.diag.omega <- is.omega & !is.diag.omega
  d <- d[!(is.off.diag.omega & d$FINAL == 0), ] ## get rid of off diag 0s
  is.diag.sigma <- grepl("SIGMA.([0-9]+\\.)\\1",d$parameter)
  is.sigma <- grepl("SIGMA.([0-9]+\\.)+",d$parameter)
  is.off.diag.sigma <- is.sigma & !is.diag.sigma
  d <- d[!(is.off.diag.sigma & d$FINAL == 0), ] ## get rid of off diag 0s


  is.diag.omega <- grepl("OMEGA.([0-9]+\\.)\\1",d$parameter) ## redefine
  is.omega <- grepl("OMEGA.([0-9]+\\.)+",d$parameter) ## redefine
  is.off.diag.omega <- is.omega & !is.diag.omega  ## redefine
  is.diag.sigma <- grepl("SIGMA.([0-9]+\\.)\\1",d$parameter) ## redefine
  is.sigma <- grepl("SIGMA.([0-9]+\\.)+",d$parameter) ## redefine
  is.off.diag.sigma <- is.sigma & !is.diag.sigma ## redefine

  ## sort so that THETAs first, then diagonal OMEGAs, then off diag, then SIGMA, then OBJ

  par.char <- as.character(d$parameter)
  par.order <- c(sort(par.char[grepl("THETA",par.char)]),
                 sort(par.char[is.diag.omega]),
                 sort(par.char[is.off.diag.omega]),
                 sort(par.char[grepl("SIGMA",par.char)]),
                 "OBJ",
                 sort(par.char[grepl("CONDNUM",par.char)]))
  if(!identical(sort(par.order),sort(as.character(d$parameter)))) stop("Bug in code. Debug.")
  d$parameter <- factor(d$parameter,levels=par.order)
  d$type <- NA
  d$type[grepl("THETA",par.char)] <- "THETA"
  d$type[is.diag.omega] <- "OMEGAVAR"
  d$type[is.off.diag.omega] <- "OMEGACOV"
  d$type[grepl("SIGMA",par.char)] <- "SIGMA"
  d$type[grepl("OBJ",par.char)] <- "OBJ"
  if(has_final_est){
    d$type[grepl("CONDNUM",par.char)] <- "CONDNUM"
    d$type <- factor(d$type,levels=c("THETA","OMEGAVAR","OMEGACOV","SIGMA","OBJ","CONDNUM"))
  } else {
    d$type <- factor(d$type,levels=c("THETA","OMEGAVAR","OMEGACOV","SIGMA","OBJ"))
  }
  d <- d[order(d$type),]
  d$unit <- NA
  d$SEunit <- NA
  if(!"SE" %in% names(d)) {
    namesd <- names(d)
    d$SE <- NA
    final_pos <- grep("FINAL",namesd)
    d <- d[,c(namesd[1:final_pos],"SE",namesd[(final_pos+1):length(namesd)])]
  }
  d$is_final <- has_final_est
  d
}

coef_ext0 <- function(ext.file){
  ## raw function to generate parameter table from ext.file.
  extout <- read_ext0(ext.file = ext.file)
  ext2coef(extout,file_name=ext.file)
}



#' @importFrom stats coef
#' @export
stats::coef


#' @importFrom stats AIC
#' @export
stats::AIC

#' @export
AIC.data.frame <- function(object, ..., k = 2){
  if(is_single_na(object)) return(NA)
  params <- object
  params <- params[grepl("THETA|OMEGA|SIGMA", params$type), ]
  
  n_parameters <- nrow(params)
  ofv(object) + k*n_parameters
}

#' @importFrom stats nobs
#' @export
stats::nobs


#' @importFrom stats BIC
#' @export
stats::BIC


# BIC.nmcoef <- function(object, ...){
#   AIC(object, ..., k = log(nobs.nm(object)))
# }



