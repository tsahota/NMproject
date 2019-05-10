ext2coef <- function(extout,file_name){
  ## raw function to generate parameter table from ext.file.

  if(!requireNamespace("reshape2", quietly = TRUE))
    stop("reshape2 needed for this function to work. Please install it.",
         call. = FALSE)

  d <- extout

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

  d <- reshape2::melt(data = d, variable.name = "Parameter",
                      measure.vars = par.names)
  if(!"Parameter" %in% names(d)) stop("melt has failed - could be due to reshape being loaded. reshape can interfere with reshape2")


  d <- reshape2::dcast(data = d,
                       stats::as.formula(paste(paste(names(d)[!names(d) %in% c("TYPE","value")],collapse=" + "),
                                               "~ TYPE")),
                       value.var = "value")

  ## messy hard coding - consider refactoring if need more than just eigenvalues
  if(has_final_est & length(cond_num) > 0){
    dlast <- d[nrow(d),]
    dlast$Parameter <- "CONDNUM"
    dlast$FINAL <- cond_num
    dlast$SE <- 0

    d <- rbind(d,dlast)
  }

  if(!has_final_est) names(d)[names(d) %in% "ITER"] <- "FINAL"

  d <- d[order(d$EST.NO,decreasing = TRUE),]
  d$file <- file_name

  is.diag.omega <- grepl("OMEGA.([0-9]+\\.)\\1",d$Parameter)
  is.omega <- grepl("OMEGA.([0-9]+\\.)+",d$Parameter)
  is.off.diag.omega <- is.omega & !is.diag.omega
  d <- d[!(is.off.diag.omega & d$FINAL == 0), ] ## get rid of off diag 0s
  is.diag.sigma <- grepl("SIGMA.([0-9]+\\.)\\1",d$Parameter)
  is.sigma <- grepl("SIGMA.([0-9]+\\.)+",d$Parameter)
  is.off.diag.sigma <- is.sigma & !is.diag.sigma
  d <- d[!(is.off.diag.sigma & d$FINAL == 0), ] ## get rid of off diag 0s


  is.diag.omega <- grepl("OMEGA.([0-9]+\\.)\\1",d$Parameter) ## redefine
  is.omega <- grepl("OMEGA.([0-9]+\\.)+",d$Parameter) ## redefine
  is.off.diag.omega <- is.omega & !is.diag.omega  ## redefine
  is.diag.sigma <- grepl("SIGMA.([0-9]+\\.)\\1",d$Parameter) ## redefine
  is.sigma <- grepl("SIGMA.([0-9]+\\.)+",d$Parameter) ## redefine
  is.off.diag.sigma <- is.sigma & !is.diag.sigma ## redefine

  ## sort so that THETAs first, then diagonal OMEGAs, then off diag, then SIGMA, then OBJ

  par.char <- as.character(d$Parameter)
  par.order <- c(sort(par.char[grepl("THETA",par.char)]),
                 sort(par.char[is.diag.omega]),
                 sort(par.char[is.off.diag.omega]),
                 sort(par.char[grepl("SIGMA",par.char)]),
                 "OBJ",
                 sort(par.char[grepl("CONDNUM",par.char)]))
  if(!identical(sort(par.order),sort(as.character(d$Parameter)))) stop("Bug in code. Debug.")
  d$Parameter <- factor(d$Parameter,levels=par.order)
  d$Type <- NA
  d$Type[grepl("THETA",par.char)] <- "THETA"
  d$Type[is.diag.omega] <- "OMEGAVAR"
  d$Type[is.off.diag.omega] <- "OMEGACOV"
  d$Type[grepl("SIGMA",par.char)] <- "SIGMA"
  d$Type[grepl("OBJ",par.char)] <- "OBJ"
  if(has_final_est){
    d$Type[grepl("CONDNUM",par.char)] <- "CONDNUM"
    d$Type <- factor(d$Type,levels=c("THETA","OMEGAVAR","OMEGACOV","SIGMA","OBJ","CONDNUM"))
  } else {
    d$Type <- factor(d$Type,levels=c("THETA","OMEGAVAR","OMEGACOV","SIGMA","OBJ"))
  }
  d <- d[order(d$Type),]
  d$Unit <- NA
  d$SEUnit <- NA
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

#' Gets parameters values
#'
#' Produces data.frame of parameter values
#' @param object object of class nmexecute
#' @param trans logical. Default = TRUE. Should parameters be transformed
#' @param ... additional arguments to carry to coef
#' @return a \code{data.frame} of parameter values

coef_nm <- function(object,trans,...){

  d <- coef_ext0(object$output$psn.ext)
  d$file <- object$ctl
  if(!unique(d$is_final)) d$file <- paste0(d$file,"*")
  d$is_final <- NULL
  if(!trans) return(d)

  p <- param_info(object)

  p$Parameter <- paste0("THETA",p$N)

  d0 <- d[,names(d)[!names(d) %in% "Unit"]]
  d1 <- p[,c("Name","Parameter","Unit","trans")]

  d <- merge(d0,d1,all.x = TRUE,by="Parameter")
  d$Name[is.na(d$Name)] <- as.character(d$Parameter)[is.na(d$Name)]
  d$Name <- factor(d$Name,levels=d$Name)
  d$transUnit <- d$Unit
  d$transSEUnit <- d$SEUnit
  ## transformations
  d$FINAL.TRANS <- d$FINAL
  d$SE.TRANS <- d$SE
  ## RATIO data
  d$SE.TRANS[d$trans %in% "RATIO"] <- 100*d$SE[d$trans %in% "RATIO"]/d$FINAL[d$trans %in% "RATIO"]
  d$transSEUnit[d$trans %in% "RATIO"] <- "%"
  ## LOG
  d$FINAL.TRANS[d$trans %in% c("LOG","LOGODDS")] <- exp(d$FINAL[d$trans %in% c("LOG","LOGODDS")])
  d$SE.TRANS[d$trans %in% c("LOG","LOGODDS")] <- 100*sqrt((exp(d$SE[d$trans %in% c("LOG","LOGODDS")]^2)-1))
  d$transSEUnit[d$trans %in% c("LOG","LOGODDS")] <- "%"
  ## LOGIT
  if("LOGIT" %in% d$trans){
    d$FINAL.TRANS[d$trans %in% "LOGIT"] <- 100*1/(1+exp(-d$FINAL[d$trans %in% "LOGIT"]))
    d$transUnit[d$trans %in% "LOGIT"] <- "%"
    # delt <- lapply(which(d$trans %in% "LOGIT"),function(i){
    #   par <- c(logit=d$FINAL[i])
    #   separ <- c(logit=d$SE[i])
    #   car::deltaMethod(par,"1/(1+exp(-logit))",vcov.=separ^2)
    # })
    # delt <- do.call(rbind,delt)
    # d$SE.TRANS[d$trans %in% "LOGIT"] <- 100*delt$SE
  }
  ## OMEGA
  d$trans[grepl("OMEGA.([0-9]+\\.)\\1",d$Parameter)] <- "OM"   ## temp code - make an identifyer for OMEGA.X.X
  d$SE.TRANS[d$trans %in% "OM"] <- 100*(d$SE[d$trans %in% "OM"]/d$FINAL[d$trans %in% "OM"])/2
  d$FINAL.TRANS[d$trans %in% "OM"] <- 100*sqrt(exp(d$FINAL[d$trans %in% "OM"])-1)
  d$transUnit[d$trans %in% "OM"] <- "CV%"
  d$transSEUnit[d$trans %in% "OM"] <- "%"
  ## COV
  d$trans[grepl("OMEGA.([0-9]+\\.)+",d$Parameter) & !d$trans %in% "OM"] <- "COV" ## temp code
  # if("COV" %in% d$trans){
  #   omx <- gsub("^OMEGA\\.([0-9]+)\\.([0-9]+)\\.","\\1",d$Parameter[d$trans %in% "COV"])
  #   omy <- gsub("^OMEGA\\.([0-9]+)\\.([0-9]+)\\.","\\2",d$Parameter[d$trans %in% "COV"])
  #   omx <- paste0("OMEGA.",omx,".",omx,".")
  #   omy <- paste0("OMEGA.",omy,".",omy,".")
  #   sdx <- sqrt(d$FINAL[match(omx,d$Parameter)])
  #   sdy <- sqrt(d$FINAL[match(omy,d$Parameter)])
  #   d$FINAL.TRANS[d$trans %in% "COV"] <- d$FINAL[d$trans %in% "COV"]/(sdx*sdy)
  #   d$transUnit[d$trans %in% "COV"] <- "CORR.COEF"
  #   ## COV[X,Y]/(SD[X]*SD[Y])
  #   ## know SE(COV[X,Y]) and SE[SDX^2] and SE[SDY^2]
  #   ## Need covariance matrix between these though - from .cov file.
  #   ## SQRT(VAR(COV[X,Y]/(SD[X]*SD[Y])))
  #   cov.file <- object$output$psn.cov
  #   dc <- utils::read.table(cov.file,skip=1,header = TRUE)
  #   for(i in seq_along(which(d$trans %in% "COV"))){
  #     ## loop through each COV variable and generate absolute SE
  #     names.c <- c(omx[i],omy[i],as.character(d$Parameter[d$trans %in% "COV"][i]))
  #     names.c <- d$Parameter[d$Parameter %in% names.c] ## reorder
  #     names.c2 <- gsub("\\.([0-9]+)\\.([0-9]+)\\.","(\\1,\\2)",names.c)
  #
  #     ## same order as names.c - important
  #     vcov <- dc[match(names.c2,dc$NAME),as.character(names.c)]
  #     rownames(vcov) <- names(vcov)
  #     vcov <- as.matrix(vcov)
  #
  #     pmean <- d$FINAL[match(names.c,d$Parameter)]  ## may as well recompute FINALs
  #     names(pmean) <- d$Name[match(names.c,d$Parameter)]
  #
  #     formula.i <- paste0(names.c[3],"/(sqrt(",names.c[1],")*sqrt(",names.c[2],"))")
  #     #tmp <- car::deltaMethod(pmean,formula.i,vcov.=vcov)
  #     #d$SE.TRANS[d$trans %in% "COV"][i] <- tmp$SE
  #   }
  #
  # }

  ## get names back to what they should be
  d$FINAL <- d$FINAL.TRANS
  d$FINAL.TRANS <- NULL
  d$SE <- d$SE.TRANS
  d$SE.TRANS <- NULL
  d$Unit <- d$transUnit
  d$transUnit <- NULL
  d$SEUnit <- d$transSEUnit
  d$transSEUnit <- NULL
  d$Parameter <- d$Name
  d$Name <- NULL
  d

}

#' @export
coef.nm <- function(object,...){
  
  if(is_single_na(object)) return(NA)

  trans_arg <- list(...)$trans
  if(is.null(trans_arg)) {
    trans <- TRUE
  } else {
    trans <- trans_arg
  }
  ans <- try(coef_nm(object=object,trans=trans), silent = TRUE)
  if(inherits(ans, "try-error")) dplyr::tibble() else ans
}


run_record0 <- function(..., coef.func = coef_ext0){
  coef.func <- coef.func
  d <- lapply(list(...),coef.func)
  d <- do.call(rbind,d)
  d$Unit[is.na(d$Unit)] <- ""
  d$SEUnit[is.na(d$SEUnit)] <- ""
  if("trans" %in% d$trans) d$trans[is.na(d$trans)] <- ""   ## optional item
  d <- d[,names(d)[!names(d) %in% c("EVALUATION", "EST.NO","EST.NAME")]]
  d$Estimate <- NA
  d$Estimate[d$Parameter!="OBJ"] <- paste0(signif(d$FINAL[d$Parameter!="OBJ"],3)," (",signif(d$SE[d$Parameter!="OBJ"],3),d$SEUnit[d$Parameter!="OBJ"],")")
  d$Estimate[d$Parameter=="OBJ"] <- round(d$FINAL[d$Parameter=="OBJ"],3)
  d <- d[,names(d)[!names(d) %in% c("SE","FINAL")]]
  d <- reshape2::dcast(data = d,
                       stats::as.formula(paste(paste(names(d)[!names(d) %in% c("file","Estimate")],collapse=" + "),
                                               "~ file")),
                       value.var = "Estimate")

  d <- d[order(d$Type,d$Parameter),]
  d$SEUnit <- NULL
  d
}

#' Run record
#'
#' @param ... objects of class nmexecute
#' @param trans logical (default=TRUE).
#'  If possible TRUE means transformation will be attempted
#' @export

run_record <- function(...,trans=TRUE){
  a <- list(...)
  classes <- sapply(a,function(a) inherits(a,"nm"))

  if(any(classes)){
    coef.func <- coef_nm
    formals(coef.func)$trans <- trans
  } else {
    coef.func <- coef_ext0
  }
  run_record.tmp <- function(...) run_record0(...,coef.func=coef.func)
  do.call(run_record.tmp,a)

}

summary0 <- function(object, ref_model = NA, ...){
  d <- dplyr::as_tibble(list(m=object))
  d$ofv <- ofv(d$m)
  d$dofv <- d$ofv - ofv(ref_model) 
  
  n_parameters_fun <- function(x){
    if(!inherits(x, "nm")) return(NA)
    params <- coef.nm(x)
    if(!"Type" %in% names(params)) return(NA)
    params <- params[grepl("THETA|OMEGA|SIGMA", params$Type), ]
    nrow(params)
  }
  
  rr_fun <- function(x){
    if(!inherits(x, "nm")) return(NA)
    params <- try(run_record(x, trans = FALSE), silent = TRUE)
    if(inherits(params, "try-error")) return(NA)
    params
  }
  
  if(!is_single_na(ref_model)) {
    rr_ref <- run_record(ref_model, trans = FALSE)
    d$rr <- lapply(d$m, rr_fun)
  }
  
  ## get the parameter relative to ref_model
  
  get_extra_params <- function(rr){
    if(is_single_na(rr)) return(dplyr::as_tibble(NA))
    rr_diff <- suppressMessages(suppressWarnings(dplyr::anti_join(rr, rr_ref)))
    rr_diff <- rr_diff[,c(1,4)]
    names(rr_diff)[2] <- "val"
    rr_diff <- dplyr::as_tibble(rr_diff)
    
    ans <- as.list(rr_diff$val)
    names(ans) <- rr_diff$Parameter
    
    dplyr::as_tibble(ans)
  }
  
  #get_extra_params(rr[[90]])
  
  #rrtmp <- lapply(rr, get_extra_params)
  
  ## for each rr, get additional params
  
  base_n <- n_parameters_fun(ref_model)
  d$df <- sapply(d$m, n_parameters_fun) - base_n
  d$p_chisq <- 1-stats::pchisq(-d$dofv, df = d$df)
  d$ref_cn <- cond_num(ref_model)
  d$cond_num <- cond_num(d$m)
  d$AIC <- AIC.list(d$m)
  d$BIC <- BIC.list(d$m)
  
  if(!is_single_na(ref_model)) {
    d$row <- 1:nrow(d)
    d <- do.call(dplyr::bind_rows,by(d, d$row, function(d){
      dplyr::bind_cols(d, get_extra_params(d$rr[[1]]))
    }))
    d$rr <- NULL
    d$row <- NULL
    d$value <- NULL
  }
  
  d$m <- NULL
  #d <- d %>% arrange(p_chisq, dofv)
  d  
}

#' @export
summary.nm <- function(object, ref_model = NA, ...){
  object <- list(object)
  summary0(object, ref_model = ref_model, ...)
}

#' @export
summary.list <- function(object, ref_model = NA, ...){
  summary0(object, ref_model = ref_model, ...)
}



run_summary <- function(r, db = NULL){
  res <- list()
  outputs <- unlist(r$output)
  if(!is.null(outputs)) existing_files <- file.exists(outputs) else
    existing_files <- NA
  res$outputs_present <- sum(existing_files)/length(existing_files)
  res$last_update <- last_modified(r)
  if(missing(db)){
    res$status <- run_status(r)$status
  } else {
    res$status <- run_status(r, db = db)$status 
  }
  res$lst_exists <- NA
  res$stop_time_reached <- NA
  res$ofv <- NA
  if(r$type %in% "execute"){
    res$lst_exists <- file.exists(r$output$psn.lst)
    res$stop_time_reached <- FALSE
    if(res$lst_exists) {
      lst_file <- readLines(r$output$psn.lst)
      last_lst_file <- lst_file[max(1,length(lst_file)-10):length(lst_file)]
      res$stop_time_reached <- any(grepl("Stop Time",last_lst_file))
      objv_lines <- suppressWarnings(which(grepl("#OBJV",lst_file)))
      if(length(objv_lines)==0) {
        res$ofv <- NA
      } else {
        objv_lines <- max(objv_lines)
        lst_file[objv_lines]
        objv <- as.numeric(gsub("[#OBJV:* ]","",lst_file[objv_lines]))
        if(length(objv)!=1) stop("Couldn't get ofv. Debug")
        res$ofv <- objv
      }
    }
  }
  as.data.frame(res)
}

#' Generate table of runs
#' @param db_name character. Name of db
#' @export
run_table <- function(db_name = "runs.sqlite"){
  d <- nmdb_get(db_name)
  res <- lapply(d$object, unserialize, refhook=NULL)
  res <- lapply(res, run_summary, db = d)
  res <- do.call(rbind,res)
  res <- cbind(data.frame(entry=d$entry),res)
  d <- merge(d,res)
  nmdb_printable_db(d)
}

#' @export
AIC.nm <- function(object, ..., k = 2){
  if(length(object) == 1) if(is.na(object)) return(NA)
  params <- try(coef.nm(object),silent = TRUE)
  if(inherits(params, "try-error")) return(NA)
  params <- params[grepl("THETA|OMEGA|SIGMA", params$Type), ]
  
  n_parameters <- nrow(params)
  ofv(object) - k*n_parameters
}

#' @export
AIC.list <- function(object, ..., k = 2){
  args <- as.list(match.call()[-1])
  
  call_f <- function(x, args){
    args[["object"]] <- x
    do.call(AIC.nm, args)
  }
  
  sapply(object, call_f, args = args)
}

#' @importFrom stats nobs
#' @export
stats::nobs

#' @export
nobs.nm <- function(object, ...){
  if(length(object) == 1) if(is.na(object)) return(NA)
  d <- get_data(object, filter = TRUE)
  d <- d %>% dplyr::filter(.data$EVID %in% 0)
  if("MDV" %in% names(d)){
    d <- d %>% dplyr::filter(!.data$MDV %in% 1) 
  }
  nrow(d)
}


#' @importFrom stats BIC
#' @export
stats::BIC


#' @export
BIC.nm <- function(object, ...){
  AIC.nm(object, ..., k = log(nobs.nm(object)))
}

#' @export
BIC.list <- function(object, ...){
  d <- tibble::tibble(m = object)
  d$data_name <- sapply(object, function(object) {
    if(length(object) == 1) if(is.na(object)) return(NA)
    object$input$data_name 
  })
  
  d <- d %>% dplyr::group_by(.data$data_name) %>%
    dplyr::mutate(lognobs = log(nobs.nm(dplyr::first(.data$m))))
  
  mapply(AIC.nm, object = object, k = d$lognobs, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  
}



