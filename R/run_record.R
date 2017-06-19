ext2coef <- function(extout,file_name){
  ## raw function to generate parameter table from ext.file.
  
  if(!requireNamespace("reshape2", quietly = TRUE))
    stop("reshape2 needed for this function to work. Please install it.",
         call. = FALSE)

  d <- extout

  d <- d[d$TYPE %in% c("FINAL","SE"),]
  d <- d[d$EST.NO %in% max(d$EST.NO), ]

  d <- d[,c(names(d)[grepl("THETA|SIGMA|OMEGA",names(d))],
            c("OBJ","EST.NAME","EST.NO","EVALUATION","TYPE"))]

  par.names <- names(d)[match("THETA1",names(d)):match("OBJ",names(d))]
  
  d <- reshape2::melt(data = d, variable.name = "Parameter",
                      measure.vars = par.names)
  d <- reshape2::dcast(data = d, 
                       stats::as.formula(paste(paste(names(d)[!names(d) %in% c("TYPE","value")],collapse=" + "),
                                        "~ TYPE")),
                       value.var = "value")
  
  d <- d[order(d$EST.NO,decreasing = TRUE),]
  d$file <- file_name

  is.diag.omega <- grepl("OMEGA.([0-9]+\\.)\\1",d$Parameter)
  is.omega <- grepl("OMEGA.([0-9]+\\.)+",d$Parameter)
  is.off.diag.omega <- is.omega & !is.diag.omega
  d <- d[!(is.off.diag.omega & d$FINAL %in% 0), ] ## get rid of off diag 0s
  is.diag.omega <- grepl("OMEGA.([0-9]+\\.)\\1",d$Parameter) ## redefine
  is.omega <- grepl("OMEGA.([0-9]+\\.)+",d$Parameter) ## redefine
  is.off.diag.omega <- is.omega & !is.diag.omega  ## redefine
  ## sort so that THETAs first, then diagonal OMEGAs, then off diag, then SIGMA, then OBJ

  par.char <- as.character(d$Parameter)
  par.order <- c(sort(par.char[grepl("THETA",par.char)]),
                 sort(par.char[is.diag.omega]),
                 sort(par.char[is.off.diag.omega]),
                 sort(par.char[grepl("SIGMA",par.char)]),
                 "OBJ")

  if(!identical(sort(par.order),sort(as.character(d$Parameter)))) stop("Bug in code. Debug.")
  d$Parameter <- factor(d$Parameter,levels=par.order)
  d$Type <- NA
  d$Type[grepl("THETA",par.char)] <- "THETA"
  d$Type[is.diag.omega] <- "OMEGAVAR"
  d$Type[is.off.diag.omega] <- "OMEGACOV"
  d$Type[grepl("SIGMA",par.char)] <- "SIGMA"
  d$Type[grepl("OBJ",par.char)] <- "OBJ"
  d$Type <- factor(d$Type,levels=c("THETA","OMEGAVAR","OMEGACOV","SIGMA","OBJ"))
  d <- d[order(d$Type),]
  d$Unit <- NA
  d$SEUnit <- NA
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
#' @export

coef_nm <- function(object,trans,...){

  d <- coef_ext0(object$output$psn.ext)
  d$file <- object$ctl
  if(!trans) return(d)

  p <- object$param_info

  p$Parameter <- paste0("THETA",p$N)
  ## TODO: read parameter names from $OMEGA
  ## either $OMEGA in same way as $THETA (but without units in comments)
  ## or $OMEGA BLOCK

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
  d$SE.TRANS[d$trans %in% "OM"] <- 100*d$SE[d$trans %in% "OM"]/d$FINAL[d$trans %in% "OM"]
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

  trans_arg <- list(...)$trans
  if(is.null(trans_arg)) {
    trans <- TRUE
  } else {
    trans <- trans_arg
  }
  coef_nm(object=object,trans=trans)
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

run_summary <- function(r){
  res <- list()
  outputs <- unlist(r$output)
  if(!is.null(outputs)) existing_files <- file.exists(outputs) else
    existing_files <- NA
  res$outputs_present <- sum(existing_files)/length(existing_files)
  res$last_update <- last_modified(r)
  res$status <- run_status(r)$status
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
      objv_lines <- which(grepl("#OBJV",lst_file))
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

#' @export
run_table <- function(){
  d <- nmdb_get()
  res <- lapply(d$object,unserialize,refhook=NULL)
  res <- lapply(res,run_summary)
  res <- do.call(rbind,res)
  res <- cbind(data.frame(entry=d$entry),res)
  d <- merge(d,res)
  nmdb_printable_db(d)
}

