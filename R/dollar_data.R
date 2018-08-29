#' Generate $INPUT values from NONMEM compatible dataset
#'
#' @param d data.frame. NONMEM compatible dataset
#' @param keep character vector. Names of columns to keep
#' @param rename named character vector. Renaming instructions
#' @export

dollar_data <- function(d,keep,rename){
  keep0 <- sapply(d,is_a_number)
  if(missing(keep)){
    message("Excluding all non-numeric items")
    keep <- keep0
  } else {
    unmatched <- keep[!keep %in% names(d)]
    if(length(unmatched)>0) stop(paste(paste(unmatched,collapse=","),"not found"))
    keep <- names(d) %in% keep
    keep.nonnumeric <- names(d)[keep & !keep0]
    if(length(keep.nonnumeric)>0)
      warning(paste0("Following non-numerics also dropped: \"",paste(keep.nonnumeric,collapse="\",\""),"\""))
    keep <- keep[!keep %in% keep.nonnumeric]
  }
  n.remaining <- length(which(keep))
  if(n.remaining > 45) warning(paste(n.remaining,"non-dropped variables for nonmem. Consider reducing"))
  #if(length(intersect(names(d)[!keep],names(rename)))>0) stop("can't drop renamed columns")

  ## rename variables in
  if(!missing(rename)){
    tmp <- match(names(d),names(rename))
    tmp <- rename[tmp]
    names(d)[!is.na(tmp)] <- tmp[!is.na(tmp)]
  }

  for(i in c("ID","AMT","EVID","CMT","TIME","MDV","RATE","DATE")){
    if(i %in% names(d)[!keep]) {
      if(i == "DATE") warning("DATE is dropped, NONMEM will try to use this")
      warning(paste("Are you sure",i,"shouldn't be kept?"))
    }
  }

  for(i in c("EXP","DEXP","LOG","RES","WRES","PRED","IPRED","PHI")){
    if(i %in% names(d)) warning(paste(i,"is already defined by NONMEM. Consider renaming"))
  }

  s <- paste0(names(d),ifelse(!keep,"=DROP",""))
  s <- gsub("\\.","",s) ## remove .'s
  s <- toupper(s)

  ## warning when core columns aren't present
  core.vars <- c("ID","AMT","EVID","CMT","TIME","MDV")
  cove.vars.missing <- core.vars[!core.vars %in% s]
  if(length(cove.vars.missing)>0) warning(paste0("Core variables not defined: \"",
                                                 paste(cove.vars.missing,collapse=","),"\""))

  s <- paste(s,collapse = " ")
  ## wrap text every first space after 60 characters
  s <- strsplit(gsub("(.{60}\\S*) ","\\1~",s),"~")[[1]]
  lapply(s,message)
  return(invisible(s))
}

is_a_number <- function(x){
  if(inherits(x, "integer")) return(TRUE)
  if(inherits(x, "numeric")) return(TRUE)
  suppressWarnings({
    if(inherits(x, "factor")) x_numeric <- as.numeric(levels(x))[x] else
      if(inherits(x, "logical")) x_numeric <- as.numeric(as.character(x)) else
        if(inherits(x, "character")) x_numeric <- as.numeric(x) else
          stop("unknown class - needs development")
  })
  nas_x_numeric <- length(which(is.na(x_numeric)))
  nas_x <- length(which(is.na(x)))
  return(nas_x_numeric == nas_x)
}
