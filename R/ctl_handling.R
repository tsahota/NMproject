is_dollar_line <- function(l) grepl("^\\s*;*\\s*\\$",l)
is_nm_comment_line <- function(l) grepl("^\\s*;",l)
rem_dollars <- function(s) gsub("\\s*\\$\\S*\\s*","",s)
rem_comment <- function(s,char=";") gsub(paste0("^([^",char,"]*)",char,"*.*$"),"\\1",s)
get_comment <- function(s,char=";") gsub(paste0("^[^",char,"]*",char,"*(.*)$"),"\\1",s)

setup_dollar <- function(x, type, add_dollar_text = TRUE){
  ## if $TYPE isn't in x, add it
  if(add_dollar_text){
    if(!grepl(paste0("\\s*\\",type),x[1], ignore.case = TRUE)){
      if(grepl("THETA|OMEGA|SIGMA|PK|PRED|ERROR|DES", type)){
        x <- c(type, x)
      } else {
        x[1] <- paste(type,x[1]) 
      }
    }    
  }
  x <- strsplit(x, "\n")
  x <- sapply(x, function(i){ ## strsplt turns "" -> character(), convert back
    if(length(i) == 0) "" else i
  })
  names(x) <- NULL
  class(x) <- c(paste0("nm.",tolower(gsub("^\\$","",type))),"nm_subroutine")
  x
}

#' Constructor/converter to ctl_character
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Convert a variety of objects into a `ctl_character` class
#' 
#' @param r Either class `nmexecute`, `character`, `ctl_list`, `ctl_character`.
#' @return Object of class `ctl_character`.
#' @keywords internal

ctl_character <- function(r){
  if(inherits(r, "ctl_character")) return(r)
  if(inherits(r, "nmexecute")) {
    ctl <- readLines(r$ctl)
    class(ctl) <- c("ctl_character", "character")
    attr(ctl, "file_name") <- r$ctl
    return(ctl)
  }
  if(inherits(r, "ctl_list")) {
    file_name <- attributes(r)$file_name
    ctl <- ctl_r2nm(r)
    attr(ctl, "file_name") <- file_name
    return(ctl)
  }
  if(inherits(r, "character")){
    if(length(r) == 1){
      ctl_name <- search_ctl_name(r)
      ctl <- readLines(ctl_name)
      class(ctl) <- c("ctl_character", "character")
      attr(ctl, "file_name") <- ctl_name
      return(ctl)
    } else {
      class(r) <- c("ctl_character", "character")
      return(r)
    }
  }
  stop("cannot coerce to ctl_character")
}

#' Constructor/converter to ctl_list
#' 
#' @param r Either class `nmexecute`, `character`, `ctl_list`, `ctl_character`.
#' @return Object of class `ctl_list`.
#' @keywords internal

ctl_list <- function(r){
  UseMethod("ctl_list")
}

#' @export
ctl_list.ctl_character <- function(r){
    ctl <- ctl_nm2r(r)
    attr(ctl, "file_name") <- attributes(r)$file_name
    ctl
}

#' @export
ctl_list.ctl_list <- function(r){
  r
}

#' @export
ctl_list.character <- function(r){
  if(length(r) == 1){
    ctl <- ctl_character(r)
    file_name <- attributes(ctl)$file_name
    ctl <- ctl_nm2r(ctl)
    attr(ctl, "file_name") <- file_name
    return(ctl)
  } else stop("cannot coerce to ctl_list")
}

ctl_nm2r <- function(ctl){

  ctl0 <- ctl
  dol <- grep("^\\s*\\$",ctl)
  dol <- which(is_dollar_line(ctl))
  dol[1] <- 1

  ## get type info for each dol

  dol.type <- function(ctl){
    sc <- paste(ctl,collapse = " ")
    type <- gsub("^[^\\$]*\\$([\\S]+).*$","\\1",sc, perl = TRUE)
    type <- getOption("available_nm_types")[grep(substr(type,1,3),getOption("available_nm_types"))]
    if(length(type)==0) type <- NA
    type
  }

  ctl2 <- list()
  start <- dol[1]
  finish <- dol[2]-1
  for(i in seq_along(dol)){
    ###########################
    ## define start and finish
    start <- dol[i]
    if(finish+1 < start) start <- finish+1 ## start is sorted
    finish <- dol[i+1]-1  ## initial guess for finish
    if(i==length(dol)) {
      finish <- length(ctl)
    } else {
      last.line <- ctl[finish]
      while(is_nm_comment_line(last.line) & !is_dollar_line(last.line)){
        finish <- finish -1
        last.line <- ctl[finish]
      }
    }
    ###########################
    ## start and finish defined
    tmp <- ctl[start:finish]
    type <- dol.type(tmp)
    if(is.na(type)) type <- paste0("UNKNOWN",i)
    class(tmp) <- c(paste0("nm.",tolower(gsub("^\\$","",type))),"nm_subroutine")
    #class(tmp) <- c(paste0("nm.",tolower(type)),"nm_subroutine")
    ctl2[[i]] <- tmp
  }
  ctl <- ctl2

  x <- lapply(ctl,function(s) class(s))

  ## find consecutive statements and combine them
  ## can use a for loop

  for(i in rev(seq_along(x))){
    if(i==1) break
    if(identical(x[i],x[i-1])) {
      ctl[[i-1]] <- c(ctl[[i-1]], ctl[[i]])
      class(ctl[[i-1]]) <- class(ctl[[i]])
      ctl[[i]] <- NULL
    }
  }
  names(ctl) <- sapply(ctl,function(s) gsub("NM\\.","",toupper(class(s)[1])))
  class(ctl) <- "ctl_list"
  ctl
}

ctl_r2nm <- function(x) {
  ctl <- unlist(x,use.names = FALSE)
  class(ctl) <- c("ctl_character")
  ctl
}

theta_nm2r <- function(x){
  x <- rem_dollars(x)
  x <- gsub("FIX","",x) ## ignore FIX for now
  x <- x[!grepl("^\\s*$",x)] ## remove $THETA
  x <- gsub("\\t"," ",x)     ## change tabs to spaces
  x <- x[!grepl("^\\s*;.*",x)]  ## remove comment only rows
  x0 <- x
  x <- rem_comment(x,";")
  x <- paste(x,collapse = " ")
  x <- gsub("\\(\\s*\\S*(\\s*)\\S*(\\s\\)S*\\s)\\)","\\(~",x)
  x <- gsub("\\(","\\(~",x)
  x <- strsplit(x,"\\(|\\)")[[1]]
  x <- x[!grepl("^\\s*$",x)]

  x <- lapply(x,function(x){
    if(substr(x,1,1)!="~"){
      x <- strsplit(x,"[ ,]")[[1]]
      x <- x[!x %in% c("","FIX")]
      x <- suppressWarnings(as.numeric(x))
      x <- data.frame(lower=NA,init=x,upper=NA)
    } else {
      x <- gsub("~","",x)
      x <- strsplit(x,"[ ,]")[[1]]
      x <- x[!x %in% c("","FIX")]
      x <- suppressWarnings(as.numeric(x))
      #x <- as.data.frame(t(x))
      if(length(x)==1) x <- data.frame(lower=NA,init=x,upper=NA) else
        if(length(x)==2) x <- data.frame(lower=x[1],init=x[2],upper=NA) else
          if(length(x)==3) x <- data.frame(lower=x[1],init=x[2],upper=x[3])
      if(!length(x) %in% 1:3) stop("can't figure out bounds")
    }
    x
  })
  x <- do.call(rbind,x)
  x$N <- 1:nrow(x)
  class(x) <- c(class(x),"r.theta")
  comments <- get_comment(x0,";")

  if(length(comments) > max(x$N)) {
    warning("More comments than THETAs found. Something wrong")
    comments <- rep("",max(x$N))
  }

  tmp <- strsplit(comments,";")
  x$name <- sapply(tmp,"[",1)
  x$name <- rem_trailing_spaces(x$name)
  x$unit <- sapply(tmp,"[",2)
  x$unit <- rem_trailing_spaces(x$unit)
  x$trans <- sapply(tmp,"[",3)
  x$trans <- rem_trailing_spaces(x$trans)
  x$trans[is.na(x$trans) & x$lower %in% 0] <- "RATIO"
  x$parameter <- paste0("THETA",x$N)
  x
}

rem_trailing_spaces <- function(x){
  x <- gsub("\\s(?!\\S)","",x,perl = TRUE)
  x <- gsub("^\\s*","",x,perl = TRUE)
  x[grepl("^ *$",x)] <- NA
  x
}

#' Get parameter information
#'
#' @param ctl Character. Path to control file.
#' @keywords internal

param_info <- function(ctl){
  UseMethod("param_info")
}
param_info.default <- function(ctl){
  ctl <- ctl_list(ctl)
  if("THETA" %in% names(ctl)) return(theta_nm2r(ctl$THETA)) else
    return(data.frame())
}

#' Convert NONMEM code to R ready
#' 
#' Parses NONMEM code and attempts to make it evaluable as R code.
#' 
#' @param code Character vector of NONMEM code block.
#' @param eta_to_0 Logical (default = `TRUE`). Should all etas be set to 0.
#' 
#' @return Character vector of R code.
#' 
#' @keywords internal

nonmem_code_to_r <- function(code, eta_to_0 = TRUE){
  pk_block <- rem_comment(code)
  
  pk_block <- pk_block[!grepl("^\\s*\\$.*", pk_block)]

  if(eta_to_0){
    pk_block <- gsub("\\bETA\\(([0-9]+)\\)","0", pk_block)
  }
  
  ## will replace both THETA and ETA
  pk_block <- gsub("ETA\\(([0-9]+)\\)","ETA\\1", pk_block) 
  
  pk_block <- gsub("\\bLOG\\b","log", pk_block)
  pk_block <- gsub("\\bEXP\\b","exp", pk_block)
  pk_block <- gsub("\\bIF\\b","if", pk_block)
  
  pk_block <- gsub("\\bTHEN\\b","{", pk_block)
  pk_block <- gsub("\\bENDIF\\b","}", pk_block)
  pk_block <- gsub("\\bELSE\\b","} else {", pk_block)
  ## TODO: handle IF THEN (no ENDIF) blocks

  pk_block <- gsub("\\.EQ\\.","==",pk_block)
  pk_block <- gsub("\\.NE\\.","!=",pk_block)
  pk_block <- gsub("\\.EQN\\.","==",pk_block)
  pk_block <- gsub("\\.NEN\\.","!=",pk_block)
  pk_block <- gsub("\\./E\\.","!=",pk_block)
  pk_block <- gsub("\\.GT\\.",">",pk_block)
  pk_block <- gsub("\\.LT\\.","<",pk_block)
  pk_block <- gsub("\\.GE\\.",">=",pk_block)
  pk_block <- gsub("\\.LE\\.","<=",pk_block)
  pk_block
  
  
}


#' @rdname comment_out
#' @name comment_out
#' @title Comment lines of control file
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Comment out lines of code with that are matched by a patter string.
#' 
#' @param m An nm object.
#' @param pattern Character regex. Passed to [gsub()].
#' 
#' @seealso [gsub_ctl()], [target()]
#' @export
comment_out <- function(m, pattern = ".*"){
  m %>% gsub_ctl(paste0("(",pattern,")"), "; \\1")
}

#' @rdname comment_out
#' 
#' @export
uncomment <- function(m, pattern = ".*"){
  m %>% gsub_ctl(paste0("^;+\\s*(",pattern,")"), "\\1")
}

#' Pattern replacement for control file contents
#'
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#'
#' A wrapper around `gsub` so that control files may be modified using `gsub`
#' syntax.  Can be useful for simple find replace operations in a control
#' stream.  Ensure you use the "view diff" app afterwards to make sure the find
#' replace proceeded as intended.
#'
#' @param m An nm object.
#' @param pattern Argument passed to [gsub()].
#' @param replacement Argument passed to [gsub()].
#' @param ... Additional arguments passed to [gsub()].
#' @param dollar Character name of subroutine.
#' 
#' @seealso [apply_manual_edit()]
#' @examples 
#' 
#' \dontrun{
#' 
#' m1 %>% gsub_ctl("ISAMPLE=300", "ISAMPLE=600")
#' 
#' }
#' 
#' @export

gsub_ctl <- function(m, pattern, replacement, ..., dollar = NA_character_){
  UseMethod("gsub_ctl")
}

#' @export
gsub_ctl.nm_generic <- function(m, pattern, replacement, ..., dollar = NA_character_){

  text <- get_target_text(m)
  text <- gsub(pattern, replacement, text, ...)
  
  m <- m %>% set_target_text(text)
  m
}
#' @export
gsub_ctl.nm_list <- Vectorize_nm_list(gsub_ctl.nm_generic, SIMPLIFY = FALSE)
