is_dollar_line <- function(l) grepl("^\\s*;*\\s*\\$",l)
is_nm_comment_line <- function(l) grepl("^\\s*;",l)
rem_dollars <- function(s) gsub("\\$\\S*","",s)
rem_comment <- function(s,char=";") gsub(paste0("^([^",char,"]*)",char,"*.*$"),"\\1",s)
get_comment <- function(s,char=";") gsub(paste0("^[^",char,"]*",char,"*(.*)$"),"\\1",s)

setup_dollar <- function(x,type){
  x[1] <- paste(type,x[1])
  names(x) <- NULL
  class(x) <- paste0("nm.",tolower(gsub("^\\$","",type)))
  x
}


#' Convert controls stream to first abstraction layer
#' @param ls character vector with NONMEM control stream contents
#' @return object of class r.ctl
#' @export
ctl_nm2r <- function(ls){

  ls0 <- ls
  dol <- grep("^\\s*\\$",ls)
  dol <- which(is_dollar_line(ls))
  dol[1] <- 1

  ## get type info for each dol

  dol.type <- function(ls){
    sc <- paste(ls,collapse = " ")
    type <- gsub("^[^\\$]*\\$([\\S]+).*$","\\1",sc, perl = TRUE)
    type <- getOption("available_nm_types")[grep(substr(type,1,3),getOption("available_nm_types"))]
    if(length(type)==0) type <- NA
    type
  }

  ls2 <- list()
  start <- dol[1]
  finish <- dol[2]-1
  for(i in seq_along(dol)){
    ###########################
    ## define start and finish
    start <- dol[i]
    if(finish+1 < start) start <- finish+1 ## start is sorted
    finish <- dol[i+1]-1  ## initial guess for finish
    if(i==length(dol)) {
      finish <- length(ls)
    } else {
      last.line <- ls[finish]
      while(is_nm_comment_line(last.line) & !is_dollar_line(last.line)){
        finish <- finish -1
        last.line <- ls[finish]
      }
    }
    ###########################
    ## start and finish defined
    tmp <- ls[start:finish]
    type <- dol.type(tmp)
    if(is.na(type)) type <- paste0("UNKNOWN",i)
    class(tmp) <- paste0("nm.",tolower(type))
    ls2[[i]] <- tmp
  }
  ls <- ls2

  x <- sapply(ls,function(s)class(s))

  ## find consecutive statements and combine them
  ## can use a for loop

  for(i in rev(seq_along(x))){
    if(i==1) break
    if(x[i]==x[i-1]) {
      ls[[i-1]] <- c(ls[[i-1]],ls[[i]])
      class(ls[[i-1]]) <- class(ls[[i]])
      ls[[i]] <- NULL
    }
  }
  names(ls) <- sapply(ls,function(s)gsub("NM\\.","",toupper(class(s))))
  class(ls) <- "r.ctl"
  ls
}

#' Convert first abstraction layer to control stream
#' @param x object of class r.ctl
#' @return character vector with control stream contents
#' @export
ctl_r2nm <- function(x) unlist(x,use.names = FALSE)

#' Convert $THETA to R abstraction layer
#' @param x character vector with $THETA information
#' @return object abstracting $THETA contents
#' @export
theta_nm2r <- function(x){
  x <- rem_dollars(x)
  x <- x[!x %in% ""]
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
      x <- as.numeric(x)
      x <- data.frame(lower=NA,init=x,upper=NA)
    } else {
      x <- gsub("~","",x)
      x <- strsplit(x,"[ ,]")[[1]]
      x <- x[!x %in% c("","FIX")]
      x <- as.numeric(x)
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

  if(length(comments)!=max(x$N)) {  ##TODO: put a check on total number of thetas
    #warning("no comments on $THETA found, setting comments to be empty")
    comments <- rep("",max(x$N))
  }

  tmp <- strsplit(comments,";")
  x$Name <- sapply(tmp,"[",1)
  x$Name <- rem_trailing_spaces(x$Name)
  x$Unit <- sapply(tmp,"[",2)
  x$Unit <- rem_trailing_spaces(x$Unit)
  x$trans <- sapply(tmp,"[",3)
  x$trans <- rem_trailing_spaces(x$trans)
  x$trans[is.na(x$trans) & x$lower %in% 0] <- "RATIO"
  x$Parameter <- paste0("THETA",x$N)
  x
}

#' remove trailing spaces
rem_trailing_spaces <- function(x){
  x <- gsub("\\s(?!\\S)","",x,perl = TRUE)
  x <- gsub("^\\s*","",x,perl = TRUE)
  x[grepl("^ *$",x)] <- NA
  x
}

#' Convert R abstraction layer to $THETA
#' @param x object abstracting $THETA contents
#' @return character vector with $THETA information
#' @export

theta_r2nm <- function(x){
  x0 <- x
  x0$Name[is.na(x0$Name)] <- paste0("THETA",x0$N[is.na(x0$Name)])
  x0$Unit[!is.na(x0$Unit)] <- paste(";",x0$Unit[!is.na(x0$Unit)])
  x0$Unit[is.na(x0$Unit)] <- ""
  x0$trans[!is.na(x0$trans)] <- paste(";",x0$trans[!is.na(x0$trans)])
  x0$trans[is.na(x0$trans)] <- ""
  x0$COM <- paste(x0$Name,x0$Unit,x0$trans)
  x0$COM <- rem_trailing_spaces(x0$COM)
  x <- by(x,x$N,function(d){
    if(!is.na(d$lower)) paste0("(",d$lower,",",d$init,")") else d$init
  })
  x <- unlist(x)
  x <- paste(x,";",x0$COM)
  setup_dollar(x,"$THETA")
}


