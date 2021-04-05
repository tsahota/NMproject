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
  names(x) <- NULL
  class(x) <- c(paste0("nm.",tolower(gsub("^\\$","",type))),"nm_subroutine")
  x
}

## TODO: replace control stream objects with different (non-nested) classes:
##  class character = name of control file (methods need (new) file_name arg)
##  class ctl = character vector of control file
##  class rctl = r format list of control file

## no class - assume this is a file name.
## class ctl_character = simple character vector
## class ctl_list = list (first abstraction layer)
## TBD subsequent layers of abstraction.


## change ctl_nm2r to as.rctl etc.
## all functions can be overloaded to work on all classes
##  output the same class as input
## need constructors for each.

#' Constructor/converter to ctl_character
#' @param r either class nmexecute, character, ctl_list, ctl_character
#' @return object of class ctl_character
#' @export
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
      #stop("cannot coerce to ctl_character")
    }
  }
  stop("cannot coerce to ctl_character")
}

#' print ctl file from an nm object
#' 
#' @param r object of class nm
#' @export
print_ctl <- function(r) {
  print(ctl_character(r))
  invisible(r)
}

#' Constructor/converter to ctl_list
#' @param r either class nmexecute, character, ctl_list, ctl_character
#' @return object of class ctl_list
#' @export

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
ctl_list.nmexecute <- function(r){
  ctl <- ctl_character(r)
  file_name <- attributes(ctl)$file_name
  ctl <- ctl_nm2r(ctl)
  attr(ctl, "file_name") <- file_name
  return(ctl)  
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

#' Convert controls stream to first abstraction layer
#' @param ctl character vector with NONMEM control stream contents
#' @return object of class r.ctl
#' @export
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

#' Convert first abstraction layer to control stream
#' @param x object of class r.ctl
#' @return character vector with control stream contents
#' @export
ctl_r2nm <- function(x) {
  ctl <- unlist(x,use.names = FALSE)
  class(ctl) <- c("ctl_character")
  ctl
}

#' Convert $THETA to R abstraction layer
#' @param x character vector with $THETA information
#' @return object abstracting $THETA contents
#' @export
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

#' Convert R abstraction layer to $THETA
#' @param x object abstracting $THETA contents
#' @return character vector with $THETA information
#' @export

theta_r2nm <- function(x){
  x0 <- x
  x0$name[is.na(x0$name)] <- paste0("THETA",x0$N[is.na(x0$name)])
  x0$unit[!is.na(x0$unit)] <- paste(";",x0$unit[!is.na(x0$unit)])
  x0$unit[is.na(x0$unit)] <- ""
  x0$trans[!is.na(x0$trans)] <- paste(";",x0$trans[!is.na(x0$trans)])
  x0$trans[is.na(x0$trans)] <- ""
  x0$COM <- paste(x0$name,x0$unit,x0$trans)
  x0$COM <- rem_trailing_spaces(x0$COM)
  x <- by(x,x$N,function(d){
    if(!is.na(d$lower)) paste0("(",d$lower,",",d$init,")") else d$init
  })
  x <- unlist(x)
  x <- paste(x,";",x0$COM)
  setup_dollar(x,"$THETA")
}

#' Update subroutine
#' 
#' @param ctl object coercible to ctl_list
#' @param ... named replacement argument of class character
#' @param which_dollar (default = 1) which subroutine if multiple matches
#' @param append logical (default = FALSE).  Should results be appended to subroutine
#' @examples
#' \dontrun{
#' m1 %>% update_dollar(THETA=c("1             	; KA ; h-1 ; LOG
#'                              -2.5            ; K  ; h-1 ; LOG
#'                              -0.5          	; V  ; L ; LOG")) %>%
#'        write_ctl()
#' }
#' @export
update_dollar <- function(ctl,..., which_dollar = 1, append = FALSE){
  UseMethod("update_dollar")
}

#' @export
update_dollar.default <- function(ctl,..., which_dollar = 1, append = FALSE){
  ctl <- ctl_list(ctl)
  arg <- list(...)
  if(length(arg) != 1) stop("need argument")
  
  dollar_name <- names(arg)
  replace <- arg[[1]]
  replace <- strsplit(replace, "\n")[[1]]
  replace <- trimws(replace)
  dollar_matches <- which(names(ctl) %in% dollar_name)
  dollar_match <- dollar_matches[which_dollar]
  dollar_name <- names(ctl)[dollar_match]
  
  if(append) replace <- c(ctl[[dollar_name]], replace)
  
  ctl[[dollar_name]] <- setup_dollar(replace,paste0("$",dollar_name))
  ctl
}


#' Get parameter information
#'
#' @param ctl character. Path to control file
#' @export
param_info <- function(ctl){
  UseMethod("param_info")
}
param_info.default <- function(ctl){
  ctl <- ctl_list(ctl)
  if("THETA" %in% names(ctl)) return(theta_nm2r(ctl$THETA)) else
    return(data.frame())
}

update_parameters0 <- function(ctl,coef_from,type = c("THETA","OMEGA","SIGMA")){
  type <- match.arg(type)

  ctl_lines <- ctl
  params <- ctl_lines[[type]]

  contents <- rem_comment(params)
  comments <- get_comment(params)
  comments[!grepl("^\\s*$",comments)] <- paste0(";",comments[!grepl("^\\s*$",comments)])

  final_params <- coef_from
  final_params <- final_params[,c("parameter", "FINAL")]
  final_params <- final_params[grepl(type,final_params$parameter),]

  if(type %in% c("OMEGA","SIGMA")){
    final_params$ROW <- as.numeric(gsub(paste0(type,"\\.([0-9]+)\\..*"),"\\1",final_params$parameter))
    final_params$COL <- as.numeric(gsub(paste0(type,"\\.[0-9]+\\.([0-9]+).*"),"\\1",final_params$parameter))
    final_params <- final_params[order(final_params$ROW, final_params$COL), ]
  }

  if(type %in% c("THETA")){
    final_params$ROW <- as.numeric(gsub(paste0(type,"([0-9]+)"),"\\1",final_params$parameter))
    final_params <- final_params[order(final_params$ROW), ]
  }

  contents1 <- paste0(paste0(contents,collapse = "\n"),"\n")
  contents1 <- gsub("\\$THETA","$THETA&",contents1)
  contents1 <- gsub("(\\$OMEGA BLOCK\\s*\\([0-9]+\\))","\\1&",contents1)
  contents1 <- gsub("(\\$OMEGA)( [^B])","\\1&\\2",contents1)
  contents1 <- gsub("(\\$SIGMA BLOCK\\s*\\([0-9]+\\))","\\1&",contents1)
  contents1 <- gsub("(\\$SIGMA)( [^B])","\\1&\\2",contents1)

  contents1 <- gsub(",\\s*",",",contents1)
  contents2 <- gsub("([-\\.0-9])[ \\t]+([-\\.0-9])","\\1&\\2",contents1)
  while(!identical(contents2,contents1)){
    contents1 <- contents2
    contents2 <- gsub("([-\\.0-9])[ \\t]+([-\\.0-9])","\\1&\\2",contents1)
  }

  contents1 <- strsplit(contents1,"(?<=\\n)",perl = TRUE)[[1]]
  contents1 <- gsub("\\n","_",contents1)

  contents1 <- unlist(strsplit(contents1,"(?<=&)",perl = TRUE))

  matched_replacements <- grepl("^(\\s*&?\\s*_?\\s*)-?\\.?[0-9]+\\.?[0-9]*E*e*-*\\+*[0-9]*(\\s*(FIX)?&?\\s*_?\\s*)$",contents1) |
    grepl(".*SAME.*",contents1) |
    grepl("(^\\s*&?\\s*_?\\s*\\(.*,).*(\\)\\s*(FIX)?&?\\s*_?\\s*)$",contents1) |
    grepl("(^\\s*&?\\s*_?\\s*\\(.*,).*(,.*\\)\\s*(FIX)?&?\\s*_?\\s*)$",contents1)

  if(type %in% c("THETA")){
    if(length(which(matched_replacements)) > nrow(final_params)) stop("something wrong. debug")
    if(length(which(matched_replacements)) != nrow(final_params)) warning("different numbers of parameters in outputs. Are you using $PRIOR?")
  }

  for (j in seq_along(which(matched_replacements))){
    i <- which(matched_replacements)[j]
    contents1[i] <- gsub("^(\\s*&?\\s*_?\\s*)-?\\.?[0-9]+\\.?[0-9]*E*-*\\+*[0-9]*(\\s*(FIX)?&?\\s*_?\\s*)$",
                         paste0("\\1",signif(final_params$FINAL[j],5),"\\2"),contents1[i])
    contents1[i] <- gsub("(^\\s*&?\\s*_?\\s*\\([^,]*,)[^,]*(,?[^,]*\\)\\s*(FIX)?&?\\s*_?\\s*)$",
                         paste0("\\1",signif(final_params$FINAL[j],5),"\\2"),contents1[i])
    #contents1[i] <- gsub("(^\\s*&?\\s*_?\\s*\\(.*,).*(\\)\\s*(FIX)?&?\\s*_?\\s*)$",
    #                     paste0("\\1",signif(final_params$FINAL[j],5),"\\2"),contents1[i])
    contents1[i] <- gsub("(^\\s*&?\\s*_?\\s*\\(.*,).*(,.*\\)\\s*(FIX)?&?\\s*_?\\s*)$",
                         paste0("\\1",signif(final_params$FINAL[j],5),"\\2"),contents1[i])
  }

  contents1 <- paste0(contents1,collapse = " ")
  contents1 <- strsplit(contents1,"_")[[1]]
  contents1 <- gsub("&"," ",contents1)

  new_params <- paste(contents1,comments)
  class(new_params) <- paste0("nm.",tolower(type))

  ctl_lines[[type]] <- new_params

  ctl_lines
}

#' gsub for ctl file
#' 
#' @param ctl object coercible into ctl_character
#' @param pattern argument passed to gsub
#' @param replacement argument passed to gsub
#' @param ... additional arguments passed to gsub
#' @param dollar character name of subroutine
#' @export

gsub_ctl <- function(ctl, pattern, replacement, ..., dollar = NA_character_){
  UseMethod("gsub_ctl")
}

#' @export
gsub_ctl.default <- function(ctl, pattern, replacement, ..., dollar = NA_character_){
  if(is.na(dollar)){
    ctl <- ctl_character(ctl)
    ctl <- gsub(pattern, replacement, x = ctl, ...)    
  } else {
    ctl <- ctl_list(ctl)
    ctl[[dollar]] <- gsub(pattern, replacement, x = ctl[[dollar]], ...)    
  }
  ctl_list(ctl)
}

append_dollar <- function(ctl_lines, after = NULL, ...){
  ctl_lines <- ctl_list(ctl_lines)
  if(is.null(after)) append_after <- length(ctl_lines) else
    append_after <- max(which(grepl(after,names(ctl_lines))))
  if(length(after)!=1) stop("cannot find determine \"after\"")
  if(is.na(after)) stop("cannot find determine \"after\"")
  if(after %in% -Inf) stop("cannot find determine \"after\"")
  attributes_ctl_lines <- attributes(ctl_lines)
  attributes_ctl_lines$names <- NULL
  ctl_lines <- append(ctl_lines, list(...) ,after = append_after)
  attributes_ctl_lines$names <- names(ctl_lines)
  attributes(ctl_lines) <- attributes_ctl_lines
  ctl_lines
}

#' change to estimation control stream to sim
#'
#' @param ctl_lines character vector. ctl file read into R
#' @param subpr numeric. Number of subproblems
#' @param seed numeric
#' @export

change_to_sim <- function(ctl_lines,subpr=1,seed=1){
  ctl_lines <- ctl_list(ctl_lines)
  ctl_lines$EST <- paste0(";",ctl_lines$EST)
  ctl_lines$COV <- paste0(";",ctl_lines$COV)
  if("SIM" %in% names(ctl_lines)){
    ctl_lines$SIM <- gsub("^\\s*;+(.*)","\\1",ctl_lines$SIM)
    ctl_lines$SIM <- gsub("(SUBPR[^=]*\\s*=\\s*)[0-9]+",paste0("\\1",subpr),ctl_lines$SIM)
    ctl_lines$SIM <- gsub("(\\$SIM[^\\s]*\\s*\\()[0-9]+(\\))",paste0("\\1",seed,"\\2"),ctl_lines$SIM)
  } else {
    ## insert before $TABLE, after $ERROR/$PRED
    #pred_error_pos <- which(grepl("ERROR|PRED",names(ctl_lines)))
    #if(length(pred_error_pos) > 1) stop("multiple $ERROR/$PREDs detected - should only have one or the other?")
    #ctl_lines <- append(ctl_lines,list(SIM=NA),pred_error_pos)
    ctl_lines <- append_dollar(ctl_lines, SIM=NA, "ERROR|PRED|SIGMA|OMEGA")
    ctl_lines$SIM <- paste0("$SIM (",seed,") ONLYSIM SUBPR=",subpr)
  }

  e <- ctl_lines$ERROR
  fflag1_pos <- grep("F_FLAG\\s*=\\s*1",e)
  if(length(fflag1_pos) > 0) {
    message("attempting to remove F_FLAG code... check this")
    y_line <- grep("^\\s*Y\\s*=.*(EPS|ETA).*$",e)
    if_pos <- grep("^\\s*IF.*\\sTHEN",e)
    endif_pos <- grep("^\\s*ENDIF",e)
    if_statements <- lapply(seq_along(if_pos),function(i)if_pos[i]:endif_pos[i])

    y_if_pos <- which(sapply(if_statements,function(i) y_line %in% i))

    if_statements[[y_if_pos]]
    e[setdiff(if_statements[[y_if_pos]], y_line)] <- paste(";",e[setdiff(if_statements[[y_if_pos]], y_line)])
    ctl_lines$ERROR <- e
  }
  ctl_list(ctl_lines)
}

#' Add a covariate to a NONMEM model
#'
#' @param ctl object coercible to ctl_list
#' @param param character. Name of parameter
#' @param cov character. Name of covariate
#' @param state numeric or character. Number/name of state (see details)
#' @param continuous logical (default = TRUE). is covariate continuous?
#' @param time_varying optional logical. is the covariate time varying?
#' @param additional_state_text optional character. custom state variable to be passed to param_cov_text
#' @param id_var character (default = "ID"). Needed if time_varying is missing.
#' @param force logical (default = FALSE). Force covariate in even if missing values found
#' @param force_TV_var logical (default = FALSE). Force covariates only on TV notation parameters
#' @param init optional numeric/character vector.  Initial estimate of additional parameters
#' @param lower optional numeric/character vector.  lower bound of additional parameters
#' @param upper optional numeric/character vector.  Upper bound of additional parameters
#' 
#' @details 
#' available states:
#' "2" or "linear":
#'   PARCOV= ( 1 + THETA(1)*(COV - median))
#' 
#' "3" or "hockey-stick":
#'   IF(COV.LE.median) PARCOV = ( 1 + THETA(1)*(COV - median))
#'   IF(COV.GT.median) PARCOV = ( 1 + THETA(2)*(COV - median))
#'                     
#' "4" or "exponential":
#'    PARCOV= EXP(THETA(1)*(COV - median))
#' 
#' "5" or "power":
#'    PARCOV= ((COV/median)**THETA(1))
#'    
#' "power1":
#'    PARCOV= ((COV/median))
#'    
#' "power0.75":
#'    PARCOV= ((COV/median)**0.75)
#' 
#' "6" or "log-linear":
#'    PARCOV= ( 1 + THETA(1)*(LOG(COV) - log(median)))
#' 
#' @examples 
#' \dontrun{
#' 
#' m1WT <- m1 %>% child("m1WT") %>%
#'   add_cov(param = "CL", cov = "WT", state = "power") %>% 
#'   run_nm()
#'   
#' ## compare results
#' 
#' rr(c(m1, m1WT))
#' summary_wide(c(m1, m1WT))
#'   
#' }
#' 
#' 
#' @export

add_cov <- function(ctl, param, cov, state = 2, continuous = TRUE,
                    time_varying, additional_state_text, id_var = "ID",
                    force = FALSE, force_TV_var = FALSE, 
                    init, lower, upper){
  UseMethod("add_cov")
}

#' Remove a covariate to a NONMEM model
#'
#' Only works with covariates added with add_cov
#'
#' @param ctl object coercible to ctl_list
#' @param param character. Name of parameter
#' @param cov character. Name of covariate
#' @param state numeric or character. Number/name of state (see details)
#' @param continuous logical (default = TRUE). is covariate continuous?
#' @param time_varying optional logical. is the covariate time varying?
#' @param additional_state_text optional character. custom state variable to be passed to param_cov_text
#' @param id_var character (default = "ID"). Needed if time_varying is missing.
#' 
#' @details 
#' available states:
#' "2" or "linear":
#'   PARCOV= ( 1 + THETA(1)*(COV - median))
#' 
#' "3" or "hockey-stick":
#'   IF(COV.LE.median) PARCOV = ( 1 + THETA(1)*(COV - median))
#'   IF(COV.GT.median) PARCOV = ( 1 + THETA(2)*(COV - median))
#'                     
#' "4" or "exponential":
#'    PARCOV= EXP(THETA(1)*(COV - median))
#' 
#' "5" or "power":
#'    PARCOV= ((COV/median)**THETA(1))
#'    
#' "power1":
#'    PARCOV= ((COV/median))
#'    
#' "power0.75":
#'    PARCOV= ((COV/median)**0.75)
#' 
#' "6" or "log-linear":
#'    PARCOV= ( 1 + THETA(1)*(LOG(COV) - log(median)))
#' 
#' @examples 
#' \dontrun{
#' 
#' m1noWT <- m1 %>% child("m1noWT") %>%
#'   remove_cov(param = "CL", cov = "WT") %>% 
#'   run_nm()
#'   
#' ## compare results
#' 
#' rr(c(m1, m1noWT))
#' summary_wide(c(m1, m1noWT))
#'   
#' }
#' 
#' @export

remove_cov <- function(ctl, param, cov, state = 2, continuous = TRUE,
                       time_varying, additional_state_text, id_var = "ID"){
  UseMethod("remove_cov")
}

param_cov_text <- function(param,cov,state,data,theta_n_start,continuous = TRUE,
                           state_text = list(
                             "2" = "PARCOV= ( 1 + THETA(1)*(COV - median))",
                             "linear" = "PARCOV= ( 1 + THETA(1)*(COV - median))",
                             "3" = c("IF(COV.LE.median) PARCOV = ( 1 + THETA(1)*(COV - median))",
                                     "IF(COV.GT.median) PARCOV = ( 1 + THETA(2)*(COV - median))"),
                             "hockey-stick" = c("IF(COV.LE.median) PARCOV = ( 1 + THETA(1)*(COV - median))",
                                                "IF(COV.GT.median) PARCOV = ( 1 + THETA(2)*(COV - median))"),
                             "4" = "PARCOV= EXP(THETA(1)*(COV - median))",
                             "exponential" = "PARCOV= EXP(THETA(1)*(COV - median))",
                             "5" = "PARCOV= ((COV/median)**THETA(1))",
                             "power" = "PARCOV= ((COV/median)**THETA(1))",
                             "power1" = "PARCOV= ((COV/median))",
                             "power0.75" = "PARCOV= ((COV/median)**0.75)",
                             "6" = "PARCOV= ( 1 + THETA(1)*(LOG(COV) - log(median)))",
                             "log-linear" = "PARCOV= ( 1 + THETA(1)*(LOG(COV) - log(median)))"),
                           additional_state_text, ...){

  if(!missing(additional_state_text)) {
    if(is.null(names(additional_state_text))) stop("additional_state_text needs to be a named list")
    if(any(names(additional_state_text) %in% names(state_text)))
      stop("additional_state_text entries cannot overwrite base states:\n ",
           "create new state name")
    state_text <- append(state_text, additional_state_text)
  }
  
  if(!continuous) {
    
    if(!missing(state_text)) 
      stop("not currently allowed to modify state_text for categorical covariates.
consider using the default with state \"linear\" or use additional_state_text")
    
    if(!(2 %in% state | "linear" %in% state))
      stop("categorical covariates can only be used with state 2 (or \"linear\")")
    
    ##modify state_text for for categorical
    unique_vals <- table(data[[cov]]) %>% sort(decreasing = TRUE)
    unique_vals <- names(unique_vals)
    
    two_text <- sapply(seq_along(unique_vals), function(i){
      val <- unique_vals[i]
      def_text <- paste0("IF(COV.EQ.",val,") PARCOV = ")
      if(i == 1) def_text <- paste0(def_text, 1) else
        def_text <- paste0(def_text, "( 1 + THETA(",i-1,"))")
    })
    
    state_text$"2" <- two_text
    state_text$"linear" <- two_text      
  }
  
  dstate_text <- data.frame(state = names(state_text))
  dstate_text$text <- state_text
  
  par_cov_text <- dstate_text$text[dstate_text$state %in% state]
  par_cov_text <- par_cov_text[[1]]
  par_cov_text <- gsub("PAR", param, par_cov_text)
  par_cov_text <- gsub("COV", cov, par_cov_text)

  if(any(grepl("log\\(median\\)", par_cov_text))){
    ## get data
    data_temp <- tapply(data[[cov]], data$ID, stats::median, na.rm = TRUE)
    value <- signif(stats::median(log(data_temp), na.rm = TRUE), 3)
    if(value>0){
      par_cov_text <- gsub("log\\(median\\)", value , par_cov_text)
    } else {
      par_cov_text <- gsub("-\\s*log\\(median\\)", paste0("+ ", -value) , par_cov_text)
    }
  }
    
  if(any(grepl("median", par_cov_text))){
    ## get data
    data_temp <- tapply(data[[cov]], data$ID, stats::median, na.rm = TRUE)
    value <- signif(stats::median(data_temp, na.rm = TRUE), 3)
    if(value>0){
      par_cov_text <- gsub("median", value , par_cov_text)
    } else {
      par_cov_text <- gsub("-\\s*median", paste0("+ ", -value) , par_cov_text)
    }
  }
  
  ## renumber thetas
  n <- 1
  n_replace <- theta_n_start
  par_cov_text <- gsub("THETA\\(([0-9]+)\\)",
                       "THETA\\(X\\1\\)", 
                       par_cov_text)

  while(TRUE){
    if(!any(grepl(paste0("THETA\\(X",n,"\\)"), par_cov_text))) break
    par_cov_text <- gsub(paste0("(THETA\\()X",n,"(\\))"),
                              paste0("\\1",n_replace,"\\2"),
                              par_cov_text)
    n <- n + 1
    n_replace <- n_replace + 1
  }
  if(n-1 > 30) warning("You're adding ", n-1, " parameters. Are you insane?", call. = FALSE)
  attributes(par_cov_text) <- list(n = n-1)
  par_cov_text

}

#' get OFV
#'
#' @param r object of class nm
#' @include nm.R
#' @export
ofv <- function(r){
  UseMethod("ofv")
}

#' @export
ofv.default <- function(r){
  if(is_single_na(r)) return(NA)
  stop("don't know how to handle type")
}

#' @export
ofv.data.frame <- function(r){
  #if(is_empty_nmcoef(r)) return(NA)
  if(nrow(r) == 0) return(NA)
  r$FINAL[r$parameter %in% "OBJ"]
}

#' @export
ofv.list <- function(r){
  sapply(r, ofv)
}

is_empty_nmcoef <- function(r){
  if(!inherits(r, "nmcoef")) return(FALSE)
  if(!inherits(r, "data.frame")) return(FALSE)
  if(nrow(r) > 0) return(FALSE)
  if(ncol(r) > 0) return(FALSE)
  return(TRUE)
}



update_table_numbers <- function(ctl, run_id){
  if(is_single_na(ctl)) return(NA)
  ctl <- ctl_list(ctl)
  if(missing(run_id)) run_id <- get_run_id(attributes(ctl)$file_name)
  ctl$TABLE <- gsub(paste0("(FILE\\s*=\\s*\\S*tab)\\S*\\b"),paste0("\\1",run_id),ctl$TABLE)
  ctl
}


#' get file name
#' 
#' @param ctl object coercible into ctl_list
#' @export

file_name <- function(ctl){
  ctl <- ctl_list(ctl)
  attr(ctl, "file_name")
}

#' make new control file based on previous
#'
#' @param ctl object of class nmexecute, ctl_list, ctl_character or character
#' @param new_seed character or numeric. new seed
#' @return object of class ctl_character
#' @export

change_seed <- function(ctl,new_seed){
  ctl <- ctl_character(ctl)
  gsub("(^\\$SIM\\S*\\s+\\()[0-9]+(\\).*$)",paste0("\\1",new_seed,"\\2"),ctl)
}

#' convert nonmem code to R ready
#' 
#' This is a developer function
#' 
#' @param code character vector of NONMEM code block
#' @param eta_to_0 logical (default = TRUE) set all etas to 0
#' @export

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
