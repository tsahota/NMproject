is_dollar_line <- function(l) grepl("^\\s*;*\\s*\\$",l)
is_nm_comment_line <- function(l) grepl("^\\s*;",l)
rem_dollars <- function(s) gsub("\\s*\\$\\S*\\s*","",s)
rem_comment <- function(s,char=";") gsub(paste0("^([^",char,"]*)",char,"*.*$"),"\\1",s)
get_comment <- function(s,char=";") gsub(paste0("^[^",char,"]*",char,"*(.*)$"),"\\1",s)

setup_dollar <- function(x,type){
  x[1] <- paste(type,x[1])
  names(x) <- NULL
  class(x) <- paste0("nm.",tolower(gsub("^\\$","",type)))
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
      #class(r) <- c("ctl_character", "character")
      #return(r)
      stop("cannot coerce to ctl_character")
    }
  }
  stop("cannot coerce to ctl_character")
}

#' Constructor/converter to ctl_list
#' @param r either class nmexecute, character, ctl_list, ctl_character
#' @return object of class ctl_list
#' @export
ctl_list <- function(r){
  if(inherits(r, "ctl_character")) {
    ctl <- ctl_nm2r(r)
    attr(ctl, "file_name") <- attributes(r)$file_name
    return(ctl)
  }
  if(inherits(r, "nmexecute")) {
    ctl <- ctl_character(r)
    file_name <- attributes(ctl)$file_name
    ctl <- ctl_nm2r(ctl)
    attr(ctl, "file_name") <- file_name
    return(ctl)
  }
  if(inherits(r, "ctl_list")) {
    return(r)
  }
  if(inherits(r, "character")){
    if(length(r) == 1){
      ctl <- ctl_character(r)
      file_name <- attributes(ctl)$file_name
      ctl <- ctl_nm2r(ctl)
      attr(ctl, "file_name") <- file_name
      return(ctl) 
    } else stop("cannot coerce to ctl_list")
  }
  stop("cannot coerce to ctl_list")
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
    class(tmp) <- paste0("nm.",tolower(type))
    ctl2[[i]] <- tmp
  }
  ctl <- ctl2

  x <- sapply(ctl,function(s)class(s))

  ## find consecutive statements and combine them
  ## can use a for loop

  for(i in rev(seq_along(x))){
    if(i==1) break
    if(x[i]==x[i-1]) {
      ctl[[i-1]] <- c(ctl[[i-1]],ctl[[i]])
      class(ctl[[i-1]]) <- class(ctl[[i]])
      ctl[[i]] <- NULL
    }
  }
  names(ctl) <- sapply(ctl,function(s)gsub("NM\\.","",toupper(class(s))))
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
  x <- x[!grepl("^\\s*$",x)]
  x <- gsub("\\t"," ",x)
  x <- x[!grepl("^\\s*;.*",x)]
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


#' Get parameter information
#'
#' @param ctl character. Path to control file
#' @export
param_info <- function(ctl){
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
  final_params <- final_params[,c("Parameter", "FINAL")]
  final_params <- final_params[grepl(type,final_params$Parameter),]

  if(type %in% c("OMEGA","SIGMA")){
    final_params$ROW <- as.numeric(gsub(paste0(type,"\\.([0-9]+)\\..*"),"\\1",final_params$Parameter))
    final_params$COL <- as.numeric(gsub(paste0(type,"\\.[0-9]+\\.([0-9]+).*"),"\\1",final_params$Parameter))
    final_params <- final_params[order(final_params$ROW, final_params$COL), ]
  }

  if(type %in% c("THETA")){
    final_params$ROW <- as.numeric(gsub(paste0(type,"([0-9]+)"),"\\1",final_params$Parameter))
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

#' update parameters from a control stream
#'
#' @param ctl object coercible into ctl_list
#' @param from class nm. object from which to extract results
#' @export

update_parameters <- function(ctl, from){
  if(missing(from) & inherits(ctl, "nmexecute")) from <- ctl
  ctl_lines <- ctl_list(ctl)
  coef_from <- coef.nm(from, trans=FALSE)
  ctl_lines <- update_parameters0(ctl_lines, coef_from, type = "THETA")
  ctl_lines <- update_parameters0(ctl_lines, coef_from, type = "OMEGA")
  ctl_lines <- update_parameters0(ctl_lines, coef_from, type = "SIGMA")
  ctl_lines
}

#' change to estimation control stream to sim
#'
#' @param ctl_lines character vector. ctl file read into R
#' @param subpr numeric. Number of subproblems
#' @param seed numeric
#' @export

change_to_sim <- function(ctl_lines,subpr=1,seed=1){
  ctl_lines <- ctl_nm2r(ctl_lines)
  ctl_lines$EST <- paste0(";",ctl_lines$EST)
  ctl_lines$COV <- paste0(";",ctl_lines$COV)
  if("SIM" %in% names(ctl_lines)){
    ctl_lines$SIM <- gsub("^\\s*;+(.*)","\\1",ctl_lines$SIM)
    ctl_lines$SIM <- gsub("(SUBPR[^=]*\\s*=\\s*)[0-9]+",paste0("\\1",subpr),ctl_lines$SIM)
    ctl_lines$SIM <- gsub("(\\$SIM[^\\s]*\\s*\\()[0-9]+(\\))",paste0("\\1",seed,"\\2"),ctl_lines$SIM)
  } else {
    ## insert before $TABLE, after $ERROR/$PRED
    pred_error_pos <- which(grepl("ERROR|PRED",names(ctl_lines)))
    if(pred_error_pos > 1) stop("multiple $ERROR/$PREDs detected - is this right?")
    ctl_lines <- append(ctl_lines,list(SIM=NA),pred_error_pos)
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
  ctl_r2nm(ctl_lines)
}

#' change to estimation control stream to sim
#'
#' @param ctl object coercible to ctl_list
#' @param param character. Name of parameter
#' @param cov character. Name of covariate
#' @param state numeric. Number of state
#' @param continuous logical (default = TRUE). is covariate continuous?
#' @param time_varying logical (default = FALSE). is the covariate time varying
#' @param custom_state_text optional character. custom state variable to be passed to param_cov_text
#' @export

add_cov <- function(ctl, param, cov, state = 2, continuous = TRUE,
                    time_varying = FALSE, custom_state_text){

  ctl <- ctl_list(ctl)

  PK_section <- rem_comment(ctl$PK)
  
  if(time_varying){
    tvparam <- param
  } else {
    tvparam <- paste0("TV",param)
  }

  existing_param_rel <- any(grepl(paste0("\\b",tvparam,"COV"), PK_section))
  existing_param_cov_rel <- any(grepl(paste0("\\b",tvparam,cov), PK_section))
  if(existing_param_cov_rel) stop("covariate relation already exists, cannot add")

  param_info <- param_info(ctl)
  theta_n_start <- max(param_info$N) + 1

  relation_start_txt <- paste0(";;; ",tvparam,"-RELATION START")
  relation_end_txt <- paste0(";;; ",tvparam,"-RELATION END")

  definition_start_txt <- paste0(";;; ",tvparam,cov,"-DEFINITION START")
  definition_end_txt <- paste0(";;; ",tvparam,cov,"-DEFINITION END")

  if(!existing_param_rel){
    par_relation_text <- paste0(tvparam,"COV=",tvparam,cov)

    ## insert at beginning
    ctl$PK <- c(ctl$PK[1],"",
                relation_start_txt,
                par_relation_text,
                relation_end_txt,
                ctl$PK[-1])

    tv_definition_row <- which(grepl(paste0("^\\s*",tvparam,"\\s*="), rem_comment(ctl$PK)))
    dont_count <- which(grepl(paste0("^\\s*",tvparam,"\\s*=.*",tvparam), rem_comment(ctl$PK)))
    tv_definition_row <- setdiff(tv_definition_row, dont_count)
    if(length(tv_definition_row) > 1) stop("can't find unique TV parameter definition in $PK")
    if(length(tv_definition_row) == 0) stop("can't find TV parameter definition in $PK")

    ctl$PK <- c(ctl$PK[1:tv_definition_row],"",
                paste0(tvparam," = ", tvparam,"COV*",tvparam),
                ctl$PK[(tv_definition_row+1):length(ctl$PK)])

  }

  if(existing_param_rel){
    ctl$PK <- gsub(paste0(tvparam,"COV="),
                   paste0(tvparam,"COV=",tvparam,cov,"*"),ctl$PK)
  }

  ## use state to get the relationship in there.
  data <- suppressMessages(get_data(ctl, filter = TRUE))
  if(!missing(custom_state_text)) {
    param_cov_text <- param_cov_text(param=tvparam,cov=cov,state = state,
                                     data = data,
                                     theta_n_start = theta_n_start,
                                     continuous = continuous,
                                     custom_state_text = custom_state_text)
  } else {
    param_cov_text <- param_cov_text(param=tvparam,cov=cov,state = state,
                                     data = data,
                                     theta_n_start = theta_n_start,
                                     continuous = continuous)
  }

  ctl$PK <- c(ctl$PK[1],"",
              definition_start_txt,
              param_cov_text,
              definition_end_txt,
              ctl$PK[-1])

  ## add thetas
  n_add_thetas <- attr(param_cov_text, "n")
  if(n_add_thetas > 0){
    if(n_add_thetas == 1) {
      theta_lines <- paste0("$THETA  (-1,0.0001,5) ; ",tvparam, cov, state)
    } else {
      theta_lines <- paste0("$THETA  (-1,0.0001,5) ; ",tvparam, cov, state,"_",seq_len(n_add_thetas))
    }
    ctl$THETA <- c(ctl$THETA,theta_lines)
  }

  ctl

}

param_cov_text <- function(param,cov,state,data,theta_n_start,continuous = TRUE,
                           state_text = list(
                             "2" = "PARCOV= ( 1 + THETA(1)*(COV - median))",
                             "3" = c("IF(COV.LE.median) PARCOV = ( 1 + THETA(1)*(COV - median))",
                                     "IF(COV.GT.median) PARCOV = ( 1 + THETA(2)*(COV - median))"),
                             "4" = "PARCOV= EXP(THETA(1)*(COV - median))",
                             "5" = "PARCOV= ((COV/median)**THETA(1))"),
                           custom_state_text, ...){

  if(!missing(custom_state_text)) state_text <- append(state_text, custom_state_text)

  if(!continuous) {

    unique_vals <- table(data[[cov]]) %>% sort(decreasing = TRUE)
    unique_vals <- names(unique_vals)

    two_text <- sapply(seq_along(unique_vals), function(i){
      val <- unique_vals[i]
      def_text <- paste0("IF(COV.EQ.",val,") PARCOV = ")
      if(i == 1) def_text <- paste0(def_text, 1) else
        def_text <- paste0(def_text, "( 1 + THETA(",i-1,"))")
    })

    state_text <- list("2" = two_text)
  }

  dstate_text <- data.frame(state = names(state_text))
  dstate_text$text <- state_text

  par_cov_text <- dstate_text$text[dstate_text$state %in% state]
  par_cov_text <- par_cov_text[[1]]
  par_cov_text <- gsub("PAR", param, par_cov_text)
  par_cov_text <- gsub("COV", cov, par_cov_text)

  if(any(grepl("median", par_cov_text))){
    ## get data
    data_temp <- tapply(data[[cov]], data$ID, stats::median, na.rm = TRUE)
    value <- signif(stats::median(data_temp, na.rm = TRUE), 3)
    par_cov_text <- gsub("median", value , par_cov_text)
  }

  ## renumber thetas
  n <- 1
  n_replace <- theta_n_start
  while(TRUE){
    if(!any(grepl(paste0("THETA\\(",n,"\\)"), par_cov_text))) break
    par_cov_text <- gsub(paste0("(THETA\\()",n,"(\\))"),paste0("\\1",n_replace,"\\2"),
                         par_cov_text)
    n <- n + 1
    n_replace <- n_replace + 1
  }
  attributes(par_cov_text) <- list(n = n-1)
  par_cov_text

}

#' write control file
#'
#' @param ctl object of class character, ctl_character, or ctl_list
#' @param run_id character or numeric. new run_id
#' @param dir character. Directory to place file. Default = getOption("models.dir")
#' @export

write_ctl <- function(ctl, run_id, dir = getOption("models.dir")){

  if(!any(c("ctl_list", "ctl_character", "character", "nmexecute") %in% class(ctl)))
    stop("ctl needs to be class nmexecute, character, ctl_character, or ctl_list")
  
  if(missing(run_id)) run_id <- get_run_id(attributes(ctl)$file_name)

  if(inherits(run_id, "nmexecute")) run_id <- run_id$run_id

  ctl <- ctl_character(ctl)
  
  ctl_name <- search_ctl_name(run_id, models_dir = dir)
  
  attr(ctl, "file_name") <- ctl_name
  
  writeLines(ctl, ctl_name)
  tidyproject::setup_file(ctl_name)
  message("written: ", ctl_name)
  invisible(ctl)

}

update_table_numbers <- function(ctl, run_id){
  ctl <- ctl_list(ctl)
  if(missing(run_id)) run_id <- get_run_id(attributes(ctl)$file_name)
  ctl$TABLE <- gsub(paste0("(FILE\\s*=\\s*\\S*tab)\\S*\\b"),paste0("\\1",run_id),ctl$TABLE)
  ctl
}

#' make new control file based on previous
#'
#' @param r object coercible into ctl_list
#' @param run_id character or numeric. new run_id
#' @param based_on optional character new run_id
#' @export

new_ctl <- function(r, run_id, based_on){
  ctl <- ctl_list(r)
  if(missing(based_on)){
    if(inherits(r, "nm")){
      based_on <- r$run_id
    } else {
      ## get "tab" lines
      temp <- ctl$TABLE[grepl("FILE\\s*=\\s*\\S*tab",ctl$TABLE)]
      if(length(temp) == 0) stop("specify based_on argument")
      ## get unique
      temp <- unique(gsub(".*FILE\\s*=\\s*\\S*tab(\\S*)\\b.*","\\1",temp))
      if(length(temp) != 1) stop("specify based_on argument")
      if(nchar(temp) == 0) stop("specify based_on argument")
      based_on <- temp
    }
  }
  
  if(file.exists(run_id)) file_name <- run_id else
    file_name <- from_models(paste0(getOption("model_file_stub"),run_id,".",getOption("model_file_extn")))
  
  attr(ctl, "file_name") <- file_name
    
  ctl <- update_table_numbers(ctl, run_id)
  ctl[[1]] <- gsub("^(\\s*;;\\s*[0-9]*\\.\\s*Based on:).*",paste("\\1",based_on),ctl[[1]])
  ctl[[1]] <- gsub("^(\\s*;;\\s*\\w*\\.\\s*Author:).*",paste("\\1",Sys.info()["user"]),ctl[[1]])
  ctl
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

#' get data set
#'
#' @param r object coercible into ctl_character
#' @param filter logical (default = FALSE). Should NONMEM ignore filter be applied
#' @param ... additional arguments for read.csv
#' @export
get_data <- function(r, filter = FALSE, ...){
  ## doesn't rely on data base or r object contents
  file_name <- from_models(get_data_name(ctl_character(r)))

  if(normalizePath(dirname(file_name), mustWork = FALSE) == normalizePath("DerivedData")){
    d <- read_derived_data(basename(get_stub_name(file_name)))
  } else {
    d <- utils::read.csv(file_name, na = ".", ...)
  }
  
  if(filter) {
    data_filter <- parse(text = data_filter_char(r))
    d <- subset(d, eval(data_filter))
  }
  d
}


