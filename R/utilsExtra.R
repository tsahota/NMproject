## used create nm_list methods
## rule = if single arg, it will simplify (unlist) output i.e. get
##  otherwise it will set
Vectorize_nm_list <- function (FUN, vectorize.args = arg.names, SIMPLIFY = FALSE, USE.NAMES = TRUE, 
                               invisible = FALSE, replace_arg = "text", pre_glue = FALSE,
                               exclude_classes = c("ggplot"))
{
  missing_SIMPLIFY <- missing(SIMPLIFY)
  arg.names <- as.list(formals(FUN))
  arg.names[["..."]] <- NULL
  arg.names <- names(arg.names)
  vectorize.args <- as.character(vectorize.args)
  if (!length(vectorize.args)) 
    return(FUN)
  if (!all(vectorize.args %in% arg.names)) 
    stop("must specify names of formal arguments for 'vectorize'")
  collisions <- arg.names %in% c("FUN", "SIMPLIFY", "USE.NAMES", 
                                 "vectorize.args")
  if (any(collisions)) 
    stop(sQuote("FUN"), " may not have argument(s) named ", 
         paste(sQuote(arg.names[collisions]), collapse = ", "))
  FUNV <- function() {
    args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
    ## MODIFIED: additional line to ensure if no args, the function is executed once
    if(length(args) == 0) args = formals(FUN)
    names <- if (is.null(names(args))) 
      character(length(args))
    else names(args)
    dovec <- names %in% vectorize.args
    ## added following to exclude certain classes from vectorisation
    skip <- sapply(args, function(arg) any(class(arg) %in% exclude_classes))
    names(skip) <- NULL
    dovec <- dovec & !skip
    ## glue replace arg
    if(pre_glue & length(args[dovec]) > 0 & replace_arg %in% names(args[dovec])){
      
      ## create an index data.frame to get replace_arg the right length
      di <- data.frame(
        i_1 = seq_along(args[dovec][[1]]),
        i_replace = seq_along(args[dovec][[replace_arg]])
      )
      
      ## fill replace_arg
      args[dovec][[replace_arg]] <- args[dovec][[replace_arg]][di$i_replace]
      
      for(i in seq_along(args[dovec][[replace_arg]])){
        replace_arg_value <- args[dovec][[replace_arg]][i]
        m <- args[dovec][[1]][[i]] ## nm_generic
        if (is.character(replace_arg_value)) {
          args[dovec][[replace_arg]][i] <- stringr::str_glue(replace_arg_value, 
                                                             .envir = m)
        }
      }
    }
    ## added m assignment for later, changed SIMPLIFY to false always
    # if(one_d_if_single_nm_list & 
    #    is_nm_list(args[dovec][[1]]) & length(args[dovec][[1]]) == 1){ ## if just a single run, just run FUN
    #   dovec <- rep(FALSE, length = length(dovec))
    #   dovec[1] <- TRUE ## make first one (nm object) true
    # }
    m <- do.call("mapply", c(FUN = FUN, args[dovec], MoreArgs = list(args[!dovec]), 
                             SIMPLIFY = FALSE, USE.NAMES = USE.NAMES))
    ## modified rest of this (inner) function
    ## if missing simplify will use rule of whether only m is supplied
    ## if so, simplify
    if(missing_SIMPLIFY) {
      ## if all replace args are not present - this is a getter function
      if(all(!replace_arg %in% names(args))) SIMPLIFY <- TRUE
      #SIMPLIFY <- length(args) <= n_args_to_simplify
    }
    
    if(SIMPLIFY) {
      m <- unlist(m)
      names(m) <- NULL
      return(m)
      #if(invisible) return(invisible(m)) else return(m)
    }
    if(is_nm_list(m)) { 
      m <- as_nm_list(m)
    }
    if(invisible) return(invisible(m)) else return(m)
  }
  formals(FUNV) <- formals(FUN)
  FUNV
}

#' @export
is_nm_list <- function(x){
  if(!inherits(x, "list") & !inherits(x, "environment")) return(FALSE)
  
  is_valid_subobject <- function(x){
    #if(is_single_na(x)) return(TRUE) ## na's allowed
    inherits(x, "nm_generic")
  }
  
  all(sapply(x, is_valid_subobject))
}

run_id0 <- function(m){
  val <- sapply(m, function(m) m[["run_id"]])
  names(val) <- NULL
  val
}

#' @export
as_nm_list <- function(m){
  UseMethod("as_nm_list")
}
#' @export
as_nm_list.default <- function(m){
  stop("don't know how to handle type")
}
#' @export
as_nm_list.nm_list <- function(m){
  m
}
#' @export
as_nm_list.list <- function(m){
  if(is_nm_list(m)){
    class(m) <- c("nm_list", "list")
    names(m) <- unique_id(m)#paste0(run_id0(m),":",type(m),":",run_in(m))
    return(m)
  } else {
    stop("list not coercible to nm_list")
  }
}
#' @export
as_nm_list.nm_generic <- function(m){
  m <- list(m)
  as_nm_list.list(m)
}

#' @export
is_nm_generic <- function(x){
  inherits(x, "nm_generic")
}

#' @export
as_nm_generic <- function(m){
  UseMethod("as_nm_generic")
}
#' @export
as_nm_generic.nm_generic <- function(m) m
#' @export
as_nm_generic.nm_list <- function(m){
  if(length(m)>1) stop("cannot coerce more than one object to nm_generic")
  m <- m[[1]]
  if(!inherits(m, "nm_generic")) stop("could not coerce to nm_generic")
  m
}

#' @export
c.nm_list <- function(...){
  ## try append
  basic_list <- lapply(list(...), function(ob){
    class(ob) <- "list"
    ob
  })
  basic_list <- do.call(c, basic_list)
  as_nm_list(basic_list)
  # 
  # basic_list <- lapply(list(...), '[[', i = 1)
  # as_nm_list(basic_list)
}

#' @export
c.nm_generic <- function(...){
  basic_list <- list(...)
  as_nm_list(basic_list)
}

#' @export
'[.nm_list' <- function(x, i, j, ...) {
  class(x) <- "list"
  val <- NextMethod()
  as_nm_list(val)
}

#' @export
unique.nm_list <- function(x, incomparables = FALSE, ...) {
  class(x) <- "list"
  val <- NextMethod()
  as_nm_list(val)
}

## experimental - goes against dplyr, maybe delete if not useful
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  condition <- condition %in% TRUE
  .data[condition, ] <- .data[condition, ] %>% dplyr::mutate(...)
  .data
}

## generic already defined
## internal function
data_ignore_char.nm_generic <- function(r){
  dol_data <- r %>% target("$DATA") %>% text
  dol_data <- dol_data[!dol_data %in% ""]
  dol_data <- rem_comment(dol_data)
  
  ignore_present <- any(grepl(".*IGNORE\\s*=\\s*\\(",dol_data))
  accept_present <- any(grepl(".*ACCEPT\\s*=\\s*\\(",dol_data))
  
  type <- NA
  if(ignore_present & accept_present) stop("cannot identify ignore columns")
  if(ignore_present) type <- "IGNORE"
  if(accept_present) type <- "ACCEPT"
  no_filter <- is.na(type)
  
  if(!no_filter){
    filter_statements <- paste0(".*",type,"\\s*=\\s*\\((\\S[^\\)]+)\\)*.*")
    dol_data <- dol_data[grepl(filter_statements, dol_data)]
    filter_statements <- gsub(filter_statements,"\\1",dol_data)
    filter_statements <- unlist(strsplit(filter_statements,","))
    filter_statements <- gsub("\\.EQ\\.","==",filter_statements)
    filter_statements <- gsub("\\.NE\\.","!=",filter_statements)
    filter_statements <- gsub("\\.EQN\\.","==",filter_statements)
    filter_statements <- gsub("\\.NEN\\.","!=",filter_statements)
    filter_statements <- gsub("\\./E\\.","!=",filter_statements)
    filter_statements <- gsub("\\.GT\\.",">",filter_statements)
    filter_statements <- gsub("\\.LT\\.","<",filter_statements)
    filter_statements <- gsub("\\.GE\\.",">=",filter_statements)
    filter_statements <- gsub("\\.LE\\.","<=",filter_statements)
    filter_statements <- paste(filter_statements, collapse= " | ")
    if("ACCEPT" %in% type) filter_statements <- paste0("!(",filter_statements,")")
  } else {
    filter_statements <- "FALSE"
  }
  filter_statements
}
data_ignore_char.nm_list <- Vectorize_nm_list(data_ignore_char.nm_generic, SIMPLIFY = TRUE)


gsub_in_brackets <- function(pattern, replacement, x){
  x <- gsub("\\(", "~(", x)
  x <- gsub("\\)", ")~", x)
  x <- paste0(x, collapse = "\n")
  x <- strsplit(x, split = "~")[[1]]
  x[grepl("^\\(.*\\)",x)] <- 
    gsub(pattern, replacement, x[grepl("^\\(.*\\)",x)])
  x <- paste(x, collapse = "")
  x <- paste(x, " ")  ## added to make sure final \n doesn't shorten vector
  x <- strsplit(x, split = "\n")[[1]]
  x[length(x)] <- trimws(x[length(x)])
  x
}

gsub_out_brackets <- function(pattern, replacement, x){
  x <- gsub("\\(", "~(", x)
  x <- gsub("\\)", ")~", x)
  x <- paste0(x, collapse = "\n")
  x <- strsplit(x, split = "~")[[1]]
  x[!grepl("^\\(.*\\)",x)] <- 
    gsub(pattern, replacement, x[!grepl("^\\(.*\\)",x)])
  x <- paste(x, collapse = "")
  x <- paste(x, " ")  ## added to make sure final \n doesn't shorten vector
  x <- strsplit(x, split = "\n")[[1]]
  x[length(x)] <- trimws(x[length(x)])
  x
}

na.locf <- function(x) {
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v)+1]
}

