## used create nm_list methods
## rule = if single arg, it will simplify (unlist) output i.e. get
##  otherwise it will set
Vectorize_nm_list <- function (FUN, vectorize.args = arg.names, SIMPLIFY = FALSE, USE.NAMES = TRUE, 
                               invisible = FALSE, replace_arg = "text")
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
    ## added m assignment for later, changed SIMPLIFY to false always
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
      if(invisible) return(invisible(m)) else return(m)
    }
    if(is_nm_list(m)) { 
      m <- as_nm_list(m)
    }
    if(invisible) return(invisible(m)) else return(m)
  }
  formals(FUNV) <- formals(FUN)
  FUNV
}

is_nm_list <- function(x){
  if(!inherits(x, "list")) return(FALSE)
  
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

as_nm_list <- function(m){
  UseMethod("as_nm_list")
}
as_nm_list.default <- function(m){
  stop("don't know how to handle type")
}
as_nm_list.nm_list <- function(m){
  m
}
as_nm_list.list <- function(m){
  if(is_nm_list(m)){
    names(m) <- run_id0(m)
    class(m) <- c("nm_list", "list")
    return(m)
  } else {
    stop("list not coercible to nm_list")
  }
}
as_nm_list.nm_generic <- function(m){
  m <- list(m)
  as_nm_list.list(m)
}

as_nm_generic <- function(m){
  UseMethod("as_nm_generic")
}
as_nm_generic.nm_generic <- function(m) m
as_nm_generic.nm_list <- function(m){
  if(length(m)>1) stop("cannot coerce more than one object to nm_generic")
  m <- m[[1]]
  if(!inherits(m, "nm_generic")) stop("could not coerce to nm_generic")
  m
}

c.nm_list <- function(...){
  basic_list <- lapply(list(...), '[[', i = 1)
  as_nm_list(basic_list)
}

c.nm_generic <- function(...){
  basic_list <- list(...)
  as_nm_list(basic_list)
}

'[.nm_list' <- function(x, i, j, ...) {
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