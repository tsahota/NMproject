Vectorize_nm_list <- function(FUN, vectorize.args = arg.names, SIMPLIFY = FALSE, USE.NAMES = TRUE,
                              invisible = FALSE, replace_arg = "text", pre_glue = FALSE,
                              exclude_classes = c("ggplot"),
                              lazy_eval = c(),
                              non_lazy_eval = c()) {
  ## used create nm_list methods
  ## rule = if single arg, it will simplify (unlist) output i.e. get
  ##  otherwise it will set
  if (length(lazy_eval) > 0 & length(non_lazy_eval) > 0) {
    stop("cannot have both lazy_eval and non_lazy_eval specified")
  }

  missing_SIMPLIFY <- missing(SIMPLIFY)
  arg.names <- as.list(formals(FUN))
  arg.names[["..."]] <- NULL
  arg.names <- names(arg.names)
  vectorize.args <- as.character(vectorize.args)
  if (!length(vectorize.args)) {
    return(FUN)
  }
  if (!all(vectorize.args %in% arg.names)) {
    stop("must specify names of formal arguments for 'vectorize'")
  }
  collisions <- arg.names %in% c(
    "FUN", "SIMPLIFY", "USE.NAMES",
    "vectorize.args"
  )
  if (any(collisions)) {
    stop(
      sQuote("FUN"), " may not have argument(s) named ",
      paste(sQuote(arg.names[collisions]), collapse = ", ")
    )
  }
  FUNV <- function() {
    args <- as.list(match.call())[-1L]
    ## if non_lazy_eval is specified, convert to lazy_eval
    if (length(non_lazy_eval) > 0) lazy_eval <- setdiff(names(args), non_lazy_eval)

    args_eval <- lapply(args[!names(args) %in% lazy_eval], eval, parent.frame())
    args_lazy <- args[names(args) %in% lazy_eval]
    args <- c(args_eval, args_lazy)[names(args)]
    ## MODIFIED: additional line to ensure if no args, the function is executed once
    if (length(args) == 0) args <- formals(FUN)
    names <- if (is.null(names(args))) {
      character(length(args))
    } else {
      names(args)
    }
    dovec <- names %in% vectorize.args
    ## added following to exclude certain classes from vectorisation
    skip <- sapply(args, function(arg) any(inherits(arg, exclude_classes)))
    names(skip) <- NULL
    dovec <- dovec & !skip
    ## glue replace arg
    if (pre_glue & length(args[dovec]) > 0 & replace_arg %in% names(args[dovec])) {

      ## create an index data.frame to get replace_arg the right length
      di <- data.frame(
        i_1 = seq_along(args[dovec][[1]]),
        i_replace = seq_along(args[dovec][[replace_arg]])
      )

      ## fill replace_arg
      args[dovec][[replace_arg]] <- args[dovec][[replace_arg]][di$i_replace]

      for (i in seq_along(args[dovec][[replace_arg]])) {
        replace_arg_value <- args[dovec][[replace_arg]][i]
        m <- args[dovec][[1]][[i]] ## nm_generic
        if (is.character(replace_arg_value)) {
          args[dovec][[replace_arg]][i] <- stringr::str_glue(replace_arg_value,
            .envir = m
          )
        }
      }
    }
    ## added m assignment for later, changed SIMPLIFY to false always
    # if(one_d_if_single_nm_list &
    #    is_nm_list(args[dovec][[1]]) & length(args[dovec][[1]]) == 1){ ## if just a single run, just run FUN
    #   dovec <- rep(FALSE, length = length(dovec))
    #   dovec[1] <- TRUE ## make first one (nm object) true
    # }
    m <- do.call("mapply", c(
      FUN = FUN, args[dovec], MoreArgs = list(args[!dovec]),
      SIMPLIFY = FALSE, USE.NAMES = USE.NAMES
    ))
    ## modified rest of this (inner) function
    ## if missing simplify will use rule of whether only m is supplied
    ## if so, simplify
    if (missing_SIMPLIFY) {
      ## if all replace args are not present - this is a getter function
      if (all(!replace_arg %in% names(args))) SIMPLIFY <- TRUE
      # SIMPLIFY <- length(args) <= n_args_to_simplify
    }

    if (SIMPLIFY) {
      m <- unlist(m)
      names(m) <- NULL
      return(m)
      # if(invisible) return(invisible(m)) else return(m)
    }
    if (is_nm_list(m)) {
      m <- as_nm_list(m)
    }
    if (invisible) {
      return(invisible(m))
    } else {
      return(m)
    }
  }
  formals(FUNV) <- formals(FUN)
  FUNV
}
