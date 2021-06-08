#' Write derived data file
#'
#' @description
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Will write a dataset and an .RDS version of it to the (by default)
#' "DerivedData" directory.  The main benefit of the .RDS dataset is that
#' functions like [input_data()] and [output_table()] can
#' use it for rapid reading speeding up overall function.
#'
#' @param d A data.frame. Data frame to be saved.
#' @param name Character. Name of file (with or without extension). If not a
#'   path, will save to DerivedData directory.
#' @param ...  Additional arguments to be passed to [utils::write.csv()].
#'
#' @details If there is no "DerivedData" data directory and you are using a
#'   different structure the argument `name` must be a (relative) path to
#'   an existing directory where you want your NONMEM ready dataset to be stored.
#'
#' @seealso [read_derived_data()], [input_data()], [exclude_rows()]
#'
#' @examples
#' \dontrun{
#'
#' ## read a dataset that's been copie into SourceData
#' d <- read.csv("SourceData/orig_data.csv")
#'
#' ## modify it
#' d <- d[d$ID < 10, ]
#'
#' d %>% write_derived_data("DerivedData/data.csv")
#'
#' ## load it again either with
#' d <- read_derived_data("data")
#'
#' ## or more commonly if it is associated with run (e.g. m1),
#' ## you can use input_data() to load it via the nm object
#'
#' d <- input_data(m1)
#'
#' }
#' @export

write_derived_data <- function(d, name, ...){
  UseMethod("write_derived_data")
}

#' @export
write_derived_data.data.frame <- function(d, name, ...){
  
  name <- tools::file_path_sans_ext(name)
  
  if(dirname(name) %in% "."){
    RDS_name <- file.path("DerivedData",paste0(name,".RDS"))
    csv_name <- file.path("DerivedData",paste0(name,".csv"))
  } else {  ## directory is specified
    RDS_name <- paste0(name,".RDS")
    csv_name <- paste0(name,".csv")
  }
  
  d <- as.data.frame(d)
  if(!inherits(d, "data.frame")) stop("d needs to be a data.frame or coercible into one")
  
  dir.create(dirname(RDS_name), showWarnings = FALSE, recursive = TRUE)
  saveRDS(d, file = RDS_name)
  utils::write.csv(d, file = csv_name, na = ".", row.names=FALSE, quote=FALSE, ...)
  
  message("written: ")
  message(RDS_name)
  message(csv_name)
}

#' @export
write_derived_data.list <- function (d, name, ...) 
{
  vectorize.args = c("d", "name")
  SIMPLIFY = TRUE
  USE.NAMES = TRUE
  FUN = write_derived_data.data.frame
  arg_call <- as.list(match.call())[-1L]
  ##########
  args <- lapply(arg_call, eval, parent.frame())
  names <- if (is.null(names(args))) 
    character(length(args))
  else names(args)
  dovec <- names %in% vectorize.args
  do.call("mapply", c(FUN = FUN, args[dovec], MoreArgs = list(args[!dovec]), 
                      SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES))
  ##########
  invisible()
}


#' Read derived data
#'
#' @description
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Read the derived data directly instead of via the nm object which is what
#' [input_data()] does.
#'
#' @param name Name or path of file (with or without extension).
#' @param na Character to be passed to [utils::read.csv()].
#' @param silent Logical (default = `TRUE`). Should messages be suppressed.
#' @param ...  Additional arguments to be passed to [utils::read.csv()].
#' @seealso [write_derived_data()], [input_data()], [exclude_rows()].
#'
#' @examples
#' \dontrun{
#'
#' ## read a dataset that's been copie into SourceData
#' d <- read.csv("SourceData/orig_data.csv")
#'
#' ## modify it
#' d <- d[d$ID < 10, ]
#'
#' d %>% write_derived_data("DerivedData/data.csv")
#'
#' ## load it again either with
#' d <- read_derived_data("data")
#'
#' ## or more commonly if it is associated with run (e.g. m1),
#' ## you can use input_data() to load it via the nm object
#'
#' d <- input_data(m1)
#'
#' }
#' @export

read_derived_data <- function(name, na = ".", silent = FALSE, ...){
  
  if(length(name) != 1) stop("name should have length 1", call. = FALSE)
  
  load_file <- NA
  
  if(file.exists(name)){
    if(grepl("\\.RDS", name)) load_file <- "RDS" else
      if(grepl("\\.csv", name)) load_file <- "csv" else
        stop("file is not RDS or csv")
  } else { ## file doesn't exist
    orig_name <- tools::file_path_sans_ext(name)
    name <- file.path("DerivedData", paste0(orig_name,".RDS"))
    if(file.exists(name)) load_file <- "RDS" else {
      name <- file.path("DerivedData", paste0(orig_name,".csv"))
      if(file.exists(name)) load_file <- "csv" else
        stop("file is not RDS or csv")
    }
  }
  
  ## load_file should be set now
  
  if(identical(load_file, "RDS")){
    if(!silent) message("loading: ", name)
    d <- readRDS(file = name)
  }
  if(identical(load_file, "csv")){
    if(!silent) message("loading: ", name)
    d <- utils::read.csv(name, na = na, ...)
  }
  return(d)
}

#' Exclude rows of NONMEM dataset
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#'
#' A mechanism for excluding outliers during data cleaning.  Create exploratory
#' plots, identify rows of the dataset to be considered outliers for exclusion,
#' and then feed that filtered dataset into this function to exclude them from
#' the dataset.  Requires a corresponding `IGNORE` statement - see argument
#' descriptions for more details.
#' 
#' 
#' @param d A `data.frame` for containing the full NONMEM dataset.  Should
#'   contain a column for identifying excluded rows named with the `exclude_col`
#'   argument.
#' @param dexcl A smaller `data.frame` consisting of rows to be ignored.  Need
#'   not contain all columns of `d` but each column should be present in `d`.
#' @param exclude_col Character (default = `"EXCL"`). Name of a binary exclude
#'   column in `d`. This should be accompanied with a `IGNORE=(EXCL.GT.0)`
#'   statement in $DATA.
#'
#' @return A modified version of `d` with `exclude_col` set to `1` for rows
#'   coinciding with `dexcl`.
#' 
#' @seealso [read_derived_data()], [write_derived_data()]
#' 
#' @examples
#' \dontrun{
#' ## use with dplyr
#' dexcl <- d %>% filter(ID == 23, TIME > 18, TIME < 24) %>% select(ID, TIME, DV, EXCL)
#' dexcl  ## view rows to be excluded
#' d <- d %>% exclude_rows(dexcl)
#' }
#' @export

exclude_rows <- function(d, dexcl, exclude_col = "EXCL"){
  if(any(!names(dexcl) %in% names(d))) stop("dexcl must contain a subset of the columns of d")
  excluded <- do.call(paste, d[, names(dexcl)]) %in% do.call(paste, dexcl)
  if(nrow(dexcl) != length(which(excluded))) stop("couldn't find all rows")
  if(!exclude_col %in% names(d)) d[[exclude_col]] <- 0
  d[[exclude_col]][excluded] <- 1
  d
}

