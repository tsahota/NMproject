#' Read input dataset of an nm object
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Uses `data_path` field of object to locate data and read in.
#' 
#' @param m An nm object.
#' @param filter Logical (default = `FALSE`). Applies NONMEM ignore statement to
#'   filter dataset.
#' @param na Character. Passed to [utils::read.csv()]
#' @param ... Additional arguments passed to either [read_derived_data()] (if
#'   [write_derived_data()] was used to create derived dataset) or
#'   [utils::read.csv()]
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' d <- input_data(m1)
#' 
#' ## only non-ignored rows
#' d_nonignore <- input_data(m1, filter = TRUE)
#' 
#' }
#' 
#' @export
input_data <- function(m, filter = FALSE, na = ".", ...){
  UseMethod("input_data")
}
#' @export
input_data.nm_generic <- function(m, filter = FALSE, na = ".", ...){
  file_name <- data_path(m)
  if(is.na(file_name)) return(dplyr::tibble())
  
  if(normalizePath(dirname(file_name), mustWork = FALSE) == normalizePath("DerivedData", mustWork = FALSE)){
    d <- read_derived_data(basename(tools::file_path_sans_ext(file_name)),...)
  } else {
    d <- utils::read.csv(file_name, na = na, ...)
  }
  
  if(filter) {
    data_filter <- parse(text = data_filter_char(m, data = d))
    d <- subset(d, eval(data_filter))
  }
  d
}
#' @export
input_data.nm_list <- function(m, filter = FALSE, na = ".", ...){
  data_paths <- data_path(m)
  if(length(unique(data_paths)) != 1) stop("multiple data files detected. Aborting...")
  m <- as_nm_generic(m[[1]])
  d <- input_data(m, filter = filter, na = na, ...)
  d
}

#' Get/set ignore statement from control file contents
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' @param ctl An nm object.
#' @param ignore_char Optional character. Ignore statement to set in $DATA.
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' ignore(m1)  ## display ignore statement
#' 
#' m1 %>% ignore("ID > 10") ## changes ignore to ignore IDs > 10.
#' 
#' }
#' 
#' @export
ignore <- function(ctl, ignore_char){
  UseMethod("ignore")
}
#' @export
ignore.nm_generic <- function(ctl, ignore_char){
  m <- ctl
  if(missing(ignore_char)){
    return(data_ignore_char(m))
  }
  ctl <- ctl_contents(m)
  ctl <- update_ignore(ctl, ignore_char)
  m <- m %>% ctl_contents_simple(ctl)
  m
}
#' @export
ignore.nm_list <- Vectorize_nm_list(ignore.nm_generic, replace_arg = "ignore_char")

update_ignore <- function(ctl, ignore_char){
  ctl <- ctl_list(ctl)
  
  ignore_present <- any(grepl(".*IGNORE\\s*=\\s*\\(",ctl$DATA))
  if(ignore_present){
    ## remove any row that matches exactly
    ctl$DATA <- ctl$DATA[!grepl("^(\\s*)IGNORE\\s*=\\s*\\(*\\S[^\\)]+\\)*(\\s*)$",ctl$DATA)]
    ## remove only bracketed IGNORE statement if other things are on the line.
    ctl$DATA <- gsub("(.*)IGNORE\\s*=\\s*\\(+\\S[^\\)]+\\)+(.*)",
                     "\\1\\2", ctl$DATA)
  }
  
  ignore_char <- gsub("\\s*\\|\\s*", ", ", ignore_char)
  
  ignore_char <- gsub("==",".EQ.",ignore_char)
  ignore_char <- gsub("!=",".NE.",ignore_char)
  ignore_char <- gsub(">",".GT.",ignore_char)
  ignore_char <- gsub("<",".LT.",ignore_char)
  ignore_char <- gsub(">=",".GE.",ignore_char)
  ignore_char <- gsub("<=",".LE.",ignore_char)
  ignore_char <- gsub("\\s+(\\.\\S+\\.)\\s+", "\\1", ignore_char)
  
  ignore_char <- paste0("IGNORE=(",ignore_char,")")
  
  last_line <- ctl$DATA[length(ctl$DATA)]
  
  if(grepl("^\\s*$", last_line)){
    ctl$DATA[length(ctl$DATA)] <- ignore_char
  } else {
    ctl$DATA <- append(ctl$DATA, ignore_char) 
  }
  ctl$DATA <- append(ctl$DATA, "")
  ctl
  
}
