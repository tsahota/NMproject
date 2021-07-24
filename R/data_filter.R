#' Get ignore statement
#'
#' @param r An object coercible into ctl_list.
#' @param data A `data.frame` (default = missing) optional input dataset from R.
#' 
#' @return Character representation of $DATA IGNORE statement.
#' 
#' @keywords internal

data_ignore_char <- function(r, data) {
  UseMethod("data_ignore_char")
}

data_ignore_char.nm_generic <- function(r, data) {
  dol_data <- r %>% dollar("$DATA")
  dol_data <- dol_data[!dol_data %in% ""]
  dol_data <- rem_comment(dol_data)

  ###
  dol_data <- paste(dol_data, collapse = ";")
  ## remove IGNORE=@
  dol_data <- gsub("IGNORE\\s*=\\s*@", "", dol_data)
  ## remove IGNORE=#
  dol_data <- gsub("IGNORE\\s*=\\s*#", "", dol_data)

  if (any(grepl("IGNORE\\s*=\\s*C\\b", dol_data))) {
    warning("NMproject currently doesn't work with IGNORE=C type runs.
       It is recommended to modify the control file to use IGNORE=@ instead.",
      call. = FALSE
    )
  }

  ignore_present <- any(grepl(".*IGNORE\\s*=\\s*\\(", dol_data))
  accept_present <- any(grepl(".*ACCEPT\\s*=\\s*\\(", dol_data))


  ## can now assume that only one is TRUE

  type <- NA
  if (ignore_present & accept_present) stop("cannot identify ignore columns")
  if (ignore_present) type <- "IGNORE"
  if (accept_present) type <- "ACCEPT"
  if (!ignore_present & !accept_present) {
    return("FALSE")
  } ## do not ignore anything

  ## remove stuff before first IGNORE/ACCEPT
  dol_data <- gsub(paste0(".*?(", type, ".*)"), "\\1", dol_data)
  ## remove IGNORE=
  dol_data <- gsub(
    paste0(type, "\\s*=*"),
    "", dol_data
  )
  # ensure bracketed ignore expression are comma separated
  dol_data <- gsub("\\)\\s+\\(", "),(", dol_data)
  ## remove brackets
  dol_data <- gsub("\\(", "", dol_data)
  dol_data <- gsub("\\)", "", dol_data)
  ## remove spaces
  dol_data <- gsub("\\s*", "", dol_data)
  ## remove blank lines
  dol_data <- gsub(";+", ";", dol_data)
  ## should now be only statemetns with ,; separators

  dol_data <- unlist(strsplit(dol_data, split = "[,;]"))

  if (missing(data)) data <- input_data(r, filter = FALSE)

  r_data_names <- names(data)
  ## now get nonmem names
  dollar_input <- r %>% dollar("INPUT")
  nonmem_data_names <- gsub("\\$\\w+", "", dollar_input)
  nonmem_data_names <- unlist(strsplit(nonmem_data_names, split = "\\s"))
  nonmem_data_names <- nonmem_data_names[!nonmem_data_names %in% ""]
  nonmem_data_names <- gsub("(\\w+)=DROP", "\\1", nonmem_data_names)
  nonmem_data_names <- gsub("\\w+=(\\w+)", "\\1", nonmem_data_names)
  # if(length(r_data_names) != length(nonmem_data_names))
  #  stop("length of items in $INPUT doesn't match dataset")
  name_chart <- data.frame(r_data_names, nonmem_data_names, stringsAsFactors = FALSE)
  name_chart <- name_chart[name_chart$r_data_names != name_chart$nonmem_data_names, ]

  # filter_statements <- paste0(".*",type,"\\s*=\\s*\\((\\S[^\\)]+)\\)*.*")
  # dol_data <- dol_data[grepl(filter_statements, dol_data)]
  # filter_statements <- gsub(filter_statements,"\\1",dol_data)
  # filter_statements <- unlist(strsplit(filter_statements,","))
  filter_statements <- dol_data
  filter_statements <- gsub("\\.EQ\\.", "==", filter_statements)
  filter_statements <- gsub("\\.NE\\.", "!=", filter_statements)
  filter_statements <- gsub("\\.EQN\\.", "==", filter_statements)
  filter_statements <- gsub("\\.NEN\\.", "!=", filter_statements)
  filter_statements <- gsub("\\./E\\.", "!=", filter_statements)
  filter_statements <- gsub("\\.GT\\.", ">", filter_statements)
  filter_statements <- gsub("\\.LT\\.", "<", filter_statements)
  filter_statements <- gsub("\\.GE\\.", ">=", filter_statements)
  filter_statements <- gsub("\\.LE\\.", "<=", filter_statements)

  ## substitute names from
  for (i in seq_len(nrow(name_chart))) {
    nonmem_data_name <- paste0("\\b", name_chart$nonmem_data_names[i], "\\b")
    r_data_name <- name_chart$r_data_names[i]
    filter_statements <- gsub(
      nonmem_data_name,
      r_data_name,
      filter_statements
    )
  }

  filter_statements <- paste(filter_statements, collapse = " | ")
  if ("ACCEPT" %in% type) filter_statements <- paste0("!(", filter_statements, ")")

  filter_statements
}
data_ignore_char.nm_list <- Vectorize_nm_list(data_ignore_char.nm_generic, SIMPLIFY = TRUE)

#' Get filter statement
#'
#' Opposite of [data_ignore_char()].
#'
#' @param r Object coercible into ctl_list.
#' @param ... Arguments passed to data_ignore_char.
#' 
#' @return Negation of character value of [data_ignore_char()].
#' 
#' @keywords internal
data_filter_char <- function(r, ...) {
  ignore_char <- data_ignore_char(r, ...)
  if (ignore_char == "FALSE") {
    return("TRUE")
  }
  ignored <- !grepl("^!\\((.*)\\)", ignore_char)
  accepted <- !ignored
  if (accepted) {
    return(gsub("^!\\((.*)\\)", "\\1", ignore_char))
  } else {
    return(paste0("!(", ignore_char, ")"))
  }
}
