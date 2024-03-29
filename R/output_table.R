#' Get NONMEM output tables
#'
#' This function may become deprecated soon. The function [output_table()] is a
#' more user friendly wrapper for this function.
#'
#' @param r An nm object.
#' @param dorig Optional `data.frame`. NONMEM input dataset.
#' @param ... Additional arguments to pass on to [read.csv()].
#'
#' @return A list of `tibble`s with merged version of all output $TABLEs and the
#'   input data.  Additional columns will be `INNONMEM` which will be TRUE for
#'   rows that were not ignored by NONMEM.  For simulation control files there
#'   is also `DV_OUT` which will contain simulated `DV` values. `DV` will always
#'   be unmodified from the input dataset.
#'
#' @keywords internal
#' @export

nm_output <- function(r, dorig, ...) {
  UseMethod("nm_output")
}

#' @export
nm_output.nm_generic <- function(r, dorig, ...) {
  r <- as_nm_generic(r) ## because nm_list method is identical
  wait_finish(r)
  
  out_file_info <- r %>% dollar("TABLE") %>% paste(collapse = "\n") %>% strsplit("\\$", perl = TRUE)
  out_file_info <- out_file_info[[1]] %>%
    lapply(function(sub_text) {
      sub_text <- paste(sub_text, collapse = "\n")
      filename_exists <- grepl(".*?FILE\\s*=\\s*(\\S+)\\b.*", sub_text)
      if (!filename_exists) return(data.frame())
      filename <- gsub(".*?FILE\\s*=\\s*(\\S+)\\b.*", "\\1", sub_text)
      format_exists <- grepl(".*?FORMAT\\s*=\\s*(\\S+)\\b.*", sub_text)
      format <- gsub(".*?FORMAT\\s*=\\s*(\\S+)\\b.*", "\\1", sub_text)
      if (!format_exists) format <- NA_character_
      comma_sep <- grepl(",", format)
      data.frame(filename, format, comma_sep)
    }) %>%
    dplyr::bind_rows()

  d <- lapply(seq_len(nrow(out_file_info)), function(i) {
    
    out_path <- file.path(run_in(r), out_file_info$filename[i])
    if (out_file_info$comma_sep[i]) {
      d <- nm_read_table(out_path, skip = 1, header = TRUE, sep = ",")      
    } else {
      d <- nm_read_table(out_path, skip = 1, header = TRUE)
    }

  })

  ## TODO: this will break if some tables have FIRSTONLY
  nrows <- sapply(d, nrow)
  if (length(unique(nrows[!nrows %in% 0])) > 1) {
    stop("output tables are different sizes")
  }

  d <- do.call(cbind, d)
  d <- d[, !duplicated(names(d))]

  if (missing(dorig)) dorig <- input_data(r, ...)

  filter_statements <- data_filter_char(r)
  if (identical(filter_statements, "TRUE")) {
    dORD <- seq_len(nrow(dorig))
  } else {
    expre <- parse(text = filter_statements)
    ## temporarily put NAs to zero for applying filtering
    ## this is what NONMEM does where "." = 0
    ## except column C
    dorig_zeros <- dorig
    dorig_zeros_match <- is.na(dorig_zeros)
    if ("C" %in% colnames(dorig_zeros_match)) {
      dorig_zeros_match[, "C"] <- FALSE
    }
    dorig_zeros[dorig_zeros_match] <- 0
    dORD <- which(with(dorig_zeros, eval(expre)))
  }

  if (nrow(d) %% length(dORD) != 0) {
    stop("something wrong... when R reads in original dataset
         and applies filter ", filter_statements, ",
         there's ", length(dORD), "rows, but NONMEM output has ", nrow(d), " rows")
  }

  ctl_contents <- ctl_character(ctl_contents(r))
  sim_ctl <- any(grepl("^\\s*\\$SIM", rem_comment(ctl_contents)))

  nreps <- nrow(d) / length(dORD)

  if ("PRKEY" %in% names(d)) stop("name conflict with PRKEY in xpose table. aborting...")
  if ("PRKEY" %in% names(dorig)) stop("name conflict with PRKEY in original data. aborting...")

  d$PRKEY <- dORD
  dorig$PRKEY <- 1:nrow(dorig)
  if (sim_ctl) {
    if ("SIM" %in% names(d)) stop("name conflict with SIM in xpose table. aborting...")
    if ("SIM" %in% names(dorig)) stop("name conflict with SIM in original data. aborting...")
    d$SIM <- rep(1:nreps, each = length(dORD))
    message("Adding column: SIM")
  }

  d$INNONMEM <- TRUE

  ## want a DV_OUT columsn
  if ("DV_OUT" %in% names(d)) warning("name conflict with DV_OUT in xpose table. replacing...")
  d$DV_OUT <- d$DV
  d$DV <- NULL
  d <- d[, c(setdiff(names(d), names(dorig)[!names(dorig) %in% c("PRKEY")]))]
  # dorig <- dorig[,names(dorig)[!names(dorig) %in% c("DV")]]

  # d$.tempORD <- 1:nrow(d) ## to preserve order (old code merge())
  if (!sim_ctl) {
    d2 <- dplyr::full_join(d, dorig, by = "PRKEY")    
  } else {
    d2 <- dplyr::left_join(d, dorig, by = "PRKEY")    
  }

  # d2 <- d2[order(d2$.tempORD), ]
  # d2$.tempORD <- NULL

  d2$INNONMEM <- d2$INNONMEM %in% TRUE
  if (nreps > 1) d2$SIM[is.na(d2$SIM)] <- 0

  ## row number check
  if (!sim_ctl) if (nrow(d2) != nrow(d) * (nreps - 1) / nreps + nrow(dorig)) stop("merge went wrong. debug")

  message("Adding column: PRKEY")

  return(d2)
}

#' @export
nm_output.nm_list <- nm_output.nm_generic

#' @name output_table
#' @rdname output_table
#' @title Reads all $TABLE outputs and merge with input dataset
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Produces a single merged output dataset will all columns of $INPUT dataset.
#' This is useful for reuse of exploratory data plots as diagnostic plots as all
#' columns including text columns used for `ggplot` facetting will be present.
#'
#' @param r An object of class nm.
#' @param only_append Optional character vector. If missing will append all,
#'   otherwise will append only those variables requested.
#' @param ... Optional additional arguments to pass on to read.csv of orig data.
#'
#' @return A list of `tibble`s with merged version of all output $TABLEs and the
#'   input data.  Additional columns will be `INNONMEM` which will be TRUE for
#'   rows that were not ignored by NONMEM.  For simulation control files there
#'   is also `DV_OUT` which will contain simulated `DV` values. `DV` will always
#'   be unmodified from the input dataset.
#'
#' @seealso [nm_render()], [input_data()]
#'
#' @examples
#'
#' ## requires NONMEM to be installed
#'
#' \dontrun{
#'
#' ## exploratory data plot
#' read_derived_data("DerivedData/data.csv") %>%
#'   ggplot(aes(x = TIME, y = DV)) +
#'   theme_bw() +
#'   geom_point() +
#'   geom_line(aes(group = ID)) +
#'   facet_wrap(~STUDYTXT)
#'
#' m1 %>%
#'   output_table_first() %>%
#'   ggplot(aes(x = TIME, y = DV)) +
#'   theme_bw() +
#'   geom_point() +
#'   geom_line(aes(group = ID)) +
#'   facet_wrap(~STUDYTXT) +
#'   ## additional layer for overlaying IPRED curves
#'   geom_line(aes(y = IPRED, group = ID))
#' }
#'
#' @export
output_table <- function(r, only_append = c(), ...) {
  UseMethod("output_table")
}

#' @export
output_table.default <- function(r, only_append = c(), ...) {
  out_path <- file.path(run_dir_path(r), "NMout.RDS")
  if (!file.exists(out_path)) {
    do <- nm_output(r, ...)
    saveRDS(do, file = out_path)
  } else {
    do <- readRDS(out_path)
  }
  if (length(only_append) > 0) {
    do <- do[, c(names(input_data(r)), only_append)]
  }
  return(dplyr::as_tibble(do))
}

#' @export
output_table.nm_generic <- output_table.default
#' @export
output_table.nm_list <- Vectorize_nm_list(output_table.nm_generic, SIMPLIFY = FALSE)

#' @rdname output_table
#' @return `output_table_first` will return a `tibble` with a single run.
#' @export
output_table_first <- function(r, ...) {
  UseMethod("output_table_first")
}

#' @export
output_table_first.nm_list <- function(r, ...) {
  if (length(r) > 1) stop("only works on length 1 objects", call. = FALSE)
  outtab <- output_table(r, ...)
  outtab <- outtab[[1]]
  outtab
}

ctl_table_paths <- function(ctl) {
  UseMethod("ctl_table_paths")
}

ctl_table_paths.nm_generic <- function(ctl) {
  ## path should go from base directory
  ## in psn directory
  file.path(output_location(ctl), ctl_table_files(ctl_contents(ctl)))
}

ctl_table_paths.nm_list <- Vectorize_nm_list(ctl_table_paths.nm_generic, SIMPLIFY = FALSE)

output_location <- function(m) file.path(run_in(m), dirname(lst_path(m)))
