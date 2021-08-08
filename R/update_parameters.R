update_parameters0 <- function(ctl, coef_from, type = c("THETA", "OMEGA", "SIGMA")) {
  type <- match.arg(type)

  ctl_lines <- ctl
  params <- ctl_lines[[type]]

  contents <- rem_comment(params)
  comments <- get_comment(params)
  comments[!grepl("^\\s*$", comments)] <- paste0(";", comments[!grepl("^\\s*$", comments)])

  final_params <- coef_from
  final_params <- final_params[, c("parameter", "FINAL")]
  final_params <- final_params[grepl(type, final_params$parameter), ]

  if (type %in% c("OMEGA", "SIGMA")) {
    final_params$ROW <- as.numeric(gsub(paste0(type, "\\.([0-9]+)\\..*"), "\\1", final_params$parameter))
    final_params$COL <- as.numeric(gsub(paste0(type, "\\.[0-9]+\\.([0-9]+).*"), "\\1", final_params$parameter))
    final_params <- final_params[order(final_params$ROW, final_params$COL), ]
  }

  if (type %in% c("THETA")) {
    final_params$ROW <- as.numeric(gsub(paste0(type, "([0-9]+)"), "\\1", final_params$parameter))
    final_params <- final_params[order(final_params$ROW), ]
  }

  contents1 <- paste0(paste0(contents, collapse = "\n"), "\n")
  contents1 <- gsub("\\$THETA", "$THETA&", contents1)
  contents1 <- gsub("(\\$OMEGA BLOCK\\s*\\([0-9]+\\))", "\\1&", contents1)
  contents1 <- gsub("(\\$OMEGA)( [^B])", "\\1&\\2", contents1)
  contents1 <- gsub("(\\$SIGMA BLOCK\\s*\\([0-9]+\\))", "\\1&", contents1)
  contents1 <- gsub("(\\$SIGMA)( [^B])", "\\1&\\2", contents1)

  contents1 <- gsub(",\\s*", ",", contents1)
  contents2 <- gsub("([-\\.0-9])[ \\t]+([-\\.0-9])", "\\1&\\2", contents1)
  while (!identical(contents2, contents1)) {
    contents1 <- contents2
    contents2 <- gsub("([-\\.0-9])[ \\t]+([-\\.0-9])", "\\1&\\2", contents1)
  }

  contents1 <- strsplit(contents1, "(?<=\\n)", perl = TRUE)[[1]]
  contents1 <- gsub("\\n", "_", contents1)

  contents1 <- unlist(strsplit(contents1, "(?<=&)", perl = TRUE))

  matched_replacements <- grepl("^(\\s*&?\\s*_?\\s*)-?\\.?[0-9]+\\.?[0-9]*E*e*-*\\+*[0-9]*(\\s*(FIX)?&?\\s*_?\\s*)$", contents1) |
    grepl(".*SAME.*", contents1) |
    grepl("(^\\s*&?\\s*_?\\s*\\(.*,).*(\\)\\s*(FIX)?&?\\s*_?\\s*)$", contents1) |
    grepl("(^\\s*&?\\s*_?\\s*\\(.*,).*(,.*\\)\\s*(FIX)?&?\\s*_?\\s*)$", contents1)

  if (type %in% c("THETA")) {
    if (length(which(matched_replacements)) > nrow(final_params)) stop("something wrong. debug")
    if (length(which(matched_replacements)) != nrow(final_params)) warning("different numbers of parameters in outputs. Are you using $PRIOR?")
  }

  for (j in seq_along(which(matched_replacements))) {
    i <- which(matched_replacements)[j]
    contents1[i] <- gsub(
      "^(\\s*&?\\s*_?\\s*)-?\\.?[0-9]+\\.?[0-9]*E*-*\\+*[0-9]*(\\s*(FIX)?&?\\s*_?\\s*)$",
      paste0("\\1", signif(final_params$FINAL[j], 5), "\\2"), contents1[i]
    )
    contents1[i] <- gsub(
      "(^\\s*&?\\s*_?\\s*\\([^,]*,)[^,]*(,?[^,]*\\)\\s*(FIX)?&?\\s*_?\\s*)$",
      paste0("\\1", signif(final_params$FINAL[j], 5), "\\2"), contents1[i]
    )
    # contents1[i] <- gsub("(^\\s*&?\\s*_?\\s*\\(.*,).*(\\)\\s*(FIX)?&?\\s*_?\\s*)$",
    #                     paste0("\\1",signif(final_params$FINAL[j],5),"\\2"),contents1[i])
    contents1[i] <- gsub(
      "(^\\s*&?\\s*_?\\s*\\(.*,).*(,.*\\)\\s*(FIX)?&?\\s*_?\\s*)$",
      paste0("\\1", signif(final_params$FINAL[j], 5), "\\2"), contents1[i]
    )
  }

  contents1 <- paste0(contents1, collapse = " ")
  contents1 <- strsplit(contents1, "_")[[1]]
  contents1 <- gsub("&", " ", contents1)

  new_params <- paste(contents1, comments)
  class(new_params) <- paste0("nm.", tolower(type))

  ctl_lines[[type]] <- new_params

  ctl_lines
}

#' Update initial estimates to final estimates
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' @param ctl An nm object.
#' @param from Optional nm object. The completed object from which to extract
#'   results.  If not specified, `from` will be taken to be `ctl`.
#'
#' @return An nm object with modified `ctl_contents` field.
#'
#' @examples 
#' 
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#'              
#'
#' m1 %>% dollar("THETA")
#' 
#' ## requires NONMEM to be installed
#' \dontrun{
#' m1 %>% run_nm() %>% wait_finish()
#' m1 <- m1 %>% update_parameters()
#' m1 %>% dollar("THETA")
#' }
#' @export

update_parameters <- function(ctl, from) {
  UseMethod("update_parameters")
}


#' @export
update_parameters.nm_generic <- function(ctl, from) {
  m <- ctl
  ctl <- m %>% ctl_contents()
  if (missing(from)) from <- m
  wait_finish(from)
  ctl_lines <- ctl

  coef_from <- coef(from, trans = FALSE)
  ctl_lines <- update_parameters0(ctl_lines, coef_from, type = "THETA")
  # message("cannot update IOV model parameters")
  ctl_lines <- update_parameters0(ctl_lines, coef_from, type = "OMEGA")
  ctl_lines <- update_parameters0(ctl_lines, coef_from, type = "SIGMA")

  m <- m %>% ctl_contents_simple(ctl_lines)
  m
}
#' @export
update_parameters.nm_list <- Vectorize_nm_list(update_parameters.nm_generic, SIMPLIFY = FALSE)
