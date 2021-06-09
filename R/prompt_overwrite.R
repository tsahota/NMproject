prompt_overwrite <- function(paths, new_path_contents = c()) {
  if (length(paths) == 0) {
    return(NA_character_)
  }

  behaviour <- overwrite_behaviour()
  if (!"ask" %in% behaviour) {
    return(NA_character_)
  }

  question <- "The following directories & files will be overwritten"
  msg <- paste(paste(paths, collapse = "\n "))

  if (length(new_path_contents) > 0 & requireNamespace("diffobj")) {
    if (length(paths) > 1) stop("can only do diff with one path")
    if (!is.character(new_path_contents)) stop("expecting character vector for new_path_contents")
    ## can now assume one path
    old_contents <- readLines(paths)
    new_contents <- new_path_contents
    attributes(new_contents) <- NULL
    if (!identical(new_contents, old_contents)) {
      diff_val <- diffobj::diffChr(old_contents, new_contents, format = "raw")
      if (length(as.character(diff_val)) <= 30) { ## diff limit
        msg <- paste(msg, "\n", paste(diff_val, collapse = "\n"))
      }
    } else {
      ## if same, no need to prompt
      return(NA_character_)
    }
  }

  if (rstudioapi::isAvailable()) {
    proceed <- rstudioapi::showQuestion(question, msg,
      ok = "overwrite",
      cancel = "abort"
    )
  } else {
    message(question)
    message(msg)
    message("overwrite and proceed?")
    ans <- readline("Type \"y\" or \"n\": ")
    proceed <- tolower(ans) == "y"
    if (length(proceed > 1)) stop("only one answer allowed", call. = FALSE)
  }
  if (!proceed) stop("aborting", call. = FALSE)
}
