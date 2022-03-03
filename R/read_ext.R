read_ext0 <- function(ext.file) {
  ## Raw function to read in and format an ext.file.
  if (!file.exists(ext.file)) {
    return(data.frame())
  } # stop("ext file for run does not exist yet",call. = FALSE)
  s <- scan(ext.file, what = "character", sep = "\n", quiet = TRUE)
  tab.rows <- grep("TABLE", s)
  if (length(tab.rows) == 0) { ## file incomplete
    return(data.frame())
  }
  cut.points <- c(tab.rows, length(s) + 1)

  headings <- s[tab.rows]
  headings <- gsub("^TABLE NO.\\s+[0-9]+:\\s", "", headings)
  headings <- gsub(": Goal.*", "", headings)

  dlist <- lapply(seq_along(tab.rows), function(i) {
    if ((cut.points[i] + 1) > (cut.points[i + 1] - 1)) {
      return(data.frame())
    }
    d <- s[(cut.points[i] + 1):(cut.points[i + 1] - 1)]
    tmp <- file()
    writeLines(d, tmp)
    d <- utils::read.table(tmp, header = TRUE)
    d$EST.NO <- i
    d$EST.NAME <- headings[i]
    match_obj <- grepl("OBJ$", names(d))
    if (length(which(match_obj)) > 1) stop("more than one OBJ column. debug")
    names(d)[match_obj] <- "OBJ"
    d$OBJ <- as.numeric(as.character(d$OBJ))
    d$TYPE <- NA
    d$TYPE[d$ITERATION >= 0] <- "ITER"
    d$TYPE[d$ITERATION > -1000000000 & d$ITERATION < 0] <- "BURN"
    d$TYPE[d$ITERATION == -1000000000] <- "FINAL"
    d$TYPE[d$ITERATION == -1000000001] <- "SE"
    d$TYPE[d$ITERATION == -1000000002] <- "EIGEN"
    d$TYPE[d$ITERATION == -1000000003] <- "CONDNUM"
    d$EVALUATION <- grepl("Evaluation", d$EST.NAME)
    close(tmp)
    d
  })
  do.call(rbind, dlist)
}
