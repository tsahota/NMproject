is_single_na <- function(x) if(length(x) == 1) is.na(x) else FALSE

#' check if git is available on command line

git_cmd_available <- Sys.which("git") != ""
