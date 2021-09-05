#' Set up code completion for NMproject
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Intelligent code completion is an experimental way to type NMproject code.
#' This function modifies/creates `r.snippets`.
#'
#' @param force Logical.  The default is `FALSE` which will require user confirmation before editting `r.snippets`
#'
#' @return No return value, called for side effects.
#'
#' @export

setup_nmproject_code_completion <- function(force = FALSE) {

  snippet_path <- snippet_path()
  template_path <- system.file("extdata", "r.snippets", package = "NMproject")

  snippet_exists <- file.exists(snippet_path)

  ## check to see if modification is needed.
  if (snippet_exists) {
    template_contents <- readLines(template_path)
    snippet_contents <- readLines(snippet_path)

    last_line_blank <- snippet_contents[length(snippet_contents)]
    last_line_blank <- grepl("^\\s*$", last_line_blank)
    if (!last_line_blank) snippet_contents <- c(snippet_contents, "")

    last_line_blank <- template_contents[length(template_contents)]
    last_line_blank <- grepl("^\\s*$", last_line_blank)
    if (!last_line_blank) template_contents <- c(template_contents, "")

    matching_snippets <- sapply(c("new_nm", "child"), function(snippet_name) {
      to_index <- snippet_index(snippet_name, snippet_contents)
      from_index <- snippet_index(snippet_name, template_contents)
      identical(snippet_contents[to_index], template_contents[from_index])
    })

    if (all(matching_snippets)) {
      message("matching snippets, no update needed")
      return(invisible())
    }

  }

  ## Check the user is OK to make the modification

  if (!force) {

    if (!interactive()) stop("Needs to be run interactively with force = FALSE")

    msg <- "This will modify your user level RStudio snippets settings:
    {usethis::ui_path(snippet_path)}
    Are you OK with this?"

    ans <- usethis::ui_yeah(msg, yes = "Yes", no = "Abort", shuffle = FALSE)
    if (!ans) usethis::ui_stop("Aborting")

  }

  ## can now assume we have consent

  if (!snippet_exists) {
    file.copy(template_path, snippet_path)
  } else {
    ## modify the snippets file
    tryCatch({
      for (snippet_name in c("new_nm", "child")) {

        to_index <- snippet_index(snippet_name, snippet_contents)
        from_index <- snippet_index(snippet_name, template_contents)

        ## remove old if present
        if (length(to_index) != 0) {
          snippet_contents <- snippet_contents[-to_index]
        }

        snippet_contents <- c(
          snippet_contents, template_contents[from_index]
        )
      }
    }, error = function(e) {
      stop("Hit an error in modifying r.snippets, aborting to be safe")
    })

    write(snippet_contents, file = snippet_path)
    usethis::ui_done("Intelligent code completion ready, hit TAB to activate")

  }
}



snippet_index <- function(name, contents) {

  start_match <- grep(paste("snippet", name), contents)

  if (length(start_match) == 0) return(integer())
  if (length(start_match) > 1) stop("cannot find unique match")

  remaining_contents <- contents[start_match:length(contents)]

  space_match <- grep(paste("^\\s*$"), remaining_contents)

  if (length(space_match) == 0) stop("cannot find trailing empty line")

  ## first space after snippet will be used to end snippet
  space_match <- space_match[1]

  ## get index from beginning
  space_match <- start_match + space_match - 1

  start_match:space_match

}


snippet_path <- function() {

  rstudio_version <- rstudioapi::getVersion()
  if (rstudio_version < "1.3") {
    snippet_path <- "~/.R/snippets/r.snippets"
  } else {
    if (Sys.info()["sysname"] %in% "Windows") {
      snippet_path <- file.path(
        Sys.getenv("APPDATA"),
        "RStudio", "snippets", "r.snippets"
      )
    } else {
      snippet_path <- "~/.config/rstudio/snippets/r.snippets"
    }
  }
  snippet_path

}
