#' Set up code completion for NMproject
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Intelligent code completion is an experimental way to type NMproject code.
#' This function modifies/creates `r.snippets`.  Needs to be run interactively.
#' Will ask for user confirmation since snippets are an RStudio config setting
#'
#' @param force Logical.  The default is `FALSE` which will require user
#'   confirmation before editting `r.snippets`.
#'
#' @param snippet_path Character path to the `r.snippets` file.
#'
#' @return No return value, called for side effects.
#'
#' @export


setup_code_completion <- function(force = FALSE,
                                            snippet_path = find_snippet_path()) {

  if (!force) {
    if (!is_rstudio()) return(invisible())
    if (!interactive()) return(invisible())
  }

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

    if (all(matching_snippets)) return(invisible())
  }

  ## Check the user is OK to make the modification

  if (!force) {

    if (!interactive()) stop("Needs to be run interactively with force = FALSE")

    msg <- "Do you want to modify your user level RStudio settings to enable NMproject code completion?"

    ans <- usethis::ui_yeah(msg, yes = "Yes", no = "Abort", shuffle = FALSE)
    if (!ans) usethis::ui_stop("Aborting")

  }

  ## can now assume we have consent

  if (!snippet_exists) {
    dir.create(dirname(snippet_path), showWarnings = FALSE, recursive = TRUE)
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

    dir.create(dirname(snippet_path), showWarnings = FALSE, recursive = TRUE)
    write(snippet_contents, file = snippet_path)
  }
  usethis::ui_done("Snippet file updated: {usethis::ui_path(snippet_path)}")
  
  if (!is.null(rstudioapi::getActiveProject())) {
    ans <- usethis::ui_yeah("RStudio needs to restart for changes to come into effect",
                            yes = "Restart now", no = "I'll do it later", shuffle = FALSE)
    if (ans) rstudioapi::openProject()
  } else {
    usethis::ui_todo("Restart RStudio (Session -> Quit Session) for changes to take effect")
  }
}

snippets_startup_message <- function() {

  if (!is_rstudio()) return(invisible())
  if (!interactive()) return(invisible())

  snippet_path <- find_snippet_path()
  template_path <- system.file("extdata", "r.snippets", package = "NMproject")

  snippet_exists <- file.exists(snippet_path)

  msg <- "To set up NMproject code completion: 'setup_code_completion()'"

  if (!snippet_exists) {
    packageStartupMessage(msg)
    return(invisible())
  }

  ## check to see if modification is needed.
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

  if (!all(matching_snippets)) packageStartupMessage(msg)

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


find_snippet_path <- function() {

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
