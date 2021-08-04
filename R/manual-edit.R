#' Start manual edit
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Needed for "manual edit" RStudio 'Addin' functionality.
#'
#' @param m An nm object
#' @param combine_patch Logical. Should patch be added first before edit.
#'
#' @return A `list` with the temporary control file name and patch details (name
#'   and path).
#'
#' @keywords internal
#' @export
start_manual_edit <- function(m, combine_patch = NA_character_) {
  # if(.Platform$OS.type != "unix")
  #   stop("patching functionality only implemented for linux/unix systems\n consider manual_edit() instead",
  #        call. = FALSE)

  if (is_nm_list(m) & length(m) > 1) {
    stop("m cannot refer to multiple runs", call. = FALSE)
  }

  time_stamp <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")

  patch_name <- paste0("patch-", Sys.info()["user"], "-", time_stamp)
  patch_path <- file.path(nm_default_dir("models"), "patches", patch_name)
  dir.create(dirname(patch_path), showWarnings = FALSE, recursive = TRUE)

  temp_ctl_path <- file.path(run_in(m), paste0("manual_", ctl_name(m)))
  mnew <- m %>%
    ctl_path(temp_ctl_path) %>%
    write_ctl(force = TRUE)

  ## soft unstage all first, then add only the file
  if (!git_cmd_available) stop("need git available from system() for this to work")

  if (!user_values_exist()) stop("git user.name and/or user.email not set")

  system("git reset", intern = TRUE) ## for some reason git2r::reset() doesn't reset
  git2r::add(path = ctl_path(mnew))
  git2r::commit(message = paste("before manual change: ", ctl_path(mnew)))

  if (!is.na(combine_patch)) m <- m %>% apply_manual_edit(combine_patch)

  usethis::edit_file(ctl_path(mnew)) ## edit new one

  res <- list()
  res$new_ctl_path <- temp_ctl_path
  res$patch_name <- patch_name
  res$patch_path <- patch_path

  res
}

user_values_exist <- function() {
  all(c("user.email", "user.name") %in% names(git2r::config()$global)) |
    all(c("user.email", "user.name") %in% names(git2r::config()$local))
}

check_git_username <- function() {
  if (!user_values_exist()) {
    message("manual edits need git a user.name and user.email")

    if (interactive()) {
      username <- rstudioapi::showPrompt(title = "First time git set up for NMproject", "enter a user.name")
      email <- rstudioapi::showPrompt(title = "First time git set up for NMproject", "enter a user.email")
      usethis::use_git_config(user.name = username, user.email = email)
      if (!user_values_exist()) { ## if still none - error
        stop("manual edits needs your git installation configured with
a user.name and user.email, set this up on the command line with:
git config user.name \"[name]\"
git config user.email \"[name@example.com]\"
")
      } else {
        try(
          {
            repo <- git2r::init(usethis::proj_get())
            git2r::add(repo, path = "README.Rmd")
            git2r::commit(repo, message = "added README.Rmd", all = TRUE)
          },
          silent = TRUE
        )
      }
    }
    if (!interactive()) stop("manual edits needs your git installation configured with
a user.name and user.email, set this up with:
 usethis::use_git_config(user.name = [your name], user.email = [your email])")
  }
  invisible(TRUE)
}

diff_manual_edit <- function(m, res) {
  git2r::add(path = res$new_ctl_path)
  git2r::diff(git2r::repository(), index = TRUE, as_char = TRUE, filename = res$patch_path)
  ## remove last commit and file
  system("git reset HEAD^1", intern = TRUE) ## for some reason git2r::reset() doesn't reset
  unlink(res$new_ctl_path)
}


#' View a patch
#'
#' Use the "view patch" RStudio 'Addin' for viewing patches instead.
#'
#' @param patch_name Character. Name of patch
#'
#' @keywords internal
#' @export
view_patch <- function(patch_name) {
  patch_path <- file.path(nm_default_dir("models"), "patches", patch_name)
  file.show(patch_path)
}

new_patch_app <- function() {
  ctx <- rstudioapi::getSourceEditorContext()
  selected_text <- ctx$selection[[1]]$text
  final_pipe_present <- grepl("\\s*%>%\\s*$", selected_text)
  final_newline_present <- grepl("\\n\\s*$", selected_text)

  m <- get_single_object_for_app()

  res <- start_manual_edit(m)

  message(
    "---Manual edit---
    Instructions:
    1) edit control file
    2) save & close
    Press ENTER when done..."
  )
  readline()

  ## now diff ctl_path(m) and old_file_path

  diff_manual_edit(m, res)

  if (final_pipe_present) {
    if (final_newline_present) {
      code_to_add <- paste0("  apply_manual_edit(\"", res$patch_name, "\")")
    } else {
      code_to_add <- paste0("
  apply_manual_edit(\"", res$patch_name, "\")")
    }
  } else {
    code_to_add <- paste0(" %>%
  apply_manual_edit(\"", res$patch_name, "\")")
  }


  rstudioapi::insertText(ctx$selection[[1]]$range$end,
    text = code_to_add,
    id = ctx$id
  )

  message("apply_manual_edit() statement added to script")
}

modify_patch_app <- function() {
  ctx <- rstudioapi::getSourceEditorContext()
  selected_text <- ctx$selection[[1]]$text
  final_pipe_present <- grepl("\\s*%>%\\s*$", selected_text)
  final_newline_present <- grepl("\\n\\s*$", selected_text)

  before_edit <- gsub("(.*)%>%.*apply_manual_edit.*", "\\1", selected_text)

  patch_name <- gsub(".*%>%.*apply_manual_edit\\(\"(.*)\"\\).*", "\\1", selected_text)

  m <- eval(parse(text = before_edit))

  res <- start_manual_edit(m, combine_patch = patch_name)

  message(
    "---Manual edit---
    Instructions:
    1) edit control file
    2) save & close
    Press ENTER when done..."
  )
  readline()

  ## now diff ctl_path(m) and old_file_path

  diff_manual_edit(m, res)

  if (final_pipe_present) {
    if (final_newline_present) {
      code_to_add <- paste0(trimws(before_edit), "  apply_manual_edit(\"", res$patch_name, "\") %>%")
    } else {
      code_to_add <- paste0(trimws(before_edit), " %>%
  apply_manual_edit(\"", res$patch_name, "\") %>%")
    }
  } else {
    code_to_add <- paste0(trimws(before_edit), " %>%
  apply_manual_edit(\"", res$patch_name, "\")")
  }

  rstudioapi::insertText(
    text = code_to_add,
    id = ctx$id
  )

  message("apply_manual_edit() statement modified in script")
}

make_patch_app <- function() {
  check_git_username()

  ctx <- rstudioapi::getSourceEditorContext()
  selected_text <- ctx$selection[[1]]$text
  selected_text <- gsub("\\s*%>%\\s*$", "", selected_text)
  ## see if last function is apply_manual_edit

  ## get last function used in piping
  full_expr <- parse(text = selected_text)[[1]]

  if (as.character(full_expr[[1]]) == "<-") {
    full_expr <- full_expr[[3]] ## rhs of assignment
  }

  if (as.character(full_expr[[1]]) == "%>%") {
    last_fun_name <- as.character(full_expr[[3]][[1]])
  }

  if (as.character(full_expr[[1]]) != "%>%") { ## normal function
    last_fun_name <- as.character(full_expr[[1]])
  }

  if (last_fun_name == "apply_manual_edit") {
    modify_patch_app()
  } else {
    new_patch_app()
  }
}


view_patch_app <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  selected_text <- ctx$selection[[1]]$text
  if (selected_text == "") {
    line <- ctx$selection[[1]]$range$start[[1]]
    selected_text <- ctx$contents[line]
  }

  selected_text <- gsub(".*(patch-[^\"]+)\".*", "\\1", selected_text)

  # selected_text <- gsub("\"","", selected_text)

  view_patch(selected_text)
}
