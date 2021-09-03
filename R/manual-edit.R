#' Start manual edit
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Needed for "manual edit" RStudio 'Addin' functionality.  Performs a git
#' reset, git adds and commits a temporary version of control file to index (for
#' subsequent diff), opens editor for edits.  The intent is that after the diff
#' is created the commit will be removed.
#'
#' @param m An nm object
#' @param combine_patch Character. Optional patch to be applied first.
#' @param replace_ctl If not `NA_character_`, before displaying, the file will
#'   be replaced with this
#'
#' @return A `list` with the temporary control file name and patch details (name
#'   and path).
#'
#' @keywords internal
#' @export
start_manual_edit <- function(m, combine_patch = NA_character_, replace_ctl = NA_character_) {
  # if(.Platform$OS.type != "unix")
  #   stop("patching functionality only implemented for linux/unix systems\n consider manual_edit() instead",
  #        call. = FALSE)

  if (is_nm_list(m) & length(m) > 1) {
    stop("m cannot refer to multiple runs", call. = FALSE)
  }

  time_stamp <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")

  patch_id <- paste0(Sys.info()["user"], "-", time_stamp)
  patch_name <- paste0("patch-", patch_id)
  patch_path <- file.path(nm_dir("models"), "patches", patch_name)
  dir.create(dirname(patch_path), showWarnings = FALSE, recursive = TRUE)

  #temp_ctl_path <- file.path(run_in(m), paste0("manual_", ctl_name(m)))
  temp_ctl_path <- file.path(nm_dir("models"), paste0("base-", patch_id))
  mnew <- m %>%
    ctl_path(temp_ctl_path) %>%
    write_ctl(force = TRUE)

  ## soft unstage all first, then add only the file
  if (!git_cmd_available) stop("need git available from system() for this to work")
  if (!user_values_exist()) stop("git user.name and/or user.email not set")

  system("git reset", intern = TRUE) ## for some reason git2r::reset() doesn't reset
  git2r::add(path = ctl_path(mnew))
  git2r::commit(message = paste("pre-manual edit: ", patch_id))

  if (!is.na(combine_patch)) {
    mnew <- mnew %>%
      apply_manual_edit(combine_patch) %>%
      write_ctl(force = TRUE)
  }

  if (!identical(replace_ctl, NA_character_)) {
    writeLines(replace_ctl, ctl_path(mnew))
  }

  usethis::ui_silence(usethis::edit_file(ctl_path(mnew))) ## edit new one

  res <- list()
  res$patch_id <- patch_id
  res$new_ctl_path <- temp_ctl_path
  res$patch_name <- patch_name
  res$patch_path <- patch_path

  res
}

git_user_name_exists <- function() {
  config_names <- c(names(git2r::config()$global), names(git2r::config()$local))
  "user.name" %in% config_names
}

git_user_email_exists <- function() {
  config_names <- c(names(git2r::config()$global), names(git2r::config()$local))
  "user.email" %in% config_names
}

user_values_exist <- function() {
  config_names <- c(names(git2r::config()$global), names(git2r::config()$local))
  all(c("user.email", "user.name") %in% config_names)
}

check_git_uservalues <- function() {

  cmd <- NA_character_

  if (!git_user_name_exists()) {
    txt <- "user.name"
    cmd <- "usethis::use_git_config(user.name = 'Jane')"
  }
  if (!git_user_email_exists()) {
    txt <- "user.email"
    cmd <- "usethis::use_git_config(user.email = 'jane@example.com')"
  }
  if (!git_user_name_exists() & !git_user_email_exists()) {
    txt <- "user.name & user.email"
    cmd <- "usethis::use_git_config(user.name = 'Jane', user.email = 'jane@example.com')"
  }

  if(is.na(cmd)) return(invisible())

  usethis::ui_stop(paste0("git ", txt, "not set up. Set up like so:\n   ",
                          "{usethis::ui_code(\"", cmd, "\")}",
                          "\nto set up"))

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
  ## git2r::reset() doesn't work for some reason
  on.exit(system("git reset HEAD", intern = TRUE))

  diff_text <- git2r::diff(git2r::repository(), index = TRUE, as_char = TRUE)
  diff_text <- strsplit(diff_text, "\n")[[1]]
  writeLines(diff_text, res$patch_path)

  ## following doesn't work for some reason
  #git2r::diff(git2r::repository(), index = TRUE, as_char = TRUE, filename = res$patch_path)
  ## remove last commit and file
  unlink(res$new_ctl_path)
}


#' View a patch
#'
#' Use the "view patch" RStudio 'Addin' for viewing patches instead.
#'
#' @param patch_id Character. Name of patch
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
#' @export
view_patch <- function(patch_id) {

  patch_name <- paste0("patch-", patch_id)
  patch_path <- file.path(nm_dir("models"), "patches", patch_name)
  if (!file.exists(patch_path)) stop("patch file doesn't exist")
  patch_contents <- readLines(patch_path)
  cat(patch_contents, sep = "\n")

}

new_patch_app <- function() {
  ctx <- rstudioapi::getSourceEditorContext()
  selected_text <- ctx$selection[[1]]$text
  final_pipe_present <- grepl("\\s*%>%\\s*$", selected_text)
  final_newline_present <- grepl("\\n\\s*$", selected_text)

  m <- get_single_object_for_app()

  ## before doing start_manual_edit - log the current commit for later resetting
  if (!git_cmd_available) stop("need git available from system() for this to work")
  if (!user_values_exist()) stop("git user.name and/or user.email not set")
  git_log <- git2r::commits()
  if (length(git_log) == 0) stop("Need a least one commit to continue.
One should have been created on directory creation.  Run \"git commit\" on
command line diagnose the error")
  orig_commit <- git_log[[1]]

  res <- start_manual_edit(m)
  on.exit(unlink(res$new_ctl_path))

  ans <- usethis::ui_yeah("---INSTRUCTIONS---

 1) {usethis::ui_field('EDIT')} control file
 2) {usethis::ui_field('SAVE')} the file

Follow the above instructions and indicate if you happy to proceed?", yes = "Yes", no = "Abort", shuffle = FALSE)

  if(!ans) {
    system(paste("git reset", orig_commit$sha), intern = TRUE)
    return(invisible())
  }

  ## now diff ctl_path(m) and old_file_path

  diff_manual_edit(m, res)

  if (final_pipe_present) {
    if (final_newline_present) {
      code_to_add <- paste0("  apply_manual_edit(\"", res$patch_id, "\") %>%")
    } else {
      code_to_add <- paste0("
  apply_manual_edit(\"", res$patch_id, "\") %>%")
    }
  } else {
    code_to_add <- paste0(" %>%
  apply_manual_edit(\"", res$patch_id, "\")")
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

  patch_id <- gsub(".*%>%.*apply_manual_edit\\(\"(.*)\"\\).*", "\\1", selected_text)

  m <- eval(parse(text = before_edit))

  ## before doing start_manual_edit - log the current commit for later resetting
  if (!git_cmd_available) stop("need git available from system() for this to work")
  if (!user_values_exist()) stop("git user.name and/or user.email not set")
  git_log <- git2r::commits()
  if (length(git_log) == 0) stop("Need a least one commit to continue.
One should have been created on directory creation.  Run \"git commit\" on
command line diagnose the error")
  orig_commit <- git_log[[1]]

  res <- start_manual_edit(m, combine_patch = patch_id)
  on.exit(unlink(res$new_ctl_path))

  ans <- usethis::ui_yeah("---INSTRUCTIONS---

 1) {usethis::ui_field('EDIT')} control file
 2) {usethis::ui_field('SAVE')} the file

Follow the above instructions and indicate if you happy to proceed?", yes = "Yes", no = "Abort", shuffle = FALSE)

  if(!ans) {
    system(paste("git reset", orig_commit$sha), intern = TRUE)
    return(invisible())
  }

  ## now diff ctl_path(m) and old_file_path

  diff_manual_edit(m, res)

  if (final_pipe_present) {
    if (final_newline_present) {
      code_to_add <- paste0(trimws(before_edit), "  apply_manual_edit(\"", res$patch_id, "\") %>%")
    } else {
      code_to_add <- paste0(trimws(before_edit), " %>%
  apply_manual_edit(\"", res$patch_id, "\") %>%")
    }
  } else {
    code_to_add <- paste0(trimws(before_edit), " %>%
  apply_manual_edit(\"", res$patch_id, "\")")
  }

  rstudioapi::insertText(
    text = code_to_add,
    id = ctx$id
  )

  message("apply_manual_edit() statement modified in script")
}

resolve_manual_edit <- function() {
  ctx <- rstudioapi::getSourceEditorContext()
  selected_text <- ctx$selection[[1]]$text
  final_pipe_present <- grepl("\\s*%>%\\s*$", selected_text)
  final_newline_present <- grepl("\\n\\s*$", selected_text)

  before_edit <- gsub("(.*)%>%.*apply_manual_edit.*", "\\1", selected_text)

  patch_id <- gsub(".*%>%.*apply_manual_edit\\(\"(.*)\"\\).*", "\\1", selected_text)

  m <- eval(parse(text = before_edit))

  ctl_with_merge <- m %>%
    apply_manual_edit(patch_id, return_merge_conf_ctl = TRUE)

  ctl_with_merge <- ctl_with_merge[[1]]

  ## before doing start_manual_edit - log the current commit for later resetting
  if (!git_cmd_available) stop("need git available from system() for this to work")
  if (!user_values_exist()) stop("git user.name and/or user.email not set")
  git_log <- git2r::commits()
  if (length(git_log) == 0) stop("Need a least one commit to continue.
One should have been created on directory creation.  Run \"git commit\" on
command line diagnose the error")
  orig_commit <- git_log[[1]]

  res <- start_manual_edit(m, replace_ctl = ctl_with_merge)
  on.exit(unlink(res$new_ctl_path))
  ans <- usethis::ui_yeah("---INSTRUCTIONS---

 1) {usethis::ui_field('REPAIR')} control file
    a) Merge conflicts are indicated by the markers <<<<<<<, =======, and >>>>>>>
    b) {usethis::ui_field('EDIT')} these sections
 2) {usethis::ui_field('SAVE')} the file

Follow the above instructions and indicate if you happy to proceed?", yes = "Yes", no = "Abort", shuffle = FALSE)

  if(!ans) {
    system(paste("git reset", orig_commit$sha), intern = TRUE)
    return(invisible())
  }

  ## now diff ctl_path(m) and old_file_path

  diff_manual_edit(m, res)

  if (final_pipe_present) {
    if (final_newline_present) {
      code_to_add <- paste0(trimws(before_edit), "  apply_manual_edit(\"", res$patch_id, "\") %>%")
    } else {
      code_to_add <- paste0(trimws(before_edit), " %>%
  apply_manual_edit(\"", res$patch_id, "\") %>%")
    }
  } else {
    code_to_add <- paste0(trimws(before_edit), " %>%
  apply_manual_edit(\"", res$patch_id, "\")")
  }

  rstudioapi::insertText(
    text = code_to_add,
    id = ctx$id
  )

  message("apply_manual_edit() statement modified in script")

}

make_patch_app <- function() {
  check_git_uservalues()

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

resolve_manual_edit_app <- function() {
  check_git_uservalues()

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
    resolve_manual_edit()
  } else {
    stop("the last function applied to selection needs to be apply_manual_edit()")
  }
}




view_patch_app <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  selected_text <- ctx$selection[[1]]$text
  if (selected_text == "") {
    line <- ctx$selection[[1]]$range$start[[1]]
    selected_text <- ctx$contents[line]
  }

  selected_text <- gsub(".*\"([^\"]+)\".*", "\\1", selected_text)

  # selected_text <- gsub("\"","", selected_text)

  view_patch(selected_text)
}
