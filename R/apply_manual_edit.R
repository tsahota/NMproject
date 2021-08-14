#' Apply a manual edit patch
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' It is best to allow the "manual edit" RStudio 'Addin' to write this function
#' in your script for you.  After a tracked manual edit is performed, a patch
#' file is created and saved in the "patches" subdirectory of
#' `nm_dir("models")`.  This function applies the patch to the object.
#'
#' @param m An nm object.
#' @param patch_id Character name of patch.  Corresponds to the file name in
#'   the "patches" subdirectory of `nm_dir("models")`.
#'
#' @details Generally best to to apply patches before automatic edits and
#'   changes in directories e.g. via `run_in()`.  If patches are applied to
#'   NONMEM control file sections that are likely to change in the future, the
#'   patch may fail to apply.  In this case, it is best to view the patch (via
#'   the "view patch" RStudio 'Addin') and manually re-implement the changes again
#'   in a new manual edit.
#'
#' @return An nm object with modified `ctl_contents` field.
#'
#' @export
apply_manual_edit <- function(m, patch_id) {
  UseMethod("apply_manual_edit")
}

#' @export
apply_manual_edit.nm_generic <- function(m, patch_id) {
  
  temp_ctl_path <- file.path(nm_dir("models"), paste0("base-", patch_id))
  mnew <- m %>%
    ctl_path(temp_ctl_path) %>%
    write_ctl(force = TRUE)
  
  if (!git_cmd_available) stop("need git available from system() for this to work")
  if (!user_values_exist()) stop("git user.name and/or user.email not set")
  git_log <- git2r::commits()
  orig_commit <- git_log[[1]]
  on.exit({
    unlink(temp_ctl_path)
    system(paste("git reset", orig_commit$sha), intern = TRUE)
  })
  
  system("git reset", intern = TRUE) ## for some reason git2r::reset() doesn't reset
  git2r::add(path = temp_ctl_path)
  git2r::commit(message = paste("temp commit: ", ctl_path(m)))
  
  ## find the commit
  find_commit <- paste("manual edit: ", patch_id)
  found_commits <- git_log[sapply(git_log, function(commit) commit$message) %in% find_commit]
  if (length(found_commits) != 1) stop("could not find commit corresponding to: ", patch_id, call. = FALSE)
  found_commit <- found_commits[[1]]

  cherry_pick_res <- suppressWarnings(
    system(paste("git cherry-pick", found_commit$sha), 
           intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
  )
  if (1 %in% attributes(cherry_pick_res)$status) {
    usethis::ui_stop("apply_manual_edit() hit an unresolvable merge conflict, 
            this is normally due to the control file having changed significantly
            since the manual edit was performed. 
            You can repair this by highlighting the code and going to:
                     Addins -> Resolve merge conflict")
  }
  
  out_file <- readLines(temp_ctl_path)

  m <- m %>% ctl_contents_simple(out_file)

  invisible(m)
}

#' @export
apply_manual_edit.nm_list <- Vectorize_nm_list(apply_manual_edit.nm_generic, SIMPLIFY = FALSE)
