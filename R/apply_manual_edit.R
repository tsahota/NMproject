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
#' @param patch_name Character name of patch.  Corresponds to the file name in
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
apply_manual_edit <- function(m, patch_name) {
  UseMethod("apply_manual_edit")
}

#' @export
apply_manual_edit.nm_generic <- function(m, patch_name) {
  patch_path <- file.path(
    nm_dir("models"),
    "patches",
    patch_name
  )
  if (!file.exists(patch_path)) stop("patch file doesn't exist")

  patch_text <- readLines(patch_path)

  temp_ctl_path <- file.path(run_in(m), paste0("manual_", ctl_name(m)))
  mnew <- m %>%
    ctl_path(temp_ctl_path) %>%
    write_ctl(force = TRUE)

  ## edit and save temporary patch
  patch_text <- gsub("(a/).*?manual_\\S+", paste0("\\1", temp_ctl_path), patch_text, perl = TRUE)
  patch_text <- gsub("(b/).*?manual_\\S+", paste0("\\1", temp_ctl_path), patch_text, perl = TRUE)

  patch_path_tmp <- paste0(patch_path, ".tmp")
  writeLines(patch_text, patch_path_tmp)

  patch_cmd <- paste("git apply -C1", patch_path_tmp)

  if (!git_cmd_available) stop("need git available from system() for this to work")
  res <- system_cmd(patch_cmd, intern = TRUE) ## win = no need to use system_nm, no file sync issues

  if (1 %in% attributes(res)$status) {
    ## try again with whitespace fix, needed for some versions of git
    patch_cmd <- paste("git apply --whitespace=fix -C1", patch_path_tmp)
    res <- system_cmd(patch_cmd, intern = TRUE)

    if (1 %in% attributes(res)$status) {
      usethis::ui_oops("apply_manual_edit() failed to apply patch, 
           this is normally due to the control file having changed since the 
           patch was created. Right clicking the patch text and selecting 
           Addins -> View patch will show the edits the patch is attempting to 
           apply.  This can be used to create a new patch.")
    }
  }

  out_file <- readLines(temp_ctl_path)

  m <- m %>% ctl_contents_simple(out_file)

  invisible(m)
}

#' @export
apply_manual_edit.nm_list <- Vectorize_nm_list(apply_manual_edit.nm_generic, SIMPLIFY = FALSE)
