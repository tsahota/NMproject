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
#' @param return_merge_conf_ctl Logical (default = `FALSE`). If there a merge
#'   conflict produced, should the ctl file be returned?
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
apply_manual_edit <- function(m, patch_id, return_merge_conf_ctl = FALSE) {
  UseMethod("apply_manual_edit")
}

#' @export
apply_manual_edit.nm_generic <- function(m, patch_id, return_merge_conf_ctl = FALSE) {
  
  ## retrain backwards compatibility with "patch-sdlkfjs.." format patch ids.
  patch_id <- gsub("patch-", "", patch_id)
  
  temp_ctl_path <- file.path(nm_dir("models"), paste0("base-", patch_id))
  mnew <- m %>%
    ctl_path(temp_ctl_path) %>%
    write_ctl(force = TRUE)
  on.exit(unlink(temp_ctl_path))
  
  patch_name <- paste0("patch-", patch_id)
  patch_path <- file.path(nm_dir("models"), "patches", patch_name)
  patch_file_exists <- file.exists(patch_path)
  if (!patch_file_exists) stop("patch file doesn't exist")
  
  patch_cmd <- paste("git apply -C1", patch_path)
  
  suppressWarnings(
    res <- system_cmd(patch_cmd, intern = TRUE) ## win = no need to use system_nm, no file sync issues    
  )

  
  if (isTRUE(attributes(res)$status > 0)) {
    ## try again with whitespace fix, needed for some versions of git
    patch_cmd <- paste("git apply --whitespace=fix -C1", patch_path)
    suppressWarnings(
      res <- system_cmd(patch_cmd, intern = TRUE)
    )
    
    if (isTRUE(attributes(res)$status > 0)) {
      
      if (!git_cmd_available) stop("need git available from system() for this to work")
      
      ## get current commit for later resetting
      git_log <- git2r::commits()
      if( length(git_log) == 0) {
        usethis::ui_stop("apply_manual_edit() hit an unresolvable merge conflict, 
            this is normally due to the control file having changed significantly
            since the manual edit was performed. Right clicking the patch text and 
            selecting  Addins -> View patch will show the edits the patch is attempting 
            to apply.  This can be used to create a new patch.")
      }
      orig_commit <- git_log[[1]]
      
      ## Look for the commit in the history
      find_commit <- paste("pre-manual edit: ", patch_id)
      found_commits <- git_log[sapply(git_log, function(commit) commit$message) %in% find_commit]
      commit_found <- length(found_commits) == 1
      
      if (commit_found) {
        check_git_uservalues()
        ## create a commit so the 3way can be done
        system("git reset", intern = TRUE) ## for some reason git2r::reset() doesn't reset
        git2r::add(path = temp_ctl_path)
        on.exit(system(paste("git reset", orig_commit$sha), intern = TRUE), add = TRUE)
        
        ## If no changes are present, we can exit
        commit_diff <- git2r::diff(git2r::repository(), index = TRUE, as_char = TRUE)
        commit_diff <- strsplit(commit_diff, "\n")[[1]]
        empty_commit_coming <- length(commit_diff) == 0
        
        if (empty_commit_coming) {
          usethis::ui_stop("apply_manual_edit() hit an unresolvable merge conflict, 
            this is normally due to the control file having changed significantly
            since the manual edit was performed. Right clicking the patch text and 
            selecting  Addins -> View patch will show the edits the patch is attempting 
            to apply.  This can be used to create a new patch.")
        }
        
        commit_msg <- paste("temp commit: ", ctl_path(m))
        git2r::commit(message = commit_msg)
        
        patch_cmd <- paste("git am -C1 --3way <", patch_path)
        
        res <- suppressWarnings(
          system_cmd(patch_cmd, intern = TRUE)
        )
        
        if (any(grepl("git config", res))) {
          usethis::ui_stop("Need to set project level user.name and user.email:
            {usethis::ui_code(\"usethis::use_git_config(scope = 'project', user.name = 'Jane', user.email = 'jane@example.com')\")}
          retry when finished")
        }
        
        if (isTRUE(attributes(res)$status > 0)) { ## error detected
          ## two options:
          ##   merge conflict 
          ##   other fail (e.g. whitespace)
          git2r::add(path = temp_ctl_path)
          added_stuff <- git2r::diff(git2r::repository(), index = TRUE, as_char = TRUE)
          conflict_present <- FALSE
          if (length(added_stuff) == 1) {
            conflict_present <- any(grepl("\\+<<<<<<<", added_stuff))
          }
          ## can now use conflict_present
          
          if (conflict_present) {
            #on.exit(system("git am --abort", intern = TRUE))
            
            warn_msg <- paste0("manual edit merge conflict detected:\n\n", 
                               "-------------------------------------------\n",
                               added_stuff,
                               "-------------------------------------------\n")
            
            if (return_merge_conf_ctl) return(readLines(temp_ctl_path))
            
            usethis::ui_info(warn_msg)
            usethis::ui_stop("You can resolve this conflict by selecting the code
            and ensuring the apply_manual_edit() is the last pipe highlighted
            and then selecting Addins -> Resolve manual edit conflict")
            
            ## instead can we just fix the merge conflict here?
            ## reproducibility and predictability must be no. 1
            ## involuntarily fixing merge conflict could result in strange
            ## behaviour.  The patch wouldnt' match up to what is done.
          }
          
          patch_cmd <- paste("git am --whitespace=fix -C1 --3way <", patch_path)
          res <- system_cmd(patch_cmd, intern = TRUE)
          
          if (isTRUE(attributes(res)$status > 0)) { ## error detected
            ## two options:
            ##   merge conflict 
            ##   other fail (non whitespace but other)
            git2r::add(path = temp_ctl_path)
            added_stuff <- git2r::diff(git2r::repository(), index = TRUE, as_char = TRUE)
            conflict_present <- FALSE
            if (length(added_stuff) == 1) {
              conflict_present <- any(grepl("\\+<<<<<<< ours", added_stuff))
            }   
            
            if (conflict_present) {
              warn_msg <- paste0("manual edit merge conflict detected:\n\n", 
                                 "-------------------------------------------\n",
                                 added_stuff,
                                 "-------------------------------------------\n")
              
              if (return_merge_conf_ctl) return(readLines(temp_ctl_path))
              
              usethis::ui_info(warn_msg)
              usethis::ui_stop("You can resolve this conflict by highlighting the code
          in your script up to this apply_manual_edit() statement and selecting:
            Addins -> Resolve manual edit conflict")
              
            }
          }
        }
      } else {
        usethis::ui_stop("apply_manual_edit() hit an unresolvable merge conflict, 
            this is normally due to the control file having changed significantly
            since the manual edit was performed. Right clicking the patch text and 
            selecting  Addins -> View patch will show the edits the patch is attempting 
            to apply.  This can be used to create a new patch.") 
      }
    }
  }
  
  out_file <- readLines(temp_ctl_path)
  
  m <- m %>% ctl_contents_simple(out_file)
  
  invisible(m)
}

#' @export
apply_manual_edit.nm_list <- Vectorize_nm_list(apply_manual_edit.nm_generic, SIMPLIFY = FALSE)
