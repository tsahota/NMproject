#' @include utilsExtra.R

NULL

#' Apply a manual edit patch
#'
#' Best to allow the "manual edit" RStudio addin to write this function for you
#'
#' @param m nm object
#' @param patch_name character name of patch
#'
#' @details Generally best to to apply patches before automatic edits and
#'   changes in directories e.g. via \code{run_in()}
#'
#' @export
apply_manual_edit <- function(m, patch_name){
  UseMethod("apply_manual_edit")
}

#' @export
apply_manual_edit.nm_generic <- function(m, patch_name){
  
  patch_path <- file.path(nm_default_dir("models"), 
                          "patches", 
                          patch_name)
  if(!file.exists(patch_path)) stop("patch file doesn't exist")
  
  patch_text <- readLines(patch_path)
  
  temp_ctl_path <- file.path(run_in(m), paste0("manual_", ctl_name(m)))
  mnew <- m %>% ctl_path(temp_ctl_path) %>% write_ctl(force = TRUE)
  
  ## edit and save temporary patch
  patch_text <- gsub("(a/).*?manual_\\S+", paste0("\\1", temp_ctl_path), patch_text, perl = TRUE)
  patch_text <- gsub("(b/).*?manual_\\S+", paste0("\\1", temp_ctl_path), patch_text, perl = TRUE)
  
  patch_path_tmp <- paste0(patch_path, ".tmp")
  writeLines(patch_text, patch_path_tmp)
  
  patch_cmd <- paste("git apply -C1", patch_path_tmp)
  
  if(!git_cmd_available) stop("need git available from system() for this to work")  
  res <- system_cmd(patch_cmd, intern = TRUE)  ## win = no need to use system_nm, no file sync issues
  
  if(1 %in% attributes(res)$status){
    ## try again with whitespace fix, needed for some versions of git 
    patch_cmd <- paste("git apply --whitespace=fix -C1", patch_path_tmp)
    res <- system_cmd(patch_cmd, intern = TRUE)
    
    if(1 %in% attributes(res)$status){
      stop("patch failed", call. = TRUE) 
    }
  }
  
  out_file <- readLines(temp_ctl_path)
  
  m <- m %>% ctl_contents_simple(out_file)
  
  invisible(m)
  
}

#' @export
apply_manual_edit.nm_list <- Vectorize_nm_list(apply_manual_edit.nm_generic, SIMPLIFY = FALSE)
