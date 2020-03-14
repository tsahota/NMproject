start_manual_edit <- function(m, name){
  m %>% write_ctl()            ## update file
  edit_file(ctl_path(m))
  invisible(m)
}

#' Perform manual edit of control file
#' 
#' @param m nm object
#' @param description character. Description of edit for documentation purposes
#' @export
manual_edit <- function(m, description){
  m %>% start_manual_edit()
  message(
    "---Manual edit---
    Instructions:
    1) edit control file
    2) save & close
    Press ENTER when done...")
  readline()
  m %>% stop_manual_edit()
}

stop_manual_edit <- function(m){
  old_m <- m
  #old_target <- target(m)
  m <- m %>% ctl(ctl_path(m), update_ctl = FALSE)  ## update object
  
  old_ctl <- as.character(ctl_character(ctl(as_nm_generic(old_m))))
  new_ctl <- as.character(ctl_character(ctl(as_nm_generic(m))))
  
  if(requireNamespace("diffobj", quietly = TRUE)){
    dff <- diffobj::diffChr(new_ctl, old_ctl, format = "ansi256")
    #dff <- diffobj::diffChr(new_ctl, old_ctl, format = "html")
    
    ## TODO: store patch in object for later retrieval
    ## learn how git2r calls libgit - maybe something useful there
    
    message("--- file diff: new_ctl and old_ctl colours show additions/deletions---")
    print(dff)
    #m <- m %>% target(old_target)
  }
  
  invisible(m)
}

#' @export
create_patch <- function(m){
  
  if(.Platform$OS.type != "unix") 
    stop("patching functionality only implemented for linux/unix systems\n consider manual_edit() instead",
         call. = FALSE)
  
  time_stamp <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  
  patch_name <- paste0("patch-", Sys.info()["user"], "-", time_stamp)
  patch_path <- file.path(getOption("models.dir"), "patches", patch_name)
  dir.create(dirname(patch_path), showWarnings = FALSE, recursive = TRUE)
  
  old_file_path <- file.path(run_in(m), paste0(ctl_name(m), ".old"))
  
  old_ctl_path <- ctl_path(m)
  new_ctl_path <- file.path(run_in(m), paste0("manual_", ctl_name(m)))
  
  m %>% write_ctl()
  mnew <- m %>% ctl_path(new_ctl_path) %>% write_ctl()
  
  edit_file(ctl_path(mnew)) ## edit new one
  
  message(
    "---Manual edit---
    Instructions:
    1) edit control file
    2) save & close
    Press ENTER when done...")
  readline()
  
  ## now diff ctl_path(m) and old_file_path
  
  diff_cmd <- paste("diff -u", ctl_path(m), new_ctl_path, ">", patch_path)
  
  system(diff_cmd)
  
  unlink(new_ctl_path)
  
  message("patch created:\n ", patch_path, "\n")
  
  message("copy-paste the following into your script to apply:\n
  [nm_object] %>%
  apply_patch(\"", patch_name,"\")")
  
}

#' @export
view_patch <- function(patch_name){
  patch_path <- file.path(getOption("models.dir"), "patches", patch_name)  
  file.show(patch_path)
}

#' @export
apply_patch <- function(m, patch_name){
  
  patch_path <- file.path(getOption("models.dir"), "patches", patch_name)  
  
  write_ctl(m)
  
  ctl_paths <- ctl_path(m)
  
  patch_cmd <- paste("patch -i", patch_path, ctl_paths)
  patch_cmd <- paste(patch_cmd, collapse = " ; ")
  
  ## for some reason no "patch" on AZ rstudio server :(
  ## use system_nm instead
  system_nm(patch_cmd, dir = getwd())
  
  m <- m %>% ctl(ctl_path(m), update_ctl = FALSE)
  
  invisible(m)
  
}
