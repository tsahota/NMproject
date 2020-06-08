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
  
  if(.Platform$OS.type == "unix") 
    stop("manual_edit() is for non-unix systems.  Use manual_patch() instead",
         call. = FALSE)
  
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
  m <- m %>% ctl_contents(ctl_path(m), update_ctl = FALSE)  ## update object
  
  old_ctl <- as.character(ctl_character(ctl_contents(as_nm_generic(old_m))))
  new_ctl <- as.character(ctl_character(ctl_contents(as_nm_generic(m))))
  
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
manual_patch <- function(m){
  
  res <- start_manual_edit_unix(m)
  
  message(
    "---Manual edit---
    Instructions:
    1) edit control file
    2) save & close
    Press ENTER when done...")
  readline()
  
  ## now diff ctl_path(m) and old_file_path
  
  diff_manual_edit(m, res)
  
  message("patch created:\n ", res$patch_path, "\n")
  
  message("copy-paste the following into your script to apply:\n
  [nm_object] %>%
  apply_patch(\"", res$patch_name,"\")

(dont forget to comment your code)")
  
}

#' @export
start_manual_edit_unix <- function(m){
  if(.Platform$OS.type != "unix") 
    stop("patching functionality only implemented for linux/unix systems\n consider manual_edit() instead",
         call. = FALSE)
  
  if(is_nm_list(m) & length(m) > 1)
    stop("m cannot refer to multiple runs", call. = FALSE)
  
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
  
  res <- list()
  res$new_ctl_path <- new_ctl_path
  res$patch_name <- patch_name
  res$patch_path <- patch_path
  
  res
}

#' @export
diff_manual_edit <- function(m, res){
  diff_cmd <- paste("diff -u", ctl_path(m), res$new_ctl_path, ">", res$patch_path)
  system(diff_cmd)
  unlink(res$new_ctl_path)
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
  
  unlink(paste0(ctl_paths, ".rej"))
  patch_cmd <- paste("patch -i", patch_path, ctl_paths)
  patch_cmd <- paste(patch_cmd, collapse = " ; ")
  unlink(paste0(ctl_paths, ".rej"))
  
  ## for some reason no "patch" on AZ rstudio server :(
  ## use system_nm instead
  system_nm(patch_cmd, dir = getwd())
  
  m <- m %>% ctl_contents(ctl_path(m), update_ctl = FALSE)
  
  invisible(m)
  
}

manual_patch_app <- function() {
  
  shiny_dir <- system.file("extdata/manual_patch",package="NMproject")
  .sso_env$.currentwd <- getwd()  # see zzz.R for .sso_env
  on.exit({
    .sso_env$.currentwd <- NULL
  }, add = TRUE)
  viewer <- shiny::paneViewer(300)
  shiny::runGadget(shiny::shinyAppDir(shiny_dir), viewer = viewer)
  
}

get_single_object_for_app <- function(){
  ctx <- rstudioapi::getActiveDocumentContext()
  selected_text <- ctx$selection[[1]]$text
  if(selected_text == ""){
    line <- ctx$selection[[1]]$range$start[[1]]
    pos <- ctx$selection[[1]]$range$start[[2]]
    
    selected_text <- ctx$contents[line]
    
    selected_text <- gsub("^(.*)<-.*", "\\1", selected_text)
    selected_text <- trimws(selected_text)
    
    if(selected_text == "")
      stop("couldn't find object in selected line")
    
    result <- try(is_nm_list(get(selected_text)), silent = TRUE)
    
    if(inherits(result, "try-error"))
      stop("couldn't find object in selected line")
  }
  
  m <- eval(parse(text = selected_text))  
  m
}

nm_tran_app <- function(){
  m <- get_single_object_for_app()
  nm_tran(m)
}

nm_diff_app <- function(){
  m <- get_single_object_for_app()
  nm_diff(m)
}

in_cache_app <- function(){
  m <- get_single_object_for_app()
  in_cache(m)
}

view_patch_app <- function(){

  ctx <- rstudioapi::getActiveDocumentContext()
  selected_text <- ctx$selection[[1]]$text
  if(selected_text == ""){
    line <- ctx$selection[[1]]$range$start[[1]]
    selected_text <- ctx$contents[line]
  }
  
  selected_text <- gsub(".*(patch-[^\"]+)\".*","\\1", selected_text)
  
  #selected_text <- gsub("\"","", selected_text)
  
  view_patch(selected_text)
  
}