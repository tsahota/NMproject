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
  
  .Deprecated("manual_edit() is old. Use the make manual edit patch addin instead")
  
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

#' internal function: start manual edit
#' 
#' Needed for addin functionality
#' 
#' @param m nm object
#' @param combine_patch logical. should patch be added first before edit
#' 
#' @export
start_manual_edit_unix <- function(m, combine_patch = NA_character_){
  # if(.Platform$OS.type != "unix") 
  #   stop("patching functionality only implemented for linux/unix systems\n consider manual_edit() instead",
  #        call. = FALSE)
  
  if(is_nm_list(m) & length(m) > 1)
    stop("m cannot refer to multiple runs", call. = FALSE)
  
  time_stamp <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
  
  patch_name <- paste0("patch-", Sys.info()["user"], "-", time_stamp)
  patch_path <- file.path(getOption("models.dir"), "patches", patch_name)
  dir.create(dirname(patch_path), showWarnings = FALSE, recursive = TRUE)
  
  ## this isn't used - delete
  
  temp_ctl_path <- file.path(run_in(m), paste0("manual_", ctl_name(m)))
  mnew <- m %>% ctl_path(temp_ctl_path) %>% write_ctl(force = TRUE)

  ## soft unstage all first, then add only the file
  if(!git_cmd_available) stop("need git available from system() for this to work")

  system("git reset", intern = TRUE) ## for some reason git2r::reset() doesn't reset
  git2r::add(path = ctl_path(mnew))
  git2r::commit(message = paste("before manual change: ", ctl_path(mnew)))
  
  if(!is.na(combine_patch)) m <- m %>% apply_manual_edit(combine_patch)
  
  edit_file(ctl_path(mnew)) ## edit new one
  
  res <- list()
  res$new_ctl_path <- temp_ctl_path
  res$patch_name <- patch_name
  res$patch_path <- patch_path
  
  res
}

diff_manual_edit <- function(m, res){
  git2r::add(path = res$new_ctl_path)
  git2r::diff(git2r::repository(), index = TRUE, as_char = TRUE, filename = res$patch_path)
  ## remove last commit and file 
  system("git reset HEAD^1", intern = TRUE) ## for some reason git2r::reset() doesn't reset
  unlink(res$new_ctl_path)
}


#' internal function: view a patch
#' 
#' Use the addin for viewing patches instead
#' 
#' @param patch_name character. name of patch
#' 
#' @export
view_patch <- function(patch_name){
  patch_path <- file.path(getOption("models.dir"), "patches", patch_name)  
  file.show(patch_path)
}

#' apply a manual edit
#' 
#' Best used with "make manual edit patch" addin.
#' 
#' @param m nm object
#' @param patch_name character name of patch
#' 
#' @details Generally best to to apply patches before automatic edits
#'  and changes in directories e.g. via \code{run_in()}
#' 
#' @export
apply_manual_edit <- function(m, patch_name){
  UseMethod("apply_manual_edit")
}

#' @export
apply_manual_edit.nm_generic <- function(m, patch_name){
  
  patch_path <- file.path(getOption("models.dir"), 
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
  
  patch_cmd <- paste("git apply", patch_path_tmp, "-C1")
  
  if(!git_cmd_available) stop("need git available from system() for this to work")  
  res <- system(patch_cmd, intern = TRUE)  ## win = no need to use system_nm, no file sync issues

  if(1 %in% attributes(res)$status){
    stop("patch failed", call. = TRUE)
  }
  
  out_file <- readLines(temp_ctl_path)
  
  if(0){  ## delete this if working in AZ
    ctl_paths <- ctl_path(m)
    patch_cmd <- paste("patch -i", patch_path, ctl_paths)
    patch_cmd <- paste(patch_cmd, collapse = " ; ")
    
    ## for some reason no "patch" on AZ rstudio server :(
    ## use system_nm instead
    
    ## 1.save the file via ssh  
    ## remove from orig and rej files in system_nm instead of R
    ##  because sometimes delay
    ## also get modified file via the command (to avoid file sync issue)
    
    patch_cmd <- paste0(
      ## save the file via ssh  
      paste0("echo ", 
             shQuote(gsub("\\$", "\\\\$", paste(text(as_nm_generic(m)), collapse = "\n"))),
             " > ", ctl_paths), ";",
      ## remove orig and rej files
      paste0("rm -f ", paste0(ctl_paths, ".orig")), ";",
      paste0("rm -f ", paste0(ctl_paths, ".rej")), ";",
      #    paste0("ls ", paste0(ctl_paths), "*;"),
      #    paste0("md5sum ", paste0(ctl_paths), ";"),
      patch_cmd, ";",
      #    paste0("rm -f ", paste0(ctl_paths, ".orig")), ";",
      #    paste0("rm -f ", paste0(ctl_paths, ".rej")), ";",
      #    paste0("ls ", paste0(ctl_paths), "*;"),
      #    paste0("md5sum ", paste0(ctl_paths), ";"),
      "echo patched file below ;",
      paste0("cat ", ctl_paths)
    )
    
    out <- system_nm(patch_cmd, dir = getwd(), intern = TRUE)
    
    out_file <- out[seq_along(out) > match("patched file below" , out)]
    preamble <- out[seq_along(out) < match("patched file below" , out)]
    cat(preamble, sep = "\n")
    
    was_nm_list <- inherits(m, "nm_list")
    
    m <- as_nm_generic(m) %>% 
      ctl_contents_simple(out_file)
    
    if(was_nm_list) m <- as_nm_list(m)
    
    
  }
  

  m <- m %>% ctl_contents_simple(out_file)
  
  invisible(m)
  
}

#' @export
apply_manual_edit.nm_list <- Vectorize_nm_list(apply_manual_edit.nm_generic, SIMPLIFY = FALSE)

manual_patch_app <- function() {
  
  ## have base object
  
  ## include a patch to modify
  
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
  
  old_behaviour <- overwrite_behaviour()
  overwrite_behaviour("skip")
  on.exit(overwrite_behaviour(old_behaviour))
  
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

new_patch_app <- function(){

  ctx <- rstudioapi::getActiveDocumentContext()
  selected_text <- ctx$selection[[1]]$text
  
  m <- get_single_object_for_app()
  
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
  
  code_to_add <- paste0(" %>%
  apply_manual_edit(\"", res$patch_name,"\")")
  
  rstudioapi::insertText(ctx$selection[[1]]$range$end, 
                         text = code_to_add,
                         id = ctx$id)
  
  message("apply_manual_edit() statement added to script")
  if(!is.na(parent_run_id(m)))
    warning("manual edits are subject to breakage if parent control files
are changed.  It is recommended to use dollar() for custom edits of downstream (child) runs")
  
}

modify_patch_app <- function(){
  
  ctx <- rstudioapi::getActiveDocumentContext()
  selected_text <- ctx$selection[[1]]$text
  
  before_edit <- gsub("(.*)%>%.*apply_manual_edit.*", "\\1", selected_text)
  
  patch_name <- gsub(".*%>%.*apply_manual_edit\\(\"(.*)\"\\).*", "\\1", selected_text)
  
  m <- eval(parse(text = before_edit))  
  
  res <- start_manual_edit_unix(m, combine_patch = patch_name)
  
  message(
    "---Manual edit---
    Instructions:
    1) edit control file
    2) save & close
    Press ENTER when done...")
  readline()
  
  ## now diff ctl_path(m) and old_file_path
  
  diff_manual_edit(m, res)
  
  code_to_add <- paste0(trimws(before_edit), " %>%
  apply_manual_edit(\"", res$patch_name,"\")")
  
  rstudioapi::insertText(text = code_to_add,
                         id = ctx$id)
  
  message("apply_manual_edit() statement modified in script")
  
  if(!is.na(parent_run_id(m)))
    warning("manual edits are subject to breakage if parent control files
are changed.  It is recommended to use dollar() for custom edits of downstream (child) runs")
  
}

make_patch_app <- function(){

  ctx <- rstudioapi::getActiveDocumentContext()
  selected_text <- ctx$selection[[1]]$text
  
  ## see if last function is apply_manual_edit
  
  ## get last function used in piping
  full_expr <- parse(text = selected_text)[[1]]
  
  if(as.character(full_expr[[1]]) == "<-"){
    full_expr <- full_expr[[3]]  ## rhs of assignment
  }
  
  if(as.character(full_expr[[1]]) == "%>%"){
    last_fun_name <- as.character(full_expr[[3]][[1]]) 
  }
  
  if(as.character(full_expr[[1]]) != "%>%"){ ## normal function
    last_fun_name <- as.character(full_expr[[1]]) 
  }
  
  if(last_fun_name == "apply_manual_edit") modify_patch_app() else
    new_patch_app()
    
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

overwrite_behaviour_app <- function() {
  
  shiny_dir <- system.file("extdata/overwrite_behaviour", package="NMproject")
  .sso_env$.currentwd <- getwd()  # see zzz.R for .sso_env
  on.exit({
    .sso_env$.currentwd <- NULL
  }, add = TRUE)
  #viewer <- shiny::paneViewer(300)
  viewer <- shiny::dialogViewer(dialogName = "overwrite behaviour")
  shiny::runGadget(shiny::shinyAppDir(shiny_dir), viewer = viewer)
  
}