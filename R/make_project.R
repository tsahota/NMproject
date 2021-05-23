#' Create analysis project
#'
#' Easiest way to use this function is via RStudio menus (FILE -> New Project ->
#' New Directory -> New NMproject).  However the function can be used directly
#' too.
#'
#' @param path character path (relative or absolute) to project.  If just
#'   specifying a name, this will create the analysis project in the current
#'   working directory
#' @param dirs character list or vector.  Default = `nm_default_dirs()`
#' @param style character. Currently only "simple" See details
#' @param use_renv logical (default = FALSE). should renv be used or not in
#'   project.
#' @param ... deprecated
#'
#' @details The function works like as is inspired by
#'   `starters::create_analysis_project`. There is no restriction on directory
#'   name.  It is therefore possible to violate R package naming conventions.
#'   This is to cater to users who like underscores and aren't interested in
#'   creating a package.
#'
#' @export

nm_create_analysis_project <- function(path, dirs = nm_default_dirs(), 
                                       style = c("simple"), 
                                       use_renv = FALSE, ...){
  
  validate_dir_list(dirs)
  
  style <- match.arg(style)
  name <- basename(path)
  folder <- dirname(path)
  
  if(style == "simple"){

    usethis::create_project(path = path, rstudio = TRUE, open = FALSE)
    
    current_proj <- try(usethis::proj_get(), silent = TRUE)
    if (inherits(current_proj, "try-error")) {
      current_proj <- NULL
    }
    usethis::proj_set(path)
    on.exit(usethis::proj_set(current_proj))
    
    
    usethis::use_template("README.Rmd", data = list(Package = name),
                          package = "NMproject")
    
    usethis::use_build_ignore("README.Rmd")
    
    ## no badges - skip this part of starters for now
    repo <- git2r::init(usethis::proj_get())
    git2r::add(repo, path = "README.Rmd")
    
    tryCatch({
      git2r::commit(repo, message = "added README.Rmd", all = TRUE)
    }, error = function(e){
      usethis::ui_oops("cannot commit {usethis::ui_path('README.Rmd')}. Aborting...")
    })
    
    if(use_renv) {
      if(!requireNamespace("renv", quietly = TRUE))
        stop("install renv", call. = FALSE)
      if(!requireNamespace("desc", quietly = TRUE))
        stop("install desc", call. = FALSE)
    }
    
    if(use_renv) renv::scaffold(project = usethis::proj_get())

    for(dir_name in dirs) usethis::use_directory(dir_name, ignore = TRUE)

    tryCatch({
      usethis::use_description(check_name = FALSE)
      if(use_renv) desc::desc_set_dep(package = "renv", type = "Imports", 
                                      file = usethis::proj_get())
    }, error = function(e){
      usethis::ui_info("skipping creation of {usethis::ui_path('DESCRIPTION')}")
    })
    
    tryCatch({
      usethis::use_namespace()
    }, error = function(e){
      usethis::ui_info("skipping creation of {usethis::ui_path('NAMESPACE')}")
    })
    
  }
  
  # if(style %in% "starters"){
  #   if(!requireNamespace("starters", quietly = TRUE)) 
  #     stop("install package: starters", call. = FALSE)
  #   starters::create_analysis_project(name = name, 
  #                                     folder = folder,
  #                                     dirs = dirs, 
  #                                     git = FALSE,
  #                                     external_setup = NULL, ...)    
  # }
  
  set_default_dirs_in_rprofile(file.path(folder, name, ".Rprofile"), dirs)
  return(invisible(path))
}



#' Stage files in project staging area ready for import
#'
#' @param files character vector. path of files to stage
#' @param root_dir character path to root directory of `files`. Staged files
#'   relative to `staging` directory will be same as `files` to `root_dir`. If
#'   this is not specified, will guess based on presense of `nm_default_dirs`
#' @param overwrite logical (default = FALSE).
#' @param silent logical (default = FALSE)
#' @export
stage <- function(files, root_dir,
                  overwrite = FALSE, silent = FALSE){
  
  ## send unmodified files into staging area for importation
  
  files <- normalizePath(files, winslash = "/")
  
  ##########################
  if(missing(root_dir)){
    roots <- sapply(files, function(file) {
      rprojroot::find_root(rprojroot::has_dir(nm_default_dir("scripts")) |
                             rprojroot::has_dir(nm_default_dir("models")) |
                             rprojroot::has_dir("Scripts") |
                             rprojroot::has_dir("Models"), 
                           path = file)
    })
    names(roots) <- NULL
    unique_roots <- unique(roots)
    if(length(unique_roots) == 1) root_dir <- unique_roots
    if(length(unique_roots) == 0) stop("cannot determine root directory.  Specify root_dir argument")
    if(length(unique_roots) > 1){
      stop("can't guess root_dir, multiple candidates: \n  ", paste0(unique_roots, collapse = "\n  "), "specify root_dir argument in stage()")
    }
  }
  
  destination <- relative_path(files, root_dir)
  destination <- gsub("^Scripts/", paste0(nm_default_dir("scripts"), "/"), destination)
  destination <- gsub("^Models/", paste0(nm_default_dir("models"), "/"), destination)
  destination <- gsub("^Results/", paste0(nm_default_dir("results"), "/"), destination)
  
  d <- dplyr::tibble(from = files, destination)
  d$staging <- file.path("staging", d$destination)
  
  d <- d[!is.na(d$destination), ]
  dir_names <- unique(dirname(d$staging))
  for(dir_name in dir_names) dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
  
  existing_files <- d$staging[file.exists(d$staging)]
  do_copy <- rep(TRUE, nrow(d))  ## default = copy
  if(!overwrite & length(existing_files)){
    if(!silent) message("File(s) not to be overwritten:\n",paste(paste0(" ",existing_files),collapse="\n"), "\nRename existing staged files or use overwrite=TRUE")
    do_copy[file.exists(d$staging)] <- FALSE
  }
  
  file.copy(d$from[do_copy],
            d$staging[do_copy],
            overwrite = overwrite)
  
  if(!silent) message("File(s) staged in project:\n",paste(paste0(" ",d$staging[do_copy]),collapse="\n"), "\nTo import use import()")
  
  invisible(d)
  
}

#' Import staged files into project
#'
#' @param copy_table data frame or character.
#'   if data.frame should be output from `stage()`
#'   if character path, result will be `stage()`d first
#' @param overwrite logical (default = FALSE)
#' @param silent logical (default = FALSE)
#' @param skip character (default = "\\.mod$"). pattern to skip
#' @export
import <- function(copy_table, overwrite = FALSE, silent = FALSE,
                   skip = "\\.mod$"){
  
  ## import the files_to_copy
  
  ## R scripts in Scripts to be copied with the stamp at the top
  ## Code in Models/. not to be copied - this will be handled by nm() %>% ctl("staging/...")
  ## everything else copied as is
  
  if(is.character(copy_table)){
    copy_table <- stage(copy_table, overwrite = overwrite, silent = silent)
  }
  
  copy_table <- copy_table[!is.na(copy_table$destination), ]
  ## skip everything in Models
  copy_table <- copy_table[!grepl(skip, copy_table$destination), ]
  
  copy_table$extn <- tools::file_ext(copy_table$destination)
  
  d_R <- copy_table[copy_table$extn %in% c("r", "R"), ]
  d_other <- copy_table[!copy_table$extn %in% c("r", "R"), ]
  
  existing_files <- c(
    d_R$destination[file.exists(d_R$destination)],
    d_other$destination[file.exists(d_other$destination)]
  )
  if(!overwrite & length(existing_files) > 0){
    #stop("File(s) already exist:\n",paste(paste0(" ",existing_files),collapse="\n"), "\nRename existing staged files or use overwrite=TRUE", call. = FALSE)
    if(!silent) message("File(s) not to be overwritten:\n",paste(paste0(" ",existing_files),collapse="\n"), "\nRename existing project files or use overwrite=TRUE")
    copy_table <- copy_table[!copy_table$destination %in% existing_files, ]
  }
  
  dirs <- dirname(c(d_R$destination, d_other$destination))
  dirs <- unique(dirs)
  for(path in dirs) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  
  file.copy(d_R$staging, d_R$destination, overwrite = overwrite)
  file.copy(d_other$staging, d_other$destination, overwrite = overwrite)
  
  message("Files imported:\n ",
          paste(copy_table$destination, collapse = "\n "))
  
  invisible()
  
}

#' List scripts
#'
#' @param folder string describing folder to search recursively in
#' @param extn character (can be regex) giving extension to limit search to
#' @param recursive by default TRUE
#' @examples
#' \dontrun{
#' ls_scripts('~/AZD6094/PK_liver4/') %>%
#'   info_scripts('Description') %>%
#'   filter(grepl('mod',DESCRIPTION))
#' }
#' @export

ls_scripts <- function(folder = ".", extn = "r|R|Rmd|rmd", recursive = TRUE) {
  if (is.null(extn)) {
    output <- dir(folder, recursive = recursive, full.names = TRUE)
  } else {
    file_match <- paste0("\\.(", extn, ")$")
    output <- dir(folder, recursive = recursive, full.names = TRUE, pattern = file_match)
  }
  return(normalizePath(output, winslash = "/"))
}

#' List information about scripts
#'
#' @param files vector string of file names/paths
#' @param fields vector string of field tags to display
#' @param viewer logical indicating if Rstudio viewer should be used (default = TRUE)
#' @param silent run in quiet mode (default=FALSE)
#' @param base_dirs character vector. group files together that belong to these directory paths
#' @param shorten_paths logical. Default = TRUE. Long paths will be shortened if true in displayed output (not returned object)
#' @examples
#' \dontrun{
#' ls_scripts('~/AZD6094/PK_liver4/') %>%
#'   info_scripts('Description') %>%
#'   filter(grepl('mod',DESCRIPTION))
#' }
#' @export
info_scripts <- function(files, fields = c("Description"), viewer = TRUE, silent = FALSE, 
                         base_dirs = NULL, shorten_paths = TRUE) {
  if (length(fields) > 0) {
    res <- lapply(files, function(file.name) {
      ## per file
      suppressWarnings({
        s <- readLines(file.name, n = 30)
        field.vals <- as.data.frame(lapply(fields, function(field) {
          field <- gsub(paste0("^.*",field,"s*:\\s*(.*)$"), "\\1",
                        s[grepl(paste0("^.*", field, "s*:\\s*"), s,ignore.case = TRUE)],
                        ignore.case = TRUE)
          field <- field[!field %in% ""]
          if (length(field) == 0) 
            return(as.character(NA))
          field[1]  ## in case multiple, take only first
        }))
        names(field.vals) <- fields
      })
      field.vals
    })
    res <- do.call(rbind, res)
  } else res <- data.frame(row.names = seq_along(files))
  
  d <- cbind(data.frame(FULL = normalizePath(files, winslash = "/"),
                        FOLDER = normalizePath(dirname(files), winslash = "/"),
                        NAME = basename(files), stringsAsFactors = FALSE), res)
  
  if (!is.null(base_dirs)) {
    base_dirs <- normalizePath(base_dirs, winslash = "/")
    
    all_matches <- unlist(lapply(base_dirs, function(base_dir) {
      grep(paste0("^", base_dir), d$FULL)
    }))
    
    if (length(unique(all_matches)) != length(all_matches)) 
      stop("duplicate file matches found. Check base directories are not subsets of one another")
    
    for (base_dir in base_dirs) {
      match_base <- grepl(paste0("^", base_dir), d$FULL)
      
      d$FOLDER[match_base] <- gsub(paste0("^(", base_dir, ").*$"), "\\1", d$FULL[match_base])
      d$NAME[match_base] <- gsub(paste0("^", base_dir, .Platform$file.sep, "*(.*)$"), 
                                 "\\1", d$FULL[match_base])
    }
  }
  
  d <- cbind(data.frame(FOLDER = d$FOLDER, NAME = d$NAME, stringsAsFactors = FALSE), 
             res)
  
  if (shorten_paths) {
    dshort <- cbind(data.frame(FOLDER = short_path(d$FOLDER), NAME = d$NAME, stringsAsFactors = FALSE), 
                    res)
  } else {
    dshort <- d
  }
  
  if (!silent) {
    if (viewer) 
      get("View")(dshort, "available files")
  }
  invisible(d)
}

#' Search for files matching raw text search
#'
#' @param files vector string of file names/paths
#' @param text string (can be regex) to search for
#' @param search_title logical (default=TRUE). should matching occur in title
#' @param search_contents logical (default=TRUE). should matching occur in file contents
#' 
#' @seealso [ls_scripts()], [ls_code_library()], [stage()]
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' ls_scripts("Scripts") %>% search_raw("AUC")  ## finds all scripts containing string "AUC"
#' 
#' ## regex match find instances of AUC() function being used
#' ls_scripts("Scripts") %>% search_raw("AUC\\(")
#' 
#' ## bring file(s) into staging area of project
#' ls_scripts("Scripts") %>% search_raw("AUC\\(") %>% stage()
#' 
#' }
#' 
#' @export

search_raw <- function(files, text, search_title=TRUE, search_contents=TRUE) {
  res <- unlist(sapply(files, function(file.name) {
    if(search_contents){
      suppressWarnings(s <- readLines(file.name))
      s <- grep(text, s)
    } else s <- c()
    if(search_title) s <- c(s,grep(text, file.name))
    if (suppressWarnings(length(s) == 0)) 
      return(NULL) else return(file.name)
  }))
  names(res) <- NULL
  res
}

#' List files in code library
#'
#' @param pattern optional character. filter the code library use regex
#' 
#' @seealso [code_library()], [preview()], [stage()], [import()]
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' ls_code_library("Models/ADVAN2.mod") %>%
#'   stage()
#'   
#' }
#' 
#' @export

ls_code_library <- function(pattern = ".") {
  paths <- ls_scripts(extn = ".*", folder = getOption("code_library_path"), recursive = TRUE)
  paths[grepl(pattern, paths)]
}

#' Code Library
#'
#' Function not designed for direct use.  Instead use the RStudio "addin" on the
#' Addins menu. In the shiny, select the file, and click "preview" to view and
#' [stage()] to bring into the "staging" area of your project.  See
#' vignette at <https://tsahota.github.io/NMproject/> for a video showing use
#' of the app. Non-NONMEM code can then be imported into the project separately
#' with the the [import()] function.
#'
#' @param extn vector string of extensions to include
#' @param fields character vector of fields to extract
#' @param viewer logical indicating if viewer should be used to display results
#'   (default=FALSE)
#' @param silent logical indicating if messages should be silenced
#'   (default=FALSE)
#' @param return_info logical (default = FALSE). Return data.frame of results
#'   (FALSE= returns file paths)
#'
#' @details requires `getOption("code_library_path")` to be set
#'
#' @seealso [ls_code_library()], [preview()],
#'   [stage()], [import()]
#'
#' @export
code_library <- function(extn = NULL, fields = "Description", viewer = TRUE, silent = FALSE, 
                         return_info = FALSE) {
  if (is.null(getOption("code_library_path"))) {
    stop("no code library found check getOption(\"code_library_path\")")
    return(invisible(data.frame()))
  }
  
  files <- ls_code_library()
  if (!is.null(extn)) {
    file_match <- paste0("\\.(", extn, ")$")
    files <- files[grepl(file_match, files)]
  }
  
  if (viewer == FALSE & !return_info) {
    return(files)
  }
  tryCatch({
    info <- info_scripts(files, fields = fields, viewer = viewer, silent = silent, 
                         base_dirs = getOption("code_library_path"))
  }, error = function(e) {
    if (grepl("duplicate file", e$message)) 
      e$message <- paste0(e$message, ".\n  Check getOption(\"code_library_path\") points to non-overlapping folders")
    stop(e)
  })
  if (return_info) {
    if (silent) 
      return_ob <- invisible(info) else return_ob <- info
  } else {
    return_ob <- normalizePath(files, winslash = "/")
  }
  if (viewer == FALSE) 
    return(info)
  if (viewer == TRUE) 
    return(invisible(files))
  
}


#' Display code library search path
#'
#' @export
code_library_path <- function() getOption("code_library_path")

#' Attach code library
#'
#' Attaches a path(s) to to the code library search path
#'
#' @param path character vector with paths to attach to

attach_code_library <- function(path) {
  options(code_library_path = unique(c(path, getOption("code_library_path"))))
}

short_path <- function(x) {
  split_paths <- strsplit(x, .Platform$file.sep)  #[[1]]
  short_paths <- lapply(split_paths, function(split_path) {
    if (length(split_path) > 5) 
      split_path.short <- c(split_path[1:3], "..", split_path[(length(split_path) - 
                                                                 1):length(split_path)]) else split_path.short <- split_path
                                                                 do.call(file.path, as.list(split_path.short))
  })
  unlist(short_paths)
}

#' System/shell command wrapper
#'
#' @param cmd character. command to send to shell
#' @param dir character. directory to run command in
#' @param ... other arguments passed to system command
#' @export
system_cmd <- function(cmd,dir=".",...){
  if(!dir %in% ".") if(file.exists(dir)) {currentwd <- getwd(); setwd(dir) ; on.exit(setwd(currentwd))} else
    stop(paste0("Directory \"",dir,"\" doesn't exist."))
  getOption("system_cmd")(cmd,...)
}

#' List directories
#' 
#' Wrapper around list.dirs() but includes maxdepth and pattern arguments
#'  and removes full.names argument, always return full names.
#' 
#' @param path same as list.dirs()
#' @param full.names same as list.dirs()
#' @param recursive same as list.dirs()
#' @param maxdepth integer (default = 1) maximum depth to search
#' @param pattern character (default = missing) regex pattern match on directory name
#' 
#' @export
list_dirs <- function(path = ".", full.names = TRUE, recursive = FALSE, maxdepth = 1, pattern){
  
  dirs <- list()
  dirs[[1]] <- list.dirs(path, full.names = TRUE, recursive = FALSE)
  
  missing_pattern <- missing(pattern)
  
  return_ready <- function(dirs){ ## prep for return character vector
    dirs <- unlist(dirs)
    if(!missing_pattern) dirs <- dirs[grepl(pattern, basename(dirs))]
    if(!full.names) dirs <- basename(dirs)
    dirs
  }
  
  if(maxdepth == 1 | !recursive) return(return_ready(dirs))
  
  i <- 2
  while(i <= maxdepth){
    #for(i in 2:maxdepth){
    current_dirs <- sapply(dirs[[i-1]], function(path){
      list.dirs(path, full.names = TRUE, recursive = FALSE)
    })
    current_dirs <- unlist(current_dirs)
    names(current_dirs) <- NULL
    
    ## breakpoint if no more dirs
    if(length(current_dirs) == 0) return(return_ready(dirs))
    
    dirs[[i]] <- current_dirs
    i <- i + 1
  }
  
  return(return_ready(dirs))
  
}

#' Wait for statement to be true
#'
#' @param x expression to evaluate
#' @param timeout numeric. Maximum time (seconds) to wait
#' @param interval numeric. Number of seconds (default=1s) to wait till rechecking
#' @export
wait_for <- function(x,timeout=NULL,interval=1){
  x <- substitute(x)
  start.time <- Sys.time()
  diff.time <- 0
  while (!eval(x,envir = parent.frame())){
    diff.time <- difftime(Sys.time(),start.time,units="secs")
    if(!is.null(timeout))if(diff.time > timeout) {
      warning(paste("timed out waiting for\n",x,sep=""))
      return(invisible(FALSE))
    }
    Sys.sleep(1)
  }
  invisible(TRUE)
}

