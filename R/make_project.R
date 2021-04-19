#' Create analysis project
#'
#' @param path character path (relative or absolute) to project.  If just specifying a name, this will create the analysis project in the current working directory
#' @param dirs character list or vector.  Default = `nm_dirs()`
#' @param style character. Either "simple" (default) or "starters".  See details
#' @param ... arguments passted to `starters::create_analysis_package`
#'
#' @details if `style = "starters"` the function works as light wrapper around
#'   `starters::create_analysis_project`.  There will be restriction that `name`
#'   will not be able to contain underscores and will need to be comprised of
#'   only letters, numbers and periods. if `style = "simple"` the function
#'   sacrifices some of the R package-like structure so that underscores can be
#'   used.
#' @export

nm_create_analysis_project <- function(path, dirs = nm_dirs(), 
                                       style = c("simple",
                                                 "starters"), ...){
  
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
    
    #usethis::use_readme_rmd(open = FALSE)
    usethis::use_template("README.Rmd", data = list(Package = name), 
                          package = "starters")
    usethis::use_build_ignore("README.Rmd")
    
    ## no badges - skip this part of starters for now
    
    repo <- git2r::init(usethis::proj_get())
    git2r::add(repo, path = "README.Rmd")
    git2r::commit(repo, message = "added README.Rmd", all = TRUE)
    
    renv::scaffold(project = usethis::proj_get())

    for(dir_name in dirs) usethis::use_directory(dir_name, ignore = TRUE)

    tryCatch({
      usethis::use_description()
      desc::desc_set_dep(package = "renv", type = "Imports", 
                         file = usethis::proj_get())
      }, error = function(e){
      usethis::ui_info("skipping creation of {usethis::ui_path('DESCRIPTION')}")
    })
    
    tryCatch(usethis::use_namespace(), error = function(e){
      usethis::ui_info("skipping creation of {usethis::ui_path('NAMESPACE')}")
    })
    
    tryCatch(usethis::use_vignette(name = name), error = function(e){
      usethis::ui_info("skipping creation of {usethis::ui_path('vignette/')}")
    })

  }
  
  if(style %in% "starters"){
    if(!requireNamespace("starters")) stop("install package: starters", call. = FALSE)
    starters::create_analysis_project(name = name, 
                                      folder = folder,
                                      dirs = dirs, 
                                      git = FALSE,
                                      external_setup = NULL, ...)    
  }
  
  return(invisible(path))
}

#' stage files in project staging area ready for import
#'
#' @param files character vector. path of files to stage
#' @param destination default empty.  Optional destination directory
#'  by default will be equivalent location in staging/
#' @param additional_sub_dirs character vector. additional subdirectories
#'  not in standard tidyproject structure
#' @param overwrite logical (default = FALSE).
#' @param silent logical (default = FALSE)
#' @export
stage <- function(files, destination, additional_sub_dirs = nm_dirs(),
                  overwrite = FALSE, silent = FALSE){
  
  ## send unmodified files into staging area for importation
  
  files <- normalizePath(files, winslash = "/")
  
  ##########################
  sub_dirs <- c("SourceData",
                "DerivedData",
                "localpackage",
                "Scripts",
                "Models",
                models_dir(),
                "Results",
                unlist(additional_sub_dirs))
  
  sub_dirs <- unique(sub_dirs)
  
  sub_dirs <- basename(sub_dirs)
  
  sub_dirs <- unique(sub_dirs)
  
  key_dirs <- sub_dirs
  
  regex_key_dirs <- paste0("\\b", key_dirs, "\\b")
  
  files_sep <- strsplit(files, .Platform$file.sep)
  
  if(!missing(destination)){
    if(!grepl("staging", destination))
      stop("destination should be in staging area", call. = FALSE)
    destination <- file.path(destination, basename(files))
    destination <- relative_path(destination, "staging")
  } else {
    destination <- sapply(files_sep, function(file_sep){
      file_sep <- rev(file_sep)
      matches <- match(key_dirs, file_sep)
      if(all(is.na(matches))) return(NA_character_)
      matched_dir <- key_dirs[which.min(matches)]
      file_sep <- file_sep[seq_len(match(matched_dir, file_sep))]
      do.call(file.path, as.list(rev(file_sep)))
    })
  }
  
  d <- tibble::tibble(from = files, destination)
  d$staging <- file.path("staging", d$destination)
  
  d <- d[!is.na(d$destination), ]
  dir_names <- unique(dirname(d$staging))
  for(dir_name in dir_names) dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
  
  existing_files <- d$staging[file.exists(d$staging)]
  do_copy <- rep(TRUE, nrow(d))  ## default = copy
  if(!overwrite & length(existing_files)){
    #stop("File(s) already exist:\n",paste(paste0(" ",existing_files),collapse="\n"), "\nRename existing staged files or use overwrite=TRUE", call. = FALSE)
    if(!silent) message("File(s) not to be overwritten:\n",paste(paste0(" ",existing_files),collapse="\n"), "\nRename existing staged files or use overwrite=TRUE")
    #d <- d[!d$staging %in% existing_files, ]
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
#'   if data.frame should be output from \code{stage()}
#'   if character path, result will be \code{stage()}d first
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
  
  copy_script2(d_R$staging, d_R$destination, overwrite = overwrite)
  file.copy(d_other$staging, d_other$destination, overwrite = overwrite)  ## use copy_file instead?
  
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
#' @seealso \code{\link{ls_scripts}}, \code{\link{ls_code_library}}, \code{\link{stage}}
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
#' @seealso \code{\link{code_library}}, \code{\link{preview}}, \code{\link{stage}}, \code{\link{import}}
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

#' Show Code Library
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
#' @details requires \code{getOption("code_library_path")} to be set
#'
#' @seealso \code{\link{ls_code_library}}, \code{\link{preview}}
#' @examples
#'
#' \dontrun{
#'
#' code_library()
#'
#' }
#' @export
code_library <- function(extn = NULL, fields = "Description", viewer = TRUE, silent = FALSE, 
                         return_info = FALSE) {
  if (is.null(getOption("code_library_path"))) {
    if (!silent) {
      message("No directories attached. To attach add the following command:")
      message("  options(code_library_path=c(\"dir/of/scripts1\",\"dir/of/scripts2\",...))")
      message("     1. (for this session only) in the console")
      message("     2. (for this user) to ~/.Rprofile")
      message(paste0("     3. (for all users) to ", file.path(R.home(component = "home"), 
                                                              "etc", "Rprofile.site")))
      
      message(" 2. Attach for this user by putting command in ~/.Rprofile:")
    }
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
  if (!silent) 
    message("\nNOTE: Do not source scripts from the code library,\n copy them to your project with copy_script() or copy_file()")
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


#' Preview code_library file
#' @param name character indicating script in code_library to preview
#' @export
preview <- function(name) {
  ## preview files in code_library
  if(length(name)>1) stop("can only preview one file at a time")
  if (is_full_path(name)) {
    if (!file.exists(name)) 
      stop("file not found")
    file.show(name)
    return()
  }
  d <- code_library(extn = ".*", viewer = FALSE, silent = TRUE, return_info = TRUE, 
                    fields = c())
  if (!name %in% d$NAME) 
    stop("file not found in code_library")
  if (length(which(d$NAME %in% name)) > 1) 
    stop("Matched more than one file with that name.\n Try preview() again with full path")
  pos <- match(name, d$NAME)
  path <- file.path(d$FOLDER[pos], d$NAME[pos])
  file.show(path)
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

#' Replaces code library
#'
#' Replace code library search path with path(s)
#'
#' @param path character vector with paths to attach to

replace_code_library <- function(path) {
  options(code_library_path = unique(path))
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

#' commit individual file(s)
#' 
#' Has side effect that staged changed will be updated to working tree
#'
#' @param file_name character vector. File(s) to be committed
#' @export
#' @examples 
#' \dontrun{
#' commit_file("Scripts/script1.R")
#' }
commit_file <- function(file_name){
  repo <- git2r::repository(".")
  
  old_staged_files <- git2r::status(repo)
  old_staged_files <- unlist(old_staged_files$staged)
  
  if(length(old_staged_files) > 0) {
    on.exit(git2r::add(repo, path = old_staged_files))
    git2r::reset(repo, path = old_staged_files)
  }
  
  git2r::add(repo,path = file_name)
  new_staged_files <- git2r::status(repo)
  new_staged_files <- unlist(new_staged_files$staged)
  if(length(new_staged_files) == 0){
    if(getOption("git.command.line.available")){
      for(f in file_name) system_cmd(paste("git add",f))
      new_staged_files <- git2r::status(repo)
      new_staged_files <- unlist(new_staged_files$staged)
      if(length(new_staged_files) == 0){
        #nothing to commit. file already commited
        return(invisible())
      }
    } else {
      #Skipping adding files to repo: git2r failed. Do it manually if needed
      return(invisible())
    }
  }
  git2r::commit(repo,message = paste("snapshot:", paste(file_name, collapse = ",")))
}

#' system/shell command wrapper
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

#' Copy script to project directory
#'
#' Will search code library and copy script and dependencies into scripts directory.
#' Script will also be stamped with source location, time and user information
#'
#' @param from character. file name or path of file to copy
#' @param to character. file name.  default = same as from
#' @param stamp_copy logical. Create a commented timestamp at beginning of file
#' @param overwrite logical. Overwrite 'to' file if exists?
#' @param comment_char character. Comment character
#' @export
copy_script2 <- function(from, to = file.path(scripts_dir(), basename(from)), 
                         stamp_copy = TRUE, overwrite = FALSE, comment_char = "#") {
  ## User function: copies script from one location (e.g. code_library) to project
  ## scripts directory
  
  d <- data.frame(from, to, stringsAsFactors = FALSE)
  
  already_exist <- file.exists(d$to)
  
  if(!overwrite & any(already_exist))
    message("File(s) already exist:\n",
            paste(paste0("  ",d$to[already_exist]),collapse="\n"),
            "\nRename existing files or use overwrite=TRUE")
  
  d <- d[!already_exist, ]
  
  for(i in seq_len(nrow(d))){
    suppressWarnings(s0 <- readLines(d$from[i]))
    ## modify text at top of 'd$from'
    if (stamp_copy) 
      s <- c(paste0(comment_char, comment_char, " Copied from ", d$from[i], "\n##  (", 
                    Sys.time(), ") by ", Sys.info()["user"]), s0) else s <- s0
                    writeLines(s, d$to[i])
                    
  }
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

if(FALSE){
  unlink("~/projects/test.analysis.project", recursive = TRUE)
  nm_create_analysis("test.analysis.project", folder = "~/projects/", style = "starters")
  
  # # opens vignettes and doesn't allow dirs
  # unlink("~/projects/test.package.project", recursive = TRUE)
  # nm_create_package_project("test.package.project", folder = "~/projects/")
  
  unlink("~/projects/test_simple_analysis", recursive = TRUE)
  nm_create_analysis("test_simple_analysis", folder = "~/projects/", style = "simple")  
}
