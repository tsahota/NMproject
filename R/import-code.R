#' Stage files in project staging area ready for import
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Staging is a preliminary step of bringing code from external to the project
#' into the project.  The intent is it remains a snapshot of code as it was at
#' the time of `import`ing.  This aids in reproducibility because if that
#' external code is changed, the staged code will remain fixed.
#'
#' In practice, this function will rarely need to be used directly.  The easiest
#' way to bring code is via the "code library" RStudio 'Addin' shiny app.
#'
#' @param files Character vector. path of files to stage.
#' @param root_dir Character path to root directory of `files`. Staged files
#'   relative to `staging` directory will be same as `files` to `root_dir`. If
#'   this is not specified, will guess based on presence of `nm_default_dirs`
#' @param overwrite Logical (default = FALSE).
#' @param silent Logical (default = FALSE).
#'
#' @seealso [code_library()], [import()]
#'
#' @examples
#'
#' \dontrun{
#'
#' ls_code_library("Models/ADVAN2.mod") %>%
#'   stage()
#' }
#'
#' @export
stage <- function(files, root_dir,
                  overwrite = FALSE, silent = FALSE) {

  ## send unmodified files into staging area for importation

  files <- normalizePath(files, winslash = "/")

  ##########################
  if (missing(root_dir)) {
    roots <- sapply(files, function(file) {
      rprojroot::find_root(rprojroot::has_dir(nm_default_dir("scripts")) |
        rprojroot::has_dir(nm_default_dir("models")) |
        rprojroot::has_dir("Scripts") |
        rprojroot::has_dir("Models"),
      path = file
      )
    })
    names(roots) <- NULL
    unique_roots <- unique(roots)
    if (length(unique_roots) == 1) root_dir <- unique_roots
    if (length(unique_roots) == 0) stop("cannot determine root directory.  Specify root_dir argument")
    if (length(unique_roots) > 1) {
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
  ### create staging with usethis
  current_proj <- try(usethis::proj_get(), silent = TRUE)
  if (inherits(current_proj, "try-error")) {
    current_proj <- NULL
  }
  usethis::ui_silence(usethis::proj_set(getwd()))
  on.exit(usethis::ui_silence(usethis::proj_set(current_proj)))
  usethis::use_directory("staging", ignore = TRUE)
  usethis::ui_silence(usethis::proj_set(current_proj))
  for (dir_name in dir_names) dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)

  existing_files <- d$staging[file.exists(d$staging)]
  do_copy <- rep(TRUE, nrow(d)) ## default = copy
  if (!overwrite & length(existing_files)) {
    if (!silent) message("File(s) not to be overwritten:\n", paste(paste0(" ", existing_files), collapse = "\n"), "\nRename existing staged files or use overwrite=TRUE")
    do_copy[file.exists(d$staging)] <- FALSE
  }

  file.copy(d$from[do_copy],
    d$staging[do_copy],
    overwrite = overwrite
  )

  if (!silent) message("File(s) staged in project:\n", paste(paste0(" ", d$staging[do_copy]), collapse = "\n"), "\nTo import use import()")

  invisible(d)
}

#' Import staged files into project
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This function is used by the "code libary" RStudio 'Addin' to bring external
#' code into your project.
#'
#' @param copy_table A `data frame` or character. if `data.frame` should be
#'   output from `stage()`, if character path, result will be `stage()`d first.
#' @param overwrite Logical (default = `FALSE`).
#' @param silent Logical (default = `FALSE`).
#' @param skip Character (default = `"\\.mod$"`). Pattern to skip.  Model files
#'   will be imported directly into the project in order to avoid conflicts and
#'   will instead reside only in the staging area.
#'
#' @return Invisibly returns `copy_table` argument.
#'
#' @seealso [code_library()], [stage()]
#'
#' @examples
#'
#' \dontrun{
#'
#' ## both of these following operations are easier in the shiny code library
#' ## RStudio 'Addin'.
#'
#' ls_code_library("Models/ADVAN2.mod") %>%
#'   import() ## ends up in "staging/Models"
#'
#' ls_code_library("Scripts/AUC.R") %>%
#'   import() ## ends up "scripts" directory
#' }
#'
#' @export

import <- function(copy_table, overwrite = FALSE, silent = FALSE,
                   skip = "\\.mod$") {

  ## import the files_to_copy

  ## R scripts in Scripts to be copied with the stamp at the top
  ## Code in Models/. not to be copied - this will be handled by nm() %>% ctl("staging/...")
  ## everything else copied as is

  copy_table_orig <- copy_table

  if (is.character(copy_table)) {
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
  if (!overwrite & length(existing_files) > 0) {
    # stop("File(s) already exist:\n",paste(paste0(" ",existing_files),collapse="\n"), "\nRename existing staged files or use overwrite=TRUE", call. = FALSE)
    if (!silent) message("File(s) not to be overwritten:\n", paste(paste0(" ", existing_files), collapse = "\n"), "\nRename existing project files or use overwrite=TRUE")
    copy_table <- copy_table[!copy_table$destination %in% existing_files, ]
  }

  dirs <- dirname(c(d_R$destination, d_other$destination))
  dirs <- unique(dirs)
  for (path in dirs) dir.create(path, recursive = TRUE, showWarnings = FALSE)

  R_copy <- file.copy(d_R$staging, d_R$destination, overwrite = overwrite)
  other_copy <- file.copy(d_other$staging, d_other$destination, overwrite = overwrite)

  if (!silent) {
    message(
      "Files imported:\n ",
      paste(copy_table$destination, collapse = "\n ")
    )
  }

  copy_table_orig$imported <- copy_table_orig$destination %in%
    copy_table$destination

  invisible(copy_table_orig)
}

#' List scripts
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' @param folder String describing folder to search recursively in.
#' @param extn Character (can be regex) giving extension to limit search to.
#' @param recursive Logical (default = `TRUE`).  Should directories be searched
#'   recursively.
#'   
#' @return Character vector of matched file paths.
#' 
#' @examples
#' \dontrun{
#'
#' ## find all scripts with the string "AUC("
#' ls_scripts("~/path/to/analysis/Scripts") %>% search_raw("AUC\\(")
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
#' @param files Vector string of file names/paths.
#' @param fields Vector string of field tags to display.
#' @param viewer Logical indicating if RStudio viewer should be used (default =
#'   `TRUE`).
#' @param silent Run in quiet mode (default=`FALSE`).
#' @param base_dirs Character vector. group files together that belong to these
#'   directory paths.
#' @param shorten_paths Logical (default = `TRUE`). Long paths will be shortened
#'   if true in displayed output (not returned object).
#'   
#' @return A `tibble` with extracted information from `files`.
#' @keywords internal

info_scripts <- function(files, fields = c("Description"), viewer = TRUE, silent = FALSE,
                         base_dirs = NULL, shorten_paths = TRUE) {
  if (length(fields) > 0) {
    res <- lapply(files, function(file.name) {
      ## per file
      suppressWarnings({
        s <- readLines(file.name, n = 30)
        
        ######
        ## look for roxygen comments
        if(requireNamespace("roxygen2")) {
          res <- try(
            {
              has_roxygen <- any(grepl("^#'\\s*.+$", s))
              if(has_roxygen) {
                block <- roxygen2::parse_file(file.name)[[1]]
                title <- roxygen2::block_get_tags(block, tags = "title")
                if(length(title) > 0) {
                  title <- title[[1]][['val']]
                  return(data.frame(Description = title))
                }
              } 
            }, silent = TRUE
          )
          if(inherits(res, "data.frame")) {
            if(nrow(res) == 1){
              return(res)
            }
          }
        }
        #####
        
        field.vals <- as.data.frame(lapply(fields, function(field) {
          field <- gsub(paste0("^.*", field, "s*:\\s*(.*)$"), "\\1",
            s[grepl(paste0("^.*", field, "s*:\\s*"), s, ignore.case = TRUE)],
            ignore.case = TRUE
          )
          field <- field[!field %in% ""]
          if (length(field) == 0) {
            return(as.character(NA))
          }
          field[1] ## in case multiple, take only first
        }))
        names(field.vals) <- fields
      })
      field.vals
    })
    res <- do.call(rbind, res)
  } else {
    res <- data.frame(row.names = seq_along(files))
  }

  d <- cbind(data.frame(
    FULL = normalizePath(files, winslash = "/"),
    FOLDER = normalizePath(dirname(files), winslash = "/"),
    NAME = basename(files), stringsAsFactors = FALSE
  ), res)

  if (!is.null(base_dirs)) {
    base_dirs <- normalizePath(base_dirs, winslash = "/")

    all_matches <- unlist(lapply(base_dirs, function(base_dir) {
      grep(paste0("^", base_dir), d$FULL)
    }))

    if (length(unique(all_matches)) != length(all_matches)) {
      stop("duplicate file matches found. Check base directories are not subsets of one another")
    }

    for (base_dir in base_dirs) {
      match_base <- grepl(paste0("^", base_dir), d$FULL)

      d$FOLDER[match_base] <- gsub(paste0("^(", base_dir, ").*$"), "\\1", d$FULL[match_base])
      d$NAME[match_base] <- gsub(
        paste0("^", base_dir, .Platform$file.sep, "*(.*)$"),
        "\\1", d$FULL[match_base]
      )
    }
  }

  d <- cbind(
    data.frame(FOLDER = d$FOLDER, NAME = d$NAME, stringsAsFactors = FALSE),
    res
  )

  if (shorten_paths) {
    dshort <- cbind(
      data.frame(FOLDER = short_path(d$FOLDER), NAME = d$NAME, stringsAsFactors = FALSE),
      res
    )
  } else {
    dshort <- d
  }

  if (!silent) {
    if (viewer) {
      get("View")(dshort, "available files")
    }
  }
  invisible(d)
}

#' Search for files matching raw text search
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Searches through the list of supplied for matching strings of text.  Useful
#' in finding files that you know contain certain text snippets.
#'
#' @param files Vector string of files (either names or paths).
#' @param text String (can be regex) to search for.
#' @param search_title Logical (default=`TRUE`). Should matching occur in title.
#' @param search_contents Logical (default=`TRUE`). Should matching occur in
#'   file contents.
#'
#' @seealso [ls_scripts()], [ls_code_library()], [stage()]
#'
#' @examples
#'
#' \dontrun{
#'
#' ls_scripts("Scripts") %>% search_raw("AUC") ## finds all scripts containing string "AUC"
#'
#' ## regex match find instances of AUC() function being used
#' ls_scripts("Scripts") %>% search_raw("AUC\\(")
#'
#' ## bring file(s) into project
#' ls_scripts("/path/to/other/analysis/scripts/dir") %>%
#'   search_raw("AUC\\(") %>%
#'   import()
#' }
#'
#' @export

search_raw <- function(files, text, search_title = TRUE, search_contents = TRUE) {
  res <- unlist(sapply(files, function(file.name) {
    if (search_contents) {
      suppressWarnings(s <- readLines(file.name))
      s <- grep(text, s)
    } else {
      s <- c()
    }
    if (search_title) s <- c(s, grep(text, file.name))
    if (suppressWarnings(length(s) == 0)) {
      return(NULL)
    } else {
      return(file.name)
    }
  }))
  names(res) <- NULL
  res
}

#' List files in code library
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' A low level function to interact with the code library.  It is easier in most
#' cases to use the shiny "code library" RStudio 'Addin'.
#'
#' @param pattern Optional character. Filter the code library use regex.
#'
#' @seealso [code_library()], [preview()], [stage()], [import()]
#' 
#' @return Character vector of matched file paths.
#'
#' @examples
#'
#' \dontrun{
#'
#' ls_code_library("Models/ADVAN2.mod") %>%
#'   stage()
#' }
#'
#' @export

ls_code_library <- function(pattern = ".") {
  paths <- ls_scripts(extn = ".*", folder = getOption("code_library_path"), recursive = TRUE)
  paths[grepl(pattern, paths)]
}

#' Code Library
#'
#' Function not designed for direct use.  Instead use the RStudio `code library`
#' entry on the RStudio 'Addins' menu. This will open the shiny app. Select the file, and
#' click "preview" to view and [import()] to bring into the "staging" area of
#' your project.  See vignette at <https://tsahota.github.io/NMproject/> for a
#' video showing use of the app. NONMEM control files will intentionally not be
#' imported straight in the "Models" directory and instead go into
#' "staging/Models".  This staging location can be referred to when creating
#' `nm` objects with `new_nm(..., based_on = "staging/Models/[filename]")`.
#'
#' @param extn Vector string of extensions to include (default = `NULL` includes
#'   all).
#' @param fields Character vector of fields to extract.
#' @param viewer Logical indicating if viewer should be used to display results
#'   (default=`FALSE`).
#' @param silent Logical indicating if messages should be silenced
#'   (default=`FALSE`).
#' @param return_info Logical (default = `FALSE`). Return data.frame of results
#'   (FALSE= returns file paths).
#'
#' @details Requires `getOption("code_library_path")` to be set.
#'
#' @return If `return_info = TRUE`, invisibly returns output a `tibble` with
#'   code library information.  Otherwise (this may be deprecated soon), will return paths to code library
#'   files.
#'
#' @seealso [ls_code_library()], [preview()], [stage()], [import()]
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
  tryCatch(
    {
      info <- info_scripts(files,
        fields = fields, viewer = viewer, silent = silent,
        base_dirs = getOption("code_library_path")
      )
    },
    error = function(e) {
      if (grepl("duplicate file", e$message)) {
        e$message <- paste0(e$message, ".\n  Check getOption(\"code_library_path\") points to non-overlapping folders")
      }
      stop(e)
    }
  )
  if (return_info) {
    if (silent) {
      return_ob <- invisible(info)
    } else {
      return_ob <- info
    }
  } else {
    return_ob <- normalizePath(files, winslash = "/")
  }
  if (viewer == FALSE) {
    return(info)
  }
  if (viewer == TRUE) {
    return(invisible(files))
  }
}


short_path <- function(x) {
  split_paths <- strsplit(x, .Platform$file.sep) # [[1]]
  short_paths <- lapply(split_paths, function(split_path) {
    if (length(split_path) > 5) {
      split_path.short <- c(split_path[1:3], "..", split_path[(length(split_path) -
        1):length(split_path)])
    } else {
      split_path.short <- split_path
    }
    do.call(file.path, as.list(split_path.short))
  })
  unlist(short_paths)
}
