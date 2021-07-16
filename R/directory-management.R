#' Setup demo in current directory
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Following through the demo is the fastest way to learn the syntax of
#' NMproject. The default demo is a Theophylline ("theopp") pharmacometric analysis.  Scripts will
#' be copied numbered `s01_XXX.Rmd`, `s02_XXX.Rmd` in the `"Scripts"` directory and a
#' dataset into `"SourceData"`. The `"staging"` area will also be pre-filled with
#' the code library model, `"ADVAN2.mod"`.  To practice copying this yourself, see
#' [code_library()] for how the app works.
#'
#' @param demo_name Character. Name of demo. Default = "theopp".  See details to find other demos
#' @param overwrite Logical. Default changed to FALSE.
#' @param additional_demo_locations Character vector. default = NULL. Locations
#'   for demo directories.
#'
#' @details Available `demo_name` correspond to directory locations in
#' `system.file("extdata","examples",package = "NMproject")`
#'
#' @seealso [code_library()]
#' @export

setup_nm_demo <- function(demo_name = "theopp",
                          overwrite = FALSE,
                          additional_demo_locations = NULL) {
  
  examples_dir <- character()
  examples_dirs <- character()

  if (length(additional_demo_locations) > 0) {
    examples_dir <- normalizePath(additional_demo_locations, mustWork = FALSE)
    examples_dirs <- list.files(examples_dir, full.names = TRUE, recursive = FALSE)
    # examples_dir <- examples_dirs[grepl(paste0(.Platform$file.sep, demo_name,"$"), examples_dirs)]
  }
  ## TODO: rename examples to demos
  examples_dirs <- append(
    examples_dirs,
    list.files(system.file("extdata", "examples", package = "NMproject"),
      full.names = TRUE, recursive = FALSE
    )
  )
  matched_examples_dirs <- examples_dirs[grepl(paste0(.Platform$file.sep, demo_name, "$"), examples_dirs)]

  if (length(matched_examples_dirs) == 0) {
    stop("demo not found.\nAvailable demos:\n ",
      paste(unique(basename(examples_dirs)), collapse = "\n "),
      call. = FALSE
    )
  }

  # examples_dir <- append(examples_dir, system.file("extdata","examples",demo_name,package = "NMproject"))
  examples_dir <- matched_examples_dirs[1]

  files_to_copy <- dir(examples_dir, all.files = TRUE, full.names = TRUE, recursive = TRUE)

  stage_info <- stage(files_to_copy, overwrite = overwrite, silent = TRUE, 
                      find_replace_dir_names = TRUE)

  imported_info <- import(stage_info, overwrite = overwrite)
  
}


#' Run all project scripts sequentially
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Runs/renders all scripts `s01_XXX`, `s02_XXX` in the designated "scripts"
#' directory.
#'
#' @param index Numeric index for subsetting list of scripts before running.
#' @param quiet Argument passed to [rmarkdown::render()].
#'
#' @details Works with .R and .Rmd extensions.  Behaviour is to [source()] .R
#'   files and use [rmarkdown::render()] on .Rmd files
#'
#' @export
run_all_scripts <- function(index, quiet = FALSE) {
  script_files <- dir(nm_dir("scripts"), "s[0-9]+_.*?\\.R(md)?$", full.names = TRUE)

  dplan <- dplyr::tibble(script_files) %>%
    dplyr::mutate(rmd = grepl("\\.Rmd", .data$script_files))

  dplan <- dplan %>%
    dplyr::mutate(
      cmd = ifelse(.data$rmd,
        paste0("rmarkdown::render(\"", script_files, "\", quiet = ", quiet, ")"),

        paste0("source(\"", script_files, "\")")
      )
    )

  if (!missing(index)) dplan <- dplan[index, ]

  exprs <- rlang::parse_exprs(dplan$cmd)

  res <- lapply(exprs, rlang::eval_tidy)
  return(invisible(TRUE))
}

#' Is the directory an NMproject directory
#' 
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Find out whether current (or specified) directory is an NMproject directory
#' or not.
#' 
#' @param path Optional path to test if it's an NMproject or not.
#' 
#' @return Logical `TRUE` or `FALSE`
#' 
#' @export

is_nmproject_dir <- function(path = getwd()){
  
  rprofile_path <- file.path(path, ".Rprofile")
  if (!file.exists(rprofile_path)) return(FALSE)
  any(grepl("nm_default_dirs", readLines(rprofile_path)))
  
}
