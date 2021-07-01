#' Create analysis project
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This is the underlying function used by: `File` -> `New Project` ->
#' `New Directory` -> `New NMproject`.  It creates a new analysis working
#' directory with a directory structure similar to an R package.
#'
#' @param path Character path (relative or absolute) to project.  If just
#'   specifying a name, this will create the analysis project in the current
#'   working directory.  See details for naming requirements.
#' @param dirs Character list or vector.  Default = `nm_default_dirs()`
#' @param style Character. Either `"analysis"` or `"analysis-package"` See
#'   details for `path` requirements and function behaviour.
#' @param use_renv Logical (default = `FALSE`). Should `renv` be used or not in
#'   project.
#' @param readme_template_package Package name from which to load the README
#'   template (default = `"NMproject"`)
#' @param ... Deprecated.
#'
#' @details The function works like as is inspired by
#'   `starters::create_analysis_project()`. There is no restriction on directory
#'   name.  It is therefore possible to violate R package naming conventions.
#'
#'   When `style = "analysis"` is selected, the analysis directory will be
#'   package-like in structure, with the package name `"localanalysis"`.
#'   For `style = "analysis-package"`, `path` should contain only (ASCII)
#'   letters, numbers and dot, have at least two characters and start with a
#'   letter and not end in a dot.  See [Description file
#'   requirements](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file)
#'   for more information.
#'
#'   This is to cater to users who like underscores and aren't interested in
#'   creating a package.
#'
#' @section Default modelling directories:
#'
#' Default modelling directories can be modified with `nm_default_dirs` option
#' (see [options()] for information on how to modify this). A (partially) named
#' list of directories to be used by `nm_create_analysis_project` Required names
#' are `"models"`, `"scripts"` and `"results"`. By default these are set to
#' `"Models"`, `"Scripts"` and `"Results"`, respectively. Additional nameless
#' characters (e.g. `"SourceData"`) correspond to additional modelling
#' directories.
#'
#' \describe{
#'   \item{"SourceData":}{
#'   intended for unmodified source datasets entering the analysis project.
#'   }
#'   \item{"DerivedData":}{
#'   intended for cleaned and processed NONMEM ready datasets
#'   }
#'   \item{"Scripts":}{
#'   intended for all R scripts
#'   }
#'   \item{"Models":}{
#'   intended for all NONMEM modelling
#'   }
#'   \item{"Results":}{
#'   intended as default location for run diagnostics, plots and tables
#'   }
#' }
#'
#' @seealso [nm_default_dirs()] for modifying default directory structure.
#' @export

nm_create_analysis_project <- function(path, dirs = nm_default_dirs(),
                                       style = c("analysis", "analysis-package"),
                                       use_renv = FALSE, readme_template_package = "NMproject", 
                                       ...) {

  ## need to normalize path because usethis has different
  ## home directory, so use use R normalizePath to remove abiguity
  path <- normalizePath(path, mustWork = FALSE)

  validate_dir_list(dirs)

  style <- match.arg(style)
  name <- basename(path)
  folder <- dirname(path)

  if (grepl("package", style) & !valid_package_name(name)) {
    stop("directory name is not compliant for style = \"analysis-package\" see ?nm_create_analysis_project")
  }

  usethis::create_project(path = path, rstudio = TRUE, open = FALSE)
  
  current_proj <- try(usethis::proj_get(), silent = TRUE)
  if (inherits(current_proj, "try-error")) {
    current_proj <- NULL
  }
  usethis::proj_set(path)
  on.exit(usethis::proj_set(current_proj))

  write("This directory is for .R files containing R functions", file.path(path, "R", "Readme.txt"))
  usethis::use_build_ignore(file.path("R", "Readme.txt"))

  usethis::use_template("README.Rmd",
    data = list(Package = name),
    package = readme_template_package
  )

  usethis::use_build_ignore("README.Rmd")

  if (use_renv) {
    if (!requireNamespace("renv", quietly = TRUE)) {
      stop("install renv", call. = FALSE)
    }
    if (!requireNamespace("desc", quietly = TRUE)) {
      stop("install desc", call. = FALSE)
    }
  }

  if (use_renv) renv::scaffold(project = usethis::proj_get())

  for (i in seq_along(dirs)) {
    dir_name <- dirs[[i]]
    usethis::use_directory(dir_name, ignore = TRUE)

    generic_dir_name <- names(dirs)[i]
    readme_path <- file.path(dir_name, "Readme.txt")
    if (generic_dir_name %in% names(.nm_dir_descriptions)) {
      write(.nm_dir_descriptions[[generic_dir_name]], file.path(path, readme_path))
      usethis::use_build_ignore(readme_path)
    } else {
      write("Custom directory", file.path(path, readme_path))
      usethis::use_build_ignore(readme_path)
    }
  }

  tryCatch(
    {
      if (style %in% "analysis") {
        usethis::use_description(fields = list(Package = "localanalysis"), check_name = FALSE)
      }
      if (style %in% "analysis-package") {
        usethis::use_description()
      }

      if (use_renv) {
        desc::desc_set_dep(
          package = "renv", type = "Imports",
          file = usethis::proj_get()
        )
      }
    },
    error = function(e) {
      usethis::ui_info("skipping creation of {usethis::ui_path('DESCRIPTION')}")
    }
  )

  tryCatch(
    {
      usethis::use_namespace()
    },
    error = function(e) {
      usethis::ui_info("skipping creation of {usethis::ui_path('NAMESPACE')}")
    }
  )
  
  suppressMessages(devtools::build_readme(path = path))

  set_default_dirs_in_rprofile(file.path(folder, name, ".Rprofile"), dirs)
    
  ## no badges - skip this part of starters for now
  repo <- git2r::init(usethis::proj_get())
  git2r::add(repo, path = "*")
  
  tryCatch(
    {
      git2r::commit(repo, message = "Initial commit", all = TRUE)
    },
    error = function(e) {
      usethis::ui_oops("cannot commit. Aborting commit...")
    }
  )

  return(invisible(path))
}

#' Package name validator from `usethis`
#'
#' @param x Name of package.
#' @keywords internal
valid_package_name <- function(x) {
  grepl("^[a-zA-Z][a-zA-Z0-9.]+$", x) && !grepl("\\.$", x)
}
