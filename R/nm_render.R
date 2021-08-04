#' @name nm_render
#' @rdname nm_render
#' @title Create run reports
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' A wrapper around `rmarkdown::render` for nm objects.  Use markdown templates
#' to create a customised set of diagnostics to reuse on multiple models. In the
#' demo an example is shown in `Scripts/basic_gof.Rmd`, but ideally you'll
#' create your own customised version with everything you need to evaluate your
#' model. To create an R markdown diagnostic template go to `FILE` -> `New File`
#' -> `R markdown` -> `From Template` the select from one of the following:
#'
#' \itemize{ \item model diagnostic \item VPC diagnostic
#'   \item PPC diagnostic
#'   \item bootstrap results (`nm_list_render`)
#' }
#'
#' These are intentionally minimal templates that can be run as notebooks or as
#' automated diagnostics run with `nm_render`.  Follow the instructions at the
#' top of the template for more details.
#'
#' @param m An nm object.
#' @param input Character. Same as `rmarkdown::render()` arg.
#' @param output_file Character. Same as `rmarkdown::render()` arg.
#' @param args List. Same as "params" arg in `rmarkdown::render()`.
#' @param force Logical (default = `getOption("nm.force_render")`). Will force
#'   execution.
#' @param async Experimental option to use future package.
#' @param ... Additional argument passed to `rmarkdown::render()`.
#'
#' @details `input` must refer to a properly specified Rmd document. The R
#' markdown template "model diagnostic" in RStudio sets this up for you.
#'
#' These R markdown templates are usable as R Notebooks (e.g. for code
#' development and debugging) if the object `.m` is defined in the global work
#' space first.
#'
#' `nm_list_render()` is mostly used for bootstraps, and other routines where a
#' parent run spawns multiple children in the form of an nm_list
#'
#' @return The same nm object, `m`, with modified `results_files` field.
#' @examples
#' \dontrun{
#' m1 %>% nm_render("Scripts/basic_gof.Rmd")
#'
#' ## to run "Scripts/basic_gof.Rmd" as an R Notebook
#' ## first define .m
#'
#' .m <- m1 ## Now you can run "Scripts/basic_gof.Rmd" as a Notebook
#' }
#' @export
nm_render <- function(m,
                      input,
                      output_file = NA,
                      args = list(),
                      force = getOption("nm.force_render"),
                      async = FALSE,
                      ...) {
  UseMethod("nm_render")
}

#' @export
nm_render.nm_generic <- function(m,
                                 input,
                                 output_file = NA,
                                 args = list(),
                                 force = getOption("nm.force_render"),
                                 async = FALSE,
                                 ...) {
  if (.Platform$OS.type == "unix") {
    if (!"cairo" %in% getOption("bitmapType")) {
      warning("if this step fails try setting options(bitmapType=\"cairo\")")
    }
  }

  if (is.na(output_file)) {
    output_file <- paste0(
      basename(tools::file_path_sans_ext(input)),
      ".", run_dir(m), ".nb.html"
    )
  }

  if ("m" %in% names(args)) {
    stop("can't have m in arg.  m is reserved for model object")
  }

  args <- c(args, list(m = as_nm_list(m)))

  output_dir <- results_dir(m)
  output_path <- file.path(output_dir, output_file)

  ## if force is TRUE skip caching and run
  if (!force) {
    ## if output_path doesn't exist skip caching and run
    ## if(file.exists(output_path)){
    ## pull existing checksum info
    render_cache_disk <- lapply(render_cache_paths(m, input), readRDS)
    if (length(render_cache_disk) > 0) {
      ## get current checksum
      current_checksums <- render_checksums(m, input)
      ## determine matches
      matches <- sapply(render_cache_disk, function(i) {
        identical(i$checksums, current_checksums)
      })
      if (any(matches)) {
        message("nm_render cache found, skipping... use nm_render(force = TRUE) to override")
        m <- m %>% result_files(output_file)
        usethis::ui_info("run report saved in: {usethis::ui_path(output_path)}")
        return(invisible(m)) ## if up to date, skip
      }
    }
    # }
  }

  if (async) {
    if (!requireNamespace("future")) stop("install future package", call. = FALSE)
    f0 <- future::future({
      rmarkdown::render(
        input = input,
        output_file = output_file,
        output_dir = output_dir,
        params = args,
        envir = new.env(),
        ...
      )
    })
  } else {
    rmarkdown::render(
      input = input,
      output_file = output_file,
      output_dir = output_dir,
      params = args,
      envir = new.env(),
      ...
    ) 
  }

  m <- m %>% result_files(output_file)
  m <- m %>% save_render_cache(input)
  
  usethis::ui_info("run report saved in: {usethis::ui_path(output_path)}")
  
  invisible(m)
}

#' @export
nm_render.nm_list <- Vectorize_nm_list(nm_render.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

#' @rdname nm_render
#' @export
nm_list_render <- function(m,
                           input,
                           output_file = NA,
                           args = list(),
                           force = getOption("nm.force_render"),
                           async = FALSE,
                           ...) {

  # m <- m$m

  if (is.na(output_file)) {
    output_file <- paste0(
      basename(tools::file_path_sans_ext(input)),
      ".", parent_run_id(m[1]), ".nb.html"
    )
  }

  if ("m" %in% names(args)) {
    stop("can't have m in \"args\", m is reserved for model object")
  }

  args <- c(args, list(m = as_nm_list(m)))

  output_dir <- parent_results_dir(m[1])
  output_path <- file.path(output_dir, output_file)

  m_parent <- as_nm_generic(parent_run(m[1]))

  ## if force is TRUE skip caching and run
  if (!force) {
    ## if output_path doesn't exist skip caching and run
    ## if(file.exists(output_path)){
    ## pull existing checksum info
    render_cache_disk <- lapply(render_cache_paths(m_parent, input), readRDS)
    if (length(render_cache_disk) > 0) {
      ## get current checksum
      current_checksums <- render_checksums(m_parent, input)
      ## determine matches
      matches <- sapply(render_cache_disk, function(i) {
        identical(i$checksums, current_checksums)
      })
      if (any(matches)) {
        message("nm_list_render cache found, skipping... use nm_list_render(force = TRUE) to override")
        # m <- m_parent %>% result_files(output_file)
        return(invisible(m)) ## if up to date, skip
      }
    }
    # }
  }

  if (async) {
    f0 <- future::future({
      rmarkdown::render(
        input = input,
        output_file = output_file,
        output_dir = output_dir,
        params = args,
        envir = new.env(),
        ...
      )
    })
  } else {
    rmarkdown::render(
      input = input,
      output_file = output_file,
      output_dir = output_dir,
      params = args,
      envir = new.env(),
      ...
    )
  }

  ## use as_nm_generic incase m is redefined in rmd
  # m <- m %>% result_files(output_file)

  m_parent %>% save_render_cache(input)

  invisible(m)
}
