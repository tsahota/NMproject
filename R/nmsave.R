#' @rdname nmsave
#' @name nmsave
#' @title Save plots in results_dir
#' 
#' @description 
#' 
#' `r lifecycle::badge("experimental")`
#'
#' `nmsave_plot` is a wrapper around [ggplot2::ggsave()] for nm objects,
#' `nmsave_table` is a wrapper for saving `data.frame`s to file in the form of a
#' .csv file.
#' 
#' @param r An nm object.
#' @param object A list of plotting objects.
#' @param file_name Character. Name of results file.
#' @param directory Character (default = results_dir(r)). Where to save.
#' @param width Passed to ggsave.
#' @param height Passed to ggsave.
#' @param dpi Passed to ggsave.
#' @param ... Passed to ggsave.
#' @export
nmsave_plot <- function(r, object, file_name, directory = results_dir(r), 
                        width = 7, height = 5, dpi = 300, ...){
  UseMethod("nmsave_plot")
}
#' @export
nmsave_plot.nm_generic <- function(r, object, file_name, directory = results_dir(r), 
                                   width = 7, height = 5, dpi = 300, ...){
  file_name <- glue_text_nm(r, file_name)
  file_name <- unique(file_name)
  if(length(file_name) > 1) stop("multiple plot names", call. = FALSE)
  dir.create(unique(directory), showWarnings = FALSE, recursive = TRUE)
  requireNamespace("grid")
  ggplot2::ggsave(filename = file.path(unique(directory), file_name),
                  plot = object, 
                  width = width, height = height, dpi = dpi, ...)
  r <- r %>% result_files(file_name)
  invisible(r)
}
#' @export
nmsave_plot.nm_list <- Vectorize_nm_list(nmsave_plot.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

#' @rdname nmsave
#' 
#' @export
nmsave_table <- function(r, object, file_name, directory = results_dir(r), ...){
  UseMethod("nmsave_table")
}
#' @export
nmsave_table.nm_generic <- function(r, object, file_name, directory = results_dir(r), ...){
  file_name <- glue_text_nm(r, file_name)
  file_name <- unique(file_name)
  if(length(file_name) > 1) stop("multiple table names", call. = FALSE)
  dir.create(unique(directory), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(object, 
                   file = file.path(unique(directory), file_name),
                   row.names = FALSE, ...)
  r <- r %>% result_files(file_name)
  invisible(r)
}
#' @export
nmsave_table.nm_list <- Vectorize_nm_list(nmsave_table.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

