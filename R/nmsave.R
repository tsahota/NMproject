#' Save plots in results_dir
#' 
#' @param r nm object
#' @param plot_ob a list of plotting objects
#' @param plot_name character. Name of results file
#' @param plot_dir character (default = results_dir(r)). Where to save.
#' @param width passed to ggsave
#' @param height passed to ggsave
#' @param dpi passed to ggsave
#' @param ... passed to ggsave
#' @export
nmsave_plot <- function(r, plot_ob, plot_name, plot_dir = results_dir(r), 
                        width = 7, height = 5, dpi = 300, ...){
  UseMethod("nmsave_plot")
}
#' @export
nmsave_plot.nm_generic <- function(r, plot_ob, plot_name, plot_dir = results_dir(r), 
                                   width = 7, height = 5, dpi = 300, ...){
  plot_name <- glue_text_nm(r, plot_name)
  plot_name <- unique(plot_name)
  if(length(plot_name) > 1) stop("multiple plot names", call. = FALSE)
  dir.create(unique(plot_dir), showWarnings = FALSE, recursive = TRUE)
  requireNamespace("grid")
  ggplot2::ggsave(filename = file.path(unique(plot_dir), plot_name),
                  plot = plot_ob, 
                  width = width, height = height, dpi = dpi, ...)
  r <- r %>% result_files(plot_name)
  invisible(r)
}
#' @export
nmsave_plot.nm_list <- Vectorize_nm_list(nmsave_plot.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

#' Save plots in results_dir
#' 
#' @param r nm object
#' @param table_ob a list of table objects
#' @param table_name character. Name of results file
#' @param table_dir character (default = results_dir(r)). Where to save.
#' @param ... passed to ggsave
#' @export
nmsave_table <- function(r, table_ob, table_name, table_dir = results_dir(r), ...){
  UseMethod("nmsave_table")
}
#' @export
nmsave_table.nm_generic <- function(r, table_ob, table_name, table_dir = results_dir(r), ...){
  table_name <- glue_text_nm(r, table_name)
  table_name <- unique(table_name)
  if(length(table_name) > 1) stop("multiple table names", call. = FALSE)
  dir.create(unique(table_dir), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(table_ob, 
                   file = file.path(unique(table_dir), table_name),
                   row.names = FALSE, ...)
  r <- r %>% result_files(table_name)
  invisible(r)
}
#' @export
nmsave_table.nm_list <- Vectorize_nm_list(nmsave_table.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

