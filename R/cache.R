#' @include nm_object.R

run_checksums <- function(m) { ## only works on single m
  ## information determinative to whether run should be rerun
  mtmp <- m %>% run_in(file.path(run_in(m), "temp"))
  mtmp %>% write_ctl(force = TRUE)
  files <- c(ctl_path(mtmp), data_path(mtmp))

  checksums <- tools::md5sum(files)
  names(checksums) <- c("ctl", "data")
  checksums <- c(checksums,
    cmd = cmd(as_nm_generic(m))
  )
  checksums
}

render_checksums <- function(m, input) { ## only works on single m
  ## information determinative to whether run should be rerun
  # m %>% write_ctl()
  files <- c(ctl_table_paths(m), data_path(m), input)
  c(tools::md5sum(files))
}

unique_run_cache_path <- function(m) {
  file.path(
    ".cache",
    paste0(gsub(.Platform$file.sep, "--", unique_id(m)))
  )
}

run_cache_paths <- function(m) {
  pattern <- m %>%
    unique_run_cache_path() %>%
    basename()
  pattern <- paste0("^", pattern, "$")

  dir(".cache", pattern = pattern, full.names = TRUE)
}

cached_object <- function(m) {
  UseMethod("cached_object")
}

cached_object.nm_generic <- function(m) {
  path <- unique_run_cache_path(m)
  if (!file.exists(path)) {
    return(nm(NA))
  }
  readRDS(path)$object
}

cached_object.nm_list <- Vectorize_nm_list(cached_object.nm_generic, SIMPLIFY = FALSE)


cache_history <- function(r) {
  UseMethod("cache_history")
}

cache_history.nm_generic <- function(r) {
  lapply(run_cache_paths(r), readRDS)
}

cache_history.nm_list <- Vectorize_nm_list(cache_history.nm_generic, SIMPLIFY = FALSE)

cache_current <- function(m) run_checksums(m)

clear_cache <- function() unlink(".cache", recursive = TRUE)

file_friendly_unique_id <- function(m) gsub(.Platform$file.sep, "--", unique_id(m))

unique_render_cache_path <- function(m, input) {
  file.path(
    ".cache",
    paste0(
      gsub(.Platform$file.sep, "--", unique_id(m)),
      "-.-",
      gsub(.Platform$file.sep, "--", input)
    )
  )
}

render_cache_paths <- function(m, input) {
  pattern <- m %>%
    unique_render_cache_path(input) %>%
    basename()
  pattern <- paste0("^", pattern, "$")

  dir(".cache", pattern = pattern, full.names = TRUE)
}

save_run_cache <- function(m) {
  ## this is for after a run has been submitted
  unique_run_cache_path <- unique_run_cache_path(m)
  dir.create(dirname(unique_run_cache_path), recursive = TRUE, showWarnings = FALSE)

  run_cache_disk <- list(
    job_info = job_info(m),
    object = m,
    checksums = run_checksums(m)
  )
  saveRDS(run_cache_disk, file = unique_run_cache_path)

  invisible(m)
}

save_render_cache <- function(m, input) {
  ## this is for after a run has been submitted
  unique_render_cache_path <- unique_render_cache_path(m, input)
  dir.create(dirname(unique_render_cache_path), recursive = TRUE, showWarnings = FALSE)

  render_cache_disk <- list(
    object = m,
    checksums = render_checksums(m, input)
  )
  saveRDS(render_cache_disk, file = unique_render_cache_path)

  invisible(m)
}
