#' Get job stats for a completed NONMEM run
#' 
#' @description 
#' 
#' `r lifecycle::badge("experimental")`
#' 
#' Gets attributes of the run like run time, queue time.
#' 
#' @param m An nm object.
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' dc <- tibble(cores = c(1, 3, 10, 30)) %>% 
#' mutate(m = m1 %>% 
#'   child(run_id = cores) %>%  
#'   run_in("Models/m1_coretest") %>% 
#'   cmd("execute {ctl_name} -parafile={parafile} -dir={run_dir} -nodes={cores}") %>%
#'   parafile("/opt/NONMEM/nm75/run/mpilinux8.pnm") %>%
#'   cores(cores)
#' )
#' 
#' dc$m %>% cmd()
#' 
#' dc$m %>% run_nm() %>% wait_finish()
#' 
#' ## extract job statistics and plot cores vs Rtime or Ttime 
#' ## to get plots of run time and total time vs number of CPUs
#' 
#' dc$m %>%
#'   job_stats() %>%
#'   ggplot(aes(x = cores, y = Rtime)) + theme_bw() +
#'   geom_point()
#' 
#' }
#' 
#' @export
job_stats <- function(m){
  
  if(!requireNamespace("pmxTools", quietly = TRUE))
    stop("install pmxTools", call. = FALSE)
  
  if(!requireNamespace("lubridate", quietly = TRUE))
    stop("install lubridate", call. = FALSE)
  
  if(!requireNamespace("purrr", quietly = TRUE))
    stop("install purrr", call. = FALSE)
  
  d <- m %>% nm_row()
  
  d <- d %>% dplyr::ungroup() %>%
    dplyr::mutate(xml_path = nm_output_path(m, "xml"),
                  xml = purrr::map(.data$xml_path, pmxTools::read_nm))
  
  
  ## this is to avoid CRAN error below - before it was inline
  m_time_f <- function(path) lubridate::ymd_hms(file.info(path)$mtime)
  
  d %>% dplyr::mutate(
    starttime = purrr::map_chr(.data$xml, ~.x$start_datetime),
    stoptime = purrr::map_chr(.data$xml, ~.x$stop_datetime),
    starttime = lubridate::ymd_hms(.data$starttime),
    stoptime = lubridate::ymd_hms(.data$stoptime),
    Rtime = difftime(.data$stoptime, .data$starttime, units = "mins"), 
    launchtime = m %>% run_dir_path() %>% file.path("command.txt") %>% m_time_f(),
    Qtime = difftime(.data$starttime, .data$launchtime, units = "mins"),
    Ttime = difftime(.data$stoptime, .data$launchtime, units = "mins")
  )
  
}

