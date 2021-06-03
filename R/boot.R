#' Save bootstrap datasets to disk
#'
#' Used by `make_boot_datasets()`, run once per bootstrap sample
#' 
#' @param d Dataset to merge.
#' @param rsplit Splits object from rsample.
#' @param data_name Name of dataset.
#' @param data_folder Path to bootstrap datasets.
#' @param id_var Character (default = "ID"). Name of ID column.
#' @param oob Logical.  Should out of bag dataset be written (default = FALSE).
#' @param overwrite Should datasets be overwritten.
#' 
#' @keywords internal
boot_to_csv <- function(d,
                        rsplit, 
                        data_name,
                        data_folder = "DerivedData/bootstrap_datasets", 
                        id_var = "ID",
                        oob = FALSE,
                        overwrite = FALSE){
  
  if(!requireNamespace("rsample")) stop("install rsample")
  
  data_name <- tools::file_path_sans_ext(basename(as.character(data_name)))
  if(oob) data_folder <- file.path(data_folder, "oob")
  
  csv_name <- file.path(data_folder, paste0(data_name, ".csv"))
  
  if(file.exists(csv_name) & !overwrite) return(csv_name)
  
  rsample_fun <- rsample::analysis
  if(oob) rsample_fun <- rsample::assessment
  
  suppressMessages({
    dd_boot <- rsample_fun(rsplit) %>%
      dplyr::ungroup() %>%
      dplyr::select(id_var)
    dd_boot <- dd_boot %>%
      dplyr::mutate(NEWID = 1:nrow(dd_boot)) %>%
      dplyr::inner_join(d)
  })
  dd_boot$OLDID <- dd_boot[[id_var]]
  dd_boot[[id_var]] <- dd_boot$NEWID
  dd_boot$NEWID <- NULL
  
  dir.create(data_folder, showWarnings = FALSE, recursive = TRUE)
  
  utils::write.csv(dd_boot, file = csv_name, quote = FALSE, row.names = FALSE, na = ".")
  
  return(csv_name)
}

#' Write bootstrap datasets
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#'
#' Creates bootstrap datasets and returns corresponding `nm` objects. Requires
#' the necessary `rsample` splitting objects to be present. See examples.
#' 
#' @param m An nm object.
#' @param samples Number of samples.
#' @param data_folder Folder (relative path) to store datasets.
#' @param overwrite Logical (default = `FALSE`). Overwrites previous files.
#' @param id_var Character (default = `"ID"`). Name of ID column in dataset.
#' @param ... Arguments passed to [fill_input()]
#'   
#' @examples 
#' 
#' \dontrun{
#' 
#' ## in your dataset production script 
#' 
#' d <- d %>%
#' mutate(WT_C = cut(WT, breaks = 2, labels = FALSE),
#'        STRATA = paste(SEX, WT_C, sep = "_"))
#' 
#' d_id <- d %>% distinct(ID, STRATA)
#' 
#' set.seed(123)
#' 
#' ## create large set of resamples (to enable simulation to grow
#' ## without ruining seed)
#' bootsplits <- rsample::bootstraps(d_id, 100, strata = "STRATA")
#' 
#' dir.create("DerivedData", showWarnings = FALSE)
#' bootsplits %>% saveRDS("DerivedData/bootsplit_data.csv.RData")
#'
#' ## In a model development script, the following, performs a 
#' ## 100 sample bootstrap of model m1
#' 
#' m1_boot <- m1 %>% make_boot_datasets(samples = 100, overwrite = TRUE)
#' 
#' m1_boot$m %>% run_nm()
#' 
#' ## the following bootstrap template will wait for results to complete
#' m1_boot$m %>% nm_list_render("Scripts/basic_boot.Rmd")
#' 
#' }
#' 
#' @export
make_boot_datasets <- function(m,
                               samples = 10,
                               data_folder = "DerivedData/bootstrap_datasets", 
                               overwrite = FALSE,
                               id_var = "ID",
                               ...){
  
  bootsplits <- readRDS(paste0("DerivedData/bootsplit_", basename(data_path(m)), ".RData"))
  
  dboots <- bootsplits[seq_len(samples), ] # datasets created
  dboots$run_id <- 1:nrow(dboots)
  
  d <- input_data(m)
  
  dboots <- dboots %>%
    dplyr::group_by(run_id) %>%
    dplyr::mutate(
      csv_name = purrr::map2_chr(
        .data$splits, .data$run_id,
        ~boot_to_csv(d = d, rsplit = .x, data_name = .y, 
                     overwrite = overwrite, id_var = id_var)
      )
    ) %>%
    dplyr::ungroup()
  
  dboots <- dboots %>% dplyr::mutate(
    m = m %>%
      ## the following run_in will screw up parent_run_in, fix later
      run_in(file.path(run_in(m), paste0(run_id(m), "_boot"))) %>%
      data_path(.data$csv_name[1]) %>%
      fill_input(...) %>% ## doing this before child() speeds up execution
      child(run_id) %>% ## expand to fill dboots
      data_path(.data$csv_name) %>% ## set data_paths
      change_parent(m) %>%
      #parent_run_in(run_in(m)) %>% ## fix run_in
      results_dir(run_in(m))
  )
  
  dboots
}

#' Write (bootstrap) cross validation datasets
#' 
#' @description 
#' 
#' `r lifecycle::badge("experimental")`
#' 
#' Similar to [make_boot_datasets()], but sets up "out of bag" datasets for model
#' evaluation.
#' 
#' @param dboot Output from [make_boot_datasets()].
#' @param data_folder Folder to store datasets.
#' @param overwrite Logical. Overwrite previous files or not.
#' @param id_var character (default = `"ID"`). Name of ID column.
#' 
#' @return A `tibble` of nm objects similar to [make_boot_datasets()] output
#' 
#' @export
make_xv_datasets <- function(dboot,
                             data_folder = "DerivedData/bootstrap_datasets", 
                             overwrite = FALSE,
                             id_var = "ID"){
  
  d <- input_data(parent_run(dboot$m[1])) ## originally input_data(m)
  
  dboot <- dboot %>%
    dplyr::group_by(run_id) %>%
    dplyr::mutate(
      oob_csv_name = purrr::map2_chr(
        .data$splits, .data$run_id,
        ~boot_to_csv(d = d, rsplit = .x, data_name = .y, 
                     overwrite = overwrite, id_var = id_var, oob = TRUE)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      m_xv = #ifelse(is_successful(m), 
        dboot$m %>% 
        child(paste0(run_id(dboot$m), "eval")) %>%
        data_path(.data$oob_csv_name)#,
      #      nm(NA))
    )
}
