#' Save bootstrap datasets to disk
#' 
#' Internal function used by `make_boot_datasets()`, 
#'   run once per bootstrap sample
#' 
#' @param d dataset to merge
#' @param rsplit splits object from rsample
#' @param data_name name of dataset
#' @param data_folder path to bootstrap datasets
#' @param id_var character (default = "ID"). Name of ID column
#' @param oob logical.  Should out of bag dataset be written (default = FALSE)
#' @param overwrite should datasets be overwritten
#' 
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
#' @param m nm object
#' @param samples number of samples
#' @param data_folder folder to store datasets
#' @param overwrite overwrite or not
#' @param id_var character (default = "ID"). Name of ID column
#' @param ... arguments passed to fill_input
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

#' Write xv datasets
#' 
#' @param dboot output from make_boot_dataset()
#' @param data_folder folder to store datasets
#' @param overwrite overwrite or not
#' @param id_var character (default = "ID"). Name of ID column
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
