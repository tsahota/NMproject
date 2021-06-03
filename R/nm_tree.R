#' Make data.tree object
#' 
#' @description 
#' 
#' `r lifecycle::badge("experimental")`
#' 
#' Draw a tree diagram showing model development path.
#' 
#' @param ... Arguments passed to [nm_list_gather()].
#' @param summary Logical (default = `FALSE`). Should [summary_wide()] variables be appended.
#' @export
nm_tree <- function(..., summary = FALSE){
  
  if(!requireNamespace("data.tree")) stop("install data.tree")
  
  m <- nm_list_gather(...) 
  
  if(summary){
    sink(file="/dev/null")
    m_row <- m %>% 
      {suppressMessages(
        dplyr::right_join(nm_row(.data), summary_wide(.data)) 
      )}
    sink()
  } else {
    m_row <- m %>% nm_row()
  }
  
  network <- m_row %>%
    dplyr::mutate(
      parent_run_id = ifelse(is.na(.data$parent_run_id), 
                             "start", 
                             .data$parent_run_id),
      parent_run_id = ifelse(is.na(.data$parent_run_in),
                             .data$parent_run_id,
                             file.path(.data$parent_run_in, .data$parent_run_id)
      ),
      run_id = file.path(.data$run_in, .data$run_id)
    ) %>%
    dplyr::select(.data$parent_run_id, .data$run_id, dplyr::everything())
  
  tree <- data.tree::FromDataFrameNetwork(network)
  
  tree
}
