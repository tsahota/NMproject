#' @importFrom dplyr mutate
#' @export
dplyr::mutate

#' @export
mutate.nm_list <- function(.data, ...){
  dots_exp <- rlang::enexprs(...)
  data_extra <- dplyr::mutate(nm_row(.data), ...)
  
  for(name in names(dots_exp)){
    .data <- .data %>% custom_1d_field(field = name, replace = data_extra[[name]])
  }
  .data
  
}

#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @export
filter.nm_list <- function(.data, ...){
  dots_exp <- rlang::enexprs(...)
  object <- .data
  .data <- nm_row(object)
  .data$m <- object
  
  data_extra <- dplyr::filter(.data, ...)
  data_extra$m
}


new_ctl_extra <- function(m, ctl, dir = getOption("models.dir")){
  
  ctl$TABLE <- gsub(paste0("(FILE\\s*=\\s*\\S*tab)\\S*\\b"),paste0("\\1",run_id(m)),ctl$TABLE)
  ctl[[1]] <- gsub("^(\\s*;;\\s*[0-9]*\\.\\s*Based on:).*",paste("\\1",parent_run_id(m)),ctl[[1]])
  ctl[[1]] <- gsub("^(\\s*;;\\s*\\w*\\.\\s*Author:).*",paste("\\1",Sys.info()["user"]),ctl[[1]])
  
  ctl
}

## shouldn't need to delete ctls anymore

delete_ctl <- function(m){
  UseMethod("delete_ctl")
}
delete_ctl.nm_generic <- function(m){
  unlink(ctl_path(m))
  invisible(m)
}
delete_ctl.nm_list <- Vectorize_nm_list(delete_ctl.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)


in_cache <- function(r,
                     cache_ignore_cmd = FALSE, cache_ignore_ctl = FALSE, cache_ignore_data = FALSE,
                     return_checksums = FALSE){
  UseMethod("in_cache")
}

in_cache.nm_generic <- function(r,
                                cache_ignore_cmd = FALSE, cache_ignore_ctl = FALSE, cache_ignore_data = FALSE,
                                return_checksums = FALSE){
  
  .Deprecated("overwrite_behaviour", 
              msg = "this function is no longer needed with overwrite_behaviour (see app) and will probably be removed")
  r %>% write_ctl()
  ## get all md5_files
  
  run_cache_disk <- lapply(run_cache_paths(r), readRDS)
  if(length(run_cache_disk) > 0){
    current_checksums <- run_checksums(r)
    checksums_reduced <- lapply(run_cache_disk, function(i) {
      
      if(cache_ignore_cmd){  ## remove cmd check
        keep <- !names(current_checksums) %in% "cmd"
        i$checksums <- i$checksums[keep]
        current_checksums <- current_checksums[keep]
      }
      
      if(cache_ignore_ctl){  ## remove cmd check
        keep <- !names(current_checksums) %in% "ctl"
        i$checksums <- i$checksums[keep]
        current_checksums <- current_checksums[keep]
      }        
      
      if(cache_ignore_data){  ## remove cmd check
        keep <- !names(current_checksums) %in% "data"
        i$checksums <- i$checksums[keep]
        current_checksums <- current_checksums[keep]
      }
      
      ## ignore names
      #names(current_checksums) <- NULL
      #names(i$checksums) <- NULL
      
      list(
        checksums = current_checksums,
        stored_checksums = i$checksums
      )
      
      #identical(i$checksums, current_checksums)
    })
    
    matches <- sapply(checksums_reduced,
                      function(i) identical(i$checksums, 
                                            i$stored_checksums))
    
    if(any(matches)){
      return(TRUE)    ## if up to date, skip
    }
  }
  if(return_checksums) return(checksums_reduced)
  return(FALSE)
}

in_cache.nm_list <- Vectorize_nm_list(in_cache.nm_generic)

manual_patch <- function(m){
  
  res <- start_manual_edit_unix(m)
  
  message(
    "---Manual edit---
    Instructions:
    1) edit control file
    2) save & close
    Press ENTER when done...")
  readline()
  
  ## now diff ctl_path(m) and old_file_path
  
  diff_manual_edit(m, res)
  
  message("patch created:\n ", res$patch_path, "\n")
  
  message("copy-paste the following into your script to apply:\n
  [nm_object] %>%
  apply_manual_edit(\"", res$patch_name,"\")

(dont forget to comment your code)")
  
}
