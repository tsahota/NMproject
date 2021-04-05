## Copied from /home/klgk669/testproj2/Scripts/helper_funs.R
##  (2017-11-20 17:09:18) by klgk669
## Author: klgk669
## First created: 2017-11-08
## Description: Funs for sim re-est
## Keywords: 

########################################
## load packages and source functions here

########################################
## main script here

#' generate paths for simulation runs
#' 
#' @param dsc data.frame with experimental factors to vary
#' @start_dir directory name within which all runs should take place.

gen_sim_path <- function(dsc,start_dir){
  for(i in which(sapply(dsc,is.factor))){
    dsc[,i] <- as.character(dsc[,i])
  }
  sapply(1:nrow(dsc),function(i){
    location <- paste(names(dsc[i,]),as.character(dsc[i,]),sep="_",collapse = .Platform$file.sep)
    file.path(start_dir,location)
  })
}

split_by_row <- function(data) split(data, row.names(data))

by_row <- function(data,...,apply_fun=lapply) split_by_row(data) %>% apply_fun(...)

mark_completed <- function(data, task, name, result, result_character,
                           return = c("logical","character"), RData_name="progress.RData"){
  
  progress_file_name <- file.path(data$location, RData_name)
  if(file.exists(progress_file_name)) {
    load(progress_file_name)
  } else progress <- data.frame()
  ## progress is now defined
  
  if(missing(name)) name <- NA
  if(missing(result_character)) result_character <- NA
  if(missing(result)) result <- NA
  
  d <- data.frame(task, name, result = as.logical(result), result_character = as.character(result_character))
  dmatch <- merge(progress,d[,c("task","name")])
  if(nrow(dmatch) == 0){ ## if this task is new
    progress <- rbind(progress, d)
  } else {
    progress$result[progress$task %in% task & progress$name %in% name] <- as.logical(result)
    progress$result_character[progress$task %in% task & progress$name %in% name] <- as.character(result_character)
  }
  progress <- unique(progress)
  
  save(progress, file = progress_file_name)
  return <- match.arg(return)
  if(return == "logical") return_col <- "result"
  if(return == "character") return_col <- "result_character"
  get(return_col)
  
}

completed <- function(data, task, name = as.character(NA), return = c("logical","character"), RData_name="progress.RData"){
  
  return <- match.arg(return)
  progress_file_name <- file.path(data$location, RData_name)
  if(return == "logical") return_col <- "result"
  if(return == "character") return_col <- "result_character"
  
  if(!file.exists(progress_file_name)){
    if(return %in% "logical") return(FALSE) 
    if(return %in% "character") return("FALSE")
  }
  
  load(file = progress_file_name)
  progressi <- progress[progress$task %in% task &
                          progress$name %in% name,]
  if(nrow(progressi)==0) {
    if(return %in% "logical") return(FALSE) 
    if(return %in% "character") return("FALSE")
  }
  progressi[[return_col]]
}

collate_res <- function(data,data_name,object_name){
  paths <- file.path(data$location,data_name)
  res <- lapply(paths,function(path){
    if(!file.exists(path)) return(NULL)
    load(path)
    return(get(object_name))
  })
  res
}

collate_data <- function(data,data_name,object_name="data"){
  data <- collate_res(data,data_name,object_name)
  data <- do.call(rbind,data)
  data
}

change_seed <- function(ctl_contents,new_seed)
  gsub("(^\\$SIM\\S*\\s+\\()[0-9]+(\\).*$)",paste0("\\1",new_seed,"\\2"),ctl_contents)


run_list <- function(x,...){
  do.call(run,c(x,...))
}

get_progress <- function(data, RData_name = "progress.RData"){
  RData_name <- file.path(data$location, RData_name)
  if(!file.exists(RData_name)) progress <- data.frame() else load(RData_name)
  merge(data,progress)
}

