## Copied from /projects/QCP_MODELING/PMXcodelibrary/R/usage_test.R
##  (2017-12-14 11:35:29) by klgk669
list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      full.names=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}

base_dir <- "/projects/QCP_MODELING/"

#dir(base_dir)
TAs <- c("CVMD","ONC","OTHER","RIA")

get_string <- function(string){
  res <- vector()
  for(TA in TAs){
    TA_path <- file.path(base_dir,TA)
    projects <- list.dirs(TA_path,full.names = TRUE)
    for(project in projects){
      analysis_projects <- list.dirs(project,full.names = TRUE)
      for(analysis_project in analysis_projects){
        scripts_dir <- list.dirs(analysis_project,full.names = TRUE,pattern = "Scripts")
        matched <- try(ls_scripts(scripts_dir) %>% search_raw(string),silent = TRUE)
        if(inherits(matched,"try-error")) matched <- "error"
        res <- append(res,matched)
      }
    }
  }
  res
}

res1 <- get_string("NMproject")
res2 <- get_string("tidyproject")

res <- unique(c(res1,res2))


#res <- unique(res)

length(which(res == "error"))

res_worked <- res[res!="error"]

unique_analyses <- unique(dirname(res_worked))
unique_analyses

file.info(res_worked)

unique_users <- unique(file.info(res_worked)$uname)
unique_users

for(i in seq_along(unique_users)) system_nm(paste("finger -n",unique_users[i]))

