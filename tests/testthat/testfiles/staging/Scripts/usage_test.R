## Copied from /projects/QCP_MODELING/PMXcodelibrary/R/usage_test.R
##  (2018-03-10 18:09:12) by klgk669
## Copied from /projects/QCP_MODELING/PMXcodelibrary/R/usage_test.R
##  (2017-12-14 11:35:29) by klgk669

library(NMprojectAZ)
library(dplyr)
library(ggplot2)


list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  dirs
}

base_dir <- "/projects/qcp/QCP_MODELING"

TAs <- c("CVMD","ONC","OTHER","RIA")

# get_string <- function(string){
#   #res <- vector()
#   res <- tibble()
#   for(TA in TAs){
#     TA_path <- file.path(base_dir,TA)
#     projects <- list.dirs(TA_path)
#     for(project in projects){
#       analysis_projects <- list.dirs(project)
#       for(analysis_project in analysis_projects){
#         scripts_dir <- list.dirs(analysis_project,pattern = "Scripts")
#         matched <- try(ls_scripts(scripts_dir) %>% search_raw(string),silent = TRUE)
#         ######
#         ## new code
#         #if(inherits(matched,"try-error")) matched <- "error"
#         if(inherits(matched,"try-error")) return(tibble())
#         return(tibble(name = matched, TA, project, analysis_project))
#         #res <- append(res,matched)
#         ######
#
#       }
#     }
#   }
#   res
# }

get_string <- function(string){
  #res <- vector()
  res <- tibble()
  for(TA in TAs){
    TA_path <- file.path(base_dir,TA)
    projects <- list.dirs(TA_path)
    for(project in projects){
      analysis_projects <- list.dirs(project)
      for(analysis_project in analysis_projects){
        scripts_dir <- list.dirs(analysis_project,pattern = "Scripts")
        matched <- try(ls_scripts(scripts_dir) %>% search_raw(string),silent = TRUE)
        #if(inherits(matched,"try-error")) return(tibble())
        if(!inherits(matched,"try-error")) {
          if(length(matched) > 0)
            res <- bind_rows(res,
                             tibble(name = matched, TA, project, analysis_project))
        }
      }
    }
  }
  res
}


res_nm <- get_string("NMproject")
res_tidy <- get_string("tidyproject")
res_md <- get_string("\\bnm\\(")

file_info <- function(res){
  res <- unique(res)
  d <- file.info(res$name)
  row.names(d) <- NULL
  d <- tibble::as_tibble(d)
  cbind(d, res)
}

plot_count <- function(d){
  d <- d %>% dplyr::group_by(name) %>% dplyr::mutate(date = min(mtime, ctime, atime))
  d <- d %>% arrange(date)
  d$count <- 1:nrow(d)
  ggplot(d, aes(x = date, y = count)) + theme_bw() + geom_point() + geom_line()
}

plot_count_TA <- function(d){
  d <- d %>% dplyr::group_by(name) %>% dplyr::mutate(date = min(mtime, ctime, atime))
  d <- d %>% arrange(date)
  d <- d %>% group_by(TA) %>% mutate(count = seq_along(name))
  #d$count <- 1:nrow(d)
  ggplot(d, aes(x = date, y = count)) + theme_bw() +
    geom_point(aes(colour = TA)) + geom_line(aes(colour = TA))
}

d_nm <- file_info(res_md)
plot_count(d_nm)
plot_count_TA(d_nm)

d_all <- file_info(bind_rows(res_nm, res_tidy))
plot_count(d_all)
plot_count_TA(d_all %>% filter(!TA %in% "OTHER"))

d_all %>% summarise(scripts = length(name),
                    projects = length(unique(project)),
                    analysis = length(unique(analysis_project)),
                    users = length(unique(uname)))

d_nm %>% summarise(scripts = length(name),
                   projects = length(unique(project)),
                   analysis = length(unique(analysis_project)),
                   users = length(unique(uname)))


d_all %>% group_by(TA) %>% summarise(scripts = length(name),
                                     projects = length(unique(project)),
                                     analysis = length(unique(analysis_project)),
                                     users = length(unique(uname)))

d_nm %>% group_by(TA) %>% summarise(scripts = length(name),
                                    projects = length(unique(project)),
                                    analysis = length(unique(analysis_project)),
                                    users = length(unique(uname)))

d_all %>% group_by(uname) %>% summarise(count = length(name)) %>%
  arrange(desc(count)) %>% View

d_nm %>% group_by(uname) %>% summarise(count = length(name)) %>%
  arrange(desc(count)) %>% View

## i only have 13??

d_all %>% filter(uname == "klgk669")

ggplot(d_all, aes(x = uname)) + theme_bw() +
  geom_histogram()





#res <- unique(res)

length(which(res == "error"))

res_worked <- res[res!="error"]

unique_analyses <- unique(dirname(res_worked))
unique_analyses

file.info(res_worked)

unique_users <- unique(file.info(res_worked)$uname)
unique_users

for(i in seq_along(unique_users)) system_nm(paste("finger -n",unique_users[i]))



script_dirs1 <- tidyproject::ls_script_dirs("/projects/qcp/QCP_MODELING/", depth = 3)
script_dirs2 <- tidyproject::ls_script_dirs("/projects/qcp/QCP_MODELING/", depth = 4)

script_dirs <- c(script_dirs1, script_dirs2)

scripts <- tidyproject::ls_scripts(script_dirs)

nmscripts <- scripts %>% tidyproject::search_raw("NMproject|tidyproject")


try_readLines <- function(...){
  text <- try(readLines(...), silent = TRUE)
  if(inherits(text, "try-error")) return(character())
  text
}

text <- suppressWarnings(parallel::mclapply(scripts, try_readLines, mc.cores = 10))




pkgs <- parallel::mclapply(text, function(text){
  text <- try(parse(text = text), silent = TRUE)
  if(inherits(text, "try-error")) return(NA)
  unique(unlist(lapply(text, tidyproject:::recursive_lib_find)))
}, mc.cores = 10)


## want data.frame
##

dpkg <- pkgs

tab <- table(unlist(dpkg))

tab <- sort(tab, decreasing = TRUE)

d <- dplyr::tibble(name = names(tab), n_scripts_with_dependency = tab)

write.csv(d, file = "~/QCP_MODELLING_package_use.csv", quote = FALSE, col.names = TRUE, row.names = FALSE)




activity_names <- dirname(script_dirs)

x <- environment_info0(script_dirs[1:2])


get_pkg_names <- function(x){
  x <- x[grepl("\\s*\\[[0-9]+\\]", x)]
  x <- x[!grepl("LC",x)]
  x <- gsub("\\s*\\[[0-9]+\\]\\s+","",x)
  x <- paste(x, collapse = " ")
  x <- strsplit(x, "\\s+")
  x[[1]]
}

get_pkg_names(x)




TA_paths <- file.path("/projects/qcp/QCP_MODELING", TAs)

activities <- sapply(TA_paths, list.dirs)
