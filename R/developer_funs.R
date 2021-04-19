#' @include utils.R

#' Copy diagnostic funs to templates
#' 
#' Internal function (non exported)
#'   Use within a demo directory (e.g. after \code{run_all_scripts())})

copy_demo_to_templates <- function(){

  script_files <- dir(nm_default_dir("scripts"), "basic_", full.names = TRUE)
  
  template_folder <- tools::file_path_sans_ext(basename(script_files))
  
  skeleton_loc <- system.file("rmarkdown", "templates", template_folder, "skeleton", package = "NMproject")

  unlink(skeleton_loc, recursive = TRUE, force = TRUE)
  for(path in skeleton_loc){
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  ## copy script files into template
  res <- file.copy(script_files, file.path(skeleton_loc, "skeleton.Rmd"),
                   overwrite = TRUE)
  
  names(res) <- basename(script_files)
  res
  
}

copy_demo_to_demo <- function(demo = "theopp"){
  
  ## 3 things to handle
  ##  easy directories - directories that can be copied one to one
  ##  script directories - only want source scripts (no htmls etc.) transferred
  ##  staging models - this needs to go to the Models directory in example

   
  ## non script directories can be copied as is
  easy_directories <- c("localpackage",
                        "SourceData")
  
  models_dir <- "staging/Models"
  
  script_files <- dir(nm_default_dir("scripts"))
  
  script_files <- script_files[grepl("\\.R|r(md)?$", script_files) |
                                 grepl("(R|r)eadme", script_files)]
  
  script_files <- file.path(nm_default_dir("scripts"), script_files)
  script_files <- relative_path(script_files,
                                rprojroot::find_root(rprojroot::has_file(".Rprofile")))
  
  destination <- system.file("extdata", "examples", demo, package = "NMproject")
  unlink(destination, recursive = TRUE, force = TRUE)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  
  ####
  ## easy directories
  
  res1 <- file.copy(easy_directories, destination, recursive = TRUE, overwrite = TRUE)
  names(res1) <- easy_directories
  
  ####
  ## script directories

  destination_files <- file.path(destination, script_files)
  
  dir.create(file.path(destination, "Scripts"), recursive = TRUE, showWarnings = FALSE)
  res2 <- file.copy(script_files, destination_files, overwrite = TRUE)
  names(res2) <- script_files
  
  ####
  ## staging/Models
  dir.create(file.path(destination, "Models"), recursive = TRUE, showWarnings = FALSE)
  res3 <- file.copy("staging/Models", destination, recursive = TRUE, overwrite = TRUE)
  names(res3) <- "staging/Models"
  
  c(res1, res2, res3)
}

copy_demo_to_test <- function(demo = "theopp"){
  
  easy_directories <- c("localpackage",
                        "SourceData",
                        "staging")
  
  script_files <- dir(nm_default_dir("scripts"))
  
  script_files <- script_files[grepl("\\.R|r(md)?$", script_files) |
                                 grepl("(R|r)eadme", script_files)]
  
  script_files <- file.path(nm_default_dir("scripts"), script_files)
  script_files <- relative_path(script_files,
                                rstudioapi::getActiveProject())
  
  destination <- system.file("tests", "testthat", package = "NMproject")
  destination <- file.path(destination, demo)
  
  unlink(destination, recursive = TRUE, force = TRUE)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
 
  ####
  ## easy directories
  
  res1 <- file.copy(easy_directories, destination, recursive = TRUE, overwrite = TRUE)
  names(res1) <- easy_directories
  
  ### remove unneeded staging files - not so easy
  unlink(file.path(destination, "staging", "Scripts"), recursive = TRUE)
  unlink(file.path(destination, "staging", "SourceData"), recursive = TRUE)  
  
  ####
  ## .cache -> cache
  #dir.create(file.path(destination, "cache"), recursive = TRUE, showWarnings = FALSE)
  res1b <- file.copy(".cache", destination, recursive = TRUE)
  res1c <- file.rename(file.path(destination, ".cache"), file.path(destination, "cache"))
  
  ####
  ## script directories
  
  destination_files <- file.path(destination, script_files)
  
  dir.create(file.path(destination, "Scripts"), recursive = TRUE, showWarnings = FALSE)
  res2 <- file.copy(script_files, destination_files, overwrite = TRUE)
  names(res2) <- script_files
  
  ####
  ## models directories 
  
  all_model_files <- dir("Models", all.files = TRUE, recursive = TRUE, full.names = TRUE)
  
  all_model_files <- all_model_files[!grepl(".html", all_model_files)]
  
  all_model_files <- all_model_files[!all_model_files %in% ls_tempfiles()]
  
  ## specific for demo
  
  keep_m1_outs <- all_model_files[grepl("sdtabm1\\b", basename(all_model_files)) |
                                    grepl("patabm1\\b", basename(all_model_files)) |
                                    grepl("cotabm1\\b", basename(all_model_files)) |
                                    grepl("catabm1\\b", basename(all_model_files))]
  
  all_model_files <- all_model_files[!grepl("sdtab", basename(all_model_files))]
  all_model_files <- all_model_files[!grepl("patab", basename(all_model_files))]
  all_model_files <- all_model_files[!grepl("cotab", basename(all_model_files))]
  all_model_files <- all_model_files[!grepl("catab", basename(all_model_files))]
  #all_model_files <- all_model_files[!tools::file_ext(all_model_files) %in% 
  #                                     c("lst")]
  
  all_model_files <- c(all_model_files, keep_m1_outs)
  
  all_model_files <- all_model_files[!tools::file_ext(all_model_files) %in% 
                                       c("xml")]
  
  all_model_files <- all_model_files[!basename(all_model_files) %in% 
                                       c("command.txt")]
  
  all_model_files <- all_model_files[!grepl("temp", dirname(all_model_files))]
  
  # all_model_files %>%
  #   file.info() %>%
  #   arrange(desc(size)) %>%
  #   mutate(file_name = row.names(.)) %>%
  #   select(file_name, size) %>%
  #   head(10)

  dirs <- unique(dirname(all_model_files))
  destination_dirs <- file.path(destination, dirs)
  for(dirname in destination_dirs) dir.create(dirname, recursive = TRUE, showWarnings = FALSE)
  
  destination_files <- file.path(destination, all_model_files)
  #dir.create(file.path(destination, "Scripts"), recursive = TRUE, showWarnings = FALSE)
  
  res3 <- file.copy(all_model_files, destination_files, overwrite = TRUE)
  names(res3) <- script_files
  
  zip_file <- paste0(destination, ".zip")
  orig_dir <- getwd()
  on.exit(setwd(orig_dir))
  setwd(dirname(zip_file))
  unlink(zip_file)
  utils::zip(basename(zip_file), demo)
  setwd(orig_dir)
  
  ## maybe make destination and repeat zip
  file.rename(zip_file, paste0(zip_file, ".bak"))
  
  unlink(file.path(destination, "Models/c1_f2", recursive = TRUE))
  
  orig_dir <- getwd()
  on.exit(setwd(orig_dir))
  setwd(dirname(zip_file))
  unlink(zip_file)
  utils::zip(basename(zip_file), demo)
  setwd(orig_dir)
  
  extdata_loc <- system.file("extdata", package = "NMproject")
  file.copy(zip_file, extdata_loc, overwrite = TRUE)
  
  file.rename(paste0(zip_file, ".bak"), zip_file)
  unlink(destination, recursive = TRUE)
  
  message("size of ", basename(zip_file), ": ", system(paste("du -sh", zip_file), intern = TRUE))
  
}
