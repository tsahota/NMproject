
#' Get job information (if it exists)
#'
#' Requires 'get_job_info' to be defined as an option - function that takes stdout console output
#' from a job and returns a character
#'
#' @param m object class nm
#' @param text optional character to set job_info
#' @export
job_info <- function(m, text){
  UseMethod("job_info")  
}

#' Show lst file
#'
#' @param r object of class nm
#' @export
out <- function(r) {
  .Deprecated("show_out")
}

show_file <- function(file_name){
  if(.Platform$OS.type=="windows")
    file.show(file_name) else
      if(exists("file.show")) file.show(file_name) else
        utils::file.edit(file_name)
}

edit_file <- function(file_name){
  if(.Platform$OS.type=="windows")
    file.show(file_name) else
      get("file.edit")(file_name)
}


search_ctl_name <- function(r, models_dir=getOption("models.dir")){
  if(inherits(r,"nm")) ctl_name <- r$ctl
  if(inherits(r,"numeric") | inherits(r,"character")) {
    r <- as.character(r)
    rtemp <- normalizePath(r,mustWork = FALSE)
    if(file.exists2(rtemp)) ctl_name <- rtemp else {
      rtemp <- from_models(normalizePath(r,mustWork = FALSE), models_dir = models_dir)
      if(file.exists2(rtemp)) ctl_name <- rtemp else {
        ctl_name <- from_models(paste0(getOption("model_file_stub"),r,".",getOption("model_file_extn")), models_dir = models_dir)
      }
    }
  }
  ctl_name
}

file.exists2 <- function(x){ ## only true is file exists and is not a directory
  if(!file.exists(x)) return(FALSE)
  !file.info(x)$isdir
}



gsub2 <- function(pattern,replacement,x,...){
  match <- grepl(pattern,x,...)
  if(!match) return(character())
  gsub(pattern,replacement,x,...)
}

get_stub_name <- function(file_name) gsub("(.*)\\..*","\\1",file_name)

get_run_id <- function(ctl_name){
  stub_name <- get_stub_name(ctl_name)
  match <- paste0("^.*",getOption("model_file_stub"),"(.*).*$")
  ans <- gsub2(match,"\\1",stub_name)
  if(length(ans)==0) ans <- NA
  ans
}



#' Condition number of run
#' 
#' @param r object of class nm
#' @export

cond_num <- function(r){
  UseMethod("cond_num")
}

#' @export
cond_num.default <- function(r){
  if(is_single_na(r)) return(as.numeric(NA))
  if(is.data.frame(r)){
    dc <- r
    ans <- as.numeric(dc$FINAL[dc$parameter %in% "CONDNUM"])
    if(length(ans) == 0) ans <- as.numeric(NA)
    return(ans)
  }
  stop("don't know how to get cond_num of this")
}

#' @export
cond_num.nmcoef <- function(r){
  if(is_empty_nmcoef(r)) return(as.numeric(NA))
  dc <- r
  ans <- as.numeric(dc$FINAL[dc$parameter %in% "CONDNUM"])
  if(length(ans) == 0) as.numeric(NA) else ans
}

#' @export
cond_num.list <- function(r){
  sapply(r, cond_num)
}

#cond_num <- Vectorize_nm(cond_num, vectorize.args = "r", SIMPLIFY = TRUE, USE.NAMES = FALSE)


nm_steps_finished <- function(r){ # for waiting
  execution_dirs <- dirname(dir(r$run_dir,
                                pattern = "psn\\.mod$",
                                recursive = TRUE,
                                full.names = TRUE))
  execution_dirs <- unique(execution_dirs)
  lst_names <- file.path(execution_dirs,"psn.lst")

  if(length(execution_dirs) > 0){
    ## check these all exist
    finished <- sapply(lst_names,function(lst_name){
      if(!file.exists(lst_name)) return(FALSE)
      lst <- try(readLines(lst_name),silent = TRUE)
      if(inherits(lst,"try-error")) return(FALSE)
      lst <- lst[max(1,(length(lst)-5)):length(lst)]
      stopped <- any(grepl("Stop Time:",lst))
      psn_error <- file.exists(file.path(dirname(lst_name),"psn_nonmem_error_messages.txt"))
      return(stopped | psn_error)
    })
    finished <- all(finished)
  } else finished <- FALSE ## if no execution_dirs
  return(finished)
}

last_modified <- function(r){
  directory <- r$run_dir
  d <- file.info(directory)
  if(nrow(d)==0) return("")
  d <- data.frame(file=rownames(d),mtime=d$mtime)
  if(nrow(d)==0) return(as.character(NA))
  d <- d[d$mtime %in% max(d$mtime), ]
  as.character(unique(d$mtime))
}

#' Should run() wait for job to finish
#'
#' @param x logical. TRUE means run() will wait, FALSE = asynchronous execution
#' @export

wait_default <- function(x) options("wait"=x)

#' Should run() overwrite previously run jobs
#'
#' @param x logical. TRUE means run() will overwrite previous runs by default,
#' @export

overwrite_default <- function(x) options("run_overwrite"=x)

#' Should run() work in non interactive mode
#'
#' Depreciated
#'
#' @export
non_interactive_mode <- function() {
  .Deprecated("interactive_mode")
}

#' Should run() work in interactive mode
#'
#' @param value logical. TRUE = use interactive mode. FALSE = use non-interactive mode
#' @export
interactive_mode <- function(value) {
  if(missing(value)) stop("expecting TRUE/FALSE argument")
  if(!is.logical(value)) stop("expecting TRUE/FALSE argument")
  if(value){
    overwrite_default(FALSE)
    wait_default(FALSE)
  } else {
    overwrite_default(TRUE)
    wait_default(TRUE)
  }
}

#' @export
nm_tran.nm <- function(x){
  tidyproject::check_if_tidyproject()
  nm_tran.default(x$ctl)
}

#' List files to be cleaned up
#' 
#' @param r object class nm
#' @export
#' @examples 
#' \dontrun{
#' mod1 %>% extra_files
#' mod1 %>% extra_files %>% cleanup
#' }

extra_files <- function(r){
  tidyproject::check_if_tidyproject()
  if(r$type %in% "execute"){
    table_files <- ctl_table_files(r)
    files <- file.path(r$run_dir, "NM_run1", table_files)
  }
  if(r$type %in% "vpc"){
    print("TBD")
    files <- character()
  }
  return(files)
}


ctl_table_files <- function(ctl){
  UseMethod("ctl_table_files") 
}

ctl_table_files.default <- function(ctl){ 
  ctl <- ctl_character(ctl)
  s0 <- rem_comment(ctl)
  s <- grep("FILE\\s*=\\s*(\\S+)",s0,value=TRUE)
  table_files <- gsub(".*FILE\\s*=\\s*(\\S+)\\s*.*$","\\1", s)
  table_files
}

ctl_out_files <- function(ctl_file){
  UseMethod("ctl_out_files")  
}

ctl_out_files.default <- function(ctl_file){ ## will get vector of $TABLE file names from control file.
  if(!file.exists(ctl_file)) stop(paste(ctl_file, "doesn't exist"))
  dir0 <- dir(dirname(ctl_file))
  
  ctl <- readLines(ctl_file,warn = FALSE)
  
  table.files <- ctl_table_files(ctl)

  stub <- basename(ctl_file)
  stub <- gsub("(.+)\\.\\w+$","\\1",stub)

  out.files <- dir0[grepl(paste(stub,"\\.",sep=""),dir0)]
  out.files <- out.files[!grepl("scm",out.files)]
  out.files <- out.files[!out.files%in%basename(ctl_file)]

  out.files <- c(table.files,out.files)
  out.files
}

input_files <- function(run_in,run_type,ctl_name){
  r <- list()
  r$ctl <- ctl_name
  if(run_type %in% "execute"){
    r$data_name <- data_name(ctl_name)
    r$data_loc <- file.path(run_in,r$data_name)
  }
  return(r)
}

output_files <- function(run_in,run_type,run_dir,ctl_name){
  r <- list()
  if(run_type %in% "execute"){
    extn <- tools::file_ext(ctl_name)
    r$lst <- paste0(get_stub_name(ctl_name),".lst")
    r$psn.mod <- file.path(run_dir,"NM_run1","psn.mod")
    r$psn.lst <- file.path(run_dir,"NM_run1","psn.lst")
    r$psn.ext <- file.path(run_dir,"NM_run1","psn.ext")
    r$psn.cov <- file.path(run_dir,"NM_run1","psn.cov")
    r$psn.cor <- file.path(run_dir,"NM_run1","psn.cor")
    r$psn.xml <- file.path(run_dir,"NM_run1","psn.xml")
    r$ctl_out_files <- file.path(run_in,ctl_out_files(ctl_name))
  }
  return(r)
}

#' write csv for NONMEM control files
#' @param ... arguments for write.csv
#' @param na character. Default changed to ".".
#' @param row.names logical. Default changed to FALSE.
#' @param quote logical. Fefault changed to FALSE
#' @export

write.csv.nm <- function(d, ...,na=".",
                         row.names=FALSE,
                         quote=FALSE){
  
  utils::write.csv(d, ...,na=na,row.names=row.names,quote=quote)
}

#' Setup demo files
#'
#' @param demo_name character. Name of demo. Default = "theopp"
#' @param new_project character. To set up demo in a separate (new) project
#' @param file_stub character. Default = "run1". Stub to some file names
#' @param overwrite logical. Default changed to FALSE.
#' @param exclude character. Name of extension to exclude from copying
#' @param additional_demo_locations character vector. default = NULL.
#'   locations for demo directories
#' @export

setup_nm_demo <- function(demo_name="theopp",
                          new_project = NA,
                          file_stub = paste0(getOption("model_file_stub"),1),
                          overwrite=FALSE,
                          exclude=NULL,
                          additional_demo_locations = NULL){
  
  if(!is.na(new_project)){
    cwd <- getwd()
    on.exit(setwd(cwd))
    tidyproject::make_project(proj_name = new_project)
    setwd(new_project)
  }
  
  tidyproject::check_if_tidyproject()
  
  examples_dir <- character()
  examples_dirs <- character()

  if(length(additional_demo_locations) > 0) {
    examples_dir <- normalizePath(additional_demo_locations, mustWork = FALSE)
    examples_dirs <- list.files(examples_dir, full.names = TRUE, recursive = FALSE)
    #examples_dir <- examples_dirs[grepl(paste0(.Platform$file.sep, demo_name,"$"), examples_dirs)]
  }
  ## TODO: rename examples to demos
  examples_dirs <- append(examples_dirs, 
                          list.files(system.file("extdata","examples",package = "NMproject"),
                                     full.names = TRUE, recursive = FALSE))
  matched_examples_dirs <- examples_dirs[grepl(paste0(.Platform$file.sep, demo_name,"$"), examples_dirs)]
  
  if(length(matched_examples_dirs) == 0)
    stop("demo not found.\nAvailable demos:\n ",
         paste(unique(basename(examples_dirs)), collapse = "\n "), call. = FALSE)
  
  #examples_dir <- append(examples_dir, system.file("extdata","examples",demo_name,package = "NMproject"))
  examples_dir <- matched_examples_dirs[1]

  files_to_copy <- dir(examples_dir, all.files = TRUE, full.names = TRUE, recursive = TRUE)
  
  stage_info <- tidyproject::stage(files_to_copy, overwrite = overwrite, silent = TRUE)
  
  tidyproject::import(stage_info, overwrite = overwrite)

}

#' @export
run_all_scripts <- function(){
  #browser()
  script_files <- dir(scripts_dir(), "s[0-9]+_.*?\\.R(md)?$", full.names = TRUE)
  
  dplan <- tibble::tibble(script_files) %>%
    dplyr::mutate(rmd = grepl("\\.Rmd", .data$script_files))
  
  dplan <- dplan %>%
    dplyr::mutate(
      cmd = ifelse(rmd, 
                   paste0("rmarkdown::render(\"",script_files,"\")"),
                   paste0("source(\"",script_files,"\")"))
    )
  
  exprs <- rlang::parse_exprs(dplan$cmd)
  
  res <- lapply(exprs, rlang::eval_tidy)
  
}


#' Get NONMEM output tables
#'
#' This combines $TABLE output with the input data, allowing text columns to be retained for plotting/summaries.
#'
#' @param r data.frame.  object of class nmexecute
#' @param dorig data.frame. optional NONMEM input dataset.
#' @param ... additional arguments to pass on to read.csv
#' @export

nm_output <- function(r,dorig,...){
  UseMethod("nm_output")  
}

#' Get processed output table
#' 
#' @param r object of class nm
#' @param ... optional additional arguments to pass on to read.csv of orig data
#' @export

output_table <- function(r, ...){
  UseMethod("output_table") 
}

#' @export
output_table.default <- function(r, ...){
  out_path <- file.path(run_dir(r, full_path = TRUE), "NMout.RDS")
  if(!file.exists(out_path)) {
    do <- nm_output(r, ...)
    saveRDS(do, file = out_path)
  } else {
    do <- readRDS(out_path)
  }
  return(do)
}


#' Get ignore statement
#' @param r object coercible into ctl_list
#' @param data data.frame (default = missing) optional input dataset from r
#' @export
data_ignore_char <- function(r, data){
  UseMethod("data_ignore_char")
}
#' @export
data_ignore_char.default <- function(r, data){
  dol_data <- ctl_list(r)$DATA
  dol_data <- dol_data[!dol_data %in% ""]
  dol_data <- rem_comment(dol_data)
  dol_data <- unlist(strsplit(dol_data, split = "\\s"))
  
  ignore_present <- any(grepl(".*IGNORE\\s*=\\s*\\(",dol_data))
  accept_present <- any(grepl(".*ACCEPT\\s*=\\s*\\(",dol_data))
  
  type <- NA
  if(ignore_present & accept_present) stop("cannot identify ignore columns")
  if(ignore_present) type <- "IGNORE"
  if(accept_present) type <- "ACCEPT"
  no_filter <- is.na(type)
  
  ## get nonmem names and input names
  ## read in the data - need only header though
  if(missing(data)) data <- input_data(r, filter = FALSE, silent = TRUE)
  
  r_data_names <- names(data)
  ## now get nonmem names
  dollar_input <- ctl_list(r)$INPUT
  nonmem_data_names <- gsub("\\$\\w+", "", dollar_input)
  nonmem_data_names <- unlist(strsplit(nonmem_data_names, split = "\\s"))
  nonmem_data_names <- nonmem_data_names[!nonmem_data_names %in% ""]
  nonmem_data_names <- gsub("\\w+=(\\w+)", "\\1", nonmem_data_names)
  #if(length(r_data_names) != length(nonmem_data_names))
  #  stop("length of items in $INPUT doesn't match dataset")
  name_chart <- data.frame(r_data_names, nonmem_data_names, stringsAsFactors = FALSE)
  name_chart <- name_chart[name_chart$r_data_names != name_chart$nonmem_data_names,]
  
  if(!no_filter){
    filter_statements <- paste0(".*",type,"\\s*=\\s*\\((\\S[^\\)]+)\\)*.*")
    dol_data <- dol_data[grepl(filter_statements, dol_data)]
    filter_statements <- gsub(filter_statements,"\\1",dol_data)
    filter_statements <- unlist(strsplit(filter_statements,","))
    filter_statements <- gsub("\\.EQ\\.","==",filter_statements)
    filter_statements <- gsub("\\.NE\\.","!=",filter_statements)
    filter_statements <- gsub("\\.EQN\\.","==",filter_statements)
    filter_statements <- gsub("\\.NEN\\.","!=",filter_statements)
    filter_statements <- gsub("\\./E\\.","!=",filter_statements)
    filter_statements <- gsub("\\.GT\\.",">",filter_statements)
    filter_statements <- gsub("\\.LT\\.","<",filter_statements)
    filter_statements <- gsub("\\.GE\\.",">=",filter_statements)
    filter_statements <- gsub("\\.LE\\.","<=",filter_statements)
    
    ## substitute names from 
    for(i in seq_len(nrow(name_chart))){
      nonmem_data_name <- paste0("\\b", name_chart$nonmem_data_names[i], "\\b")
      r_data_name <- name_chart$r_data_names[i]
      filter_statements <- gsub(nonmem_data_name,
                                r_data_name,
                                filter_statements)
    }
    
    filter_statements <- paste(filter_statements, collapse= " | ")
    if("ACCEPT" %in% type) filter_statements <- paste0("!(",filter_statements,")")
  } else {
    filter_statements <- "FALSE"
  }
  filter_statements
}

#' Get filter statement
#' 
#' Opposite of data_ignore_char 
#' 
#' @param r object coercible into ctl_list
#' @param ... arguments passed to data_ignore_char
#' @export
data_filter_char <- function(r, ...){
  ignore_char <- data_ignore_char(r, ...)
  if(ignore_char == "FALSE") return("TRUE")
  ignored <- !grepl("^!\\((.*)\\)", ignore_char)
  accepted <- !ignored
  if(accepted){
    return(gsub("^!\\((.*)\\)", "\\1", ignore_char) )
  } else {
    return(paste0("!(",ignore_char,")"))
  }
}

#' replace ignore statement
#' @param ctl object coercible into ctl_list
#' @param ignore_char character. replacement statement
#' @export
update_ignore <- function(ctl, ignore_char){
  UseMethod("update_ignore")
}

#' @export
update_ignore.default <- function(ctl, ignore_char){
  ctl <- ctl_list(ctl)
  
  ignore_present <- any(grepl(".*IGNORE\\s*=\\s*\\(",ctl$DATA))
  if(ignore_present){
    ## remove any row that matches exactly
    ctl$DATA <- ctl$DATA[!grepl("^(\\s*)IGNORE\\s*=\\s*\\(*\\S[^\\)]+\\)*(\\s*)$",ctl$DATA)]
    ## remove only bracketed IGNORE statement if other things are on the line.
    ctl$DATA <- gsub("(.*)IGNORE\\s*=\\s*\\(+\\S[^\\)]+\\)+(.*)",
         "\\1\\2", ctl$DATA)
  }
  
  ignore_char <- gsub("\\s*\\|\\s*", ", ", ignore_char)
  
  ignore_char <- gsub("==",".EQ.",ignore_char)
  ignore_char <- gsub("!=",".NE.",ignore_char)
  ignore_char <- gsub(">",".GT.",ignore_char)
  ignore_char <- gsub("<",".LT.",ignore_char)
  ignore_char <- gsub(">=",".GE.",ignore_char)
  ignore_char <- gsub("<=",".LE.",ignore_char)
  ignore_char <- gsub("\\s+(\\.\\S+\\.)\\s+", "\\1", ignore_char)
  
  ignore_char <- paste0("IGNORE=(",ignore_char,")")
  
  last_line <- ctl$DATA[length(ctl$DATA)]
  
  if(grepl("^\\s*$", last_line)){
    ctl$DATA[length(ctl$DATA)] <- ignore_char
  } else {
    ctl$DATA <- append(ctl$DATA, ignore_char) 
  }
  ctl$DATA <- append(ctl$DATA, "")
  ctl

}

#' update sizes statement
#' @param ctl object coercible into ctl_list
#' @param sizes_char character. replacement statement
#' @export

update_sizes <- function(ctl, sizes_char){
  ctl <- ctl_character(ctl)
  if("SIZES" %in% names(ctl_list(ctl))){
    stop("can't modifying existing sizes yet")
  } else {
    dol_matches <- grep("\\s*\\$", ctl)
    if(length(dol_matches) == 0) dol_matches <- 1 else {
      dol_matches <- dol_matches[1]
    }
    before <- c()
    after <- ctl
    if(dol_matches > 1){
      before <- ctl[1:(dol_matches-1)]
      after <- ctl[dol_matches:length(ctl)]
    }
    save_attr <- attributes(ctl)
    ctl <- c(before,
      paste("$SIZES", sizes_char),
      after)
    attributes(ctl) <- save_attr
  }
  ctl_list(ctl)
}


#' Exclude rows of NONMEM dataset
#' 
#' @param d data.frame for NONMEM dataset
#' @param dexcl data.frame consisting of rows to be ignored
#' @param exclude_col character.  Name of exclude column in d
#' @examples 
#' \dontrun{
#' ## use with dplyr
#' dexcl <- d %>% filter(ID == 23, TIME > 18, TIME < 24) %>% select(ID, TIME, DV, EXCL)
#' dexcl  ## view rows to be excluded
#' d <- d %>% exclude_rows(dexcl)
#' }
#' @export

exclude_rows <- function(d, dexcl, exclude_col = "EXCL"){
  excluded <- do.call(paste, d[, names(d) %in% names(dexcl)]) %in% 
    do.call(paste, dexcl)
  excluded <- which(excluded)
  if(nrow(dexcl) != length(excluded)) stop("couldn't find all rows")
  d[[exclude_col]][excluded] <- 1
  d
}

