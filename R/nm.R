
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

#' Edit control file
#'
#' @param r object of class nm, a file name, or a run_id
#' @export
show_ctl <- function(r) {
  UseMethod("show_ctl")
}
#' @export
show_ctl.default <- function(r) {
  edit_file(search_ctl_name(r))
}

#' Show lst file
#'
#' @param r object of class nm
#' @export
show_out <- function(r){
  UseMethod("show_out")
}

#' @export
show_out.nm <- function(r) {
  show_file(r$output$psn.lst)
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

#' Run NONMEM
#' 
#' Run nm objects.  Uses system_nm() to submit the "cmd" value of object 
#' 
#' @param r objects of class nm/or list of objects class nm/or data.frame with column of nm objects
#' @param overwrite logical. Should run directory be overwritten (default=FALSE)
#' @param delete_dir logical. NA (default - directory will be deleted if no dependencies exists)
#' TRUE or FALSE. Should run_dir be deleted.
#' @param wait logical (default=FALSE). Should R wait for run to finish.
#' Default can be changed with  wait_by_default() function
#' @param update_db logical (default=TRUE). Should run_status be updated
#' @param ignore.stdout logical (default=TRUE). Parameter passed to system()
#' @param ignore.stderr logical (default=TRUE). Parameter passed to system()
#' @param initial_timeout numeric. time in seconds.
#' time period to give up on a run if directory hasn't been created.
#' @param quiet logical (default=FALSE). should system_nm output be piped to screen
#' @param intern logical. intern arg to be passed to system
#' @param force logical (default = FALSE).  Force run even results unchanged
#'
#' @return If only one object of class nm was specified, silently returns object.
#' Otherwise returns nothing.
#'
#' @export
run_nm <- function(r, overwrite=getOption("run_overwrite"),delete_dir=c(NA,TRUE,FALSE),wait=getOption("wait"),
                   update_db=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE,
                   initial_timeout=NA, quiet = getOption("quiet_run"),intern=getOption("intern"),
                   force = FALSE)
  UseMethod("run_nm")

#' @export
run_nm.default <- function(r, overwrite=getOption("run_overwrite"),delete_dir=c(NA,TRUE,FALSE),wait=getOption("wait"),
                           update_db=TRUE,ignore.stdout = TRUE, ignore.stderr = TRUE,
                           initial_timeout=NA, quiet = getOption("quiet_run"),intern=getOption("intern"),
                           force = FALSE){
  invisible(r)
}
  
#' Run NONMEM
#' 
#' Use run_nm instead. run was deprecated due to naming conflict with future package's run() function
#' 
#' @param ... objects
#' @export

run <- function(...){
  .Deprecated("run_nm", msg = "run() will soon be deprecated, use run_nm() instead")
  run_nm(...)
}

#' tests if job is finished
#'
#' @param r object class nm
#' @param initial_timeout numeric. time in seconds.
#' time period to give up on a run if directory hasn't been created.
#' @export
is_finished <- function(r,initial_timeout=NA){
  UseMethod("is_finished")
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

#' clean_run files
#'
#' @param r object class nm
#' @param delete_dir logical. NA (default), TRUE or FALSE. Should run_dir be deleted.
#' @param update_db logical (default=TRUE). Should run_status be updated
#' @export
clean_run <- function(r,delete_dir=c(NA,TRUE,FALSE),update_db=!is.null(r$db_name)){
  UseMethod("clean_run")
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

nm_output.default <- function(r,dorig,...){
  
  if(requireNamespace("xpose4")) {
    xpdb <- xpose4::xpose.data(run_id(r), directory=paste0(run_in(r),"/"))
    d <- xpdb@Data
  } else d <- data.frame()
  
  if(nrow(d) == 0){
    ctl_out_files <- r$output$ctl_out_files
    ctl_out_files <- ctl_out_files[grepl("tab", ctl_out_files)]
    
    d <- lapply(ctl_out_files, function(out_file){
      d <- utils::read.table(out_file, skip = 1, header = TRUE)
    })
    
    d <- do.call(cbind,d)
    d <- d[,!duplicated(names(d))]
  }
  
  if(missing(dorig)) dorig <- get_data(r,...)
  
  filter_statements <- data_filter_char(r)
  if(identical(filter_statements, "TRUE")){
    dORD <- seq_len(nrow(dorig))
  } else {
    expre <- parse(text=filter_statements)
    dorig[is.na(dorig)] <- 0
    dORD <- which(with(dorig,eval(expre)))    
  }

  if(nrow(d) %% length(dORD) != 0) {
    stop("something wrong... when R reads in original dataset
and applies filter ",filter_statements,",
there's ",length(dORD),"rows, but NONMEM output has ", nrow(d), " rows")
  }    

  ctl_contents <- ctl_character(r)
  sim_ctl <- any(grepl("^\\s*\\$SIM",rem_comment(ctl_contents)))
  
  nreps <- nrow(d) / length(dORD)
  
  if("PRKEY" %in% names(d)) stop("name conflict with PRKEY in xpose table. aborting...")
  if("PRKEY" %in% names(dorig)) stop("name conflict with PRKEY in original data. aborting...")

  d$PRKEY <- dORD
  dorig$PRKEY <- 1:nrow(dorig)
  if(sim_ctl){
    if("SIM" %in% names(d)) stop("name conflict with SIM in xpose table. aborting...")
    if("SIM" %in% names(dorig)) stop("name conflict with SIM in original data. aborting...")
    d$SIM <- rep(1:nreps,each=length(dORD))
    message("Adding column: SIM")
  }

  d$INNONMEM <- TRUE

  ## want a DV_OUT columsn
  if("DV_OUT" %in% names(d)) warning("name conflict with DV_OUT in xpose table. replacing...")
  d$DV_OUT <- d$DV
  d$DV <- NULL
  d <- d[,c(setdiff(names(d),names(dorig)[!names(dorig) %in% c("PRKEY")]))]
  #dorig <- dorig[,names(dorig)[!names(dorig) %in% c("DV")]]

  d2 <- merge(dorig, d, all.x = TRUE, by = "PRKEY")

  d2$INNONMEM <- d2$INNONMEM %in% TRUE
  if(nreps > 1) d2$SIM[is.na(d2$SIM)] <- 0

  ## row number check
  if(nrow(d2) != nrow(d)*(nreps-1)/nreps + nrow(dorig)) stop("merge went wrong. debug")

  message("Adding column: PRKEY")

  return(d2)
}



process_output <- function(r, ...){
  #if(!inherits(r, "nmexecute")) stop("can only currently process outputs for execute runs")
  do <- nm_output(r, ...)
  save(do, file = file.path(run_dir(r, full_path = TRUE), "NMout.RData"))
  invisible(do)
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
  out_path <- file.path(run_dir(r, full_path = TRUE), "NMout.RData")
  if(!file.exists(out_path)) {
    do <- process_output(r, ...)
  } else {
    load(out_path)
  }
  return(do)
}

#' Get ignore statement
#' @param r object coercible into ctl_list
#' @export
data_ignore_char <- function(r){
  UseMethod("data_ignore_char")
}
#' @export
data_ignore_char.default <- function(r){
  dol_data <- ctl_list(r)$DATA
  dol_data <- dol_data[!dol_data %in% ""]
  dol_data <- rem_comment(dol_data)
  
  ignore_present <- any(grepl(".*IGNORE\\s*=\\s*\\(",dol_data))
  accept_present <- any(grepl(".*ACCEPT\\s*=\\s*\\(",dol_data))
  
  type <- NA
  if(ignore_present & accept_present) stop("cannot identify ignore columns")
  if(ignore_present) type <- "IGNORE"
  if(accept_present) type <- "ACCEPT"
  no_filter <- is.na(type)
  
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
#' @export
data_filter_char <- function(r){
  ignore_char <- data_ignore_char(r)
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

