## internal function
nm_generic <- function(run_id = as.character(NA),
                       prev_run_id = as.character(NA),
                       ctl_path = file.path(getOption("models.dir"),"run{run_id}.mod"),
                       type = "execute",
                       run_dir = "{run_id}",
                       results_dir = "Results"){
 
  m <- list(type = type,
            run_id = run_id,
            prev_run_id = prev_run_id,
            results_dir = results_dir)
  
  class_name <- paste0("nm_",type)
  class(m) <- c(class_name, "nm_generic")

  m <- m %>% run_dir(run_dir)
  m <- m %>% ctl_path(ctl_path) ## transform ctl_path name
  if(file.exists(ctl_path(m))) m <- m %>% cmd(ctl_path(m))
  
  ## want to track output files (do in run_nm)
  
  m
}

#' Create a new nm object
#' 
#' Experimental new nm object for v0.5 interface.  Not compatible with previous system.
#'   The basic object this package centres around.  Most package functions act on this object.
#' 
#' @param run_id character vector. Run identifier
#' @param prev_run_id character vector (optional). Run identifier of previous run 
#' @param ctl_path character. Path to control file
#' @param type character (default = "execute").  Type of run to run
#' @param run_dir character (default = {run_id}).  Subdirectory where PsN wll run NONMEM
#' @param results_dir character (default = "Results").
#'    Directory to store results of this run
#' 
#' @return An object of class nm_list.  Object is concatenatable.
#'    Length of object corresponds to length of run_id
#' @examples 
#' \dontrun{
#' 
#' m0 <- nm2(run_id = "m0")
#' m0  ## a human readable representation
#'   
#' ## nm objects can be put into tibbles to group runs together
#' d <- tibble(run_id = c("m1","m2"))
#' d$m <- nm2(d$run_id)
#' d  
#' 
#' }
#' 
#' @export

#' @include utilsExtra.R
nm2 <- Vectorize_nm_list(nm_generic, SIMPLIFY = FALSE)

print.nm_generic <- function(object, ...){
  collapse_fields <- c("ctl")
  for(field in collapse_fields){
    if(field %in% names(object)) object[[field]] <- "...[collapsed]..."    
  }
  ## remove all raw fields from output
  remove_fields <- c(names(object)[grepl("^raw_", names(object))])
  for(field in remove_fields) object[[field]] <- NULL    
  utils::str(object, ...)
}

print.nm_list <- function(object, ...){
  for(i in seq_along(object)) {
    collapse_fields <- c("ctl")
    for(field in collapse_fields){
      if(field %in% names(object[[i]])) object[[i]][[field]] <- "...[collapsed]..."      
    }
    remove_fields <- c(names(object[[i]])[grepl("^raw_", names(object[[i]]))])
    for(field in remove_fields) object[[i]][[field]] <- NULL    
  }
  utils::str(object, ...)
}

custom_1d_field <- function(m, field, replace){
  UseMethod("custom_1d_field")
}
custom_1d_field.nm_generic <- function(m, field, replace){
  if(missing(replace)){
    if(length(m[[field]]) > 0) return(m[[field]]) else return(as.character(NA))
  }
  m[[field]] <- replace
  raw_field <- paste0("raw_",field)
  m[[raw_field]] <- replace

  ## fill in {other_field} tags
  m <- replace_tags(m, field)
  ## redo all other fields
  for(other_col in names(m)[!names(m) %in% field]) m <- replace_tags(m, other_col)
  m
}
custom_1d_field.nm_list <- Vectorize_nm_list(custom_1d_field.nm_generic, replace_arg = "replace")

## Internal convenience function for updating {tags} in a text field
replace_tags <- function(m, field){
  raw_field <- paste0("raw_",field)
  ## only proceed if "raw" field exists
  if(raw_field %in% names(m) & field %in% names(m)){
    ## start by resetting to raw
    m[[field]] <- m[[raw_field]]
    ## cycle through the other fields
    for(i in names(m)[!names(m) %in% c(field, raw_field)]){
      replace_text <- m[[i]]
      ## only proceed if prospective text is one dimensional
      if(length(replace_text) == 1){
        tag <- paste0("\\{",i,"\\}")
        if(grepl(tag, m[[field]])) {
          m[[field]] <- gsub(tag, replace_text, m[[field]])
        } 
      }
    }
  }
  m
}

## internal function for replacing tags in a string (text arg) with info from m
replace_tags_character <- function(m, text){
  UseMethod("replace_tags_character")
}
replace_tags_character.nm_generic <- function(m, text){
  for(i in names(m)){
    replace_text <- m[[i]]
    ## only proceed if prospective text is one dimensional
    if(length(replace_text) == 1){
      tag <- paste0("\\{",i,"\\}")
      if(grepl(tag, text)) {
        text <- gsub(tag, replace_text, text)
      } 
    }
  }
  text
}
replace_tags_character.nm_list <- Vectorize_nm_list(replace_tags_character.nm_generic, SIMPLIFY = TRUE)

custom_vector_field <- function(m, field, replace){
  UseMethod("custom_vector_field")
}
custom_vector_field.nm_generic <- function(m, field, replace){
  if(missing(replace)){
    if(length(m[[field]]) > 0) return(m[[field]]) else return(as.character(NA))
  }
  m[[field]] <- replace
  m
}
custom_vector_field.nm_list <- Vectorize_nm_list(custom_vector_field.nm_generic, SIMPLIFY = FALSE, replace_arg = "replace")


#' Path to NONMEM control file
#' 
#' Get and set path to control file.
#' 
#' @param m nm object
#' @param text optional character. Name of path to control file (see details).
#'   Typically, this file does not yet normally exist, but will house the code
#'   code for this run
#' 
#' @details "text" can contain "{run_id}" string.  E.g. "Models/run{run_id}.mod"
#'   will use the name "Models/runm1.mod" if run_id(m1) is "m1".
#' 
#' @examples
#' \dontrun{
#' 
#' m0 <- nm2(run_id = "m0")
#' ctl_path(m0)
#' m0 <- m0 %>% ctl_path("Models/run_m0.mod")
#' m0
#' ## warning: this has make the run identifier in old xpose4 "_m0" not "m0"!
#'
#' m1 <- m0 %>% run_id("m1")
#' ctl_path(m1)  ## run_id number has updated but ctl_path hasn't changed
#'
#' ## To avoid this use {run_id}
#' m0 <- m0 %>% ctl_path("Models/run_{run_id}.mod")
#' ctl_path(m0)
#' 
#' m1 <- m0 %>% run_id("m1")
#' m1  ## ctl_path has updated as expected
#' 
#' }
#' @export

ctl_path <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "ctl_path") else custom_1d_field(m, "ctl_path", text)
}

run_dir <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "run_dir") else custom_1d_field(m, "run_dir", text)
}

cmd <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "cmd") else custom_1d_field(m, "cmd", text)
}

type <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "type") else custom_1d_field(m, "type", text)
}

prev_run_id <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "prev_run_id") else custom_1d_field(m, "prev_run_id", text)
}

results_dir <- function(m, text) {
  if(missing(text)) custom_1d_field(m, "results_dir") else custom_1d_field(m, "results_dir", text)
}

# ctl_path <- function(m, text){
#   UseMethod("ctl_path")
# }
# ctl_path.nm_generic <- function(m, text){
#   if(missing(text)) {
#     return(m[["ctl_path"]])
#   }
#   
#   m[["raw_ctl_path"]] <- text
#   m[["ctl_path"]] <- m[["raw_ctl_path"]]
#   
#   for(i in names(m)[!names(m) %in% c("ctl_path", "raw_ctl_path")]){
#     tag <- paste0("\\{",i,"\\}")
#     replace_text <- m[[i]]
#     if(grepl(tag, m[["raw_ctl_path"]])) {
#       if(length(replace_text) > 1) stop("can't use vector valued tags")
#       m[["ctl_path"]] <- gsub(tag, replace_text, m[["raw_ctl_path"]])
#     }
#   }
#   
#   #if(!is.na(m[["run_id"]])) m[["ctl_path"]] <- gsub("\\{run_id\\}", m[["run_id"]], m[["raw_ctl_path"]])
#   m
# }
# ctl_path.nm_list <- Vectorize_nm_list(ctl_path.nm_generic)

## run_in UseMethod already definied
run_in.nm_generic <- function(x){
  if(length(x$ctl_path) > 0) return(dirname(x$ctl_path)) else 
    return(as.character(NA))
}
run_in.nm_list <- Vectorize_nm_list(run_in.nm_generic)

# cmd <- function(m, text){
#   UseMethod("cmd")
# }
# cmd.nm_generic <- function(m, text){
#   if(missing(text)) {
#     if(length(m[["cmd"]]) > 0) return(m[["cmd"]]) else return(as.character(NA))
#   }
#   m[["raw_cmd"]] <- text
#   
#   for(i in names(m)[!names(m) %in% c("cmd", "raw_cmd")]){
#     tag <- paste0("\\{",i,"\\}")
#     replace_text <- m[[i]]
#     if(grepl(tag, m[["raw_cmd"]])) {
#       if(length(replace_text) > 1) stop("can't use vector valued tags")
#       m[["cmd"]] <- gsub(tag, replace_text, m[["raw_cmd"]])
#     }
#   }
#   
#   #if("run_id" %in% names(m)) m[["cmd"]] <- gsub("\\{run_id\\}", m[["run_id"]], m[["raw_cmd"]])
#   #if("run_dir" %in% names(m)) m[["cmd"]] <- gsub("\\{run_dir\\}", m[["run_dir"]], m[["raw_cmd"]])
#   m
# }
# cmd.nm_list <- Vectorize_nm_list(cmd.nm_generic)

## run_id UseMethod already definied
run_id.nm_generic <- function(x, text, ...){
  m <- x
  if(missing(text)) {
    if(is_single_na(m)) return(as.character(NA))
    if(length(m[["run_id"]]) > 0) return(m[["run_id"]]) else return(as.character(NA))
  }

  m[["prev_run_id"]] <- m[["run_id"]]
  m[["run_id"]] <- text
  
  #old_ctl_file <- m[["ctl_path"]]
  #m <- m %>% ctl_path(m[["raw_ctl_path"]]) ## set new control name
  #m <- m %>% ctl(old_ctl_file) ## update ctl file from old ctl_file
  
  ## get names of all text fields and reapply them
  
  raw_fields_to_update <- names(m)[grepl("^raw_", names(m))]
  fields_to_update <- gsub("^raw_(.*)", "\\1", raw_fields_to_update)
  
  ## filter for only ones that exist in object
  raw_fields_to_update <- raw_fields_to_update[fields_to_update %in% names(m)]
  fields_to_update <- fields_to_update[fields_to_update %in% names(m)]
  
  for(i in seq_along(fields_to_update)){
    field <- fields_to_update[i]
    raw_field <- raw_fields_to_update[i]
    if(exists(field)){    ## function has to exist
      if(inherits(get(field), "function")) ## function has to be a function
        m <- m %>% get(field)(m[[raw_field]])
    } else {
      m <- m %>% custom_1d_field(field, m[[raw_field]])
    }
  }
  
  if("ctl" %in% names(m)) m <- m %>% ctl(m[["ctl"]]) ## update ctl object
  #if("raw_cmd" %in% names(m)) m <- m %>% cmd(m[["raw_cmd"]]) ## update command
  
  m
}
run_id.nm_list <- Vectorize_nm_list(run_id.nm_generic)

## extend ctl_list generic
ctl_list.nm_generic <- function(r){
  ctl_list(r$ctl)
}

#' set/get control file object
#'
#' @param m nm object
#' @param ctl_ob optional path to control file
#' @param update_ctl logical. Should table numbers and author fields be updated
#' @param ... additional arguments
#' 
#' @export

ctl <- function(m, ctl_ob, update_ctl = TRUE, ...){
  .Deprecated("show_ctl")
  UseMethod("ctl")
}
ctl.nm_generic <- function(m, ctl_ob, update_ctl = TRUE, ...){
  
  if(missing(ctl_ob)){
    if(length(m[["ctl"]]) > 0) return(m[["ctl"]]) else return(as.character(NA))
  }
  
  ctl <- ctl_list(ctl_ob)
  if(update_ctl) ctl <- ctl %>% new_ctl(m[["ctl_path"]], based_on = m[["prev_run_id"]])
  m[["ctl"]] <- ctl
  m
}
ctl.nm_list <- Vectorize_nm_list(ctl.nm_generic, SIMPLIFY = FALSE, replace_arg = "ctl_ob")

# prev_run_id <- function(m, text){
#   UseMethod("prev_run_id")
# }
# prev_run_id.nm_generic <- function(m, text){
#   if(missing(text)){
#     if(length(m[["prev_run_id"]]) > 0) return(m[["prev_run_id"]]) else return(as.character(NA))
#   }
#   m[["prev_run_id"]] <- text
#   m
# }
# prev_run_id.nm_list <- Vectorize_nm_list(prev_run_id.nm_generic)

# results_dir <- function(m, text){
#   UseMethod("results_dir")
# }
# results_dir.nm_generic <- function(m, text){
#   if(missing(text)){
#     if(length(m[["results_dir"]]) > 0) return(m[["results_dir"]]) else return(as.character(NA))
#   }
#   m[["results_dir"]] <- text
#   m
# }
# results_dir.nm_list <- Vectorize_nm_list(results_dir.nm_generic)

write_ctl.nm_generic <- function(ctl, dest, dir = getOption("models.dir"), ...){
  m <- ctl
  ctl_name <- ctl_path(m)
  ctl_ob <- ctl(m) %>% ctl_character()
  dir_name <- run_in(m)
  
  if(!file.exists(dir_name)) 
    dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
  
  writeLines(ctl_ob, ctl_name)
  #suppressMessages(tidyproject::setup_file(ctl_name))
  #message("written: ", ctl_name)
  invisible(m)
}
write_ctl.nm_list <- Vectorize_nm_list(write_ctl.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

start_manual_edit <- function(m, name){
  m %>% write_ctl()            ## update file
  edit_file(ctl_path(m))
  invisible(m)
}

stop_manual_edit <- function(m){
  old_m <- m
  m <- m %>% ctl(ctl_path(m), update_ctl = FALSE)  ## update object
  
  old_ctl <- as.character(ctl_character(ctl(as_nm_generic(old_m))))
  new_ctl <- as.character(ctl_character(ctl(as_nm_generic(m))))
  
  if(requireNamespace("diffobj", quietly = TRUE)){
    dff <- diffobj::diffChr(new_ctl, old_ctl, format = "ansi256")
    #dff <- diffobj::diffChr(new_ctl, old_ctl, format = "html")
    
    ## TODO: store patch in object for later retrieval
    ## learn how git2r calls libgit - maybe something useful there
    
    message("--- file diff: new_ctl and old_ctl colours show additions/deletions---")
    print(dff)
  }
  
  invisible(m)

}

result_file <- function(m, name){
  name <- replace_tags_character(m, name)
  file.path(results_dir(m), name)
}

############

if(0){
  
  m0 <- nm2(run_id = "m0")
  m0
  cmd(m0)  ## NA
  run_id(m0)  ## "m0"
  run_in(m0)
  run_dir(m0)
  type(m0)
  ctl(m0)
  
  m1 <- nm2(run_id = "m1")
  m1 <- m1 %>% cmd("qpsn -t 100 -c auto -- {type} run{run_id}.mod -dir={run_dir}")
  m1 <- m1 %>% ctl("/projects/qcp/QCP_MODELING/ONC/azd6094/poppk_20181203_pk-eff-ae-interim3/Models/runm2.mod")
  
  m1 <- m1 %>% custom_1d_field(field = "field1", replace = "text of field {run_id}")
  m1 %>% custom_1d_field(field = "field1")
  
  #m1 <- m1 %>% custom_vector_field(field = "parent_ob", replace = m0)
  #m1 %>% custom_vector_field(field = "parent_ob")
  
  cmd(m1)  ## display cmd
  ctl(m1)  ## display cmd
  
  m2 <- m1 %>% run_id("m2")
  cmd(m2)
  ctl(m2)
  
  m2boot <- m2 %>% type("bootstrap") %>%
    cmd("qpsn -t 100 -c auto -- bootstrap --samples=50 run{run_id}.mod -dir={run_id}")
    
  # m2 <- m2 %>% update_parameters(m1) %>%
  #   update_dollar_data("newdata.csv") %>%
  #   update_ignore("TAD > 50")
  
  m2 <- m2 %>% start_manual_edit()  ##(interactive)
  #' < new_ctl
  #' > old_ctl
  #' @@ 82,8 / 82,4 @@
  #'   D1 = EXP(MU_6+ETA(6))
  #' 
  #' < TVF1=EXP(THETA(5))
  #' < MU_5=LOG(TVF1)
  #' < F1 = EXP(MU_5+ETA(5))
  #' <
  #' 
  #'   K = CL/V2
  #' @@ 104,5 / 100,12 @@
  #'   LLOQ = LOG(LOQ)
  #' 
  #' > IF (BLOQ.LT.1) THEN
  #' >     F_FLAG=0
  #' Y=IPRED + EPS(1) + EPS(2)/FSAFE
  #' > ELSE
  #' >     F_FLAG=1
  #' >     Y=PHI((LLOQ-IPRED)/W)
  #' > ENDIF
  #' >
  #' 
  #'   $THETA
  m2 <- m2 %>% stop_manual_edit()  ##(interactive)    ## prints patch name to screen
  
  ## first time, 
  ##  create patch - start stop savepatch
  ## second time
  ##  if(patch exists & has been applied) do nothing
  ##  if(patch exists & has not been applied) apply patch
  
  
  m3 <- m2 %>% run_id("m3") %>%
    results_dir("Models/{run_dir}")

  result_file(m3,"gof_{run_id}.pdf")
  
  m <- m %>% register_result("gof_{run_id}.pdf")
  
  ## track the result file in the object
  file.path(results_dir(m3),paste0("gof_plot_",run_id(m3),".pdf"))
  ## start manual patch 
  
  ## want to be able to save a patch and apply it latter

  d <- dplyr::tibble(cores = 1:36) %>%
    mutate(m = m2 %>% run_id(cores),
           m = m %>% custom_1d_field("cores", cores),
           m = m %>% ctl_path("Models/m2_coretest/run{run_id}.mod"),
           m = m %>% cmd("qpsn -t 100 -c {cores} -- execute run{run_id}.mod -dir={run_id}"))

  d$m <- d$m %>% run_nm(wait = TRUE) %>%
    update_parameters() %>%
    run_nm(wait = TRUE) %>%
    gof() %>%
    print("done")
  
  d <- dplyr::tribble(
    ~CMTs, ~logDV, ~dataset,
    1,       TRUE,  "data.csv",
    1,       FALSE,  "data.csv",
    2,       TRUE,  "dataSMALL.csv",
    2,       FALSE,  "dataSMALL.csv"
  )
  
  d$m <- m2 %>% run_id(paste0("m",1:nrow(d)))
  
  cmd(d$m)

  d$m[d$CMTs %in% 1] <- d$m[d$CMTs %in% 1] %>% cmd("qpsn -t 59 -c auto -- execute run{run_id}.mod -dir={run_id}")
  
  d$cmd <- d$m %>% cmd
  d$m <- d$m %>% ctl_path("Models/baseModelTest/run{run_id}.mod")
  d$ctl_path <- d$m %>% ctl_path()
  
  #d$m[d$CMTs %in% 1] <- d$m[d$CMTs %in% 1] %>% make_one_compt()
  #d$m[d$logDV] <- d$m[d$logDV] %>% make_logDV()
  #d$m <- d$m %>% update_dollar_data(paste0("../../DerivedData/", d$dataset))
  
  d$m$m4
  d$m[4]
  d$m[[4]]
  d$m[run_id(d$m) %in% "m4"]
  
}



