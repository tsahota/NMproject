#' Remove parameter from NONMEM control file
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Attempts to remove a parameter from the NONMEM control assuming it has been
#' written according to NMproject conventions (i.e. TVPARAM notation and
#' TVPARAM + IIV_PARAM comments in $THETA/$OMEGA).  The presence of any code
#' that depends on the removed parameter will cause the control file to break.
#'
#' @param m An nm object.
#' @param name Character. Parameter name to remove.
#'
#' @export
remove_parameter <- function(m, name) {
  old_target <- target(m)
  m <- m %>%
    untarget() %>%
    gsub_ctl(paste0(".*\\b", name, "\\b.*"), "") %>%
    gsub_ctl(paste0(".*\\bTV", name, "\\b.*"), "") %>%
    gsub_ctl(paste0(".*\\bIIV_", name, "\\b.*"), "")
  m %>% target(old_target)
}


#' Add a mixed effect parameter to $PK (or $PRED)
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Primarily an internal function. This will (by default) add a parameter (mixed
#' effect) to your code $PK/$PRED and $THETA/$OMEGA.
#'
#' @param m An nm object.
#' @param name Character. Name of NONMEM variable to create.
#' @param init Numeric (default = `1`). Initial value of fixed effect.
#' @param unit Character (default = `""`). Unit of variable.
#' @param trans Character (default = `"LOG"`). Transformation of the variable.
#' @param position Integer. Not used.
#' @param after Character. Pattern to match and include the mixed effect after.
#'
#' @return An nm object with modified `ctl_contents` field.
#' 
#' @examples 
#' 
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#' 
#' m1 %>% dollar("PK")
#' m1 %>% dollar("THETA")
#' 
#' m1 <- m1 %>% add_mixed_param("ALAG1", init = 1, unit = "h", trans = "LOG")
#' 
#' m1 %>% dollar("PK")
#' m1 %>% dollar("THETA")
#' 
#' @export
add_mixed_param <- function(m, name,
                            init = 1, unit = "", trans = c("LOG"),
                            position = NA_integer_,
                            after = character()) {
  trans <- match.arg(trans)

  old_target <- target(m)

  sub_names <- names(ctl_contents(as_nm_generic(m)))

  if ("PK" %in% sub_names) PK_PRED <- "PK"
  if ("PRED" %in% sub_names) PK_PRED <- "PRED"

  m <- m %>% target(PK_PRED)

  etas <- m %>%
    grab_variables("\\bETA\\([0-9]+\\)") %>%
    sort()
  max_etas_in_pk <- 1
  if (length(etas) > 0) {
    max_etas_in_pk <- gsub("\\bETA\\(([0-9]+)\\)", "\\1", etas[length(etas)])
    max_etas_in_pk <- as.numeric(max_etas_in_pk)
  }

  n_thetas <- n_thetas(m)

  if (trans == "LOG") {
    m <- m %>%
      target(PK_PRED) %>%
      text("TV_NEWPARAM_=EXP(THETA(_N_PARAM_))
MU__MU_PARAM_=LOG(TV_NEWPARAM_)
_NEWPARAM_ = EXP(MU__MU_PARAM_+ETA(_MU_PARAM_))
", append = TRUE, after = after) %>%
      target("THETA") %>%
      text(paste(
        signif(log(init), 2), "      ; TV_NEWPARAM_ ; _UNIT_PARAM_ ; ",
        trans
      ), append = TRUE) %>%
      target("OMEGA") %>%
      text("0.1     ; IIV__NEWPARAM_ ; LOG", append = TRUE)
  }

  m <- m %>%
    untarget() %>%
    gsub_ctl("_N_PARAM_", n_thetas + 1) %>%
    gsub_ctl("_NEWPARAM_", name) %>%
    gsub_ctl("_MU_PARAM_", max_etas_in_pk + 1) %>%
    gsub_ctl("_UNIT_PARAM_", unit)

  m %>% target(old_target)
}


rename_parameter_ <- function(m, new_name, name) {
  UseMethod("rename_parameter_")
}

rename_parameter_.nm_generic <- function(m, new_name, name) {

  ## comment out "new_param =" rows
  m <- m %>% comment_out(paste0("^\\s*", new_name, "\\s*\\="))

  text <- get_target_text(m)

  commented <- grepl("^\\s*;", text)

  text[!commented] <- text[!commented] %>%
    stringr::str_replace_all(paste0("\\b", name, "\\b"), paste0(new_name)) %>%
    stringr::str_replace_all(paste0("\\bTV", name, "\\b"), paste0("TV", new_name)) %>%
    stringr::str_replace_all(paste0("\\bIIV_", name, "\\b"), paste0("IIV_", new_name))

  m <- m %>% set_target_text(text)
  m

  # m <- m %>% untarget %>%
  #   gsub_ctl(paste0("\\b",name,"\\b"), paste0(new_name)) %>%
  #   gsub_ctl(paste0("\\bTV",name,"\\b"), paste0("TV",new_name)) %>%
  #   gsub_ctl(paste0("\\bIIV_",name,"\\b"), paste0("IIV_",new_name))
  # m %>% target(old_target)
}

rename_parameter_.nm_list <- Vectorize_nm_list(rename_parameter_.nm_generic, SIMPLIFY = FALSE)

#' Rename a parameter in NONMEM control stream
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' @param m An nm object.
#' @param ... Named arguments with character values indicated old names.
#'
#' @examples
#' \dontrun{
#'
#' m1 %>% rename_parameter(CL = "KE")
#' ## renames KE to CL
#' }
#' @export
rename_parameter <- function(m, ...) {
  rename_list <- list(...)
  new_name <- names(rename_list)
  name <- unlist(rename_list)
  rename_parameter_(m, new_name, name)
}

n_thetas <- function(m) {
  param_info <- param_info(m)
  nrow(param_info[grepl("THETA", param_info$parameter), ])
}
