#' @name add_remove_covs
#' @rdname add_remove_covs
#' @title Add/remove a covariate to a NONMEM model
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Follows PsN coding conventions to add covariates into a model.  The advantage
#' is no need to create a .scm file, just directly modify the NONMEM control
#' file contents. This function is used by [covariate_step_tibble()] for
#' stepwise covariate model development.
#'
#' @param ctl An nm object or an object coercible to `ctl_list`.
#' @param param Character. Name of parameter.
#' @param cov Character. Name of covariate.
#' @param state Numeric or character. Number or name of state (see details).
#' @param continuous Logical (default = `TRUE`). Is covariate continuous?
#' @param time_varying Optional logical. is the covariate time varying?
#' @param additional_state_text Optional character (default = empty). Custom
#'   state variable to be passed to `param_cov_text`.
#' @param id_var Character (default = `"ID"`). Needed if time_varying is
#'   missing.
#' @param force Logical (default = `FALSE``). Force covariate in even if missing
#'   values found.
#' @param force_TV_var Logical (default = `FALSE`). Force covariates only on
#'   `TV` notation parameters.
#' @param init Optional numeric/character vector.  Initial estimate of
#'   additional parameters.
#' @param lower Optional numeric/character vector.  lower bound of additional
#'   parameters.
#' @param upper Optional numeric/character vector.  Upper bound of additional
#'   parameters.
#'
#' @details Available `state`s:
#'
#' \describe{
#'
#'  \item{"2" or "linear"}{
#'   PARCOV= ( 1 + THETA(1)*(COV -median))
#'  }
#'
#'  \item{"3" or "hockey-stick"}{
#'   IF(COV.LE.median) PARCOV = ( 1 + THETA(1)&ast;(COV - median))
#'   IF(COV.GT.median) PARCOV = ( 1 + THETA(2)&ast;(COV - median))
#'  }
#'
#'  \item{"4" or "exponential"}{
#'   PARCOV= EXP(THETA(1)*(COV - median))
#'  }
#'
#'  \item{"5" or "power"}{
#'   PARCOV= ((COV/median)**THETA(1))
#'  }
#'
#'  \item{"power1"}{
#'   PARCOV= ((COV/median))
#'  }
#'
#'  \item{"power0.75"}{
#'   PARCOV= ((COV/median)**0.75)
#'  }
#'
#'  \item{"6" or "log-linear"}{
#'   PARCOV= ( 1 + THETA(1)*(LOG(COV) - log(median)))
#'  }
#'
#' }
#'
#' @return An nm object with modified `ctl_contents` field.
#' @seealso [covariate_step_tibble()], [test_relations()]
#'
#' @examples 
#' 
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#'
#' \dontrun{
#'  
#' m1WT <- m1 %>% child("m1WT") %>%
#'   add_cov(param = "V", cov = "WT", state = "power")
#'
#' m1 %>% dollar("PK")
#' m1WT %>% dollar("PK")  ## notice SCM style code added
#' 
#' nm_diff(m1WT)
#'
#' run_nm(c(m1, m1WT))
#' rr(c(m1, m1WT))
#' summary_wide(c(m1, m1WT)) 
#' }
#'
#' @export

add_cov <- function(ctl, param, cov, state = 2, continuous = TRUE,
                    time_varying, additional_state_text = list(), id_var = "ID",
                    force = FALSE, force_TV_var = FALSE,
                    init, lower, upper) {
  UseMethod("add_cov")
}

#' @export
add_cov.nm_generic <- function(ctl, param, cov, state = 2, continuous = TRUE,
                               time_varying, additional_state_text = list(), id_var = "ID",
                               force = FALSE, force_TV_var = FALSE,
                               init, lower, upper) {
  m <- ctl
  ctl <- ctl_contents(m)
  param <- as.character(param)
  cov <- as.character(cov)
  state <- as.character(state)
  continuous <- as.logical(continuous)

  dol_PK <- "PRED"
  if ("PK" %in% names(ctl)) dol_PK <- "PK"

  PK_section <- rem_comment(ctl[[dol_PK]])

  data <- suppressMessages(input_data(m, filter = TRUE))

  if (!cov %in% names(data)) {
    stop_txt <- paste0("can't find ", cov, " in data")
    if (force) warning(stop_txt, call. = FALSE) else stop(stop_txt, call. = FALSE)
  }

  if (any(is.na(data[[cov]]))) {
    stop_txt <- paste0("missing values in ", cov, " detected")
    if (force) warning(stop_txt, call. = FALSE) else stop(stop_txt, call. = FALSE)
  }

  if (length(unique(data[[cov]])) > 5 & !continuous) {
    warning(length(unique(data[[cov]])), " unique values for ", cov, " found. are you sure it's categorical?",
      call. = FALSE
    )
  }

  if (length(unique(data[[cov]])) <= 1) {
    warning(length(unique(data[[cov]])), " unique values for ", cov, " found. are you sure about this?",
      call. = FALSE
    )
  }

  if (missing(time_varying)) {
    max_levels <- max(tapply(data[[cov]], data[[id_var]], function(x) length(unique(x))), na.rm = TRUE)
    if (max_levels > 1) time_varying <- TRUE else time_varying <- FALSE
  }

  if (force_TV_var) {
    tvparam <- paste0("TV", param)
  } else {
    if (time_varying) {
      tvparam <- param
    } else {
      ## try TV param if exists
      if (any(grepl(paste0("\\bTV", param, "\\b"), PK_section))) {
        tvparam <- paste0("TV", param)
      }
    }
  }

  if (!any(grepl(paste0("\\bTV", param, "\\b"), PK_section))) {
    stop("cant find parameter in control file", call. = FALSE)
  }

  existing_param_rel <- any(grepl(paste0("\\b", tvparam, "COV"), PK_section))
  existing_param_cov_rel <- any(grepl(paste0("\\b", tvparam, cov), PK_section))
  if (existing_param_cov_rel) {
    return(as_nm_generic(nm(NA)))
    # stop("covariate relation already exists, cannot add", call. = FALSE)
  }

  param_info <- param_info(ctl)
  theta_n_start <- max(param_info$N) + 1

  relation_start_txt <- paste0(";;; ", tvparam, "-RELATION START")
  relation_end_txt <- paste0(";;; ", tvparam, "-RELATION END")

  definition_start_txt <- paste0(";;; ", tvparam, cov, "-DEFINITION START")
  definition_end_txt <- paste0(";;; ", tvparam, cov, "-DEFINITION END")

  if (!existing_param_rel) {
    par_relation_text <- paste0(tvparam, "COV=", tvparam, cov)

    ## insert at beginning
    ctl[[dol_PK]] <- c(
      ctl[[dol_PK]][1], "",
      relation_start_txt,
      par_relation_text,
      relation_end_txt,
      ctl[[dol_PK]][-1]
    )

    tv_definition_row <- which(grepl(paste0("^\\s*", tvparam, "\\s*="), rem_comment(ctl[[dol_PK]])))
    dont_count <- which(grepl(paste0("^\\s*", tvparam, "\\s*=.*\\b", tvparam), rem_comment(ctl[[dol_PK]])))
    tv_definition_row <- setdiff(tv_definition_row, dont_count)
    if (length(tv_definition_row) > 1) stop("can't find unique TV parameter definition in $PK")
    if (length(tv_definition_row) == 0) stop("can't find TV parameter definition in $PK")

    ctl[[dol_PK]] <- c(
      ctl[[dol_PK]][1:tv_definition_row], "",
      paste0(tvparam, " = ", tvparam, "COV*", tvparam),
      ctl[[dol_PK]][(tv_definition_row + 1):length(ctl[[dol_PK]])]
    )
  }

  if (existing_param_rel) {
    ctl[[dol_PK]] <- gsub(
      paste0(tvparam, "COV="),
      paste0(tvparam, "COV=", tvparam, cov, "*"), ctl[[dol_PK]]
    )
  }

  ## use state to get the relationship in there.
  param_cov_text <- param_cov_text(
    param = tvparam, cov = cov, state = state,
    data = data,
    theta_n_start = theta_n_start,
    continuous = continuous,
    additional_state_text = additional_state_text
  )

  ctl[[dol_PK]] <- c(
    ctl[[dol_PK]][1], "",
    definition_start_txt,
    param_cov_text,
    definition_end_txt,
    ctl[[dol_PK]][-1]
  )

  ## add thetas
  n_add_thetas <- attr(param_cov_text, "n")
  if (n_add_thetas > 0) {
    if (missing(init)) {
      init <- rep("0.0001", n_add_thetas)
      if (state == 3 | state == "power") {
        init <- rep(0.8, n_add_thetas)
      }
    }

    if (missing(lower)) {
      lower <- rep(-1, n_add_thetas)
    }

    if (missing(upper)) {
      upper <- rep(5, n_add_thetas)
    }


    if (any(lower > init)) stop("lower bound > initial estimate")
    if (any(upper < init)) stop("upper bound < initial estimate")

    if (n_add_thetas == 1) {
      theta_lines <- paste0("$THETA  (", lower, ",", init, ",", upper, ") ; ", tvparam, cov, state)
    } else {
      theta_lines <- paste0("$THETA  (", lower, ",", init, ",", upper, ") ; ", tvparam, cov, state, "_", seq_len(n_add_thetas))
    }
    ctl$THETA <- c(ctl$THETA, theta_lines)
  }

  m <- m %>% ctl_contents_simple(ctl)
}

#' @export
add_cov.nm_list <- Vectorize_nm_list(add_cov.nm_generic, SIMPLIFY = FALSE)


#' @rdname add_remove_covs
#'
#' @details
#' `remove_cov` only works with covariates added with `add_cov`.
#'
#' @examples
#' \dontrun{
#'
#' m1noWT <- m1 %>% child("m1noWT") %>%
#'   remove_cov(param = "CL", cov = "WT") %>%
#'   run_nm()
#'
#' ## compare results
#'
#' rr(c(m1, m1noWT))
#' summary_wide(c(m1, m1noWT))
#' }
#'
#' @export

remove_cov <- function(ctl, param, cov, state = 2, continuous = TRUE,
                       time_varying, id_var = "ID") {
  UseMethod("remove_cov")
}

#' @export
remove_cov.nm_generic <- function(ctl, param, cov, state = 2, continuous = TRUE,
                                  time_varying, id_var = "ID") {
  m <- ctl
  ctl <- ctl_contents(m)
  param <- as.character(param)
  cov <- as.character(cov)
  state <- as.character(state)
  continuous <- as.logical(continuous)

  if ("PK" %in% names(ctl)) dol_PK <- "PK" else dol_PK <- "PRED"

  PK_section <- rem_comment(ctl[[dol_PK]])

  data <- suppressMessages(input_data(m, filter = TRUE))

  if (any(is.na(data[[cov]]))) warning("missing values in ", cov, " detected")

  if (missing(time_varying)) {
    max_levels <- max(tapply(data[[cov]], data[[id_var]], function(x) length(unique(x))), na.rm = TRUE)
    if (max_levels > 1) time_varying <- TRUE else time_varying <- FALSE
  }

  if (time_varying) {
    tvparam <- param
  } else {
    tvparam <- paste0("TV", param)
  }

  existing_param_rel <- which(grepl(paste0("\\b", tvparam, "COV"), PK_section))
  existing_param_cov_rel <- which(grepl(paste0("\\b", tvparam, cov), PK_section))

  ## remove parm vs specific cov code
  match_start <- grep(paste0(";;; ", tvparam, cov, "-DEFINITION START"), ctl[[dol_PK]])
  match_end <- grep(paste0(";;; ", tvparam, cov, "-DEFINITION END"), ctl[[dol_PK]])
  if (length(match_start) == 0 | length(match_end) == 0) {
    return(as_nm_generic(nm(NA)))
    # stop("can't find cov definition code - did you add with add_cov()?")
  }

  ctl_matched <- ctl[[dol_PK]][match_start:match_end]
  theta_match <- gregexpr("THETA\\([0-9]+\\)", ctl_matched)

  thetas <- lapply(seq_along(theta_match), function(i) {
    matchi <- theta_match[[i]]
    ctl_matchedi <- ctl_matched[i]
    if (length(matchi) == 1) {
      if (matchi %in% -1) {
        return(NULL)
      }
    }
    sapply(seq_along(matchi), function(j) {
      matchij <- matchi[j]
      len <- attr(matchi, "match.length")[j]
      return(substr(ctl_matchedi, matchij, matchij + len - 1))
    })
  })
  thetas <- unlist(thetas)
  theta_n <- gsub("THETA\\(([0-9]+)\\)", "\\1", thetas)
  theta_n <- sort(as.numeric(theta_n))
  reduce_thetas <- length(theta_n)
  if (reduce_thetas > 0) {
    ctl_char <- ctl_character(ctl)

    next_theta <- max(theta_n) + 1
    keep_going <- TRUE
    while (keep_going) {
      next_text_to_match <- paste0("THETA\\(", next_theta, "\\)")
      next_text_to_replace <- paste0("THETA\\(", next_theta - reduce_thetas, "\\)")
      if (!any(grepl(next_text_to_match, ctl_char))) {
        keep_going <- FALSE
      } else {
        ctl_char <- gsub(next_text_to_match, next_text_to_replace, ctl_char)
        next_theta <- next_theta + 1
      }
    }
    ctl <- ctl_list(ctl_char)
  }

  ctl[[dol_PK]] <- ctl[[dol_PK]][setdiff(seq_along(ctl[[dol_PK]]), match_start:match_end)]

  ## adjust/remove parm vs any cov code

  match_start <- grep(paste0(";;; ", tvparam, "-RELATION START"), ctl[[dol_PK]])
  match_end <- grep(paste0(";;; ", tvparam, "-RELATION END"), ctl[[dol_PK]])
  if (length(match_start) == 0 | length(match_end) == 0) {
    stop("can't find cov relation code - did you add with add_cov()?")
  }

  rel_section <- ctl[[dol_PK]][match_start:match_end]

  rel_index_all <- grep(paste0(tvparam, "COV=", tvparam, cov), rel_section)
  unique_rel_match <- grep(paste0(tvparam, "COV=", tvparam, cov, "$"), rel_section)
  if (length(unique_rel_match) > 1) {
    stop("can't identify unique cov relation code line- did you add with add_cov()?")
  }
  if (length(unique_rel_match) == 1) { ## only covariate on this param
    ctl[[dol_PK]] <- ctl[[dol_PK]][setdiff(seq_along(ctl[[dol_PK]]), match_start:match_end)]
    match_param_rel <- grep(paste0("\\b", tvparam, "COV\\b"), rem_comment(ctl[[dol_PK]]))
    if (length(match_param_rel) != 1) {
      stop("can't identify parameter modification line- did you add with add_cov()?")
    }
    ctl[[dol_PK]] <- ctl[[dol_PK]][setdiff(seq_along(ctl[[dol_PK]]), match_param_rel)]
  }
  if (length(unique_rel_match) == 0) { ## (maybe - test this) other covariates
    if (length(rel_index_all) > 1) {
      stop("can't identify unique cov relation code line- did you add with add_cov()?")
    }

    text_match_at_start <- paste0("^(", tvparam, "COV=)", tvparam, cov, "\\*(.+)")
    match_at_start <- grep(text_match_at_start, ctl[[dol_PK]])

    text_match_after_start <- paste0("^(", tvparam, "COV=.+)\\*", tvparam, cov, "(.*)")
    match_after_start <- grep(text_match_after_start, ctl[[dol_PK]])

    if (length(match_at_start) + length(match_after_start) != 1) {
      stop("couldn't identify cov relation in relation code line- did you add with add_cov()?")
    }

    if (length(match_at_start)) {
      ctl[[dol_PK]] <- gsub(text_match_at_start, "\\1\\2", ctl[[dol_PK]])
    }

    if (length(match_after_start)) {
      ctl[[dol_PK]] <- gsub(text_match_after_start, "\\1\\2", ctl[[dol_PK]])
    }
  }

  matched_theta <- grep(paste0("\\$THETA\\s.*;.*", tvparam, cov), ctl$THETA)
  # if(length(matched_theta) == 0)
  #  stop("can't find $THETA entry to remove- did you add with add_cov()?")

  ctl$THETA <- ctl$THETA[setdiff(seq_along(ctl$THETA), matched_theta)]

  m <- m %>% ctl_contents_simple(ctl)
}

#' @export
remove_cov.nm_list <- Vectorize_nm_list(remove_cov.nm_generic, SIMPLIFY = FALSE)

#' Prepare forward covariate step
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Takes a base nm object and a set of relationships to test (from
#' [test_relations()]) and prepares a `tibble` of NONMEM runs.
#'
#' The goal of NMproject's covariate modelling functions is to provide a
#' stepwise covariate method *with manual decision* making.  This important to
#' ensure that the full model selection/evaluation criteria (should be defined
#' in statistical analysis plans) can be applied at every step rather than just
#' log likelihood ratio testing, where the most significant model may be
#' unstable, may worsen model predictions or may only be slightly more
#' significant than a more physiologically plausible covariate relationship.
#'
#' The functions [test_relations()], [covariate_step_tibble()],
#' [bind_covariate_results()] together comprise NMproject stepwise covariate
#' method with manual decision.  The goal is to be part way between PsN's SCM
#' and completely manual process at each forward and backward elimination step.
#' The syntax of how covariates are included is the same as PsN's SCM routine -
#' See [PsN documentation](https://uupharmacometrics.github.io/PsN/docs.html)
#' for more information.
#'
#' @param base An nm object.
#' @param run_id Base run_id to construct run_ids of covariate runs.
#' @param run_in Character.  See [run_in()].
#' @param dtest `dplyr::tibble` with testing relations (from
#'   [test_relations()]).
#' @param direction Character. `"forward"` (default) or `"backward"`.
#' @param ... Additional arguments passed to [add_cov()].
#'
#' @return Will return `dtest` a `dplyr::tibble` with appended columns.
#'
#' @seealso [test_relations()], [bind_covariate_results()], [add_cov()]
#'
#' @examples
#' \dontrun{
#'
#' dtest <- test_relations(param = c("KA", "K", "V"),
#'                         cov = c("LIN1", "LIN2", "LIN3", "RND1", "RND2", "RND3"),
#'                         state = c("linear", "power"),
#'                         continuous = TRUE) %>%
#'          test_relations(param = c("KA", "K", "V"),
#'                         cov = "BN1",
#'                         state = "linear",
#'                         continuous = FALSE)
#'
#' ## create tibble of covariate step with model objects as column m
#' dsm1 <- m1 %>% covariate_step_tibble(run_id = "m1_f1",
#'                                      dtest = dtest,
#'                                      direction = "forward")
#'
#' ## run all models greedily
#' dsm1$m <- dsm1$m %>% run_nm()
#'
#' ## extract results and put into tibble
#' dsm1 <- dsm1 %>% bind_covariate_results()
#'
#' ## sort by BIC (for example) and view
#' dsm1 <- dsm1 %>% arrange(BIC)
#' dsm1
#'
#' ## check condition number, covariance,...
#' ## run any diagnostics here
#'
#' ## when happy with selection, select run for subsequent step
#'
#' m1_f1 <- dsm1$m[1] ## select most signifcant BIC
#' # alternative select by relationship
#' m1_f1 <- dsm1 %>%
#'   filter(param = "CL", cov = "BWT", state = "power") %$%
#'   m
#'
#' ## do next forward step
#'
#' dsm2 <- m1_f1 %>% covariate_step_tibble(run_id = "m1_f2",
#'                                         dtest = dtest,
#'                                         direction = "forward")
#'
#' ## continue ...
#' }
#' @export

covariate_step_tibble <- function(base, run_id, run_in = nm_default_dir("models"), dtest, direction = c("forward", "backward"), ...) {
  direction <- match.arg(direction)

  ## check dtest

  if (!inherits(dtest, "data.frame")) stop("dtest needs to be a tibble of tested relations - use tested_relations()")

  required_cols <- c("cov", "param", "state", "continuous")
  if (direction == "backward") {
    required_cols <- c("cov", "param")
  }

  for (col in required_cols) {
    if (!col %in% names(dtest)) stop("need a ", col, " column in dtest")
  }

  ## check no covariates are in dataset and have no missing values
  dinput <- dplyr::tibble(input_data(base))
  test_covs <- unique(dtest$cov)
  if (any(!test_covs %in% names(dinput))) {
    stop(
      "cannot find covariates:\n ",
      paste(unique(test_covs[!test_covs %in% names(dinput)]), collapse = ","),
      "\nin dataset"
    )
  }
  incomplete_cases <- names(dinput)[!stats::complete.cases(t(dinput))]
  if (any(test_covs %in% incomplete_cases)) {
    stop(
      "the following covariates contain missing values:\n ",
      paste(unique(test_covs[test_covs %in% incomplete_cases]), collapse = ","),
      "\nstopping to be safe. Impute missing values in input dataset"
    )
  }

  len_unique_cols <- sapply(dinput[, test_covs], function(i) length(unique(i)))
  if (any(len_unique_cols <= 1)) {
    stop(
      "the following covariates only have a single value:\n ",
      paste(test_covs[len_unique_cols <= 1], collapse = ","),
      "\nstopping to be safe. Impute missing values in input dataset"
    )
  }

  dsc <- dtest

  # start_dir <- file.path(nm_default_dir("models"), paste0(run_id(base),"_",label))
  # start_dir <- run_in(base)
  # if(length(start_dir) > 1) stop("non unique parents", call. = FALSE)
  # if(length(start_dir) < 1) stop("can't find parent run in location", call. = FALSE)
  start_dir <- file.path(run_in, run_id)

  if (direction == "forward") {
    dsc$location <- gen_sim_path(dsc[, c("param", "cov", "state")],
      start_dir,
      include_names = FALSE
    )
  } else {
    dsc$location <- gen_sim_path(dsc[, c("param", "cov")],
      start_dir,
      include_names = FALSE
    )
  }

  ## store included relation
  ##   control stream text - prefer this.

  run_ids <- paste(run_id, dsc$param, dsc$cov, dsc$state, sep = "_")

  dsc$m <- base %>% child(run_id = run_ids)

  if (direction == "forward") {
    dsc$m <- add_cov(
      ctl = dsc$m,
      param = dsc$param,
      cov = dsc$cov,
      state = dsc$state,
      continuous = dsc$continuous,
      ...
    )
  } else {
    dsc$m <- remove_cov(
      ctl = dsc$m,
      param = dsc$param,
      cov = dsc$cov,
      ...
    )

    dsc <- dsc[!is.na(dsc$m), ] ## don't want covariates that aren't there
    dsc <- dsc[!duplicated(paste(dsc$param, dsc$cov)), ] ## dont want duplicate states
    dsc$state <- NULL
    dsc$continuous <- NULL
  }

  dsc$m <- dsc$m %>% run_in(dsc$location)
  dsc$m <- dsc$m %>% results_dir(dsc$location)

  dsc
}

#' Add run results into a covariate tibble
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Extracts results from completed covariate runs and combines them into the
#' covariate [tibble()].
#'
#' The goal of NMproject's covariate modelling functions is to provide a
#' stepwise covariate method *with manual decision* making.  This important to
#' ensure that the full model selection/evaluation criteria (should be defined
#' in statistical analysis plans) can be applied at every step rather than just
#' log likelihood ratio testing, where the most significant model may be
#' unstable, may worsen model predictions or may only be slightly more
#' significant than a more physiologically plausible covariate relationship.
#'
#' The functions [test_relations()], [covariate_step_tibble()],
#' [bind_covariate_results()] together comprise NMproject stepwise covariate
#' method with manual decision.  The goal is to be part way between PsN's SCM
#' and completely manual process at each forward and backward elimination step.
#' The syntax of how covariates are included is the same as PsN's SCM routine -
#' See [PsN documentation](https://uupharmacometrics.github.io/PsN/docs.html)
#' for more information.
#'
#' @param dsc An output `tibble` from [covariate_step_tibble()].
#' @param nm_col Character (default = `"m"`). Name of column to store nm objects.
#' @param parameters Character (default = `"new"`).  Passed to [summary_wide()].
#'
#' @return An modified version of `dsc` with additional columns from
#'   [summary_wide()] for model selection purposes.
#'
#' @seealso [covariate_step_tibble()] and [nm_render()] for rendering diagnostic
#'   reports for (subsets of) models in `nm_col`.
#'
#' @examples
#' \dontrun{
#' ## create tibble of covariate step with model objects as column m
#' dsm1 <- m1 %>% covariate_step_tibble(
#'   run_id = "m1_f1",
#'   dtest = dtest,
#'   direction = "forward"
#' )
#'
#' ## run all models greedily
#' dsm1$m <- dsm1$m %>% run_nm()
#'
#' wait_finish(dsm1$m)
#'
#' ## extract results and put into tibble
#' dsm1 <- dsm1 %>% bind_covariate_results()
#' 
#' ## plot goodness of fit diagnostics top 3 models (in terms of p-value)
#' dsm1$m[1:3] %>% nm_render("Scripts/basic_gof.Rmd")
#' 
#' }
#' @export
bind_covariate_results <- function(dsc, nm_col = "m", parameters = "new") {
  dsum <- summary_wide(dsc[[nm_col]], parameters = parameters, trans = FALSE)

  dsc <- dsc[, names(dsc)[!names(dsc) %in% names(dsum)]]

  dsc %>%
    dplyr::bind_cols(dsum) %>%
    dplyr::arrange(.data$p_chisq)
}

#' Generate tibble of covariate relations to test
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' The goal of NMproject's covariate modelling functions is to provide a
#' stepwise covariate method *with manual decision* making.  This important to
#' ensure that the full model selection/evaluation criteria (should be defined
#' in statistical analysis plans) can be applied at every step rather than just
#' log likelihood ratio testing, where the most significant model may be
#' unstable, may worsen model predictions or may only be slightly more
#' significant than a more physiologically plausible covariate relationship.
#'
#' The functions [test_relations()], [covariate_step_tibble()],
#' [bind_covariate_results()] together comprise NMproject stepwise covariate
#' method with manual decision.  The goal is to be part way between PsN's SCM
#' and completely manual process at each forward and backward elimination step.
#' The syntax of how covariates are included is the same as PsN's SCM routine -
#' See [PsN documentation](https://uupharmacometrics.github.io/PsN/docs.html)
#' for more information.
#'
#' @param dtest Optional existing `dtest` to append (from an previous use
#'   [test_relations()]).
#' @param param Character. Name of parameter(s).
#' @param cov Character. Name of covariate(s).
#' @param state Numeric or character. Number/name of state (see details).
#' @param continuous Logical (default = `TRUE`). If `FALSE`, will treat the
#'   covariate as categorical.
#'
#' @details
#'
#' Setting vector values for `param`, `cov`, and `state`, will expand the grid
#' to test each value with every other value greedily.  This is similar to
#' [expand.grid()] available states (see also [add_cov()]):
#'
#' \describe{
#'
#'  \item{"2" or "linear"}{
#'   PARCOV= ( 1 + THETA(1)*(COV -median))
#'  }
#'
#'  \item{"3" or "hockey-stick"}{
#'   IF(COV.LE.median) PARCOV = ( 1 + THETA(1)&ast;(COV - median))
#'   IF(COV.GT.median) PARCOV = ( 1 + THETA(2)&ast;(COV - median))
#'  }
#'
#'  \item{"4" or "exponential"}{
#'   PARCOV= EXP(THETA(1)*(COV - median))
#'  }
#'
#'  \item{"5" or "power"}{
#'   PARCOV= ((COV/median)**THETA(1))
#'  }
#'
#'  \item{"power1"}{
#'   PARCOV= ((COV/median))
#'  }
#'
#'  \item{"power0.75"}{
#'   PARCOV= ((COV/median)**0.75)
#'  }
#'
#'  \item{"6" or "log-linear"}{
#'   PARCOV= ( 1 + THETA(1)*(LOG(COV) - log(median)))
#'  }
#'
#' }
#'
#'
#' @seealso [add_cov()], [covariate_step_tibble()], [bind_covariate_results()]
#'
#' @examples
#'
#' dtest <- test_relations(param = c("KA", "K", "V"),
#'                         cov = c("LIN1", "LIN2", "LIN3", "RND1", "RND2", "RND3"),
#'                         state = c("linear", "power"),
#'                         continuous = TRUE) %>%
#'          test_relations(param = c("KA", "K", "V"),
#'                         cov = "BN1",
#'                         state = "linear",
#'                         continuous = FALSE)
#'
#' dtest
#' @export
test_relations <- function(dtest, param, cov, state, continuous) {
  if (missing(param)) {
    return(dplyr::tibble())
  }
  if (length(continuous) > 1) stop("continous can only be TRUE/FALSE")
  dtest_new <- expand.grid(param = param, cov = cov, state = state, stringsAsFactors = FALSE)
  dtest_new <- dplyr::as_tibble(dtest_new)
  dtest_new$continuous <- continuous
  if (!missing(dtest)) dtest_new <- dplyr::bind_rows(dtest, dtest_new)
  return(dtest_new)
}

#' Generate paths for simulation runs
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Internal function to create nested directory structure paths.
#'
#' @param dsc A `tibble` with experimental factors to vary.
#' @param start_dir Directory name within which all runs should take place.
#' @param include_names Logical (default = TRUE). Should names be included.
#' @keywords internal

gen_sim_path <- function(dsc, start_dir, include_names = TRUE) {
  for (i in which(sapply(dsc, is.factor))) {
    dsc[, i] <- as.character(dsc[, i])
  }
  sapply(1:nrow(dsc), function(i) {
    if (include_names) location <- paste(names(dsc[i, ]), as.character(dsc[i, ]), sep = "_", collapse = .Platform$file.sep)
    if (!include_names) location <- paste(as.character(dsc[i, ]), sep = "_", collapse = .Platform$file.sep)
    file.path(start_dir, location)
  })
}


param_cov_text <- function(param, cov, state, data, theta_n_start, continuous = TRUE,
                           state_text = list(
                             "2" = "PARCOV= ( 1 + THETA(1)*(COV - median))",
                             "linear" = "PARCOV= ( 1 + THETA(1)*(COV - median))",
                             "3" = c(
                               "IF(COV.LE.median) PARCOV = ( 1 + THETA(1)*(COV - median))",
                               "IF(COV.GT.median) PARCOV = ( 1 + THETA(2)*(COV - median))"
                             ),
                             "hockey-stick" = c(
                               "IF(COV.LE.median) PARCOV = ( 1 + THETA(1)*(COV - median))",
                               "IF(COV.GT.median) PARCOV = ( 1 + THETA(2)*(COV - median))"
                             ),
                             "4" = "PARCOV= EXP(THETA(1)*(COV - median))",
                             "exponential" = "PARCOV= EXP(THETA(1)*(COV - median))",
                             "5" = "PARCOV= ((COV/median)**THETA(1))",
                             "power" = "PARCOV= ((COV/median)**THETA(1))",
                             "power1" = "PARCOV= ((COV/median))",
                             "power0.75" = "PARCOV= ((COV/median)**0.75)",
                             "6" = "PARCOV= ( 1 + THETA(1)*(LOG(COV) - log(median)))",
                             "log-linear" = "PARCOV= ( 1 + THETA(1)*(LOG(COV) - log(median)))"
                           ),
                           additional_state_text = list(), ...) {
  if (length(additional_state_text) > 0) {
    if (is.null(names(additional_state_text))) stop("additional_state_text needs to be a named list")
    if (any(names(additional_state_text) %in% names(state_text))) {
      stop(
        "additional_state_text entries cannot overwrite base states:\n ",
        "create new state name"
      )
    }
    state_text <- append(state_text, additional_state_text)
  }

  if (!continuous) {
    if (!missing(state_text)) {
      stop("not currently allowed to modify state_text for categorical covariates.
consider using the default with state \"linear\" or use additional_state_text")
    }

    if (!(2 %in% state | "linear" %in% state)) {
      stop("categorical covariates can only be used with state 2 (or \"linear\")")
    }

    ## modify state_text for for categorical
    unique_vals <- table(data[[cov]]) %>% sort(decreasing = TRUE)
    unique_vals <- names(unique_vals)

    two_text <- sapply(seq_along(unique_vals), function(i) {
      val <- unique_vals[i]
      def_text <- paste0("IF(COV.EQ.", val, ") PARCOV = ")
      if (i == 1) {
        def_text <- paste0(def_text, 1)
      } else {
        def_text <- paste0(def_text, "( 1 + THETA(", i - 1, "))")
      }
    })

    state_text$"2" <- two_text
    state_text$"linear" <- two_text
  }

  dstate_text <- data.frame(state = names(state_text))
  dstate_text$text <- state_text

  par_cov_text <- dstate_text$text[dstate_text$state %in% state]
  par_cov_text <- par_cov_text[[1]]
  par_cov_text <- gsub("PAR", param, par_cov_text)
  par_cov_text <- gsub("COV", cov, par_cov_text)

  if (any(grepl("log\\(median\\)", par_cov_text))) {
    ## get data
    data_temp <- tapply(data[[cov]], data$ID, stats::median, na.rm = TRUE)
    value <- signif(stats::median(log(data_temp), na.rm = TRUE), 3)
    if (value > 0) {
      par_cov_text <- gsub("log\\(median\\)", value, par_cov_text)
    } else {
      par_cov_text <- gsub("-\\s*log\\(median\\)", paste0("+ ", -value), par_cov_text)
    }
  }

  if (any(grepl("median", par_cov_text))) {
    ## get data
    data_temp <- tapply(data[[cov]], data$ID, stats::median, na.rm = TRUE)
    value <- signif(stats::median(data_temp, na.rm = TRUE), 3)
    if (value > 0) {
      par_cov_text <- gsub("median", value, par_cov_text)
    } else {
      par_cov_text <- gsub("-\\s*median", paste0("+ ", -value), par_cov_text)
    }
  }

  ## renumber thetas
  n <- 1
  n_replace <- theta_n_start
  par_cov_text <- gsub(
    "THETA\\(([0-9]+)\\)",
    "THETA\\(X\\1\\)",
    par_cov_text
  )

  while (TRUE) {
    if (!any(grepl(paste0("THETA\\(X", n, "\\)"), par_cov_text))) break
    par_cov_text <- gsub(
      paste0("(THETA\\()X", n, "(\\))"),
      paste0("\\1", n_replace, "\\2"),
      par_cov_text
    )
    n <- n + 1
    n_replace <- n_replace + 1
  }
  if (n - 1 > 30) warning("You're adding ", n - 1, " parameters. Are you insane?", call. = FALSE)
  attributes(par_cov_text) <- list(n = n - 1)
  par_cov_text
}
