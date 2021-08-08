#' Make decision point
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Formalise process of decision making.  Creates a decision point in the
#' workflow where subsequent parts of your workflow depend on this decision,
#' e.g. if you compare a 1 compartment and 2 compartment and decide based on the
#' OFV and goodness of fit plots that the 1 compartment model is better and
#' subsequent steps will build off of this, it is worth putting a decision point
#' in your code so that if you are to rerun the workflow with a new/updated
#' dataset, the decision can be revisited prior to moving onto the parts of the
#' workflow that depend on the 1 compartment decision.  The function requests
#' inputs (`values` and `files`) that you base a decision on and stop for users
#' to remake decision if inputs change.
#'
#' @param inputs Optional non file names upon which decision depends.
#' @param file_inputs Optional file names upon which decision depends.
#' @param auto Optional logical. logical statement for automatic decisions.
#' @param outcome Character. Description of the decision outcome.
#' @param force Logical (default = `FALSE`). Force a stop in the workflow so
#'   decision has been remade.
#'
#' @details There are two ways to use `decision`:
#'
#' \describe{
#' \item{Automatic:}{
#' An `auto` decision (see examples below) works
#' like `stopifnot()`. It requires a logical (TRUE/FALSE) condition. Doing this
#' this way ensures that creates fewer points in your workflow where at the cost
#' of removing.  If updating a workflow (e.g. with an updated dataset), so long
#' as the TRUE/FALSE is TRUE, the workflow will proceed uninterrupted.  If the
#' condition flips to FALSE the workflow will stop as it will be assumed that
#' subsequent steps will no longer be valid.
#' }
#' \item{Manual:}{ Manual: Requires
#' specification of either `input` or `file_inputs` (or both) AND `outcome`.
#' Inputs represent information you have considered in your decision and
#' `outcome` is a text description of the resulting decision.  The assumption
#' made is that if inputs have not changed since the last decision was made. }
#'
#' }
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' 
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv"))
#'              
#' m2 <- m1 %>% child("m2")
#' m2WT <- m1 %>% child("m2WT")
#'
#' \dontrun{
#'
#' if(interactive()){
#' 
#' ## a decision based on summary statistics
#' decision(
#'   inputs = summary_wide(c(m1, m2, m2WT)),
#'   outcome = "m1 is better"
#' ) # next line must be end of chunk
#'
#' ## a decision based also on goodness of fit plots
#' decision(
#'   inputs = summary_wide(c(m1, m2, m2WT)),
#'   file_inputs = c(
#'     "Results/basic_gof.m1.nb.html",
#'     "Results/basic_gof.m2.nb.html"
#'   ),
#'   outcome = "m1 is better"
#' ) # next line must be end of chunk
#'
#' ## a decision based on an automatic TRUE/FALSE criteria
#' ## here we're ensuring m1 has the lowest AIC
#' decision(auto = (AIC(m1) == min(AIC(m1, m2, m3)))) 
#' 
#' }
#' }
#'
#' @export
decision <- function(inputs = c(),
                     file_inputs = c(),
                     auto = logical(),
                     outcome = character(),
                     force = FALSE) {
  if (!requireNamespace("digest")) {
    stop("install digest")
  }

  error_msg <- "decision needs revisiting."

  if ((!missing(inputs) | !missing(file_inputs)) & missing(outcome)) {
    stop("if specifying decision inputs need to specify outcome \n'outcome' = character description of decision")
  }

  if (!missing(auto)) {
    if (!auto) {
      stop("auto decision FAILED - ", error_msg, call. = FALSE)
    } else {
      message("auto decision PASSED")
      if(length(inputs) + length(file_inputs) == 0) return(invisible())
    }
  }

  wait_input <- function(inputs) {
    if (!interactive()) stop("new manual decision needed. Run interactively")
    inputs ## create inputs dependency
    cat(crayon::underline("\nmanual decision check\n"))
    cat("expected decision outcome:\n", outcome, "\n\n")
    
    ans <- usethis::ui_yeah("Do you agree with this decision?", 
                            no = c("No", "Not sure need to check manually..."))
    
    if (ans) {
      return(TRUE)
    } else {
      stop("stopping further execution", call. = FALSE)
    }
    
  }

  if (!length(inputs)) inputs <- c()

  if (length(file_inputs)) {
    if (!all(file.exists(file_inputs))) {
      stop("file(s) do not exist", call. = FALSE)
    }
    inputs <- c(inputs, tools::md5sum(file_inputs))
  }

  ## generate hashes for current call
  call_ob <- match.call()
  decision_cache_path <- file.path(nm_dir("models"), "decision_cache")
  dir.create(decision_cache_path, recursive = TRUE, showWarnings = FALSE)
  cache_name <- paste0(digest::digest(call_ob), ".RDS")
  decision_info <- list(inputs = inputs)
  cache_path <- file.path(decision_cache_path, cache_name)

  ############
  ## check cache
  if (!force) {
    cache_match <- file.exists(cache_path) ## and contents match
    if (cache_match) {
      stored_decision <- readRDS(cache_path)
      cache_match <- identical(stored_decision, decision_info)
    }
  } else {
    cache_match <- FALSE
  }
  ############
  if (!cache_match) {
    decision_accurate <- wait_input(inputs)

    if (decision_accurate) {
      ## save cache with a record of the decision
      saveRDS(decision_info, cache_path)
    } else {
      ## delete cache of inaccurate decisions
      unlink(cache_path, force = TRUE)
    }
  } else {
    message("\ndecision inputs & outcome match prior decision")
  }
}
