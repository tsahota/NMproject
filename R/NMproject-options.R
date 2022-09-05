set_nm_opts <- function() {
  ## Internal function: will set all global variables
  ## put as much AZ specific code in here.
  ## add internal code library to code library

  if (is.null(getOption("system_cmd"))) {
    options(system_cmd = function(cmd, ...) {
      if (.Platform$OS.type == "windows") shell(cmd, ...) else system(cmd, ...)
    })
  }
  if (is.null(getOption("system_nm"))) options(system_nm = function(cmd, ...) system_nm_default(cmd, ...))
  if (is.null(getOption("quiet_run"))) options(quiet_run = TRUE)
  if (is.null(getOption("intern"))) options(intern = FALSE)
  
  if (is.null(getOption("job_time_spacing"))) options(job_time_spacing = 0)

  if (is.null(getOption("available_nm_types"))) {
    options(available_nm_types = c(
      "SIZES", "PROB", "INPUT", "DATA", "SUB", "MODEL", "PK", "DES", "PRED", "ERROR",
      "THETA", "OMEGA", "SIGMA", "EST", "SIM", "COV", "TABLE"
    ))
  }

  if (is.null(getOption("nm_default_dirs"))) {
    options(nm_default_dirs = list(
      scripts = "Scripts",
      models = "Models",
      results = "Results",
      source_data = "SourceData",
      derived_data = "DerivedData"
    ))
  }
  
  if (is.null(getOption("nm_pre_commit_hook"))) options(nm_pre_commit_hook = NULL)
  if (is.null(getOption("nm_pre_push_hook"))) options(nm_pre_commit_hook = NULL)
  
  if (is.null(getOption("kill_job"))) options(kill_job = identity)

  if (is.null(getOption("nm.overwrite_behaviour"))) options(nm.overwrite_behaviour = "ask")
  if (is.null(getOption("nm.force_render"))) options(nm.force_render = FALSE)

  if (is.null(getOption("nm_default_fields"))) {
    options(nm_default_fields = list(
      cmd = "execute {ctl_name} -dir={run_dir}"
    ))
  }
  
  if (is.null(getOption("nmtran_exe_path"))) options(nmtran_exe_path = find_nm_tran_path(warn = FALSE))

  if (is.null(getOption("code_library_path"))) {
    options(code_library_path = system.file("extdata", "CodeLibrary", package = "NMproject"))
  }

  options("nm.options" = c(
    "system_cmd", "system_nm", "quiet_run", "intern", "available_nm_types",
    "nm_default_dirs", "kill_job", "nm.overwrite_behaviour",
    "nm.force_render", "nm_default_fields", "nmtran_exe_path",
    "nm_pre_commit_hook", "nm_pre_push_hook",
    "code_library_path", "job_time_spacing"
  ))
}


#' Setup analysis subdirectories
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This allows organisations/individuals with their own directory to customize
#' their directory structure
#'
#' @param dir_list Optional named list or vector. Names `"scripts"` and
#'   `"models"` must be present.  The rest can be unnamed.
#'
#' @return if `dir_list` is missing, will return value of
#' `getOption("nm_default_dirs")` otherwise will set option `nm_default_dirs`.
#' @examples
#'
#' orig_list <- nm_default_dirs()
#' orig_list
#' 
#' nm_default_dirs(list(
#'   models = "Models",
#'   scripts = "Scripts",
#'   results = "Results",
#'   source_data = "SourceData",
#'   derived_data = "Data"
#' ))
#' 
#' nm_default_dirs()
#' nm_default_dirs(orig_list)
#' 
#' 
#' 
#' @export
nm_default_dirs <- function(dir_list) {
  if (missing(dir_list)) {
    return(getOption("nm_default_dirs"))
  }
  if (!missing(dir_list)) {
    ## now assume we're setting
    options(nm_default_dirs = dir_list)
  }
}

.nm_dir_descriptions <- list(
  scripts = "Directory for R and Rmd scripts",
  models = "Directory for NONMEM runs and other 3rd party software",
  results = "Default directory for results including tables, pdfs, Rmd reports, ...",
  source_data = "Directory for unmodified source data",
  derived_data = "Directory for derived analysis-ready datasets"
)

#' Get a directory name
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Get subdirectory (relative) paths in a configuration independent way.  The
#' configuration can be modified with [nm_default_dirs()].  Can be useful in
#' scripts, where you need to refer to locations of model files or output files.
#'
#' @param name Character. Directory type.  Should be either `"scripts"`,
#'   `"models"` or `"results"`.
#' @param ... Deprecated.
#'
#' @seealso [nm_default_dirs()]
#'
#' @examples
#' nm_dir("scripts") ## will return the path to the "scripts" directory
#' nm_dir("models")
#' nm_dir("results")
#' @export

nm_dir <- function(name, ...) {
  if (missing(name)) stop("need argument")
  nm_default_dirs()[[name]]
}

set_default_dirs_in_rprofile <- function(path = ".Rprofile", dir_list = nm_default_dirs()) {

  if (!file.exists(path)) file.create(path)

  start_flag <- "# nm_default_dir modify - start"
  end_flag <- "# nm_default_dir modify - end"

  current_lines <- readLines(path)

  start_flag_hit <- grep(start_flag, current_lines)
  end_flag_hit <- grep(end_flag, current_lines)

  error_msg <- "something wrong with .Rprofile, remove previous nm_default_dir lines and try again"

  if (!length(start_flag_hit) %in% c(0, 1)) stop(error_msg, call. = FALSE)
  if (!length(end_flag_hit) %in% c(0, 1)) stop(error_msg, call. = FALSE)
  total_hits <- length(start_flag_hit) + length(start_flag_hit)
  if (!total_hits %in% c(0, 2)) stop(error_msg, call. = FALSE)
  ## hits validated

  if (total_hits == 2) { ## existing statement detected
    ## remove lines
    remove_lines <- start_flag_hit:(end_flag_hit + 1)
    current_lines <- current_lines[!seq_along(current_lines) %in% remove_lines]
    writeLines(current_lines, path)
    return(set_default_dirs_in_rprofile(path = path, dir_list = dir_list))
  }
  # now can just set

  txt <- paste0("options(nm_default_dirs=", paste(deparse(dir_list), collapse = ""), ")")
  txt <- gsub("\\s+", " ", txt)
  txt <- c(start_flag, txt, end_flag, "", current_lines)
  writeLines(txt, path)
  usethis::ui_done("setting {usethis::ui_path('nm_default_dirs')} in {usethis::ui_path('.Rprofile')}")
}


#' Setup default nm object fields
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This allows organisations/individuals with their own nm object field
#' preferences to set these.
#'
#' @param field_list Optional named list or vector. Names correspond to function
#'   names and object fields, values correspond to what will be set.
#'
#' @return if `field_list` is missing, will return value of
#' `getOption("nm_default_fields")` otherwise will set option `nm_default_fields`.
#' @examples
#'
#' nm_default_fields()
#' nm_default_fields(list(
#'   cmd = "execute {ctl_name} -dir={run_dir}"
#' ))
#' nm_default_fields()
#' 
#' @export

nm_default_fields <- function(field_list) {
  if (missing(field_list)) {
    getOption("nm_default_fields")
  } else {
    options(nm_default_fields = field_list)
  }
}

#' Setup default job_time_spacing option
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This allows organisations/individuals with their own job time spacing, used by [run_nm()] when `threads > 1`.
#'
#' @param seconds Optional numeric value. Values correspond to what will be set.
#'
#' @return if `seconds` is missing, will return value of
#' `getOption("job_time_spacing")` otherwise will set option `job_time_spacing`.
#' @examples
#'
#' job_time_spacing()
#' job_time_spacing(1)
#'
#' @export

job_time_spacing <- function(seconds) {
  if (missing(seconds)) {
    getOption("job_time_spacing")
  } else {
    options(job_time_spacing = seconds)
  }
}

#' @rdname sge_parallel_execute
#' 
#' @title Generic execute command for SGE grids
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Character to be used with the [cmd()] function to launch a parallelised job
#' on SGE.
#'
#' @details Requires `cores` and `parafile` fields to be set.
#' 
#' @return A `character` object.
#' 
#' @seealso [nm_getsetters()].
#'
#' @examples
#'
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv")) %>%
#'   cmd(sge_parallel_execute) %>%
#'   parafile("/opt/NONMEM/nm75/run/mpilinux8.pnm") %>%
#'   cores(8)
#'   
#' cmd(m1)
#'
#' m2 <- m1 %>% child("m2") ## inherits same command as above
#'
#' sge_parallel_execute ## view the character to see how psn interfaces with SGE
#' @export
sge_parallel_execute <- "execute -run_on_sge -parafile={parafile} -sge_prepend_flags='-pe orte {cores} -V' {ctl_name} -dir={run_dir} -nodes={cores}"

#' @rdname sge_parallel_execute
#' @details `sge_parallel_execute2` doubles the amount slots taken by the job
#'   (e.g. to avoid hyperthreading).
sge_parallel_execute2 <- "execute -run_on_sge -parafile={parafile} -sge_prepend_flags='-pe orte {2*cores} -V' {ctl_name} -dir={run_dir} -nodes={cores}"

#' @rdname sge_parallel_execute
#' @details `sge_parallel_execute_batch` eliminates pre-processing on master.
#'   Job submitted from compute node.  This job will occupy one slot for the
#'   duration of the NONMEM run.
#'   node.  Job pre-processed and submitted from a compute node
sge_parallel_execute_batch <- "echo \"execute -run_on_sge -parafile={parafile} -sge_prepend_flags='-pe orte {cores} -V' {ctl_name} -dir={run_dir} -nodes={cores}\" | qsub -cwd -V"

#' Generic execute command for parallelised runs
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Character to be used with the [cmd()] function to launch a parallelised run.
#'
#' @details Requires `cores` and `parafile` fields to be set.
#' 
#' @return A `character` object.
#' 
#' @seealso [nm_getsetters()].
#'
#' @examples
#'
#' # create example object m1 from package demo files
#' exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
#' m1 <- new_nm(run_id = "m1", 
#'              based_on = file.path(exdir, "Models", "ADVAN2.mod"),
#'              data_path = file.path(exdir, "SourceData", "THEOPP.csv")) %>%
#'   cmd(parallel_execute) %>%
#'   parafile("/opt/NONMEM/nm75/run/mpilinux8.pnm") %>%
#'   cores(8)
#'   
#' cmd(m1)
#'
#' m2 <- m1 %>% child("m2") ## inherits same command as above
#'
#' parallel_execute ## view the character to see how psn interfaces with SGE
#' @export
parallel_execute <- "execute -parafile={parafile} {ctl_name} -dir={run_dir} -nodes={cores}"

#' Get/set nm_tran_command
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' The function [nm_tran()] needs the location of `NMTRAN.exe` to function.
#' This is guessed at package load, assuming PsN is on the $PATH environmental
#' variable. If this is not the case, then you can manually set the path and
#' command used.
#'
#' @param text Optional character. If specified will set `nm_tran_command`
#'   otherwise it will display the current option value.
#'
#' @details `text` can just be the path to `NMTRAN.exe` in which case
#'   `nm_tran_command` will use the format `/path/to/NMTRAN.exe < {ctl_name}` to
#'   launch `NMTRAN.exe` where `{ctl_name}` is the name of the control file.
#'   Specifying
#'
#'   `nm_tran_command("/path/to/NMTRAN.exe < {ctl_name}")` is equivalent to:
#'   `nm_tran_command("/path/to/NMTRAN.exe")`
#'
#'   More complicated formats are possible with different installations which
#'   can be seen examples.
#'
#'   As with all NMproject configuration options set this up either at the
#'   beginning of your script, in your `.Rprofile` or for all users in
#'   `Rprofile.site`.  See FAQ for setting up configuration options permanently.
#'
#' @return If `text` is missing will get and return the current NMTRAN command.
#' @seealso [find_nm_tran_path()], [nm_tran()]
#' @examples
#'
#' orig_cmd <- nm_tran_command()
#' orig_cmd
#'
#' # the following two are equivalent
#' nm_tran_command("/opt/NONMEM/nm75/tr/NMTRAN.exe")
#' nm_tran_command()
#' 
#' nm_tran_command("/opt/NONMEM/nm75/tr/NMTRAN.exe < {ctl_name}")
#' nm_tran_command()
#' 
#' nm_tran_command(orig_cmd)
#' 
#' @export
nm_tran_command <- function(text) {
  if (missing(text)) {
    if (!is.null(getOption("nmtran_command"))) {
      return(getOption("nmtran_command"))
    }
    if (!is.null(getOption("nmtran_exe_path"))) {
      return(paste(getOption("nmtran_exe_path"), "< {ctl_name}"))
    }
    return(NULL)
  }
  ## set text
  options(nmtran_command = text)
  invisible()
}

#' Default system_nm function
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Not intended to be used directly in most cases.  This is called by
#' [system_nm()]
#'
#' @param cmd Character. System call to be sent to the terminal.
#' @param ... Additional arguments to be passed to `system()` or `shell()`.
#'
#' @return The return value of [system()] or [shell()] depending on the OS type.
#'
#' @seealso [system_nm()], [run_nm()]
#'
#' @keywords internal
#' @export

system_nm_default <- function(cmd, ...) {
  if (.Platform$OS.type == "windows") {
    local_env_vars <- Sys.getenv()
    stdout_unit_vars <- local_env_vars[grepl("STDOUT_UNIT|STDERR_UNIT", names(local_env_vars))]
    for (i in seq_along(stdout_unit_vars)) {
      Sys.unsetenv(names(stdout_unit_vars)[i])
    }
    on.exit({
      if (length(stdout_unit_vars) > 0) {
        do.call(Sys.setenv, as.list(stdout_unit_vars))
      }
    })
    args <- list(...)
    if (!"wait" %in% names(args)) wait <- FALSE else wait <- args$wait
    if (wait == FALSE) {
      shell(paste("START CMD /C", cmd), ...)
    } else {
      shell(cmd, ...)
    }
  } else {
    system(cmd, ...)
  }
}


#' System command for NONMEM execution
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Not intended to be used directly in most cases.  This is the function used by
#' `run_nm()`.  It can also be used directly to launch other PsN commands like
#' `sumo`.
#'
#' @param cmd Character. System call to be sent to the terminal.
#' @param dir Character. Directory (relative path) to run command in.  By
#'   default this will be the "models" directory (`nm_dir("models")`).
#' @param ... Additional arguments to be passed to `system()` or `shell()`.
#' 
#' @return The return value of `getOption("system_nm")`.
#' 
#' @seealso [run_nm()]
#'
#' @examples
#'
#' system_nm("hostname")
#' 
#' ## requires NONMEM to be installed
#' \dontrun{
#'
#' system_nm("psn --versions")
#' system_nm("sumo run1.mod")
#' }
#'
#' @export
system_nm <- function(cmd, dir = nm_dir("models"), ...) {
  if (is.null(dir) | !file.exists(dir)) dir <- "."
  if (file.exists(dir)) {
    currentwd <- getwd()
    setwd(dir)
    on.exit(setwd(currentwd))
  } else {
    stop(paste0("Directory \"", dir, "\" doesn't exist."))
  }
  getOption("system_nm")(cmd, ...)
}

#' Diagnostic test for system_nm 
#' 
#' Used for diagnostic purposes.  Will print shell outputs to screen.
#' 
#' @param ... Arguments passed to `system_nm()`.
#' @param intern Arguments passed to `system_nm()`.
#' @param ignore.stderr Arguments passed to `system_nm()`.
#' @param wait Arguments passed to `system_nm()`.
#' 
#' @keywords internal
#' 
#' @export

system_nm_intern <- function(..., intern = TRUE, ignore.stderr = TRUE, wait = TRUE) {
  system_nm(..., intern = intern, ignore.stderr = ignore.stderr, wait = wait)
}

#' System/shell command wrapper
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Will run `getOption("system_cmd")`.  A OS agnostic interface to the system
#' terminal.  Most of the time this will be the same as `system_nm` except when
#' the PsN/NONMEM execution server is location in a different location to the
#' RStudio server.
#'
#' @param cmd Character. Command to send to shell.
#' @param dir Optional character. Directory to run command in (default = current
#'   working directory)
#' @param ... Other arguments passed to system command.
#'
#' @return The return value of `getOption("system_cmd")`.
#'
#' @examples
#'
#' system_cmd("pwd")
#'
#' @export
system_cmd <- function(cmd, dir = ".", ...) {
  if (!dir %in% ".") {
    if (file.exists(dir)) {
      currentwd <- getwd()
      setwd(dir)
      on.exit(setwd(currentwd))
    } else {
      stop(paste0("Directory \"", dir, "\" doesn't exist."))
    }
  }
  getOption("system_cmd")(cmd, ...)
}

#' Overwrite behaviour of NMproject
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' This is best used via the "overwrite behaviour" RStudio 'Addin'.  Sets the
#' strategy for how to handle overwriting of previously executed control files.
#'
#' @param txt Character either `"run"`, `"stop"`, or `"skip"`.
#'
#' @return if `txt` is missing returns `getOption(\"nm.overwrite_behaviour\")`
#'   otherwise returns no value and is called for side effects (setting the
#'   `nm.overwrite_behaviour` option).
#'
#' @export
overwrite_behaviour <- function(txt = c(
                                  "ask",
                                  "overwrite",
                                  "stop",
                                  "skip"
                                )) {
  if (missing(txt)) {
    return(getOption("nm.overwrite_behaviour"))
  }
  txt <- match.arg(txt)
  options(nm.overwrite_behaviour = txt)

  # if(txt %in% "ask") message("ask: NMproject will ask before overwriting old files")
  # if(txt %in% "overwrite") message("overwrite: NMproject will overwrite previous runs without prompt")
  # if(txt %in% "stop") message("stop: NMproject will stop R execution with an error instead of running NONMEM")
  # if(txt %in% "skip") message("skip: NMproject will not run NONMEM")
  return(invisible())
}

#' @export
.overwrite_behaviour <- dplyr::tibble(
  txt = c("ask", "overwrite", "stop", "skip"),
  description = c(
    "ask before overwrite (default)",
    "overwrite all",
    "no overwriting (stop with error)",
    "no overwriting (skip, no error)"
  )
)
