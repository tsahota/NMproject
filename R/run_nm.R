#' @name run_nm
#' @rdname run_nm
#'
#' @title Run NONMEM jobs
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Run nm objects.  Uses `system_nm()` to submit the `cmd()` value of object.
#'
#' @param m An nm object.
#' @param ignore.stdout Logical (default=`TRUE`). Parameter passed to `system_nm()`.
#' @param ignore.stderr Logical (default=`TRUE`). Parameter passed to `system_nm()`.
#' @param quiet Logical (default=`FALSE`). Should `system_nm()` output be piped to
#'   screen?
#' @param intern Logical. `intern` argument to be passed to `system_nm()`.
#' @param force Logical (default = `FALSE`).  Force run even results unchanged.
#' @param cache_ignore_cmd Logical (default = `FALSE`). Should check `cmd` field
#'   with cache?
#' @param cache_ignore_ctl Logical (default = `FALSE`). Should check control file
#'   contents with cache?
#' @param cache_ignore_data Logical (default = `FALSE`). Should check dataset with
#'   cache?
#'
#' @details In grid environment it is recommended to run [nm_tran()] via the
#'   RStudio 'Addin' prior to executing this code.
#'
#'   By default, when highlighting code and evaluating it via an RStudio app,
#'   `run_nm()` will not execute and will just return the `nm` object.
#'
#'   For vector `nm` objects of length more than 1, all runs will be launched at
#'   the same time.  This could overwhelm resources if not in a grid
#'   environment.  In this case see [run_nm_batch()] for batched execution of a
#'   vector valued `nm` object.
#'
#' @return `m` with `job_info` fields populated.
#'
#' @seealso [nm_tran()]
#'
#' @examples
#' \dontrun{
#' m1 <- new_nm(
#'   run_id = "m1",
#'   based_on = "staging/Models/ADVAN2.mod",
#'   data_path = "DerivedData/data.csv"
#' ) %>%
#'   cmd("execute {ctl_name} -dir={run_dir}") %>%
#'   fill_input() %>%
#'   run_nm()
#' }
#' @export
run_nm <- function(m,
                   ignore.stdout = TRUE, ignore.stderr = TRUE,
                   quiet = getOption("quiet_run"), intern = getOption("intern"),
                   force = FALSE,
                   cache_ignore_cmd = FALSE, cache_ignore_ctl = FALSE, cache_ignore_data = FALSE) {
  UseMethod("run_nm")
}

#' @export
run_nm.nm_generic <- function(m,
                              ignore.stdout = TRUE, ignore.stderr = TRUE,
                              quiet = getOption("quiet_run"), intern = getOption("intern"),
                              force = FALSE,
                              cache_ignore_cmd = FALSE, cache_ignore_ctl = FALSE, cache_ignore_data = FALSE) {
  if (is.na(m)) {
    return(m)
  }

  ## write control stream
  ctl <- ctl_contents(m)
  if (length(ctl) == 1) {
    if (is.na(ctl)) {
      warning("no ctl_contents defined. Use ?based_on")
      return(m)
    }
  }

  m %>% write_ctl()

  ## caching
  if (!force) {
    ## pull existing checksum info
    # run_cache_disk <- lapply(run_cache_paths(m), readRDS)
    if (length(run_cache_paths(m)) > 0) {
      run_cache_disk <- readRDS(run_cache_paths(m))
      ## get current checksum
      current_checksums <- run_checksums(m)
      ## determine matches

      if (cache_ignore_cmd) { ## remove cmd check
        keep <- !names(current_checksums) %in% "cmd"
        run_cache_disk$checksums <- run_cache_disk$checksums[keep]
        current_checksums <- current_checksums[keep]
      }

      if (cache_ignore_ctl) { ## remove cmd check
        keep <- !names(current_checksums) %in% "ctl"
        run_cache_disk$checksums <- run_cache_disk$checksums[keep]
        current_checksums <- current_checksums[keep]
      }

      if (cache_ignore_data) { ## remove cmd check
        keep <- !names(current_checksums) %in% "data"
        run_cache_disk$checksums <- run_cache_disk$checksums[keep]
        current_checksums <- current_checksums[keep]
      }

      ## ignore names
      names(current_checksums) <- NULL
      names(run_cache_disk$checksums) <- NULL

      matched <- identical(run_cache_disk$checksums, current_checksums)
      if (matched) {
        message("rebuilding run from cache... use run_nm(force = TRUE) to override")
        ## update object and return
        m <- m %>% executed(TRUE)
        m <- m %>% job_info(run_cache_disk$job_info)
        m <- m %>% save_run_cache()
        return(invisible(m)) ## if up to date, skip
      }
    }
  }

  ## NONMEM will run from this point on
  ## check overwrite_behaviour() behaviour
  behaviour <- overwrite_behaviour()
  # if("stop" %in% behaviour) stop("No new NONMEM runs allowed. Stopping... \n change behaviour with overwrite_behaviour()", call. = FALSE)
  if ("skip" %in% behaviour) {
    message("skipping step as it would require overwriting \n change behaviour with overwrite_behaviour()")
    return(invisible(m))
  }

  wipe_run(m) ## this will check for "ask" or "overwrite"
  kill_job(m)

  message(paste0("Running: ", type(m), ":", ctl_path(m)))
  stdout0 <- system_nm(
    cmd = cmd(m), dir = run_in(m), wait = FALSE,
    ignore.stdout = FALSE, ignore.stderr = FALSE,
    intern = intern
  )

  if (intern) {
    cat(stdout0, sep = "\n")
    job_info <- getOption("get_job_info")(stdout0)
    if (is.null(job_info)) job_info <- NA
  } else {
    job_info <- NA
  }

  m <- m %>% executed(TRUE)
  m <- m %>% job_info(job_info)

  ## The object is now ready to be saved in cache
  m <- m %>% save_run_cache()

  invisible(m)
}

#' @export
run_nm.nm_list <- Vectorize_nm_list(run_nm.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

#' @rdname run_nm
#' @param threads Numeric.  Number of threads to run concurrently.
#' @param ... Additional arguments passed to `run_nm()`.
#'
#' @details `run_nm_batch` is a variant of `run_nm()` containing a `threads` argument that will submit [run_nm()]'s in
#' batches and wait for them to complete. If you need all the runs to complete ensure you use a
#'   [wait_finish()] statement afterwards as R console will only be
#'   blocked for until the last batch has been submitted which will be before
#'   all runs have completed
#' @export

run_nm_batch <- function(m, threads = 10, ...) {
  runs_remaining <- seq_along(m)
  while (length(runs_remaining) > 0) {
    n_to_take <- min(threads, length(runs_remaining))
    runs_to_run <- runs_remaining[seq_len(n_to_take)]
    m_sub <- m[runs_to_run]
    run_nm(m_sub, ...)
    runs_remaining <- setdiff(runs_remaining, runs_to_run)
    if (length(runs_remaining) > 0) wait_finish(m_sub)
  }
  m
}

#' Wipe previous run files
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Will remove all the output files generated by a previously completed run.
#' This is run by [run_nm()] prior to launching any jobs to ensure that output
#' files from old runs do not get mistaken for up-to-date runs.
#'
#' @param r An nm object.
#' 
#' @return No return value, called for side effects.
#' 
#' @export
wipe_run <- function(r) {
  UseMethod("wipe_run")
}

#' @export
wipe_run.nm_generic <- function(r) {
  ## assumes ctrl file is run[run_id].mod and -dir=[run_id] was used

  ## get and remove all ctl output files

  ## files are in run_in(r)
  ## assume same stub as input.

  psn_exported_files <- psn_exported_files(r)

  # ## do not include scm or mod
  # output_files <- paste0(tools::file_path_sans_ext(ctl_path(r)),
  #       c(".phi", ".ext", ".cov", ".coi", ".cor", ".lst"))

  ## and output - in case it's different
  lst_path <- file.path(run_in(r), lst_path(r))

  # ## need to get table files too
  # ctl_table_files <- file.path(run_in(r), ctl_table_files(r))

  ## run_dir
  run_dir_to_delete <- file.path(run_in(r), run_dir(r))
  if (!file.exists(run_dir_to_delete)) {
    run_dir_to_delete <- c()
  } else {
    ## can now assume directory exists
    ## make sure it's not the same directory the ctl file is in
    if (run_dir_to_delete %in% c(".", ".\\")) {
      run_dir_to_delete <- c()
    } else {
      if (normalizePath(run_dir_to_delete) == normalizePath(run_in(r))) run_dir_to_delete <- c()
    }
  }

  # ctl_out_files <- c(lst_path, output_files, ctl_table_files, run_dir_to_delete)
  ctl_out_files <- c(lst_path, psn_exported_files, run_dir_to_delete)

  ## before deleting files, check
  existing_ctl_out_files <- ctl_out_files[file.exists(ctl_out_files)]
  behaviour <- overwrite_behaviour()
  if ("stop" %in% behaviour & length(existing_ctl_out_files) > 0) {
    stop(
      "no overwriting allowed, stopping due to following files/directories:\n ",
      paste(paste(existing_ctl_out_files, collapse = "\n "))
    )
  }
  prompt_overwrite(rev(existing_ctl_out_files))

  unlink(ctl_out_files, recursive = TRUE, force = TRUE)

  invisible()
}

#' @export
wipe_run.nm_list <- Vectorize_nm_list(wipe_run.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

psn_exported_files <- function(r, minimal = FALSE) {
  UseMethod("psn_exported_files")
}

psn_exported_files.nm_generic <- function(r, minimal = FALSE) {
  ## do not include scm or mod
  if (minimal) {
    output_files <- paste0(
      tools::file_path_sans_ext(ctl_path(r)),
      c(".ext")
    )
  } else {
    output_files <- paste0(
      tools::file_path_sans_ext(ctl_path(r)),
      c(".phi", ".ext", ".cov", ".coi", ".cor", ".lst")
    )
  }

  if (minimal) {
    ctl_out_files <- c(output_files)
  } else {
    exported_table_paths <- file.path(run_in(r), ctl_table_files(ctl_contents(r)))
    ctl_out_files <- c(output_files, exported_table_paths)
  }

  ctl_out_files
}

psn_exported_files.nm_list <- Vectorize_nm_list(psn_exported_files.nm_generic, SIMPLIFY = FALSE)


ctl_table_files <- function(ctl) {
  UseMethod("ctl_table_files")
}

ctl_table_files.default <- function(ctl) {
  ctl <- ctl_character(ctl)
  s0 <- rem_comment(ctl)
  s <- grep("FILE\\s*=\\s*(\\S+)", s0, value = TRUE)
  table_files <- gsub(".*FILE\\s*=\\s*(\\S+)\\s*.*$", "\\1", s)
  table_files
}

#' @rdname temp_files
#' @name temp_files
#'
#' @title Remove temporary NONMEM files
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' NONMEM produces a lot of temporary files which can add up to a lot of disk
#' space.  One strategy to remove this is to use the `clean` option in the PsN
#' command.  However, this can automatically remove files as soon as the run
#' finishes that may be useful for debugging.  `ls_tempfiles()`` allows you to
#' list the paths of all temporary files, for a single run or for all runs for
#' inspection and deletion. `clean_run()` is a wrapper function that runs
#' `ls_tempfiles()` and deletes everything returned.  For safety is limited to
#' only deleting files associated with `nm` objects though.
#'
#' @param object Either an nm object or path to project (default = `"."`).  If a
#'   path is specified, the function will look for all runs in the directory
#'   (including subdirectories).
#' @param output_loc Optional character for locating files. Either `"run_dir"`
#'   (default) for PsN execution or `"base"` for "nmfe" execution.
#' @param run_files Optional character vector.  Search amongst only these files
#'   instead.  Default value `NA` searches based on `object`.
#' @param include_slurm_files Logical (default = `TRUE`). Include files
#'   generated by Slurm.
#' @param ctl_extension Character. Extension of control file (default = `"mod"`)
#' @param include_psn_exports Logical (default = `FALSE`). Considers files that
#'   PsN exports to the `run_in` directory as temporary
#'
#' @details Setting `include_psn_exports = TRUE` will break 'Pirana' and 'xpose'
#'   capability as these software use exported files.
#'
#' @return A `character` vector of temporary file paths
#'
#' @examples
#'
#' \dontrun{
#'
#' ls_tempfiles(m1) ## display all temp files associated with m1
#'
#' m1 %>%
#'   ls_tempfiles() %>%
#'   unlink() ## delete all m1 temp files
#'
#' ## above line is equivalent to:
#' clean_run(m1)
#'
#' ls_tempfiles() ## display all temp files in analysis project
#'
#' ls_tempfiles() %>% unlink() ## remove all temp files in analysis project
#' }
#'
#' @export
ls_tempfiles <- function(object = ".", output_loc = c("run_dir", "base"),
                         run_files = NA_character_, include_slurm_files = TRUE,
                         ctl_extension = "mod",
                         include_psn_exports = FALSE) {
  UseMethod("ls_tempfiles")
}

#' @export
ls_tempfiles.default <- function(object = ".", output_loc = c("run_dir", "base"),
                                 run_files = NA_character_, include_slurm_files = TRUE,
                                 ctl_extension = "mod",
                                 include_psn_exports = FALSE) {
  output_loc <- match.arg(output_loc)

  ## get all_run_files (in NM_run1 dir)
  if (identical(run_files, NA_character_)) {
    all_run_dirs <- list_dirs(
      object,
      pattern = "NM_run[0-9]+",
      recursive = TRUE, full.names = TRUE, maxdepth = Inf
    )

    all_run_files <- dir(all_run_dirs, full.names = TRUE)

    all_outside_run_dirs <- file.path(all_run_dirs, "..")
    all_outside_run_files <- dir(all_outside_run_dirs, full.names = TRUE)

    all_run_files <- c(all_run_files, all_outside_run_files)
  } else {
    all_run_files <- run_files
  }

  ## eliminate files we don't want

  temp_files <- c()
  non_temp_files <- c()

  all_psn.mod <- all_run_files[basename(all_run_files) == "psn.mod"]
  non_temp_files <- c(non_temp_files, all_psn.mod)

  all_run_dir_table_files <-
    lapply(all_psn.mod, function(psn.mod) {
      file.path(dirname(psn.mod), ctl_table_files(psn.mod))
    })
  all_run_dir_table_files <- unlist(all_run_dir_table_files)
  non_temp_files <- c(non_temp_files, all_run_dir_table_files)

  all_base_tables <- all_run_dir_table_files
  all_base_table_dir <- dirname(all_base_tables)
  all_base_table_dir <- file.path(all_base_table_dir, "..", "..")
  all_base_table_dir <- normalizePath(all_base_table_dir)

  if (output_loc == "run_dir") { ## add base tables to temp files
    all_base_tables <- file.path(all_base_table_dir, basename(all_run_dir_table_files))
    all_base_tables <- all_base_tables[file.exists(all_base_tables)]
    if (include_psn_exports) temp_files <- c(temp_files, all_base_tables)

    all_base_mod_files <- dir(unique(all_base_table_dir),
      pattern = paste0("\\.", ctl_extension, "$"),
      full.names = TRUE
    )

    all_base_stubs <- tools::file_path_sans_ext(all_base_mod_files)

    all_base_psn_files <- lapply(all_base_stubs, function(base_mod_stub) {
      base_dir <- dirname(base_mod_stub)
      stub <- basename(base_mod_stub)
      dir(base_dir, pattern = paste0("^", stub, "\\..*"), full.names = TRUE)
    })
    all_base_psn_files <- unlist(all_base_psn_files)

    all_base_psn_files <- all_base_psn_files[
      ## exclude mod and lst files
      !tools::file_ext(all_base_psn_files) %in% c("mod", "lst")
    ]
    if (include_psn_exports) temp_files <- c(temp_files, all_base_psn_files)
  }

  ## temp_dir is temp
  temp_files <- c(temp_files, all_run_files[grepl("temp_dir", all_run_files)])

  ## .o, .f90, Rmd, csv
  temp_files <- c(temp_files, all_run_files[tools::file_ext(basename(all_run_files)) %in% c("o", "f90", "Rmd", "csv")])

  ## specific name exclusions:
  temp_files <- c(
    temp_files,
    all_run_files[basename(all_run_files) %in%
      c(
        "INTER",
        "fort.2002",
        "model_NMrun_translation.txt",
        "modelfit.log",
        "raw_results_structure",
        "version_and_option_info.txt"
      )]
  )

  #####
  temp_files <- setdiff(temp_files, non_temp_files)

  ## expand the directories into files

  temp_dirs <- temp_files[file.info(temp_files)$isdir]

  temp_dir_files <- dir(temp_dirs, full.names = TRUE, recursive = TRUE)

  temp_files <- c(
    temp_files[!temp_files %in% temp_dirs],
    temp_dir_files
  )

  if (length(temp_files) == 0) {
    return(character())
  } ## if none return empty

  relative_path(temp_files, getwd())
}

#' @export
ls_tempfiles.nm_list <- function(object = ".", output_loc = c("run_dir", "base"),
                                 run_files = NA_character_, include_slurm_files = TRUE,
                                 ctl_extension = "mod",
                                 include_psn_exports = FALSE) {
  output_loc <- match.arg(output_loc)

  if (output_loc %in% "run_dir") {
    all_run_dirs <- list_dirs(
      run_dir_path(object),
      pattern = "NM_run[0-9]+",
      full.names = TRUE
    )
  } else {
    all_run_dirs <- list_dirs(
      run_in(object),
      full.names = TRUE
    )
  }

  all_run_files <- dir(all_run_dirs, full.names = TRUE)

  ls_tempfiles(
    run_files = all_run_files, output_loc = output_loc,
    ctl_extension = tools::file_ext(ctl_name(object)),
    include_psn_exports = include_psn_exports
  )
}

#' @rdname temp_files
#'
#' @param m An nm object
#'
#' @export
clean_run <- function(m, output_loc = c("run_dir", "base"), include_slurm_files = TRUE) {
  UseMethod("clean_run")
}
#' @export
clean_run.nm_list <- function(m, output_loc = c("run_dir", "base"), include_slurm_files = TRUE) {
  ls_tempfiles(m, output_loc = output_loc) %>%
    unlink(force = TRUE)
}

#' Write control file to disk
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Not designed to be used directly.  [run_nm()] and other functions will use
#' this to write the control file contents to the `ctl_path()` prior to
#' execution.
#'
#' @param m An nm object.
#' @param force Logical (default = `FALSE`), force write, don't ask.
#'
#' @return Invisibly returns `m` unmodified.
#'
#' @keywords internal
#' @export
write_ctl <- function(m, force = FALSE) {
  UseMethod("write_ctl")
}

#' @export
write_ctl.nm_generic <- function(m, force = FALSE) {
  ctl_name <- ctl_path(m)
  ctl_ob <- ctl_contents(m) %>% ctl_character()
  dir_name <- run_in(m)

  if (!file.exists(dir_name)) {
    dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)
  }

  if (file.exists(ctl_name) & !force) {
    behaviour <- overwrite_behaviour()
    old_contents <- readLines(ctl_name)
    new_contents <- ctl_ob
    attributes(new_contents) <- NULL
    overwrite_required <- !identical(new_contents, old_contents)
    if (overwrite_required) {
      if ("stop" %in% behaviour) {
        stop("stopping because overwrite required: change behaviour in overwrite_behaviour()")
      }
      if ("ask" %in% behaviour) {
        prompt_overwrite(ctl_name, new_path_contents = ctl_ob)
      }
      if ("skip" %in% behaviour) {
        return(invisible(m))
      }
    }
  }

  writeLines(ctl_ob, ctl_name)
  invisible(m)
}
#' @export
write_ctl.nm_list <- Vectorize_nm_list(write_ctl.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)

data_name <- function(x) UseMethod("data_name")

data_name.default <- function(x) {
  unlist(lapply(x, function(x) {
    if (!file.exists(x)) x <- file.path(nm_default_dir("models"), x)
    if (!file.exists(x)) stop("can't find control stream")
    x <- normalizePath(x)
    ctl <- readLines(x, warn = FALSE)
    data.row <- grep("^ *\\$DATA", ctl)
    if (length(data.row) < 1) stop("can't identify data row")
    if (length(data.row) > 1) {
      warning("multiple data rows found. Using first")
      data.row <- data.row[1]
    }
    ctl <- paste(ctl[data.row:length(ctl)], collapse = " ")
    data_name <- gsub("^ *\\$DATA\\s*([^ ]+).*$", "\\1", ctl)
    data_name
  }))
}
