if (0) {
  result_file <- function(m, name) {
    UseMethod("result_file")
  }

  result_file.nm_generic <- function(m, name) {
    name <- glue_text_nm(m, name)
    file.path(results_dir(m), name)
  }

  result_file.nm_list <- Vectorize_nm_list(result_file.nm_generic)

  attach_code_library <- function(path) {
    options(code_library_path = unique(c(path, getOption("code_library_path"))))
  }

  ctl_list.nmexecute <- function(r) {
    ctl <- ctl_character(r)
    file_name <- attributes(ctl)$file_name
    ctl <- ctl_nm2r(ctl)
    attr(ctl, "file_name") <- file_name
    return(ctl)
  }

  update_sizes <- function(ctl, sizes_char) {
    ctl <- ctl_character(ctl)
    if ("SIZES" %in% names(ctl_list(ctl))) {
      stop("can't modifying existing sizes yet")
    } else {
      dol_matches <- grep("\\s*\\$", ctl)
      if (length(dol_matches) == 0) {
        dol_matches <- 1
      } else {
        dol_matches <- dol_matches[1]
      }
      before <- c()
      after <- ctl
      if (dol_matches > 1) {
        before <- ctl[1:(dol_matches - 1)]
        after <- ctl[dol_matches:length(ctl)]
      }
      save_attr <- attributes(ctl)
      ctl <- c(
        before,
        paste("$SIZES", sizes_char),
        after
      )
      attributes(ctl) <- save_attr
    }
    ctl_list(ctl)
  }

  perturb_inits <- function(m, theta_log, omega_diag) {
    UseMethod("perturb_inits")
  }

  perturb_inits.nm_generic <- function(m, theta_log, omega_diag) {
    it <- m %>% init_theta()
    is_log <- which(it$trans %in% "LOG")
    it$init[is_log] <-
      stats::rnorm(length(is_log), mean = it$init[is_log], sd = theta_log)

    io <- m %>% init_omega()
    is_diag <- which(io$omega1 == io$omega2)
    io$init[is_diag] <-
      stats::rlnorm(length(is_diag),
        meanlog = log(io$init[is_diag]),
        sdlog = omega_diag
      )

    m %>%
      init_theta(it) %>%
      init_omega(io)
  }

  perturb_inits.nm_list <- Vectorize_nm_list(perturb_inits.nm_generic, SIMPLIFY = FALSE)

  insert_theta <- function(itheta,
                           theta_number = NA,
                           init = 0.1,
                           name = NA,
                           lower = NA,
                           upper = NA,
                           FIX = FALSE,
                           unit = NA,
                           trans = NA) {
    if (missing(theta_number)) theta_number <- max(c(0, stats::na.omit(itheta$theta))) + 1

    insert_line <- max(c(0, itheta$line[itheta$theta %in% (theta_number - 1)])) + 1

    itheta_pre <- itheta[itheta$line < insert_line, ]
    itheta_post <- itheta[itheta$line >= insert_line, ]
    itheta_post$line <- itheta_post$line + 1

    itheta_post$theta[(itheta_post$theta >= theta_number) %in% TRUE] <-
      itheta_post$theta[(itheta_post$theta >= theta_number) %in% TRUE] + 1


    dinsert <- data.frame(
      theta = theta_number,
      init = init,
      name = name,
      lower = lower,
      upper = upper,
      FIX = FIX,
      unit = unit,
      trans = trans,
      line = insert_line,
      pos = 1
    )

    suppressWarnings(dplyr::bind_rows(itheta_pre, dinsert, itheta_post))
  }

  insert_omega <- function(iomega,
                           omega_number = NA,
                           init = 0.1,
                           name = NA,
                           lower = NA,
                           upper = NA,
                           FIX = FALSE,
                           unit = NA,
                           trans = NA) {

    # iomega$new_line <- iomega$line
    # iomega$new_pos <- iomega$pos

    if (missing(omega_number)) omega_number <- max(c(0, stats::na.omit(iomega$omega1))) + 1

    insert_line <- max(c(0, iomega$line[iomega$omega1 %in% (omega_number - 1)])) + 1

    # iomega_pre <- iomega[iomega$line[iomega$line < insert_line], ]
    # iomega_post <- iomega[iomega$line[iomega$line >= insert_line], ]
    iomega_pre <- iomega[iomega$line < insert_line, ]
    iomega_post <- iomega[iomega$line >= insert_line, ]
    iomega_post$line <- iomega_post$line + 1

    iomega_post$omega1[(iomega_post$omega1 >= omega_number) %in% TRUE] <-
      iomega_post$omega1[(iomega_post$omega1 >= omega_number) %in% TRUE] + 1

    iomega_post$omega2[(iomega_post$omega2 >= omega_number) %in% TRUE] <-
      iomega_post$omega2[(iomega_post$omega2 >= omega_number) %in% TRUE] + 1

    iomega_post$block <- iomega_post$block + 1

    insert_block <- max(c(0, stats::na.omit(iomega$block[iomega$omega1 %in% (omega_number - 1)]))) + 1

    insert_mblock <- max(c(0, stats::na.omit(iomega$mblock[iomega$omega1 %in% (omega_number - 1)])))
    if (length(which(iomega$omega1 %in% (omega_number - 1))) > 1) {
      insert_mblock <- insert_mblock + 1
    }

    dinsert <- data.frame(
      omega1 = omega_number,
      omega2 = omega_number,
      init = init,
      name = name,
      lower = lower,
      upper = upper,
      block = insert_block,
      mblock = insert_mblock,
      FIX = FIX,
      unit = unit,
      trans = trans,
      line = insert_line,
      pos = 1
    )

    suppressWarnings(dplyr::bind_rows(iomega_pre, dinsert, iomega_post))
  }

  append_dollar <- function(ctl_lines, after = NULL, ...) {
    ctl_lines <- ctl_list(ctl_lines)
    if (is.null(after)) {
      append_after <- length(ctl_lines)
    } else {
      append_after <- max(which(grepl(after, names(ctl_lines))))
    }
    if (length(after) != 1) stop("cannot find determine \"after\"")
    if (is.na(after)) stop("cannot find determine \"after\"")
    if (after %in% -Inf) stop("cannot find determine \"after\"")
    attributes_ctl_lines <- attributes(ctl_lines)
    attributes_ctl_lines$names <- NULL
    ctl_lines <- append(ctl_lines, list(...), after = append_after)
    attributes_ctl_lines$names <- names(ctl_lines)
    attributes(ctl_lines) <- attributes_ctl_lines
    ctl_lines
  }

  change_to_sim <- function(ctl_lines, subpr = 1, seed = 1) {
    ctl_lines <- ctl_list(ctl_lines)
    ctl_lines$EST <- paste0(";", ctl_lines$EST)
    ctl_lines$COV <- paste0(";", ctl_lines$COV)
    if ("SIM" %in% names(ctl_lines)) {
      ctl_lines$SIM <- gsub("^\\s*;+(.*)", "\\1", ctl_lines$SIM)
      ctl_lines$SIM <- gsub("(SUBPR[^=]*\\s*=\\s*)[0-9]+", paste0("\\1", subpr), ctl_lines$SIM)
      ctl_lines$SIM <- gsub("(\\$SIM[^\\s]*\\s*\\()[0-9]+(\\))", paste0("\\1", seed, "\\2"), ctl_lines$SIM)
    } else {
      ## insert before $TABLE, after $ERROR/$PRED
      # pred_error_pos <- which(grepl("ERROR|PRED",names(ctl_lines)))
      # if(length(pred_error_pos) > 1) stop("multiple $ERROR/$PREDs detected - should only have one or the other?")
      # ctl_lines <- append(ctl_lines,list(SIM=NA),pred_error_pos)
      ctl_lines <- append_dollar(ctl_lines, SIM = NA, "ERROR|PRED|SIGMA|OMEGA")
      ctl_lines$SIM <- paste0("$SIM (", seed, ") ONLYSIM SUBPR=", subpr)
    }

    e <- ctl_lines$ERROR
    fflag1_pos <- grep("F_FLAG\\s*=\\s*1", e)
    if (length(fflag1_pos) > 0) {
      message("attempting to remove F_FLAG code... check this")
      y_line <- grep("^\\s*Y\\s*=.*(EPS|ETA).*$", e)
      if_pos <- grep("^\\s*IF.*\\sTHEN", e)
      endif_pos <- grep("^\\s*ENDIF", e)
      if_statements <- lapply(seq_along(if_pos), function(i) if_pos[i]:endif_pos[i])

      y_if_pos <- which(sapply(if_statements, function(i) y_line %in% i))

      if_statements[[y_if_pos]]
      e[setdiff(if_statements[[y_if_pos]], y_line)] <- paste(";", e[setdiff(if_statements[[y_if_pos]], y_line)])
      ctl_lines$ERROR <- e
    }
    ctl_list(ctl_lines)
  }

  theta_r2nm <- function(x) {
    x0 <- x
    x0$name[is.na(x0$name)] <- paste0("THETA", x0$N[is.na(x0$name)])
    x0$unit[!is.na(x0$unit)] <- paste(";", x0$unit[!is.na(x0$unit)])
    x0$unit[is.na(x0$unit)] <- ""
    x0$trans[!is.na(x0$trans)] <- paste(";", x0$trans[!is.na(x0$trans)])
    x0$trans[is.na(x0$trans)] <- ""
    x0$COM <- paste(x0$name, x0$unit, x0$trans)
    x0$COM <- rem_trailing_spaces(x0$COM)
    x <- by(x, x$N, function(d) {
      if (!is.na(d$lower)) paste0("(", d$lower, ",", d$init, ")") else d$init
    })
    x <- unlist(x)
    x <- paste(x, ";", x0$COM)
    setup_dollar(x, "$THETA")
  }

  new_script <- function(name, overwrite = FALSE, open_file = TRUE, libs = c("NMproject")) {
    ## create black script with comment fields. Add new_script to git
    # if (name != basename(name))
    #   stop("name must not be a path")
    if (name == basename(name)) {
      to_path <- file.path(nm_default_dir("scripts"), name) ## destination path
    } else {
      to_path <- name
    }
    if (file.exists(to_path) & !overwrite) {
      stop(paste(to_path, "already exists. Rerun with overwrite = TRUE"))
    }
    s <- c(
      paste0("## ", "Author: ", Sys.info()["user"]),
      paste0("## ", "First created: ", Sys.Date()),
      paste0("## ", "Description: "),
      paste0("## ", "Keywords: "),
      "",
      "########################################",
      "## load packages and source functions here",
      "",
      paste0("library(", libs, ")"),
      "",
      "########################################",
      "## main script here",
      ""
    )
    writeLines(s, to_path)
    if (open_file) {
      get("file.edit")(to_path)
    }
  }

  # @importFrom dplyr mutate
  # @export
  dplyr::mutate

  # @export
  mutate.nm_list <- function(.data, ...) {
    dots_exp <- rlang::enexprs(...)
    data_extra <- dplyr::mutate(nm_row(.data), ...)

    for (name in names(dots_exp)) {
      .data <- .data %>% custom_1d_field(field = name, replace = data_extra[[name]])
    }
    .data
  }

  # @importFrom dplyr filter
  # @export
  dplyr::filter

  # @export
  filter.nm_list <- function(.data, ...) {
    dots_exp <- rlang::enexprs(...)
    object <- .data
    .data <- nm_row(object)
    .data$m <- object

    data_extra <- dplyr::filter(.data, ...)
    data_extra$m
  }

  new_ctl_extra <- function(m, ctl, dir = nm_default_dir("models")) {
    ctl$TABLE <- gsub(paste0("(FILE\\s*=\\s*\\S*tab)\\S*\\b"), paste0("\\1", run_id(m)), ctl$TABLE)
    ctl[[1]] <- gsub("^(\\s*;;\\s*[0-9]*\\.\\s*Based on:).*", paste("\\1", parent_run_id(m)), ctl[[1]])
    ctl[[1]] <- gsub("^(\\s*;;\\s*\\w*\\.\\s*Author:).*", paste("\\1", Sys.info()["user"]), ctl[[1]])

    ctl
  }

  ## shouldn't need to delete ctls anymore

  delete_ctl <- function(m) {
    UseMethod("delete_ctl")
  }
  delete_ctl.nm_generic <- function(m) {
    unlink(ctl_path(m))
    invisible(m)
  }
  delete_ctl.nm_list <- Vectorize_nm_list(delete_ctl.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)


  in_cache <- function(r,
                       cache_ignore_cmd = FALSE, cache_ignore_ctl = FALSE, cache_ignore_data = FALSE,
                       return_checksums = FALSE) {
    UseMethod("in_cache")
  }

  in_cache.nm_generic <- function(r,
                                  cache_ignore_cmd = FALSE, cache_ignore_ctl = FALSE, cache_ignore_data = FALSE,
                                  return_checksums = FALSE) {
    .Deprecated("overwrite_behaviour",
      msg = "this function is no longer needed with overwrite_behaviour (see app) and will probably be removed"
    )
    r %>% write_ctl()
    ## get all md5_files

    run_cache_disk <- lapply(run_cache_paths(r), readRDS)
    if (length(run_cache_disk) > 0) {
      current_checksums <- run_checksums(r)
      checksums_reduced <- lapply(run_cache_disk, function(i) {
        if (cache_ignore_cmd) { ## remove cmd check
          keep <- !names(current_checksums) %in% "cmd"
          i$checksums <- i$checksums[keep]
          current_checksums <- current_checksums[keep]
        }

        if (cache_ignore_ctl) { ## remove cmd check
          keep <- !names(current_checksums) %in% "ctl"
          i$checksums <- i$checksums[keep]
          current_checksums <- current_checksums[keep]
        }

        if (cache_ignore_data) { ## remove cmd check
          keep <- !names(current_checksums) %in% "data"
          i$checksums <- i$checksums[keep]
          current_checksums <- current_checksums[keep]
        }

        ## ignore names
        # names(current_checksums) <- NULL
        # names(i$checksums) <- NULL

        list(
          checksums = current_checksums,
          stored_checksums = i$checksums
        )

        # identical(i$checksums, current_checksums)
      })

      matches <- sapply(
        checksums_reduced,
        function(i) {
          identical(
            i$checksums,
            i$stored_checksums
          )
        }
      )

      if (any(matches)) {
        return(TRUE) ## if up to date, skip
      }
    }
    if (return_checksums) {
      return(checksums_reduced)
    }
    return(FALSE)
  }

  in_cache.nm_list <- Vectorize_nm_list(in_cache.nm_generic)

  manual_patch <- function(m) {
    res <- start_manual_edit_unix(m)

    message(
      "---Manual edit---
    Instructions:
    1) edit control file
    2) save & close
    Press ENTER when done..."
    )
    readline()

    ## now diff ctl_path(m) and old_file_path

    diff_manual_edit(m, res)

    message("patch created:\n ", res$patch_path, "\n")

    message("copy-paste the following into your script to apply:\n
  [nm_object] %>%
  apply_manual_edit(\"", res$patch_name, "\")

(dont forget to comment your code)")
  }

  in_cache_app <- function() {
    m <- get_single_object_for_app()
    in_cache(m)
  }

  nmsave_multiplot <- function(r, plot_ob, plot_name, plot_dir = results_dir(r),
                               width = 7, height = 5, dpi = 300, ...) {
    UseMethod("nmsave_multiplot")
  }

  nmsave_multiplot.nm_generic <- function(r, plot_ob, plot_name, plot_dir = results_dir(r),
                                          width = 7, height = 5, ...) {
    plot_name <- glue_text_nm(r, plot_name)
    plot_name <- unique(plot_name)
    if (length(plot_name) > 1) stop("multiple plot names", call. = FALSE)
    dir.create(unique(plot_dir), showWarnings = FALSE, recursive = TRUE)

    grDevices::pdf(file.path(unique(plot_dir), plot_name), ...)
    print(plot_ob)
    grDevices::dev.off()
    r <- r %>% result_files(plot_name)
    invisible(r)
  }

  nmsave_multiplot.nm_list <- Vectorize_nm_list(nmsave_multiplot.nm_generic, SIMPLIFY = FALSE, invisible = TRUE)
}
