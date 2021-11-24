get_single_object_for_app <- function() {
  ctx <- rstudioapi::getSourceEditorContext()
  selected_text <- ctx$selection[[1]]$text

  if (selected_text == "") {
    line <- ctx$selection[[1]]$range$start[[1]]
    pos <- ctx$selection[[1]]$range$start[[2]]

    selected_text <- ctx$contents[line]

    selected_text <- gsub("^(.*)<-.*", "\\1", selected_text)
    selected_text <- trimws(selected_text)

    if (selected_text == "") {
      stop("couldn't find object in selected line")
    }

    result <- try(is_nm_list(get(selected_text)), silent = TRUE)

    if (inherits(result, "try-error")) {
      stop("couldn't find object in selected line")
    }
  }

  selected_text <- gsub("%>%\\s*$", "", selected_text)

  old_behaviour <- overwrite_behaviour()
  overwrite_behaviour("skip")
  on.exit(overwrite_behaviour(old_behaviour))

  ## temporarily disable run_nm
  selected_text <- gsub("\\brun_nm\\b\\(", "as.list(", selected_text)
  selected_text <- gsub("\\brun_nm_batch\\b\\(", "as.list(", selected_text)

  suppressMessages({
    m <- eval(parse(text = selected_text), envir = parent.frame(n = 3))
  })
  m
}

nm_tran_app <- function() {
  m <- get_single_object_for_app()
  nm_tran(m)
}

nm_diff_app <- function() {
  m <- get_single_object_for_app()
  nm_diff(m)
}

show_ctl_app <- function() {
  m <- get_single_object_for_app()
  show_ctl(m)
}

show_out_app <- function() {
  m <- get_single_object_for_app()
  show_out(m)
}

run_monitor_app <- function() {
  
  run_with_arg <- TRUE
  
  m <- try(get_single_object_for_app(), silent = TRUE)

  ## check m - if not good run_with_arg <- FALSE  
  if (inherits(m, "try-error")) {
    run_with_arg <- FALSE
  }
  
  if (run_with_arg) {
    shiny_nm(m)  
  } else {
    shiny_nm()
  }
  
}
