raw_init_theta <- function(m, replace){
  
  if(missing(replace)){
    m <- as_nm_generic(m)
    
    ctl <- m %>% ctl_contents()
    ctl_char <- ctl_character(ctl)
    
    sub_names <- names(ctl_contents(m))
    if("PK" %in% sub_names) PK_PRED <- "PK"
    if("PRED" %in% sub_names) PK_PRED <- "PRED"
    
    x_pk <- ctl[[PK_PRED]]
    x_orig <- ctl[["THETA"]]
    x <- x_orig
    x <- as.character(x)
    
    ## test
    
    # dol_theta <- "$THETA 0.1 -2 FIX (3,4 ) ; pk parameters
    # $THETA 4 ; PD
    # (2, 3) \t ; KA2 ; h ; LOG
    # 
    # (0, 3, 3) ; EC50 ; ; EXP
    # (0 3 ) ; ; EC50 ; ml/h
    # ( 4 5 6) FIX 4  ; EMAX
    # ( 4 5 6 FIX),-4.2
    # ( 4,,6), -4.25  ; EMAX2"
    # x <- strsplit(dol_theta, "\n")[[1]]
    
    d <- data.frame(x = x)
    d$line <- seq_len(nrow(d))
    ## clean up x to just what's necessary
    d$x_nc <- rem_comment(d$x)
    d$comment <- NA
    d$comment[grepl(".*;.*", d$x)] <- gsub(".+?;(.*)", "\\1", d$x[grepl(".*;.*", d$x)])
    
    ## -- clean up separators -- ##
    ## sort spaces out
    d$x_nc <- gsub("\\t","  ",d$x_nc, perl = TRUE)
    d$x_nc <- gsub("\\s+"," ",d$x_nc, perl = TRUE)
    d$x_nc <- trimws(d$x_nc)
    
    d$x_nc <- gsub("\\s+FIX", "FIX", d$x_nc)  ## remove seps so it stays with parameter and survives split
    
    ## remove spaces near commas and brackets
    d$x_nc <- gsub("\\s*,\\s*",",",d$x_nc, perl = TRUE)
    d$x_nc <- gsub("\\(\\s*","\\(",d$x_nc, perl = TRUE)  
    d$x_nc <- gsub("\\s*\\)","\\)",d$x_nc, perl = TRUE)
    d$x_nc <- gsub_in_brackets("\\s+", ",", d$x_nc)  
    d$x_nc <- gsub_out_brackets(",", " ", d$x_nc)  
    
    x_nc <- d$x_nc
    x_nc <- paste(x_nc, collapse = " \n ")
    x_nc <- strsplit(x_nc, " ")[[1]]
    
    d$x_nc2 <- strsplit(d$x_nc, " ")
    d$x_nc2[sapply(d$x_nc2, length) == 0] <- ""
    
    d <- by(d, d$line, function(d){
      d <- merge(data.frame(value = d$x_nc2[[1]]), d)
      #d <- merge(dplyr::tibble(value = d$x_nc2[[1]]), d) ## causes seg faults
      d$pos <- seq_len(nrow(d))
      d
    })
    d <- do.call(rbind, d)
    
    d$x <- NULL
    d$x_nc <- NULL
    d$x_nc2 <- NULL
    
    d$name <- NA
    d$parameter <- NA
    d$lower <- NA
    d$init <- NA
    d$upper <- NA
    
    number_regex <- "\\(?\\-?[0-9\\.]+\\)?F?I?X?\\)?"
    single_number_regex <- paste0("^(",number_regex,")$")
    lower_init_regex <- paste0("^\\((",number_regex,"),(",number_regex,")\\)F?I?X?$")  
    lower_init_upper_regex <- paste0("^\\((",number_regex,"),(",number_regex,"),(",number_regex,")\\)F?I?X?$")
    lower_upper_regex <- paste0("^\\((",number_regex,"),,(",number_regex,")\\)F?I?X?$")
    
    d$format <- NA
    d$format[grepl(single_number_regex, d$value)] <- "single_number"
    d$format[grepl(lower_init_regex, d$value)] <- "lower_init"
    d$format[grepl(lower_init_upper_regex, d$value)] <- "lower_init_upper"
    d$format[grepl(lower_upper_regex, d$value)] <- "lower_upper"
    
    d$theta <- NA
    d$theta[!is.na(d$format)] <- seq_along(d$parameter[!is.na(d$format)])
    
    d$parameter[!is.na(d$format)] <- 
      paste0("THETA",seq_along(d$parameter[!is.na(d$format)]))
    
    d$FIX <- grepl("FIX", d$value)
    d$value <- gsub("FIX", "", d$value)
    
    suppressWarnings({ ## ignore as.numeric("...") warnings
      d$init[d$format %in% "single_number"] <- 
        as.numeric(grep(single_number_regex,
                        d$value[d$format %in% "single_number"], value = TRUE))
      
      d$lower[d$format %in% "lower_init"] <- 
        as.numeric(gsub(lower_init_regex,"\\1",d$value[d$format %in% "lower_init"]))
      d$init[d$format %in% "lower_init"] <- 
        as.numeric(gsub(lower_init_regex,"\\2",d$value[d$format %in% "lower_init"]))
      
      d$lower[d$format %in% "lower_init_upper"] <- 
        as.numeric(gsub(lower_init_upper_regex,"\\1",d$value[d$format %in% "lower_init_upper"]))
      d$init[d$format %in% "lower_init_upper"] <- 
        as.numeric(gsub(lower_init_upper_regex,"\\2",d$value[d$format %in% "lower_init_upper"]))
      d$upper[d$format %in% "lower_init_upper"] <- 
        as.numeric(gsub(lower_init_upper_regex,"\\3",d$value[d$format %in% "lower_init_upper"]))
      
      d$lower[d$format %in% "lower_upper"] <- 
        as.numeric(gsub(lower_upper_regex,"\\1",d$value[d$format %in% "lower_upper"]))
      d$upper[d$format %in% "lower_upper"] <- 
        as.numeric(gsub(lower_upper_regex,"\\2",d$value[d$format %in% "lower_upper"]))      
    })
    
    
    d$format <- NULL
    
    ## grab the names 
    
    d$comment_nfields <- NA
    two_field_regex <- "^(.*?);(.*?)$"
    three_field_regex <- "^(.*?);(.*?);(.*?)$"
    d$comment_nfields[grepl(two_field_regex, d$comment)] <- 2
    d$comment_nfields[grepl(three_field_regex, d$comment)] <- 3
    
    d$name[!is.na(d$comment)] <- d$comment[!is.na(d$comment)]
    d$name[d$comment_nfields %in% 2] <- gsub(two_field_regex, "\\1", d$comment[d$comment_nfields %in% 2])
    d$name[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\1", d$comment[d$comment_nfields %in% 3])
    d$name <- trimws(d$name)
    
    d$unit <- NA
    d$unit[d$comment_nfields %in% 2] <- gsub(two_field_regex, "\\2", d$comment[d$comment_nfields %in% 2])
    d$unit[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\2", d$comment[d$comment_nfields %in% 3])
    d$unit <- trimws(d$unit)
    
    d$trans <- NA
    d$trans[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\3", d$comment[d$comment_nfields %in% 3])
    d$trans <- trimws(d$trans)
    
    d$comment_nfields <- NULL
    
    PK_thetas <- m %>% target(PK_PRED) %>% grab_variables("THETA\\([0-9+]\\)")
    ERROR_thetas <- m %>% target("ERROR") %>% grab_variables("THETA\\([0-9+]\\)")
    
    PK_thetas <- gsub("\\(","", PK_thetas)
    PK_thetas <- gsub("\\)","", PK_thetas)
    ERROR_thetas <- gsub("\\(","", ERROR_thetas)
    ERROR_thetas <- gsub("\\)","", ERROR_thetas)
    
    if(length(c(PK_thetas,ERROR_thetas)) > nrow(d))
      stop("Found more THETAs in code than in $THETA")
    
    d$SUB <- NA
    d$SUB[d$parameter %in% PK_thetas] <- PK_PRED
    d$SUB[d$parameter %in% ERROR_thetas] <- "ERROR"
    
    d <- d[, c(names(d)[!names(d) %in% c("line", "pos")], "line", "pos")]
    
    d
    
  } else {
    replace <- setup_dollar(param_r2nm_extra(replace), "$THETA", add_dollar_text = FALSE)
    old_target <- target(m)
    m <- m %>% target("THETA") %>% text(replace) %>% target(old_target)
    m
  }
}

raw_init_random <- function(m, replace, dollar = "OMEGA"){
  
  if(missing(replace)){
    
    m <- as_nm_generic(m)
    
    dollar_text <- gsub("\\$","",dollar)
    
    ctl <- m %>% ctl_contents()
    ctl_char <- ctl_character(ctl)
    
    sub_names <- names(ctl_contents(m))
    if("PK" %in% sub_names) PK_PRED <- "PK"
    if("PRED" %in% sub_names) PK_PRED <- "PRED"
    
    x_pk <- ctl[[PK_PRED]]
    x_orig <- ctl[[dollar_text]]
    x <- x_orig
    x <- as.character(x)
    
    # dol_omega <- "$OMEGA 0.1 -2 (3,4 ) ; pk parameters
    # $OMEGA 4 ; PD
    # (2, 3) \t ; IIV_KA2 ; CV% ; LOG
    # 
    # ; random comment
    # (0, 3, 3) ; IIV_EC50 ; ;
    # (0 3 ) ; ; ng/ml ; LOG
    # ( 4 5 6) 4  ; IIV_EMAX
    # $OMEGA BLOCK (3)
    # 0.1             ; IIV_KA ; CV% ; LOG
    # 0.1 0.1         ; IIV_V2 ; CV% ; LOG
    # 3 , 0.1 0.1 FIX     ; IIV_CL ; CV% ; LOG
    # $OMEGA ( 4 5 6) FIX,-4.2
    # 
    # $OMEGA ( 4,,6), -4.25  ; IIV_EMAX2
    # "
    # x <- strsplit(dol_omega, "\n")[[1]]
    
    d <- data.frame(x = x)
    d$line <- seq_len(nrow(d))
    ## clean up x to just what's necessary
    d$x_nc <- rem_comment(d$x)
    d$comment <- NA
    d$comment[grepl(".*;.*", d$x)] <- gsub(".+?;(.*)", "\\1", d$x[grepl(".*;.*", d$x)])
    
    ## -- clean up separators -- ##
    ## sort spaces out
    d$x_nc <- gsub("\\t","  ",d$x_nc, perl = TRUE)
    d$x_nc <- gsub("\\s+"," ",d$x_nc, perl = TRUE)
    d$x_nc <- trimws(d$x_nc)
    
    d$x_nc <- gsub("\\s+FIX", "FIX", d$x_nc)  ## remove seps so it stays with parameter and survives split
    
    ## remove spaces near commas and brackets
    d$x_nc <- gsub("\\s*,\\s*",",",d$x_nc, perl = TRUE)
    d$x_nc <- gsub("\\(\\s*","\\(",d$x_nc, perl = TRUE)  
    d$x_nc <- gsub("\\s*\\)","\\)",d$x_nc, perl = TRUE)
    d$x_nc <- gsub_in_brackets("\\s+", ",", d$x_nc)  
    d$x_nc <- gsub_out_brackets(",", " ", d$x_nc)  
    ## subsequent code needs OMEGA BLOCK (n) format
    d$x_nc <- gsub("BLOCK\\(", "BLOCK (",d$x_nc)
    
    x_nc <- d$x_nc
    x_nc <- paste(x_nc, collapse = " \n ")
    x_nc <- strsplit(x_nc, " ")[[1]]
    
    d$x_nc2 <- strsplit(d$x_nc, " ")
    d$x_nc2[sapply(d$x_nc2, length) == 0] <- ""
    
    d <- by(d, d$line, function(d){
      d <- merge(data.frame(value = d$x_nc2[[1]]), d)
      #d <- merge(dplyr::tibble(value = d$x_nc2[[1]]), d) ## causes seg faults
      d$pos <- seq_len(nrow(d))
      d
    })
    d <- do.call(rbind, d)
    
    d$x <- NULL
    d$x_nc <- NULL
    d$x_nc2 <- NULL
    
    d$name <- NA
    d$omega1 <- NA
    d$omega2 <- NA
    d$parameter <- NA
    d$lower <- NA
    d$init <- NA
    d$upper <- NA
    
    number_regex <- "\\(?\\-?[0-9\\.]+\\)?F?I?X?\\)?"
    single_number_regex <- paste0("^(",number_regex,")$")
    lower_init_regex <- paste0("^\\((",number_regex,"),(",number_regex,")\\)F?I?X?$")  
    lower_init_upper_regex <- paste0("^\\((",number_regex,"),(",number_regex,"),(",number_regex,")\\)F?I?X?$")
    lower_upper_regex <- paste0("^\\((",number_regex,"),,(",number_regex,")\\)F?I?X?$")
    
    d$format <- NA
    d$format[grepl(single_number_regex, d$value)] <- "single_number"
    d$format[grepl(lower_init_regex, d$value)] <- "lower_init"
    d$format[grepl(lower_init_upper_regex, d$value)] <- "lower_init_upper"
    d$format[grepl(lower_upper_regex, d$value)] <- "lower_upper"
    
    ##########################
    ## label parameters
    
    d$block <- NA
    get_block_size <- FALSE
    current_block_size <- 1
    current_block_start <- 1
    current_block_end <- 1
    omega1_counter <- 0
    omega2_counter <- 0
    for(i in seq_len(nrow(d))){
      if(is.na(d$format[i])) {
        if(d$value[i] %in% "BLOCK") get_block_size <- TRUE
        next
      }
      if(get_block_size){  ## get new block information
        n <- gsub("\\(([0-9]+)\\)", "\\1", d$value[i])
        current_block_size <- as.numeric(n)
        current_block_start <- omega1_counter + 1
        current_block_end <- current_block_start - 1 + current_block_size
        get_block_size <- FALSE
        next
      }
      
      ## current_block_size + friends are now all correct
      ## update counters
      
      if(current_block_size == 1) {             ## normal omega
        omega1_counter <- omega1_counter + 1
        omega2_counter <- omega2_counter + 1
      } else {                                     ## in a block
        if(omega1_counter != omega2_counter){      ##   on same row of omega matrix
          omega2_counter <- omega2_counter + 1
        } else {                                   ##   on a new row of omega matrix
          omega1_counter <- omega1_counter + 1
          omega2_counter <- current_block_start
        }
      }
      
      ## get name
      d$omega1[i] <- omega1_counter
      d$omega2[i] <- omega2_counter
      d$parameter[i] <- paste0(dollar_text, ".", omega1_counter, ".", omega2_counter, ".")
      
      #if(omega1_counter == 1 & omega2_counter == 1) {
      #  d$block[i] <- 1
      #} else {
      #  d$block[i] <- max(stats::na.omit(c(d$block[seq_len(i-1)], 0))) + 1
      #}
      
      if(omega1_counter == current_block_start & 
         omega2_counter == current_block_start) {
        d$block[i] <- max(stats::na.omit(c(d$block[seq_len(i-1)], 0))) + 1
      } else {
        d$block[i] <- max(stats::na.omit(c(d$block[seq_len(i-1)], 0)))
      }
      
      ## if at end of a block, reset the block size to 1
      if(omega1_counter == current_block_end & 
         omega2_counter == current_block_end) {
        current_block_size <- 1
        current_block_start <- omega1_counter + 1
        current_block_end <- current_block_start - 1 + current_block_size
      } 
      
    }
    
    #######################
    blocks <- d$block[!is.na(d$block)]
    
    tab <- table(blocks)
    diffs <- c(0,diff(tab))
    diffs[diffs != 0] <- 1
    mblock <- cumsum(diffs) + 1
    names(mblock) <- names(tab)
    d$mblock[!is.na(d$block)] <- mblock[blocks]
    
    ## lines above block are part of block
    d$block <- rev(na.locf(rev(d$block)))
    
    d$FIX <- grepl("FIX", d$value)
    d$value <- gsub("FIX", "", d$value)
    
    d$init[d$format %in% "single_number"] <- 
      suppressWarnings(as.numeric(grep(single_number_regex,
                                       d$value[d$format %in% "single_number"], value = TRUE)))
    
    d$lower[d$format %in% "lower_init"] <- 
      as.numeric(gsub(lower_init_regex,"\\1",d$value[d$format %in% "lower_init"]))
    d$init[d$format %in% "lower_init"] <- 
      as.numeric(gsub(lower_init_regex,"\\2",d$value[d$format %in% "lower_init"]))
    
    d$lower[d$format %in% "lower_init_upper"] <- 
      as.numeric(gsub(lower_init_upper_regex,"\\1",d$value[d$format %in% "lower_init_upper"]))
    d$init[d$format %in% "lower_init_upper"] <- 
      as.numeric(gsub(lower_init_upper_regex,"\\2",d$value[d$format %in% "lower_init_upper"]))
    d$upper[d$format %in% "lower_init_upper"] <- 
      as.numeric(gsub(lower_init_upper_regex,"\\3",d$value[d$format %in% "lower_init_upper"]))
    
    d$lower[d$format %in% "lower_upper"] <- 
      as.numeric(gsub(lower_upper_regex,"\\1",d$value[d$format %in% "lower_upper"]))
    d$upper[d$format %in% "lower_upper"] <- 
      as.numeric(gsub(lower_upper_regex,"\\2",d$value[d$format %in% "lower_upper"]))
    
    d$format <- NULL
    
    ## grab the names 
    d$comment_nfields <- NA
    two_field_regex <- "^(.*?);(.*?)$"
    three_field_regex <- "^(.*?);(.*?);(.*?)$"
    d$comment_nfields[grepl(two_field_regex, d$comment)] <- 2
    d$comment_nfields[grepl(three_field_regex, d$comment)] <- 3
    
    d$name[!is.na(d$comment)] <- d$comment[!is.na(d$comment)]
    d$name[d$comment_nfields %in% 2] <- gsub(two_field_regex, "\\1", d$comment[d$comment_nfields %in% 2])
    d$name[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\1", d$comment[d$comment_nfields %in% 3])
    d$name <- trimws(d$name)
    
    d$unit <- NA
    #d$unit[d$comment_nfields %in% 2] <- gsub(two_field_regex, "\\2", d$comment[d$comment_nfields %in% 2])
    d$unit[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\2", d$comment[d$comment_nfields %in% 3])
    d$unit <- trimws(d$unit)
    
    d$trans <- NA
    d$trans[d$comment_nfields %in% 2] <- gsub(two_field_regex, "\\2", d$comment[d$comment_nfields %in% 2])
    d$trans[d$comment_nfields %in% 3] <- gsub(three_field_regex, "\\3", d$comment[d$comment_nfields %in% 3])
    d$trans <- trimws(d$trans)
    
    ## modify name, unit and trans for off diagonals
    off_diagonal <- (d$omega1 != d$omega2) %in% TRUE
    if(any(off_diagonal)){
      d$unit[off_diagonal] <- NA
      d$trans[off_diagonal] <- NA
      
      off_diagonal_names <- sapply(which(off_diagonal), function(i){
        omega1 <- d$omega1[i]
        omega2 <- d$omega2[i]
        name1 <- d$name[d$omega1 %in% omega1 & d$omega2 %in% omega1]
        name2 <- d$name[d$omega1 %in% omega2 & d$omega2 %in% omega2]
        
        name1 <- gsub("IIV_", "", name1)
        name2 <- gsub("IIV_", "", name2)
        new_name <- paste0("COV_",name1,"_",name2)
        new_name
      })
      d$name[off_diagonal] <- off_diagonal_names  
    }
    
    d$comment_nfields <- NULL
    
    PK_etas <- m %>% target(PK_PRED) %>% grab_variables("\\bETA\\([0-9+]\\)")
    ERROR_etas <- m %>% target("ERROR") %>% grab_variables("\\bETA\\([0-9+]\\)")
    
    PK_etas_n <- as.numeric(gsub("ETA\\(([0-9]+)\\)", "\\1", PK_etas))
    ERROR_etas_n <- as.numeric(gsub("ETA\\(([0-9]+)\\)", "\\1", ERROR_etas))
    
    d$SUB <- NA
    if(length(PK_etas_n) > 0){
      d$SUB[grepl(paste0(dollar_text, PK_etas_n, "\\.[0-9]+", collapse = "|"), d$parameter)] <- PK_PRED
      d$SUB[grepl(paste0(dollar_text,"[0-9]+\\.", PK_etas_n, "\\b", collapse = "|"), d$parameter)] <- PK_PRED
    }
    if(length(ERROR_etas_n) > 0){
      d$SUB[grepl(paste0(dollar_text, ERROR_etas_n, "\\.[0-9]+", collapse = "|"), d$parameter)] <- "ERROR"
      d$SUB[grepl(paste0(dollar_text, "[0-9]+\\.", ERROR_etas_n, "\\b", collapse = "|"), d$parameter)] <- "ERROR"
    }
    
    d <- d[, c(names(d)[!names(d) %in% c("line", "pos")], "line", "pos")]  
    d
    
  } else {
    replace <- setup_dollar(param_r2nm_extra(replace), paste0("$",dollar), add_dollar_text = FALSE)
    old_target <- target(m)
    m <- m %>% target(dollar) %>% text(replace) %>% target(old_target)
    m
  }
}

raw_init_omega <- raw_init_random
formals(raw_init_omega)$dollar <- "OMEGA"

raw_init_sigma <- raw_init_random
formals(raw_init_sigma)$dollar <- "SIGMA"

param_r2nm_extra <- function(d){
  
  single_number <- !is.na(d$init) & is.na(d$lower) & is.na(d$upper)
  lower_init <- !is.na(d$init) & !is.na(d$lower) & is.na(d$upper)
  lower_init_upper <- !is.na(d$init) & !is.na(d$lower) & !is.na(d$upper)
  lower_upper <- !is.na(d$init) & is.na(d$lower) & !is.na(d$upper)
  
  d$value[single_number] <- d$init[single_number]
  d$value[lower_init] <- paste0("(",d$lower[lower_init],",",d$init[lower_init],")")
  d$value[lower_init_upper] <- paste0("(",d$lower[lower_init_upper],",",d$init[lower_init_upper],",",d$upper[lower_init_upper],")")
  d$value[lower_upper] <- paste0("(",d$lower[lower_upper],",,",d$upper[lower_upper],")")
  
  name <- !is.na(d$name) & is.na(d$unit) & is.na(d$trans)
  name_unit <- !is.na(d$name) & !is.na(d$unit) & is.na(d$trans)
  name_unit_trans <- !is.na(d$name) & !is.na(d$unit) & !is.na(d$trans)
  
  d$comment[name_unit_trans] <- 
    paste(d$name[name_unit_trans], d$unit[name_unit_trans], d$trans[name_unit_trans],
          sep = " ; ")
  d$comment[name_unit] <- 
    paste(d$name[name_unit], d$unit[name_unit],
          sep = " ; ")
  d$comment[name] <-  d$name[name]
  
  d$value[d$FIX %in% TRUE] <- paste(d$value[d$FIX %in% TRUE], "FIX")
  
  d <- d %>% dplyr::group_by(.data$line) %>%
    dplyr::summarise(value = paste0(.data$value, collapse = " "),
                     comment = dplyr::first(stats::na.omit(.data$comment)))
  
  com <- !is.na(d$comment)
  
  d$value[com] <- paste(d$value[com], d$comment[com], sep = "   ; ")
  d$value
  
}


#' Get/set initial parameters
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' These functions are useful to obtain and modify initial values of `$THETA`, `$OMEGA` and `$SIGMA`.
#' 
#' @param m nm object
#' @param replace optional tibble for replacement
#' @param ... mutate init_theta
#' 
#' @examples
#' \dontrun{
#' 
#' ## set initial values
#' 
#' m1 <- new_nm(run_id = "m1",
#'              based_on = "staging/Models/ADVAN2.mod",
#'              data_path = "DerivedData/data.csv") %>%
#'       fill_input() %>%
#'       init_theta(init = c(-2, 0.5, 1)) %>%
#'       init_sigma(init = c(0.1, 0.1)) %>%
#'       run_nm()
#'       
#' init_theta(m1)  ## display current $THETA in tibble-form
#' init_omega(m1)  ## display current $OMEGA in tibble-form
#' 
#' 
#' ## here we supply a named vector in a different order
#' m1 <- m1 %>% init_theta(init = c(KA = -2, V = 1))
#' m1 %>% dollar("THETA")
#' 
#' ## can also manipulate other aspects (like the FIX column) similarly
#' m1 <- m1 %>% init_theta(init = c(KA = -2, V = 1),
#'                         FIX = c(KA = TRUE))
#' m1 %>% dollar("THETA")
#'   
#' ## perturb all parameters by ~10%
#' m1 <- m1 %>% init_theta(init = rnorm(length(init), mean = init, sd = 0.1))
#' 
#' ## perturb only log transformed parameters by ~10%
#' m1 <- m1 %>% init_theta(
#'     init = ifelse(
#'     trans %in% "LOG",
#'     rnorm(length(init), mean = init, sd = 0.1),
#'     init
#'   )
#' )
#' 
#' }
#' @name init_theta
#' @export

init_theta <- function(m, replace, ...){
  UseMethod("init_theta")
}

#' @export
init_theta.nm_generic <- function(m, replace, ...){
  d <- raw_init_theta(m)
  d$orig_line <- d$line
  mutate_args <- rlang::enquos(...)
  if(missing(replace)){  ## get
    if(length(mutate_args) > 0){
      current_init <- init_theta(m)
      
      ## determine quosures produces named lists
      mutate_style <- rep(TRUE, length(mutate_args))
      
      args_eval <- try(lapply(mutate_args, rlang::eval_tidy), silent = TRUE)
      if(!inherits(args_eval, "try-error")){
        ## evaluation worked, see if names are present
        arg_names <- sapply(args_eval, function(x) length(names(x)))
        mutate_style <- arg_names == 0
      }
      
      replace <- current_init %>% dplyr::mutate(!!!(mutate_args)[mutate_style])
      
      ## handle mutate_args[!mutate_style]
      ## use names to subset
      if(!inherits(args_eval, "try-error")){
        ## do simple replace of non mutate args
        ## loop through columns and parameter values
        for(col_name in names(args_eval)){
          for(par_name in names(args_eval[[col_name]])){
            entry_eval <- args_eval[[col_name]][par_name]
            names(entry_eval) <- NULL
            if(length(replace[replace$name %in% par_name, col_name]) == 0)
              stop("parameter name not found, must be one of the following:\n ", 
                   paste(stats::na.omit(replace$name), collapse = ", "), call. = FALSE)
            replace[replace$name %in% par_name, col_name] <- entry_eval
          }
        }
      }
      
      replace <- replace %>% dplyr::mutate_if(is.numeric, ~signif(., 5))
    } else {
      d <- d[!is.na(d$parameter), ]
      d$value <- NULL
      d$comment <- NULL
      d$SUB <- NULL
      return(d) 
    }
  } else {
    if(length(mutate_args) > 0) stop("can't specify additional args and replace args at same time") 
  }
  d$row <- seq_len(nrow(d))
  d_new <- dplyr::full_join(d, replace, by = c("line", "pos"))
  d_new <- d_new[, !grepl("\\.x$", names(d_new))]
  names(d_new) <- gsub("(.*)\\.y", "\\1", names(d_new))
  d_new <- d_new[order(d_new$row), ]
  d_new$row <- NULL
  m <- m %>% raw_init_theta(d_new)
  m
}

#' @export
init_theta.nm_list <- Vectorize_nm_list(init_theta.nm_generic, SIMPLIFY = FALSE,
                                        replace_arg = "replace",
                                        exclude_classes = c("data.frame"),
                                        non_lazy_eval = c("m", "replace"))


# init_theta.nm_list <- function(m, replace, ...){
#   current_call <- match.call()
#   calling_env <- parent.frame()
#   result <- lapply(m, function(m){
#     current_call[[1]] <- as.symbol("init_theta")
#     current_call[[2]] <- m
#     eval(current_call, envir = calling_env)
#   })
#   if(is_nm_list(result)){
#     result <- as_nm_list(result)
#   }
#   result
# }

#' @rdname init_theta
#' @export
init_omega <- function(m, replace, ...){
  UseMethod("init_omega")
}

#' @export
init_omega.nm_generic <- function(m, replace, ...){
  d <- raw_init_omega(m)
  d$orig_line <- d$line
  d$orig_pos <- d$pos
  mutate_args <- rlang::enquos(...)
  if(missing(replace)){  ## get
    if(length(mutate_args) > 0){
      current_init <- init_omega(m)
      
      ## determine quosures produces named lists
      mutate_style <- rep(TRUE, length(mutate_args))
      
      args_eval <- try(lapply(mutate_args, rlang::eval_tidy), silent = TRUE)
      if(!inherits(args_eval, "try-error")){
        ## evaluation worked, see if names are present
        arg_names <- sapply(args_eval, function(x) length(names(x)))
        mutate_style <- arg_names == 0
      }
      
      replace <- current_init %>% mutate_cond(!is.na(current_init$name), !!!(mutate_args)[mutate_style])
      
      ## handle mutate_args[!mutate_style]
      ## use names to subset
      if(!inherits(args_eval, "try-error")){
        ## do simple replace of non mutate args
        ## loop through columns and parameter values
        for(col_name in names(args_eval)){
          for(par_name in names(args_eval[[col_name]])){
            entry_eval <- args_eval[[col_name]][par_name]
            names(entry_eval) <- NULL
            if(length(replace[replace$name %in% par_name, col_name]) == 0)
              stop("parameter name not found, must be one of the following:\n ", 
                   paste(stats::na.omit(replace$name), collapse = ", "), call. = FALSE)
            replace[replace$name %in% par_name, col_name] <- entry_eval
          }
        }
      }
      
      replace <- replace %>% dplyr::mutate_if(is.numeric, ~signif(., 5))
    } else {
      d$value <- NULL
      d$comment <- NULL
      d$parameter <- NULL
      d$SUB <- NULL
      return(d)
    }
  } 
  ## set
  ## need to add back in column from raw_init_omega format
  d_derived <- d[,c("value","comment","parameter","SUB", ## same as what was deleted above
                    "orig_line", "orig_pos")] 
  
  replace <- dplyr::left_join(replace, d_derived, by = c("orig_line", "orig_pos"))
  if("new_value" %in% names(replace)) {  ## for characters
    replace$value[!is.na(replace$new_value)] <- as.character(replace$new_value[!is.na(replace$new_value)])
  }
  if("new_line" %in% names(replace)) replace$line <- replace$new_line
  if("new_pos" %in% names(replace)) replace$pos <- replace$new_pos
  #debugonce(raw_init_omega)
  m <- m %>% raw_init_omega(replace)
  m
}

#' @export
init_omega.nm_list <- Vectorize_nm_list(init_omega.nm_generic, SIMPLIFY = FALSE, 
                                        replace_arg = "replace",
                                        exclude_classes = c("data.frame"),
                                        non_lazy_eval = c("m", "replace"))

#' @name init_theta
#' @export
init_sigma <- function(m, replace, ...){
  UseMethod("init_sigma")
}

#' @export
init_sigma.nm_generic <- function(m, replace, ...){
  d <- raw_init_sigma(m)
  d$orig_line <- d$line
  d$orig_pos <- d$pos
  mutate_args <- rlang::enquos(...)
  if(missing(replace)){  ## get
    if(length(mutate_args) > 0){
      current_init <- init_sigma(m)
      ## determine quosures produces named lists
      mutate_style <- rep(TRUE, length(mutate_args))
      
      args_eval <- try(lapply(mutate_args, rlang::eval_tidy), silent = TRUE)
      if(!inherits(args_eval, "try-error")){
        ## evaluation worked, see if names are present
        arg_names <- sapply(args_eval, function(x) length(names(x)))
        mutate_style <- arg_names == 0
      }
      
      replace <- current_init %>% mutate_cond(!is.na(current_init$name), !!!(mutate_args)[mutate_style])
      
      ## handle mutate_args[!mutate_style]
      ## use names to subset
      if(!inherits(args_eval, "try-error")){
        ## do simple replace of non mutate args
        ## loop through columns and parameter values
        for(col_name in names(args_eval)){
          for(par_name in names(args_eval[[col_name]])){
            entry_eval <- args_eval[[col_name]][par_name]
            names(entry_eval) <- NULL
            if(length(replace[replace$name %in% par_name, col_name]) == 0)
              stop("parameter name not found, must be one of the following:\n ", 
                   paste(stats::na.omit(replace$name), collapse = ", "), call. = FALSE)
            replace[replace$name %in% par_name, col_name] <- entry_eval
          }
        }
      }
      
      replace <- replace %>% dplyr::mutate_if(is.numeric, ~signif(., 5))
    } else {
      d$value <- NULL
      d$comment <- NULL
      d$parameter <- NULL
      d$SUB <- NULL
      return(d)
    }
  } 
  ## set
  d_derived <- d[,c("value","comment","parameter","SUB", ## same as what was deleted above
                    "orig_line", "orig_pos")] 
  
  replace <- dplyr::left_join(replace, d_derived, by = c("orig_line", "orig_pos"))
  if("new_value" %in% names(replace)) {  ## for characters
    replace$value[!is.na(replace$new_value)] <- as.character(replace$new_value[!is.na(replace$new_value)])
  }
  if("new_line" %in% names(replace)) replace$line <- replace$new_line
  if("new_pos" %in% names(replace)) replace$pos <- replace$new_pos
  
  m <- m %>% raw_init_sigma(replace)
  m
}

#' @export
init_sigma.nm_list <- Vectorize_nm_list(init_sigma.nm_generic, SIMPLIFY = FALSE, 
                                        replace_arg = "replace",
                                        exclude_classes = c("data.frame"),
                                        non_lazy_eval = c("m", "replace"))


#' @name block-omega-sigma
#' @rdname block-omega-sigma
#' @title Create or remove $OMEGA/$SIGMA BLOCKs
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Manipulate $OMEGA (and $SIGMA) BLOCKs to introduce or remove correlations.
#'
#' @param iomega Tibble output from [init_omega()] or [init_sigma()]
#' @param eta_numbers Numeric vector.  ETA numbers to put into a block or
#'   unblock for `block()` and `unblock()`, respectively. Must be contiguous
#' @param diag_init numeric. Default value for off diagonal elements
#'
#' @seealso [init_theta()], [init_omega()], [init_sigma()]
#'
#' @examples
#'
#' \dontrun{
#' io <- m1 %>% init_omega()
#' io <- io %>% block(c(2,3))
#' m1 <- m1 %>% init_omega(io)
#' m1 %>% dollar("OMEGA") ## to display $OMEGA
#' }
#'
#' @export
block <- function(iomega,
                  eta_numbers = NA,
                  diag_init = 0.01){
  
  eta_numbers <- sort(eta_numbers)
  
  if(!all(diff(eta_numbers) == 1)) stop("etas must be adjacent", call. = FALSE)
  
  start_eta <- min(eta_numbers)
  end_eta <- max(eta_numbers)
  
  #start_index <- match(iomega$block[iomega$omega1 %in% start_eta], iomega$block)
  start_index <- match(start_eta, iomega$omega1)
  end_index <- match(end_eta, iomega$omega1)
  
  if(is.na(start_index) | is.na(end_index)) stop("etas not found", call. = FALSE)
  
  omega_counts <- iomega$omega1[!is.na(iomega$omega1)]
  omega_counts <- table(omega_counts)[eta_numbers]
  
  if(any(omega_counts > 1)) stop("etas cannot already be a block", call. = FALSE)
  
  start_block <- iomega$block[iomega$omega1 %in% start_eta]
  iomega$remove <- FALSE
  iomega$remove[iomega$block %in% start_block & is.na(iomega$omega1)] <- TRUE
  
  iomega_block <- iomega[start_index:end_index, ]
  
  all_indexes <- seq_len(nrow(iomega))  ## defined to save code
  iomega_pre <- iomega[all_indexes[all_indexes < start_index], ]
  iomega_post <- iomega[all_indexes[all_indexes > end_index], ]
  
  new_block <- min(iomega_block$block)
  
  ################################
  ## insert rows for covariances - match what raw_init_omega does
  
  ## add diagonals:
  
  ddiag <- expand.grid(omega1 = stats::na.omit(iomega_block$omega1),
                       omega2 = stats::na.omit(iomega_block$omega1))
  ddiag <- ddiag[ddiag$omega1 >= ddiag$omega2, ]
  
  iomega_block <- merge(ddiag, iomega_block, all = TRUE)
  diag_index <- (iomega_block$omega1 != iomega_block$omega2) %in% TRUE
  
  iomega_block$init[diag_index] <- diag_init
  iomega_block$block <- new_block
  
  block_text_rows <- data.frame(new_value = c("$OMEGA", "BLOCK", 
                                              paste0("(",length(eta_numbers),")")),
                                block = new_block,
                                line = min(iomega_block$line, na.rm = TRUE),
                                pos = 1:3)
  
  iomega_block$line <- rev(na.locf(rev(iomega_block$line)))
  
  iomega_block <- iomega_block %>% dplyr::group_by(.data$omega1) %>%
    dplyr::mutate(pos = 1:length(.data$omega1)) %>% as.data.frame
  
  iomega_block$line <- iomega_block$line + 1
  suppressWarnings({
    iomega_block$mblock[!is.na(iomega_block$mblock)] <- 
      max(c(0, max(iomega_pre$mblock, na.rm = TRUE))) + 1
  })
  
  iomega_block <- suppressWarnings(dplyr::bind_rows(block_text_rows, iomega_block))
  
  ## post will also be one line shifted
  iomega_post$line <- iomega_post$line + 1
  
  suppressWarnings({
    should_be <- unique(stats::na.omit(iomega_block$mblock))+1
    iomega_post$mblock[!is.na(iomega_post$mblock)] <- 
      iomega_post$mblock[!is.na(iomega_post$mblock)] - 
      (min(iomega_post$mblock[!is.na(iomega_post$mblock)], na.rm = TRUE) - 
         should_be)
  })
  
  if(any(!is.na(iomega_post$block))){
    iomega_post$block <- iomega_post$block - 
      (min(iomega_post$block, na.rm = TRUE) - (new_block + 1))
  }
  ################################
  iomega <- suppressWarnings(dplyr::bind_rows(iomega_pre, iomega_block, iomega_post))
  iomega <- iomega[!(iomega$remove %in% TRUE), ]
  iomega$remove <- NULL
  iomega
  
}

block <- Vectorize(block, vectorize.args = "iomega", SIMPLIFY = FALSE)

#' @rdname block-omega-sigma
#' 
#' @examples 
#' 
#' \dontrun{
#' io <- m1 %>% init_omega()
#' io <- io %>% unblock(c(2,3))
#' m1 <- m1 %>% init_omega(io)
#' m1 %>% dollar("OMEGA") ## to display $OMEGA
#' }
#' 
#' @export
unblock <- function(iomega, eta_numbers){
  
  eta_numbers <- sort(eta_numbers)
  
  if(!all(diff(eta_numbers) == 1)) stop("etas must be adjacent", call. = FALSE)
  
  start_eta <- min(eta_numbers)
  end_eta <- max(eta_numbers)
  
  start_index <- match(start_eta, iomega$omega1)
  end_index <- match(end_eta, iomega$omega1)
  
  if(is.na(start_index) | is.na(end_index)) stop("etas not found", call. = FALSE)
  
  block_to_dismantle <- iomega$block[iomega$omega1 %in% eta_numbers]
  block_to_dismantle <- unique(block_to_dismantle)
  
  if(length(block_to_dismantle) > 1) stop("etas belong to multiple blocks", call. = FALSE)
  if(length(block_to_dismantle) == 0) stop("couldn't find block", call. = FALSE)
  
  iomega_block <- iomega[iomega$block %in% block_to_dismantle,]
  
  iomega_pre <- iomega[iomega$line < min(iomega_block$line, na.rm = TRUE), ]
  iomega_post <- iomega[iomega$line > max(iomega_block$line, na.rm = TRUE), ]
  
  ## remove $OMEGA block lines
  
  iomega_block <- iomega_block[!is.na(iomega_block$omega1),]
  iomega_block <- iomega_block[iomega_block$omega1 == iomega_block$omega2, ]
  iomega_block$pos <- 1
  suppressWarnings({
    iomega_block$block <- seq_along(iomega_block$block) +
      max(c(0, max(iomega_pre$block, na.rm = TRUE)))
  })
  
  iomega_block$mblock <- max(c(1, iomega_pre$mblock), na.rm = TRUE)
  
  if(any(!is.na(iomega_pre$line))){
    iomega_block$line <- iomega_block$line + 
      max(iomega_pre$line) + 1 - min(iomega_block$line)
  }
  
  if(any(!is.na(iomega_post$line))){
    iomega_post$line <- iomega_post$line + max(iomega_block$line) + 1 - min(iomega_post$line)
  }
  
  suppressWarnings({
    should_be <- max(iomega_block$mblock, na.rm = TRUE)
    iomega_post$mblock[!is.na(iomega_post$mblock)] <- 
      iomega_post$mblock[!is.na(iomega_post$mblock)] - 
      (min(iomega_post$mblock, na.rm = TRUE) - should_be)
  })
  
  if(any(!is.na(iomega_post$block))){
    iomega_post$block <- iomega_post$block - 
      (min(iomega_post$block, na.rm = TRUE) - (max(iomega_block$block) + 1))
  }
  
  ################################
  suppressWarnings(dplyr::bind_rows(iomega_pre, iomega_block, iomega_post))  
  
}

unblock <- Vectorize(unblock, vectorize.args = "iomega", SIMPLIFY = FALSE)
