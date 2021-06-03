dp <- dplyr::bind_rows(
  dplyr::tibble(advan = 1,trans = 1,
                 base_name = c("R2T0","V2"),
                 nm_name = c("K","V"),
                 cmt = 1,
                 oral = FALSE),
  dplyr::tibble(advan = 1,trans = 2,
                 base_name = c("R2T0","V2"),
                 relation = c("R2T0*V2", NA),
                 inv_relation = c("R2T0/V2", NA),
                 nm_name = c("CL","V"),
                 cmt = 1,
                 oral = FALSE),
  dplyr::tibble(advan = 2,trans = 1,
                 base_name = c("R1T2","R2T0","V2"),
                 nm_name = c("KA","K","V"),
                 cmt = 1,
                 oral = TRUE),
  dplyr::tibble(advan = 2,trans = 2,
                 base_name = c("R1T2","R2T0","V2"),
                 relation = c(NA, "R2T0*V2", NA),
                 inv_relation = c(NA, "R2T0/V2", NA),
                 nm_name = c("KA","CL","V"),
                 cmt = 1,
                 oral = TRUE),
  dplyr::tibble(advan = 3,trans = 1,
                 base_name = c("R2T0","R2T3","R3T2","V2"),
                 nm_name = c("K","K12","K21","V"),
                 cmt = 2,
                 oral = FALSE),
  dplyr::tibble(advan = 3,trans = 3,
                 base_name = c("R2T0","R2T3","V2","V3"),
                 relation = c("R2T0*V2", "R2T3*V2", NA , "V3+V2"),
                 inv_relation = c("R2T0/V2", "R2T3/V2", NA , "V3-V2"),
                 nm_name = c("CL","Q","V","VSS"),
                 cmt = 2,
                 oral = FALSE),
  dplyr::tibble(advan = 3,trans = 4,
                 base_name = c("R2T0","R2T3","V2","V3"),
                 relation = c("R2T0*V2", "R2T3*V2", NA , NA),
                 inv_relation = c("R2T0/V2", "R2T3/V2", NA , NA),
                 nm_name = c("CL","Q","V1","V2"),
                 cmt = 2,
                 oral = FALSE),
  ## skip advan 3 trans 5/6
  dplyr::tibble(advan = 4,trans = 1,
                 base_name = c("R1T2","R2T0","R2T3","R3T2","V2"),
                 nm_name = c("KA","K","K23","K32","V2"),
                 cmt = 2,
                 oral = TRUE),
  dplyr::tibble(advan = 4,trans = 3,
                 base_name = c("R1T2","R2T0","R2T3","V2","V3"),
                 relation = c(NA, "R2T0*V2", "R2T3*V2", NA , "V3+V2"),
                 inv_relation = c(NA, "R2T0/V2", "R2T3/V2", NA , "V3-V2"),
                 nm_name = c("KA","CL","Q","V","VSS"),
                 cmt = 2,
                 oral = TRUE),
  dplyr::tibble(advan = 4,trans = 4,
                 base_name = c("R1T2","R2T0","R2T3","V2","V3"),
                 relation = c(NA, "R2T0*V2", "R2T3*V2", NA , NA),               
                 inv_relation = c(NA, "R2T0/V2", "R2T3/V2", NA , NA),               
                 nm_name = c("KA","CL","Q","V2","V3"),
                 cmt = 2,
                 oral = TRUE),
  ## add advan 11
  dplyr::tibble(advan = 11,trans = 1,
                 base_name = c("R2T0","R2T3","R3T2","R2T4","R4T2","V2"),
                 nm_name = c("K","K12","K21","K13","K31","V"),
                 cmt = 3,
                 oral = FALSE),
  dplyr::tibble(advan = 11,trans = 4,
                 base_name = c("R2T0","R2T3","V2","V3","R2T4","V4"),
                 relation = c("R2T0*V2", "R2T3*V2", NA , NA, "R2T4*V2", NA),
                 inv_relation = c("R2T0/V2", "R2T3/V2", NA , NA, "R2T4/V2", NA),
                 nm_name = c("CL","Q2","V1","V2","Q3","V3"),
                 cmt = 3,
                 oral = FALSE),
  ## add advan 12
  dplyr::tibble(advan = 12,trans = 1,
                 base_name = c("R1T2","R2T0","R2T3","R3T2","R2T4","R4T2","V2"),
                 nm_name = c("KA","K","K23","K32","K24","K42","V2"),
                 cmt = 3,
                 oral = TRUE),
  dplyr::tibble(advan = 12,trans = 4,
                 base_name = c("R1T2","R2T0","R2T3","V2","V3","R2T4","V4"),
                 relation = c(NA,"R2T0*V2", "R2T3*V2", NA , NA, "R2T4*V2", NA),
                 inv_relation = c(NA,"R2T0/V2", "R2T3/V2", NA , NA, "R2T4/V2", NA),
                 nm_name = c("KA","CL","Q3","V2","V3","Q4","V4"),
                 cmt = 3,
                 oral = TRUE),
  ## non closed form advans
  dplyr::tibble(advan = 5, trans = 1,
                 base_name = c("RXTY","VX"),
                 nm_name = c("KXY","VX"),
                 cmt = NA,
                 oral = NA),
  dplyr::tibble(advan = 6, trans = 1,
                 base_name = c("RXTY","VX"),
                 nm_name = c("KXY","VX"),
                 cmt = NA,
                 oral = NA),
  dplyr::tibble(advan = 7, trans = 1,
                 base_name = c("RXTY","VX"),
                 nm_name = c("KXY","VX"),
                 cmt = NA,
                 oral = NA),
  dplyr::tibble(advan = 8, trans = 1,
                 base_name = c("RXTY","VX"),
                 nm_name = c("KXY","VX"),
                 cmt = NA,
                 oral = NA),
  dplyr::tibble(advan = 9, trans = 1,
                 base_name = c("RXTY","VX"),
                 nm_name = c("KXY","VX"),
                 cmt = NA,
                 oral = NA),
  dplyr::tibble(advan = 13, trans = 1,
                 base_name = c("RXTY","VX"),
                 nm_name = c("KXY","VX"),
                 cmt = NA,
                 oral = NA)
)
dp <- dplyr::as_tibble(dp)

available_advans <- dplyr::group_by(dp, .data$advan, .data$trans, .data$cmt, .data$oral)
available_advans <- dplyr::summarise(available_advans, 
                                     params = paste(nm_name, collapse = ","))
available_advans <- dplyr::mutate(available_advans,
                                  label = ifelse(is.na(trans), paste0("a",advan), paste0("a",advan,"t",trans)))
available_advans <- dplyr::ungroup(available_advans)

#' @export
.available_advans <- available_advans

default_trans <- function(advan){
  sapply(advan, function(advan){
    default_trans_vec <- available_advans$trans[available_advans$advan %in% advan]
    #if(any(is.na(default_trans_vec))) return(NA) else return(1)
    return(1)
  })
}

#' Subroutine
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Makes the necessary code changes to go from one `ADVAN` (and `TRANS`) to
#' another.
#'
#' @param m An nm object.
#' @param advan Character. desired ADVAN.
#' @param trans Character. desired TRANS.
#' @param recursive Logical (default = TRUE). Internal argument, do not modify.
#'
#' @details Can only switch between subroutines listed in `available_advans`.
#'
#' @seealso [advan()]
#'
#' @examples
#' \dontrun{
#'
#' advan(m)  ## 2
#' trans(m)  ## 1
#'
#' m <- m %>% subroutine(ADVAN = 2, TRANS = 2)
#'
#' ds <- .available_advans %>%
#'   filter(oral) %>%
#'   mutate(
#'       m = m1 %>% child(run_id = label) %>%
#'         subroutine(advan = advan, trans = trans)
#'   )
#'
#' }
#' @export

subroutine <- function(m, advan = NA, trans = 1, recursive = TRUE){
  UseMethod("subroutine")
}

#' @export
subroutine.nm_generic <- function(m, advan = NA, trans = 1, recursive = TRUE){
  
  dps <- available_advans
  dps$trans[dps$trans %in% NA] <- 1
  dp$trans[dp$trans %in% NA] <- 1
  
  old_m <- m
  old_advan <- advan(m)
  old_trans <- trans(m)
  old_ctl <- ctl_contents(m)
  
  if(advan %in% old_advan & trans %in% old_trans) return(m)
  
  if(is.na(advan)) advan <- old_advan
  if(is.na(trans)) trans <- old_trans
  
  new_advan <- advan
  new_trans <- trans
  
  ## check source is valid
  if(!any(dps$advan %in% old_advan & dps$trans %in% old_trans)){
    message("advan/trans not available:")
    message("compatible combinations:")
    print(dps)
    stop("stopping...", call. = FALSE)
  }
  
  if(!any(dps$advan %in% new_advan & dps$trans %in% new_trans)){
    message("advan/trans not available:")
    message("compatible combinations:")
    print(dps)
    stop("stopping...", call. = FALSE)
  }
  
  if(new_advan %in% c(6, 7, 8, 9, 13)){ ## first to ADVAN5
    m <- m %>% subroutine(advan = 5)
    old_advan <- advan(m)
    old_trans <- trans(m)
  }
  
  m <- m %>% advan(new_advan) %>% trans(new_trans)
  
  dold <- dp %>% dplyr::filter(.data$advan %in% old_advan,
                               .data$trans %in% old_trans)
  
  dnew <- dp %>% dplyr::filter(.data$advan %in% new_advan,
                               .data$trans %in% new_trans)
  
  if(new_advan %in% 6) stop("not yet implemented")
  
  if(new_advan %in% c(5, 6, 7, 8, 9, 13)){
    
    ## grab all existing variables.
    tv_vars <- m %>% grab_variables("\\bTV\\w*?\\b")
    vars <- gsub("TV", "", tv_vars)
    
    dtrans_detect <- dp %>% 
      dplyr::group_by(.data$advan, .data$trans) %>%
      dplyr::summarise(match_trans = all(vars %in% .data$nm_name),
                       n_params = length(.data$nm_name)) %>%
      dplyr::filter(.data$match_trans)
    
    if(nrow(dtrans_detect) < 0)
      stop("can't match $PK parameters to an available advan/trans combo (available_trans)", call. = FALSE)
    
    dtrans_detect <- dtrans_detect[1, ] ## simplest
    
    old_matched_trans <- dtrans_detect$trans
    old_matched_advan <- dtrans_detect$advan
    
    ## redefine dold
    dnew <- dp %>% dplyr::filter(.data$advan %in% old_matched_advan,
                                 .data$trans %in% old_matched_trans)
    
    dnew$advan <- new_advan
    dnew$trans <- new_trans
    
    ## add KXY definitions to $PK
    
    if(!old_advan %in% c(5, 6, 7, 8, 9, 13)){
      ## not needed if already KXTY type advan
      for(i in seq_len(nrow(dold))){
        if(!is.na(dold$inv_relation[i])){
          inv_relation <- dold$inv_relation[i]
          for(j in seq_len(nrow(dold))){
            inv_relation <- gsub(dold$base_name[j], dold$nm_name[j], inv_relation)
          }
          definition_to_add <- paste0(
            gsub("R", "K", dold$base_name[i]), " = ", inv_relation, "\n"
          )
        } else {
          definition_to_add <- paste0(
            gsub("R", "K", dold$base_name[i]), " = ", dold$nm_name[i], "\n"
          )
        }
        
        m <- m %>% target("PK") %>%
          text(definition_to_add, append = TRUE) %>%
          untarget()
      } 
    }
    
  }
  
  
  if(old_advan %in% c(5, 6, 7, 8, 9, 13)){
    
    dold <- dnew
    dold$advan <- old_advan
    dold$trans <- old_trans
    
  }
  
  
  # if(0){
  #   var_K <- m %>% grab_variables("\\bK\\b")
  #   var_KA <- m %>% grab_variables("\\bKA\\b")
  #   
  #   if(length(var_K)){
  #     m <- m %>% rename_parameter_(new_name = "K20", name = "K")
  #     ## adjust dp accordingly
  #     dp$nm_name[dp$advan %in% old_advan & dp$nm_name %in% "K"] <- "K20"
  #   }
  #   
  #   if(length(var_KA)){
  #     m <- m %>% rename_parameter_(new_name = "K12", name = "KA")
  #     ## adjust dp accordingly
  #     dp$nm_name[dp$advan %in% old_advan & dp$nm_name %in% "KA"] <- "K12"
  #   }
  #   
  #   var_KXTY <- m %>% grab_variables("\\bK[0-9]+T[0-9]+\\b")
  #   
  #   if(length(var_KXTY)){ ## if any KXTY change all to KXTY notation
  #     var_KXY <- m %>% grab_variables("\\bK[0-9]{2}\\b")
  #     var_KXY_new <- gsub("\\bK([0-9])([0-9])\\b", "K\\1T\\2", var_KXY)
  #     for(i in seq_along(var_KXY)){ 
  #       m <- m %>% rename_parameter_(new_name = var_KXY_new[i],
  #                                    name = var_KXY[i])
  #     }
  #   }
  #   
  #   var_KXY <- m %>% grab_variables("\\bK[0-9]{2}\\b")
  #   if(!length(var_KXY)){
  #     var_KXY <- m %>% grab_variables("\\bK[0-9]+T[0-9]+\\b")
  #   } 
  #   
  #   ## make RXY representations
  #   var_RXY <- var_KXY
  #   var_RXY <- gsub("K", "R", var_RXY)
  #   var_RXY <- gsub("\\bR([0-9])([0-9])\\b", "R\\1T\\2", var_RXY)
  #   
  #   ##var_KXY is now either KXY or KXTY notation.
  #   ## modify dp
  #   
  #   dp_add <- dplyr::tibble(
  #     advan = new_advan,
  #     trans = 1,
  #     base_name = var_RXY,
  #     nm_name = var_KXY
  #   )
  #   
  #   dp <- dp[!dp$advan %in% new_advan, ]
  #   
  #   dp <- dplyr::bind_rows(dp, dp_add)      
  # }
  
  ## get transformed initial thetas/omegas
  ## TODO: offload into a common transform function.
  ##  this is repeating code in coef
  thetas <- raw_init_theta(m)
  thetas$init_trans <- thetas$init
  thetas$init_trans[thetas$trans %in% c("LOG","LOGODDS")] <- 
    exp(thetas$init_trans[thetas$trans %in% c("LOG","LOGODDS")])
  thetas$init_trans[thetas$trans %in% "LOGIT"] <- 
    100*1/(1+exp(-thetas$init_trans[thetas$trans %in% "LOGIT"]))
  
  omegas <- raw_init_omega(m)
  
  d <- dplyr::full_join(dold, dnew, by = "base_name")
  
  # if(new_advan %in% c(5, 6, 7, 8, 9)){
  #   ## do Volumes here
  #   ## leave names alone otherwise they're strategy="remove"
  #   volume_rows <- is.na(d$advan.y)
  #   if(any(volume_rows)){
  #     d$advan.y[volume_rows] <- unique(d$advan.y[!volume_rows])
  #     d$trans.y[volume_rows] <- unique(d$trans.y[!volume_rows])
  #     ## name same as old
  #     d$nm_name.y[volume_rows] <- d$nm_name.x[volume_rows]
  #   }
  #   
  # }
  
  ## loop through rows
  
  for(i in seq_len(nrow(d))){
    di <- d[i, ]
    strategy <- "none"
    if(is.na(di$nm_name.x) & !is.na(di$nm_name.y)) strategy <- "add_new" else {
      if(!is.na(di$nm_name.x) & is.na(di$nm_name.y)) strategy <- "remove" else {
        if(di$nm_name.x != di$nm_name.y) strategy <- "rename"
      }
    }
    if(strategy == "none") next
    if(strategy == "rename") {
      
      ## set initial estimates from current
      
      ## if going to trans 1, use inv_relation otherwise normal
      if(new_trans %in% 1){
        relation <- di$inv_relation.x
      } else {
        relation <- di$relation.y 
      }
      
      for(j in seq_len(nrow(d))){
        dj <- d[j, ]
        relation <- gsub(dj$base_name,
                         dj$nm_name.x,
                         relation) 
      }
      
      ## find initial estimates of TVK and TVV2
      
      theta_vec <- as.list(thetas$init_trans)
      names(theta_vec) <- thetas$name
      
      relation_expr <- parse(text = relation)
      new_theta <- 
        try(with(theta_vec, eval(relation_expr)), silent = TRUE)
      
      m <- m %>% rename_parameter_(new_name = di$nm_name.y,
                                   name = di$nm_name.x)
      
      if(!inherits(new_theta, "try-error")){
        ithetai <- init_theta(m)
        ithetai$init[ithetai$name == di$nm_name.y] <- new_theta
        if(ithetai$trans[ithetai$name == di$nm_name.y] %in% c("LOG", "LOGODDS"))
          ithetai$init[ithetai$name == di$nm_name.y] <- log(new_theta)
        if(ithetai$trans[ithetai$name == di$nm_name.y] %in% c("LOGIT")){
          p <- new_theta/100
          ithetai$init[ithetai$name == di$nm_name.y] <-
            log(p/(1-p))
        }
        ithetai$init[ithetai$name == di$nm_name.y] <- 
          signif(ithetai$init[ithetai$name == di$nm_name.y], 5)
        
        m <- m %>% init_theta(ithetai)
        
      }
      
    }
    if(strategy == "add_new") {
      m <- m %>% add_mixed_param(di$nm_name.y, init = 1.1)
    }
    if(strategy == "remove") {
      m <- m %>% remove_parameter(di$nm_name.x)
    }
  }
  
  ## parameters included now
  
  ##########################  
  ## ensure parameter numbering is correct
  
  m <- m %>% update_variable_in_text_numbers("THETA(", ")")
  m <- m %>% update_variable_in_text_numbers("ETA(", ")")
  ## syncronise MUs with ETAs
  m <- m %>% gsub_ctl("MU_[0-9]+(\\s*\\+\\s*)ETA\\(([0-9]+)\\)",
                      "MU_\\2\\1ETA\\(\\2\\)")
  
  ##########################
  
  if(new_advan %in% c(5, 6, 7, 8, 9, 13)){
    
    R_regex <- "R([0-9]+)T([0-9]+)"
    R_names <- dnew$base_name[grepl(R_regex, dnew$base_name)]
    
    comp_from <- gsub(R_regex, "\\1", R_names)
    comp_to <- gsub(R_regex, "\\2", R_names)
    comp_from <- as.numeric(comp_from)
    comp_to <- as.numeric(comp_to)
    
    n_compartments <- max(c(comp_from, comp_to))
    
    if(any(grepl("\\s*\\$MODEL", text(m)))){
      ## there's already a $MODEL remove it
      
      models_text <- m %>% dollar("MODEL")
      ## look for number of = signs
      
      models_text <- paste(models_text, collapse = "\\s")
      models_text <- strsplit(models_text, "\\s")[[1]]
      n_current_compartments <- 
        length(which(grepl("\\=", models_text)))
      
      if(!old_advan %in% c(5, 6, 7, 8, 9, 13)){
        
        if(n_current_compartments != n_compartments){
          m <- m %>% delete_dollar("MODEL") 
        }
      }
    } else {
      ## no $MODEL, create
      models_text <- paste0("COMP = (COMP", seq_len(n_compartments), ")")
      models_text <- c("$MODEL", models_text)
      
      if(!old_advan %in% c(5, 6, 7, 8, 9, 13)){
        
        m <- m %>% insert_dollar("MODEL", models_text, 
                                 after_dollar = "SUB")
      }
      
    }
    
    if(new_advan %in% c(6, 7, 8, 9, 13)){ ## insert $DES
      
      lhs <- paste0("DADT(",seq_len(n_current_compartments),")")
      
      basic_param_names <- R_names
      basic_param_names <- gsub("R([0-9]+)T([0-9]+)", "K\\1T\\2", 
                                basic_param_names)
      
      d_param <- data.frame(
        name = basic_param_names,
        comp_from,
        comp_to
      )
      
      rhs <- sapply(seq_len(n_current_compartments), function(comp){
        
        positive_terms <- 
          paste(d_param$name[d_param$comp_to %in% comp], collapse = " + ")
        
        negative_terms <- 
          paste(d_param$name[d_param$comp_from %in% comp], collapse = " -")
        
        if(nchar(negative_terms) > 0) negative_terms <- paste0("-", negative_terms)
        
        if(nchar(positive_terms) + nchar(negative_terms) < 80){
          paste(positive_terms, negative_terms)
        } else {
          paste(positive_terms, "\n", negative_terms)
        }
      })
      
      des_text <- paste(lhs, " = ", rhs)
      des_text <- c("$DES", des_text)
      
      m <- m %>% delete_dollar("DES")
      
      m <- m %>% insert_dollar("DES", des_text, after_dollar = "PK")
      
    } else {
      if(any(grepl("\\s*\\$DES", text(m))))
        m <- m %>% delete_dollar("DES")
    }
    
  } else {
    
    if(any(grepl("\\s*\\$MODEL", text(m))))
      m <- m %>% delete_dollar("MODEL")
    
    if(any(grepl("\\s*\\$DES", text(m))))
      m <- m %>% delete_dollar("DES")
    
  }
  
  ## update $DES if present
  
  m
}
#' @export
subroutine.nm_list <- Vectorize_nm_list(subroutine.nm_generic, SIMPLIFY = FALSE)

#' Get/set $SUBROUTINE values in control file
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#'
#' These are mostly back end functions used by [subroutine()] and will make
#' simple ADVAN/TRANS/TOL adjustments to the NONMEM control file.  No other file
#' changes outside $SUBROUTINE will be made which makes `advan` and `trans` less
#' useful than the higher level [subroutine()] function.
#'
#' @param m An nm object.
#' @param text Optional number/character number to set to.
#' @seealso [subroutine()]
#' @name dollar_subroutine
#' @export
advan <- function(m, text){
  UseMethod("advan")
}
#' @export
advan.nm_generic <- function(m, text){
  if(missing(text)){
    sub_text <- m %>% dollar("SUB")
    if(is_single_na(sub_text)) return(NA_integer_)
    advan_match_text <- ".*\\bADVAN([0-9]+)\\b.*"
    advan_match <- grepl(advan_match_text, sub_text)
    advan <- gsub(advan_match_text, "\\1", sub_text[advan_match])
    advan <- as.integer(advan)    
    return(advan)
  } 
  old_target <- target(m)
  m <- m %>% target("SUB") %>%
    gsub_ctl("ADVAN[0-9]+", paste0("ADVAN", text)) %>% 
    target(old_target)
  
  if(text %in% c(6, 8, 9)){ ## $DES 
    m <- m %>% tol(7)
  }
  if(text %in% c(13)){ ## $DES 
    m <- m %>% tol(12)
  }
  if(!text %in% c(6, 8, 9, 13)){
    m <- m %>% tol(NA)
  }
  
  m
}
#' @export
advan.nm_list <- Vectorize_nm_list(advan.nm_generic)

#' @rdname dollar_subroutine
#' @export
trans <- function(m, text){
  UseMethod("trans")
}
#' @export
trans.nm_generic <- function(m, text){
  if(missing(text)){
    sub_text <- m %>% dollar("SUB")
    if(is_single_na(sub_text)) return(NA_integer_)
    trans_match_text <- ".*\\bTRANS([0-9]+)\\b.*"
    trans_match <- grepl(trans_match_text, sub_text)
    trans <- gsub(trans_match_text, "\\1", sub_text[trans_match])
    trans <- as.integer(trans)
    if(length(trans) == 0) {
      base_advans <- available_advans[!duplicated(available_advans$advan), ]
      base_trans <- base_advans$trans[base_advans$advan %in% advan(m)]
      trans <- base_trans
    }
    if(is.na(trans)) trans <- 1
    return(trans)
  }
  old_target <- target(m)
  m <- m %>% target("SUB") 
  
  if(any(grepl("TRANS", text(m)))){ ## TRANS already exists
    m <- m %>% gsub_ctl("TRANS\\s?\\=?\\s?[0-9]+", paste0("TRANS", text))
  } else { ## append
    
    new_text <- text(m)
    blanks <- grepl("^\\s*$", new_text)
    
    new_text[max(which(!blanks))] <- 
      paste0(new_text[max(which(!blanks))], " TRANS", text)
    
    new_text <- gsub("\\s+", " ", new_text)
    
    m <- m %>% text(new_text)
  }
  
  ## if not 
  m <- m %>% 
    gsub_ctl("\\s*TRANS(NA|1)", "") %>%
    target(old_target)
  m
}
#' @export
trans.nm_list <- Vectorize_nm_list(trans.nm_generic)

#' @rdname dollar_subroutine
#' @examples
#' \dontrun{
#' 
#' tol(m1) ## display current TOL value
#' 
#' ## set tol to 12
#' m1 <- m1 %>% tol(12)
#' 
#' }
#' @export
tol <- function(m, text){
  UseMethod("tol")
}

#' @export
tol.nm_generic <- function(m, text){
  if(missing(text)){
    sub_text <- m %>% dollar("SUB")
    if(is_single_na(sub_text)) return(NA_integer_)
    tol_match_text <- ".*\\bTOL([0-9]+)\\b.*"
    tol_match <- grepl(tol_match_text, sub_text)
    tol <- gsub(tol_match_text, "\\1", sub_text[tol_match])
    tol <- as.integer(tol)  
    if(length(tol) == 0) tol <- NA
    return(tol)
  } 
  old_target <- target(m)
  m <- m %>% target("SUB")
  
  if(any(grepl("TOL", text(m)))){ ## TOL already exists
    if(!is.na(text)){
      m <- m %>% gsub_ctl("TOL[0-9]+", paste0("TOL", text)) 
    } else {
      m <- m %>% gsub_ctl("TOL[0-9]+", "") 
    }
  } else {
    if(!is.na(text)){
      existing_text <- text(m)
      existing_text[grepl("ADVAN", existing_text)] <- 
        paste(existing_text[grepl("ADVAN", existing_text)], paste0("TOL", text))
      m <- m %>% text(existing_text)
    } 
  }
  
  m <- m %>% target(old_target)
  m
}
#' @export
tol.nm_list <- Vectorize_nm_list(tol.nm_generic)

