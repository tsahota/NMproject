#' Get/set path to dataset
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Mainly used to associate a dataset with an nm object.
#' Requires ctl_contents to already be specified.
#' 
#' @param m An nm object.
#' @param text Optional character. Path to input dataset.
#' 
#' @return if text is not specified, will return the data_path name
#'  otherwise will set data_path to the text provided
#' 
#' @examples 
#' \dontrun{
#' 
#' m1 <- new_nm(run_id = "m1",
#'              based_on = "staging/Models/ADVAN2.mod",
#'              data_path = "DerivedData/data.csv") %>%
#'       
#' data_path(m1)  ## display data name
#' 
#' }
#' 
#' @export
data_path <- function(m, text){
  UseMethod("data_path")
}
#' @export
data_path.nm_generic <- function(m, text){
  if(missing(text)){
    if(length(m[["data_path"]]) > 0) {
      return(custom_1d_field(m, "data_path")) 
    } else { 
      return(NA_character_)
    }
  }
  m <- m %>% custom_1d_field(field = "data_path", replace = text, glue = TRUE)
  
  if(!is.na(data_path(m))){
    ## update ctl contents (if it exists)
    if(!is_single_na(ctl_contents(m)))
      m <- m %>% fill_dollar_data(text)
  }
  
  m
}
#' @export
data_path.nm_list <- Vectorize_nm_list(data_path.nm_generic)

fill_dollar_data <- function(m, data_name){
  old_target <- m %>% target()
  m <- m %>% target("$DATA")
  
  data_name <- relative_path(data_name, run_in(m))
  m <- m %>% gsub_ctl("^(\\s*\\$DATA\\s+)\\S+(.*)$",paste0("\\1",data_name,"\\2"))
  
  m <- m %>% target(old_target)
  m
}

#' Fill $INPUT
#'
#' @description
#'
#' `r lifecycle::badge("stable")`
#'
#' Uses dataset to automatically fill $INPUT in control file.
#'
#' @param m An nm object.
#' @param ... Either `keep`, `drop`, or `rename` arguments.  See examples.
#'
#' @details If a new dataset with different columns is assigned to an `nm`
#'   object, `$INPUT` will not be correct and so it may necessary to apply
#'   `fill_input()` again.
#'
#'   See examples for how to use `drop` and `rename` arguments to control how
#'   `$INPUT` is written.
#'   
#' @examples
#' \dontrun{
#'
#'  m1 <- m1 %>% fill_input() 
#'  m1 %>% dollar("INPUT") ## view $INPUT
#'
#'  ## following will will drop the "RATE" column
#'  m1 <- m1 %>% fill_input(drop = "RATE")
#'  m1 %>% dollar("INPUT")
#'
#'  ## following will rename "DATE" to be "DAT0"
#'  m1 <- m1 %>% fill_input(rename = c("DAT0" = "DATE"))
#'  m1 %>% dollar("INPUT") ## view $INPUT
#'
#'
#' }
#' @export
fill_input <- function(m, ...){
  UseMethod("fill_input")
}
#' @export
fill_input.nm_generic <- function(m, ...){
  ctl <- ctl_contents(m)
  d <- suppressMessages(input_data(m))
  replace_with <- c("$INPUT", suppressMessages(dollar_input(d, ...)))
  old_target <- m %>% target()
  m <- m %>% target("INPUT") %>% text(replace_with) %>%
    target(old_target)
  m
}
#' @export
fill_input.nm_list <- Vectorize_nm_list(fill_input.nm_generic, SIMPLIFY = FALSE)

#' Delete a NONMEM subroutine from control file contents
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' @param m An nm object.
#' @param dollar Character. Name of subroutine.
#' @export
delete_dollar <- function(m, dollar){
  UseMethod("delete_dollar")
}
#' @export
delete_dollar.nm_generic <- function(m, dollar){
  ctl <- m %>% ctl_contents()
  dollar_text <- gsub("\\$","",dollar)
  ctl[[dollar_text]] <- NULL
  m <- m %>% ctl_contents_simple(ctl)
  m
}
#' @export
delete_dollar.nm_list <- Vectorize_nm_list(delete_dollar.nm_generic, SIMPLIFY = FALSE)

#' Insert a new subroutine into control file_contents
#' 
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Mostly a back end function used by other functions.
#' 
#' @param m An nm object.
#' @param dollar Character. Name of subroutine to insert.
#' @param text Character vector. Text to fill.
#' @param after_dollar Character name of preceding subroutine. The new
#'   subroutine will be inserted immediately after it.
#'   
#'   
#' @examples 
#' 
#' \dontrun{
#' 
#' m <- m %>% insert_dollar("MODEL", "
#' $MODEL
#' COMP = (CENTRAL)
#' ", after_dollar = "SUB")
#'                                  
#' }
#' 
#' @export
insert_dollar <- function(m, dollar, text, after_dollar){
  UseMethod("insert_dollar")
}
#' @export
insert_dollar.nm_generic <- function(m, dollar, text, after_dollar = NA){
  ctl <- m %>% ctl_contents()
  
  dollar_text <- gsub("\\$","",dollar)
  text <- setup_dollar(text, paste0("$", dollar_text), add_dollar_text = FALSE)
  text <- list(text)
  names(text) <- dollar_text
  
  save_attributes <- attributes(ctl)
  if(!is.na(after_dollar)) {
    after_dollar <- gsub("\\$","",after_dollar)
    after <- match(after_dollar, names(ctl)) 
    ctl <- append(ctl, text, after)
  } else {
    ctl <- append(ctl, text)
  }
  save_names <- names(ctl)
  attributes(ctl) <- save_attributes
  names(ctl) <- save_names
  
  m <- m %>% ctl_contents_simple(ctl)
  m
}
#' @export
insert_dollar.nm_list <- Vectorize_nm_list(insert_dollar.nm_generic, SIMPLIFY = FALSE)

#' Get/set existing subroutine
#'
#' @description 
#' 
#' `r lifecycle::badge("stable")`
#'
#' The fast way to see the contents of a particular subroutine directly in the R
#' console. It can also be used to set the contents of a NONMEM subroutine in
#' place of manual edits
#'
#' @param m An nm object.
#' @param dollar Character. Name of NONMEM subroutine to target.
#' @param ... Additional arguments to be passed to `text()`.  If specified these
#'   will set the contents of the subroutine.  See examples below.
#' @param add_dollar_text Logical (default = `TRUE`). Should the $XXXX string be
#'   added to text.
#'   
#' @seealso [insert_dollar()], [delete_dollar()]
#'
#' @examples
#' \dontrun{
#'
#' m1 %>% dollar("PK")  ## displays existing $PK
#'
#' m1 <- m1 %>% dollar("DES",
#' "
#' DADT(1) = -K*A(1)
#' ")
#' 
#' ## This will rewrite an existing $DES
#' ## if control file doesn't already have a $DES
#' ## use insert_dollar() instead
#'
#' }
#'
#' @export
dollar <- function(m, dollar, ..., add_dollar_text = TRUE) {
  orig_target <- m %>% target()
  ans <- m %>% target(dollar) %>% 
    text(..., add_dollar_text = add_dollar_text)
  if(is_nm_list(ans)) ans <- ans %>% target(orig_target)
  ans
}


