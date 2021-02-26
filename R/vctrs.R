#library(vctrs)
if(0){
  new_nm_generic <- function(run_id = character(),
                             run_in = character()){
    
    vctrs::vec_assert(run_id, character())
    vctrs::vec_assert(run_in, character())
    
    m <- vctrs::new_rcrd(list(
      "run_id" = run_id,
      "run_in" = run_in
    ), class = "nm_generic2")
    
    m
  }
  
  format.nm_generic2 <- function(x, ...){
    cat(utils::capture.output(str(vctrs::vec_data(x), ...)), sep = "\n")
    invisible()
  }
  
  new_nm_generic()
  
  new_nm_generic(run_id = "2", run_in = "lksdjf")
  
  x <- nm_generic("3")
  class(x) <- NULL
  x$glue_fields <- list(x$glue_fields)
  x2 <- vctrs::new_rcrd(x, class = "nm_generic3")
  
  format.nm_generic3 <- function(x, ...){
    cat(utils::capture.output(str(vctrs::vec_data(x), ...)), sep = "\n")
    invisible()
  }
  
  x2
  c(x2, x2)
  c(x2, x2)[[2]]
  
  
  ## can i get the following working?
  vec_unchop(list(nm(1:2)), list(2:1))
  
  ## this works
  vec_unchop(list(nm_list2list(nm(1:2))), list(2:1))
  
  a <- nm(1:2)
  class(a) <- c("nm_list", "list")
  vec_unchop(list(a), list(2:1)) ## error
  
  a <- nm(1:2)
  class(a) <- c("dummy", "list")
  vec_unchop(list(a), list(2:1))
  
  ## there is something in nm_list methods that screws this up
  
  a <- nm(1:2)
  class(a) <- c("nm_list", "list")
  vctrs::vec_unchop(list(a), list(2:1)) ## error
  
  a_mini <- as_nm_generic(a[[1]])
  c(a_mini, a_mini)
  
  
}
