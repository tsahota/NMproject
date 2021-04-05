#' ## All this is deprecated
#' 
#' #' @export
#' parent_old <- function(m, db){
#'   
#'   if(missing(db)) stop("requires a database containing the parent object")
#'   parent_run_id <- parent_run_id(m)
#'   if(length(parent_run_id) > 1) stop("can only get parent for one run at a time")
#'   if(is.na(parent_run_id)) stop("no parent id in object")
#'   
#'   run_ids_in_db <- run_id(db$table$m)
#'   matched_run_ids_in_db <- which(run_ids_in_db %in% parent_run_id)
#'   
#'   if(length(matched_run_ids_in_db) > 1) stop("more than one matching parent in database")
#'   if(length(matched_run_ids_in_db) == 0) stop("parent run does not exist in database")
#'   
#'   db$table$m[matched_run_ids_in_db]
#'   
#' }
#' 
#' #' @export
#' nm_db <- R6::R6Class("nm_db", list(
#'   db = tibble::tibble(quo = list(),
#'                       m = list(),
#'                       env = list(),
#'                       expr = character(),
#'                       object_name = character()),
#'   file_name = "nm_db_default.RDS",  ## should not be a defautl value
#'   initialize = function(file_name){
#'     stopifnot(is.character(file_name), length(file_name) == 1)
#'     if(tools::file_ext(file_name) != "RDS") 
#'       stop("file_name needs to have an .RDS extension", call. = FALSE)
#'     self$file_name <<- file_name
#'   },
#'   register = function(m){
#'     browser()
#'     m_quo <- rlang::enquo(m)
#'     m_expr <- rlang::quo_get_expr(m_quo)
#'     m_env <- rlang::quo_get_env(m_quo)
#'     
#'     d_new <- tibble::tibble(quo = list(m_quo),
#'                             m = list(rlang::eval_tidy(m_quo)),
#'                             expr = list(m_expr),
#'                             env = list(m_env),
#'                             object_name = deparse(m_expr))
#'     d_new <- rbind(d_new, self$db)
#'     
#'     ## if(same object in same environment) remove old
#'     d_new <- d_new[!(duplicated(d_new$quo)), ]
#'     self$db <<- d_new
#'     self$save()
#'   },
#'   refresh = function(){
#'     ## do not delete if variable not found
#'     self$db$m <<- lapply(seq_along(self$db$quo), function(i){
#'       quo <- self$db$quo[[i]]
#'       result <- try(rlang::eval_tidy(quo), silent = TRUE)
#'       if(inherits(result, "try-error")) result <- self$db$m[[i]]
#'       result
#'     })
#'     message("database synced with objects")
#'   },
#'   save = function(){
#'     self$refresh()
#'     saveRDS(self, file = self$file_name)
#'     message("database saved in ", self$file_name)
#'   },
#'   repopulate = function(overwrite = FALSE, elements = FALSE){
#'     db <- self$db
#'     for(i in seq_len(nrow(db))){
#'       object_name <- db$object_name[[i]]
#'       env <- db$env[[i]]
#'       
#'       if(grepl("\\$", object_name) & !elements){
#'         warning("Not going to repopulate ", object_name," elements. Do elements=TRUE to force") 
#'       } else {
#'         new_ob <- try(eval(rlang::expr(!!db$expr[[!!i]]), envir = env), silent = TRUE)      
#'         if(!is_try_nm_list(new_ob) | overwrite){
#'           
#'           eval(rlang::expr(!!db$expr[[!!i]] <- !!(db$m[[i]])), envir = env)
#'           
#'           new_ob <- try(eval(rlang::expr(!!db$expr[[!!i]]), envir = env), silent = TRUE)
#'           if(!is_try_nm_list(new_ob)){
#'             message("couldn't repopulate: ", object_name)
#'           } else {
#'             message("repopulated: ", object_name)
#'           }
#'         }        
#'       }
#'     }
#'   },
#'   print = function(...){
#'     table <- self$table
#'     if(nrow(table) == 0) {
#'       message("database empty. Add nm objects with .$register([nm_list])")
#'     } else {
#'       message(".$table contents:")
#'       print(table) 
#'     }
#'     invisible(self)
#'   }
#' ), active = list(
#'   raw_table = function(value){
#'     if(missing(value)){
#'       db <- self$db
#'       db$run_id <- lapply(db$m, run_id)
#'       db$type <- lapply(db$m, type)
#'       db$run_in <- lapply(db$m, run_in)
#'       db
#'     } else {
#'       stop("cannot set value")
#'     }
#'   },
#'   table = function(value){
#'     if(missing(value)){
#'       db <- self$raw_table
#'       if(nrow(db) == 0) return(tibble::tibble())
#'       db$row <- seq_len(nrow(db))
#'       db <- by(db, db$row, function(d){
#'         object_expr <- lapply(seq_along(d$m[[1]]), function(i) {
#'           new_expr <- rlang::expr("[["(!!d$expr[[1]],!!i))
#'           rlang::quo_set_expr(quo = d$quo[[1]], expr = new_expr)
#'         })
#'         object_name <- sapply(seq_along(d$m[[1]]), function(i) {
#'           new_expr <- rlang::quo_get_expr(object_expr[[i]])
#'           deparse(new_expr)
#'         })
#'         tibble::tibble(raw_table_row = d$row,
#'                        object_name = object_name,
#'                        object_expr = object_expr,
#'                        m = d$m[[1]])
#'       })
#'       d <- tibble::as_tibble(do.call(rbind, db))
#'       d
#'     } else {
#'       stop("cannot set value")
#'     }
#'   },
#'   table_full = function(value){
#'     if(missing(value)){
#'       db <- self$table
#'       dplyr::bind_cols(db, nm_row(db$m))
#'     } else {
#'       stop("cannot set value")
#'     }
#'   } 
#' ))
#' 
#' 
#' #' @export
#' load_db <- function(file_name){
#'   readRDS(file_name)
#' }