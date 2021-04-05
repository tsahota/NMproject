script_dirs1 <- tidyproject::ls_script_dirs("/projects/qcp/QCP_MODELING/", depth = 3)
script_dirs2 <- tidyproject::ls_script_dirs("/projects/qcp/QCP_MODELING/", depth = 4)

script_dirs <- c(script_dirs1, script_dirs2)

scripts <- tidyproject::ls_scripts(script_dirs)

try_readLines <- function(...){
  text <- try(readLines(...), silent = TRUE)
  if(inherits(text, "try-error")) return(character())
  text
}

text <- suppressWarnings(parallel::mclapply(scripts, try_readLines, mc.cores = 10))

pkgs <- parallel::mclapply(text, function(text){
  #lapply(text, function(text){
  text <- try(parse(text = text), silent = TRUE)
  if(inherits(text, "try-error")) return(NA)
  ans <- unique(unlist(lapply(text, tidyproject:::recursive_lib_find)))
  ans
#})
}, mc.cores = 10)


pkgs_with_dep <- parallel::mclapply(text, function(text){
  #lapply(text, function(text){
  text <- try(parse(text = text), silent = TRUE)
  if(inherits(text, "try-error")) return(NA)
  ans <- unique(unlist(lapply(text, tidyproject:::recursive_lib_find)))
  c(ans, gtools::getDependencies(ans))
#})
}, mc.cores = 10)

tab <- table(unlist(pkgs))
tab <- sort(tab, decreasing = TRUE)
d <- dplyr::tibble(name = names(tab), count = tab)
write.csv(d, file = "~/QCP_MODELLING_package_use.csv", quote = FALSE, row.names = FALSE)

tab <- table(unlist(pkgs_with_dep))
tab <- sort(tab, decreasing = TRUE)
d <- dplyr::tibble(name = names(tab), count = tab)
write.csv(d, file = "~/QCP_MODELLING_package_use_with_dep.csv", quote = FALSE, row.names = FALSE)

