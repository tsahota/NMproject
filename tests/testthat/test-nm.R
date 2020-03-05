require(tidyproject)
context("nm function works")

proj_name <- "test_nmproject"

cleanup <- function(proj_name){
  if(file.exists(proj_name)) unlink(proj_name,recursive = TRUE,force = TRUE)
  base_proj_name <- paste0(proj_name,".git")
  if(file.exists(base_proj_name)) unlink(base_proj_name,recursive = TRUE,force = TRUE)
}

test_that("nm function works",{

  currentwd <- getwd()
  make_project(proj_name)
  on.exit({
    setwd(currentwd)
    cleanup(proj_name)
  })

  setwd(proj_name)

  # copy_control("NONMEM/ADVAN2.mod","run1.mod")
  # 
  # m1 <- nm("execute run1.mod -dir=1")

})

