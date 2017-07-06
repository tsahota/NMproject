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

  copy_control("NONMEM/ADVAN2.mod","run1.mod")

  m1 <- nm("execute run1.mod -dir=1")

  expect_true(m1$type=="execute")
  expect_true(is.character(m1$run_id))
  expect_true(is.character(m1$run_dir))
  expect_true(identical(as.numeric(m1$run_id),1))

  m2 <- nm("qsubmissionscript -opt1 -opt2 -- execute run1.mod -dir=1")
  expect_true(m2$type=="execute")
  expect_true(is.character(m2$run_id))
  expect_true(is.character(m2$run_dir))
  expect_true(identical(as.numeric(m2$run_id),1))

  write(c("## Description: klm",
          "execute run1.mod -dir=1"),
        file = file.path(models_dir(),"run1.sh"))

  m3 <- nm("qsubmissionscript -opt1 -opt2 run1.sh")
  expect_true(m3$type=="execute")
  expect_true(is.character(m3$run_id))
  expect_true(is.character(m3$run_dir))
  expect_true(identical(as.numeric(m3$run_id),1))

  m4 <- nm("qsubmissionscript -opt1 -opt2 shell=run1.sh")
  expect_true(m4$type=="execute")
  expect_true(is.character(m4$run_id))
  expect_true(is.character(m4$run_dir))
  expect_true(identical(as.numeric(m4$run_id),1))

  expect_error(nm("qsubmissionscript -opt1 -opt2 run2.sh"))

  m5 <- nm("qsubmissionscript -opt1 -opt2",shell_script_name="run1.sh")
  expect_true(m5$type=="execute")
  expect_true(is.character(m5$run_id))
  expect_true(is.character(m5$run_dir))
  expect_true(identical(as.numeric(m5$run_id),1))

})

