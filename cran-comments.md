## Resubmission
This is a resubmission. In this version I have:

* Added \value entries to all .Rd files in package
* Removed examples from non-exported functions.
* Removed most instances of \dontrun in .Rd files so that most can be run.  
  In instances where a third party software is needed (NONMEM) the \dontrun tag 
  is retained and a comment is added to explain that NONMEM is needed for this 
  segment of code.  For interactive examples \dontrun is replaced with 
  if(interactive()).
* Expanded Description field of the package.
* Removed options(warn=-1) statements.
* Restrained vignettes and tests to use a single core.
* I cannot see where I am modifying .GlobalEnv. I have checked the global 
  environment with ls(envir = .GlobalEnv, all.names = TRUE) and cannot see
  anything. Apologies if I'm missing something.
* Added Nuria and Stein as contributors in DESCRIPTION
* I have contacted Peter Lawrence via LinkedIn for his personal details/email
  and inclusion as contributor and (co)author to the file: 
  inst/extdata/CodeLibrary/R/AUC.R. However he has requested he be removed 
  stating "I am now retired and have no contribution to make in this area and
  very little recollection of the AUC code you refer to". I have therefore
  removed his name at his request from the author field of the file in 
  inst/extdata and not included his name in the DESCRIPTION.
  

## Test environments
* local windows R installation, R 4.0.3
* local ubuntu 20.04 R installation, R 4.0.5
* ubuntu 16.04.6 (on travis-ci), R 4.0.2
* windows (on appveyor), R 4.1.0
* win-builder (release, oldrelease and devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Downstream dependencies

There are no downstream dependencies.
