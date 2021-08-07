## Resubmission
This is a resubmission. In this version I have:

* Added \value entries to all .Rd files in package
* Added Nuria and Stein as contributors
* I have contacted Peter Lawrence via LinkedIn for
  his personal details/email and inclusion as contributor 
  and (co)author to the file: 
  inst/extdata/CodeLibrary/R/AUC.R. However he has
  requested he be removed stating "I am now retired 
  and have no contribution to make in this area and
  very little recollection of the AUC code you refer
  to". I have therefore removed his name from the
  roxygen @author field of the file and not included
  his name in the DESCRIPTION.
* Expanded Description field of the package.
* Removed options(warn=-1) statements.
* Removed examples from non-exported functions.
* Restrained multi-core to use a single core in the vignettes 
  and tests.
* Apologies, I cannot see where I am modifying .GlobalEnv. I 
  have checked the global environment with 
  ls(envir = .GlobalEnv, all.names = TRUE)
  

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
