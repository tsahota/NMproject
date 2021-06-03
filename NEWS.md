# NMproject 0.6.0

This is the first CRAN release of NMproject.  Here are the major changes here 
relative to 0.5.1:

* Deprecation and removal of tidyproject dependency. Project libraries are now
  handled with `renv` and project creation with `usethis` packages.  The code 
  library has been moved to NMproject.
  
* New shiny interface to code library available via RStudio addins.

* `new_nm()` function to create parent `nm_list` objects.

* New manual edit interface via RStudio addins.  NMproject is the only NONMEM
  interface (known to the author) that tracks manual edits to NONMEM code in 
  the form of reusable patches.

* Fully vectorized functions and compatibility of `nm_list` objects with 
  `dplyr`. NMproject is the only R package (known to the author) with a 
  vectorized model object allowing groups of runs to be operated on using the 
  same syntax as single runs.

* Custom NMproject implementations of PsN's bootstrap, cross-validation, 
  stepwise covariate selection, simulation-re-estimation, and PPCs. All 
  controllable and subsettable via vectorized syntax.

# NMproject 0.5.1 (beta interface pre-release)

This is the last version of NMproject to be developed at AZ. It has is a 
developmental version of the newer "beta"" interface.

# NMproject 0.3.2 (alpha)

This is the last release of the alpha interface developed at AZ.



