# NMproject 0.6.2

This is the first CRAN release of NMproject.  Here are the major changes here 
relative to 0.5.1:

* Removal and deprecation of tidyproject as an NMproject dependency.

* Project libraries are now handled with `renv`.

* Analysis project creation has been moved from tidyproject to NMproject with
  light wrappers around `usethis` project creation functionality and RStudio
  project templates.  Analysis projects have a package-like structure.
  
* Expanded demo based on a theophylline pharmacometric analysis and new vignette
  based around same demo.
  
* The code library has been moved to NMproject.  New shiny interface to code 
  library available via RStudio 'Addins'.  The code library has been restructured
  to match NMproject analysis directory structure.
  
* `new_nm()` function to create parent `nm_list` objects.  This is mostly for
  creation of your first model.  Subsequent models are generated with the `child()`
  function.

* New manual edit interface via RStudio 'Addins'.  NMproject is now the only 
  NONMEM interface (known to the author) that tracks manual edits to NONMEM code
  in the form of reusable patches.

* Fully vectorised functions and compatibility of `nm_list` objects with 
  `dplyr`.

* Custom NMproject implementations of PsN's bootstrap, cross-validation, 
  stepwise covariate selection, simulation-re-estimation, and PPCs have been
  added.
  
* "overwrite behaviour" RStudio 'Addin' has been created to allow different
  behaviours when modifying and rerunning a previously executed run. The
  default is to ask for confirmation.

# NMproject 0.5.1 (beta interface pre-release)

This is the last version of NMproject to be developed at AZ. It has is a 
developmental version of the newer "beta"" interface.

# NMproject 0.3.2 (alpha)

This is the last release of the alpha interface developed at AZ.



