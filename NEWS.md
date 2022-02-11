# NMproject (development version)

* Removed reshape2 imports and replaced with tidyr.

* Added `check_installation` for users to be able to verify
  installations (fixes #18).

* All closed form ADVAN/TRANS combinations added to the code library. 
  Modified $ERROR so that log transformed DV and M3 models can be 
  obtained by uncommenting lines.

* Finished coding `subroutine()` function and included package tests.

* The run monitor addin now uses object highlighting - useful for when objects 
  are not in `.GlobalEnv` (e.g. they're in a `data.frame`).

* Reorders arguments for `decision` so that auto decisions can be 
  specified without explicity referring to arguments
  
* Removed future package from suggests and `async` arguments from `nm_render*()` functions.

* Added `path` argument to `write_ctl()` so control file can be written to 
  other locations.

* Added package hex sticker.

* Added `sge_parallel_execute2` for hyperthreaded environments.

# NMproject 0.6.7

* Added ability to use NMproject-specific code completion snippets `setup_code_completion()`.
  Typing `new_nm` will set up parent object, `child` will set up child object.

* fixed bug in Windows where R markdown templates weren't being produced.

* fixed minor windows bug where addins were executing models.

* `run_nm()` checks for psn installation in first use of R session

* `clean_tempfiles()` replaces `clean_run()`

# NMproject 0.6.6

* Added google verification and 'DocSearch'

# NMproject 0.6.5

* Fixed failing CRAN checks in macOS systems.

* Added run monitor RStudio Addin.

# NMproject 0.6.4

* Revamped the manual edit interface.  This is backwards incompatible.
  merge conflicts will now be flagged.
  
* Resolve manual edit 'addin' for 
  handling merge conflicts in manual edits (#17)

* `view_patch()` prints to console instead of opening window

* Better instructions on RStudio Addin usage.

* Fixed bug were `nm_create_analysis_project` would fail in windows if using
  the home directory due to a mismatch between how '~' is interpreted by R
  and `usethis`.

# NMproject 0.6.3

The first post CRAN release

## Main changes

* Revamped the NMproject project creation template and `nm_create_analysis_project()`
  for greater customisability ability and removed dependence on `options()` which 
  was causing strange behaviour.
  
* Expanded analysis README template for new NMprojects to run analysis level tests on
  build.  Useful for compliance checking.
  
* Added `nm_pre_commit_hook()` and `nm_pre_commit_hook()` for custom NMproject
  analysis projects
  
* Expanded model diagnostic rmarkdown template to include more 'xpose' plots.

* Included an experimental `%f>%` pipe for applying functions elementwise
  to vector valued nm objects.
  
* Included `is_nmproject_dir()`, intended for package owners of dependent packages

* Renamed `nm_default_dir()` to `nm_dir()`

* Removed `nm.cmd_default` option in favour of the more flexible `nm_default_fields`.
  See `?nm_default_fields()` for help.

* `decision()` uses `usethis::ui_yeah()` for prompting rather than `readline()`.

* `stage()` has `find_replace_dir_names` argument to handle custom NMproject
  directory structures.
  
## Minor changes

* Made `run_id` argument mandatory in `child()`.

* Added example code for first object creation in code library dialog boxes 
  when a model file is imported
  
* `new_nm()` will now fail if user tries to use a run based on one in the
  Models.  This is for safety as the Models directory should only contain 
  NMproject generated code.
  
* The code library will not display `.Rroj` or `README` files.
  
## Deprecated functions

* `preview()` is now deprecated and removed in favour of the code library 'Addin'

* `nm_default_dir` is now deprecated

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



