# NMproject

[![Travis-CI Build Status](https://travis-ci.org/tsahota/NMproject.svg?branch=master)](https://travis-ci.org/tsahota/NMproject)
[![Coverage Status](https://coveralls.io/repos/github/tsahota/NMproject/badge.svg?branch=master)](https://coveralls.io/github/tsahota/NMproject?branch=master)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tsahota/NMproject?branch=master&svg=true)](https://ci.appveyor.com/project/tsahota/NMproject)

Script based NONMEM execution on tidyprojects.  NMproject was previously an AstraZeneca project.  It is currently being reimplemented here as a community version to be compatible with a variety of architectures (standalone NONMEM and a variety of grid submission systems)
 
## Installation

```R
install.packages("devtools")
devtools::install_github("tsahota/NMproject")
```

## Quick setup

Define options specific to your installation in your user `~/.Rprofile` (or `$R_HOME/etc/Rprofile.site` if you want this to apply for all users). See FAQ for specific options.

To avoid having to start a code library from scratch, download: https://github.com/tsahota/PMXcodelibrary to a directory and add the following command to your `~/.Rprofile` (or `$R_HOME/etc/Rprofile.site`)

```r
options(code_library_path = "/path/to/PMXcodelibrary/")
```

### Instructions

* NMprojects are directories where you can work on pharmacometric analysis.
* First, set up a tidyproject with the `make_project("/path/to/project/dir"")`.
   * See tutorial at https://github.com/tsahota/tidyproject to get started with tidyproject
* Open the NMproject with the File -> Open Project menu items. NOTE: always use Rstudio to open an NMproject, never just `setwd()` to the directory.

* Install the package NMproject into the *project library* by simply installing again from with the NMproject:

```r
devtools::install_github("tsahota/NMproject")
library(NMproject)
```

View the built in code library:

```r
code_library()
```

Preview code:

```r
preview("NONMEM/ADVAN2.mod")
```

Copy it into your "Models" directory

```r
copy_control("NONMEM/ADVAN2.mod","run1.mod")
```

NOTE: default behaviour is to enforce the `runXX.mod` naming convention for control streams.  e.g. `run1.mod`, `run1vpc.mod`, `run2.mod`.

Set up a run log with:

```r
copy_script("R/nm.log.R")
```

To create NONMEM (this will not run NONMEM yet)

```r
mod1 <- nm("execute run1.mod -dir=1")
```

NOTE: the `-dir` option must be specified

If you have set up the `path.nm_tran` option (see FAQ below), you can do an NMTRAN check

```r
nm_tran(mod1)
```

To run `mod1`:

```r
run(mod1)
```

To view all runs in database:

```r
shiny_run_table()
```

To monitor a single run:

```r
shiny_run_monitor(mod1)
```


### FAQ

### + How do I set up NMproject to run jobs on a cluster via ssh?

Create a system_nm option in your `~.Rprofile` to ssh to the server and run the command, e.g.

```r

options(system_nm=function(cmd,...) {
        system(paste0("ssh -q clustername \"cd $(pwd); ",cmd,"\""),...)
})

```

### + How do can run an NMTRAN check on my control stream without the full NONMEM job?

Create a path.nm_tran option in your `~.Rprofile` with the location of nmtran, e.g.

```r
options(path.nm_tran = "path/to/nonmem/installation/tr/NMTRAN.exe")

```



