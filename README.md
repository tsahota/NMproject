# NMproject

[![Travis-CI Build Status](https://travis-ci.org/tsahota/NMproject.svg?branch=master)](https://travis-ci.org/tsahota/NMproject)
[![Coverage Status](https://coveralls.io/repos/github/tsahota/NMproject/badge.svg?branch=master)](https://coveralls.io/github/tsahota/NMproject?branch=master)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tsahota/NMproject?branch=master&svg=true)](https://ci.appveyor.com/project/tsahota/NMproject)

Script based NONMEM execution on tidyprojects
 
## Installation

```R
install.packages("devtools")
devtools::install_github("tsahota/NMproject")
```

## Quick setup

Define options specific to your installation in your user "~/.Rprofile" (or "R_HOME/etc/Rprofile.site" if you want this to apply for all users).

e.g.

```r
options(path.nm_tran = "path/to/nonmem/installation/tr/NMTRAN.exe")

```

### Instructions

* NMprojects are directories where you can work on pharmacometric analysis.
* First, set up a tidyproject with the `make_project("/path/to/project/dir"")`.
   * See tutorial at https://github.com/tsahota/tidyproject to get started with tidyproject
* Open the NMproject with the File -> Open Project menu items. NOTE: always use Rstudio to open an NMproject, never just setwd() to the directory.

* To ensure code you write which depends on the NMproject package will never break due to updates, install the package NMproject into the *project library*.  Do this by simply installing again:

```r
devtools::install_github("tsahota/NMproject")
library(NMproject)
```

The global installation of NMproject can now change over time, but code depending on NMproject in this tidyproject will never break.

Have a look at built in code library, type:

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

If you have set up the `path.nm_tran` option, try running

```r
nm_tran("Models/run1.mod")

```

To run NONMEM

```r
system_nm("execute run1.mod -dir=1")
```

NOTE: the `-dir` option must match the run id in the control stream name.  e.g. if the control stream is `run23vpc.mod` the `-dir` must be `23vpc`


### FAQ

How do I set up NMproject to run jobs on a cluster via ssh.

*Answer:* Set up the system_nm option to ssh to the server and run the command, e.g.

```r

options(system_nm=function(cmd,...) {
        system(paste0("ssh -q clustername \"cd $(pwd); ",cmd,"\""),...)
})

```

How do I set up NMproject to run jobs on a cluster via ssh.


