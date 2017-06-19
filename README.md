# NMproject

[![Travis-CI Build Status](https://travis-ci.org/tsahota/NMproject.svg?branch=master)](https://travis-ci.org/tsahota/NMproject)
[![Coverage Status](https://coveralls.io/repos/github/tsahota/NMproject/badge.svg?branch=master)](https://coveralls.io/github/tsahota/NMproject?branch=master)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tsahota/NMproject?branch=master&svg=true)](https://ci.appveyor.com/project/tsahota/NMproject)

Script based NONMEM execution on tidyprojects with shiny interface.

* Tidy code:
  * Standardised, version controlled, directory structure for all NONMEM users.
  * Enforcement of basic NONMEM control stream coding conventions around parameter naming for human and machine readability
* PMX Code library:
  * Keep NONMEM template scripts to speed creation of NONMEM runs and facilitate adherence to best practices.
  * "Attach" community-wide, organisation-wide, or individual code repositories.
  * Search the code library using keywords, tags, or raw text search.
* Script based model development:
  * Code your model development process using end-to-end R scripts.
  * Private project R libraries for long term reproducibility and consistent running of R scripts between users.
  * Behind the scenes database for run information storage.  Ensures runs do not overwrite previous outputs.
* Shiny interface:
  * Table of runs
  * Real time run tracking
  * Run comparison
  

<img src=https://user-images.githubusercontent.com/18026277/26879195-79b6f4c0-4b90-11e7-8228-01b117e64a12.png width=24.6% /><img src=https://user-images.githubusercontent.com/18026277/26879231-a046cfc0-4b90-11e7-9dbf-666086f32b9d.png width=24.5% /><img src=https://user-images.githubusercontent.com/18026277/26879238-a4a94fc0-4b90-11e7-8e8f-1b12a03f912d.png width=24.5% /><img src=https://user-images.githubusercontent.com/18026277/26879240-a7a53ebe-4b90-11e7-80fa-74bef643db29.png width=24.5% />

Youtube summary: https://www.youtube.com/watch?v=KWPOs-sa6pU

History: NMproject was previously an AstraZeneca project.  It is being reimplemented here as a community version to be compatible with a variety of architectures (standalone NONMEM and a variety of grid submission systems)
 
## Installation

NONMEM, PsN, and Rstudio are required to be installed prior to these steps. 

```R
install.packages("devtools")
devtools::install_github("tsahota/NMproject")
```

To download and configure the PMXcodelibrary, run:

```r
library(NMproject)
get_PMX_code_library("/path/to/desired/location", config_file="~/.Rprofile")
```

If you are running NONMEM and R on a desktop/laptop this should suffice.  For more complicated set ups additional configuration may be required see FAQ (below) for details.

## Instructions

* NMprojects are directories where you can work on pharmacometric analysis.
* First, set up a tidyproject with the `make_project("/path/to/project/dir"")`.
   * See tutorial at https://github.com/tsahota/tidyproject to get started with tidyproject
* Open the NMproject with the File -> Open Project menu items. NOTE: always use Rstudio to open an NMproject, never just `setwd()` to the directory.  From here you can either do the demo, or read through the "Basic commands" section below.

### Demo

To step through the theophylline example

```r
library(NMproject)
setup_nm_demo(demo_name = "theopp")
```
open `Scripts/theopp-demo.R` and step through the commands one by one.

### Basic commands

View the code library:

```r
library(NMproject)
```

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

To create NONMEM object (this will not run NONMEM yet), add the following to your script.

```r
mod1 <- nm("execute run1.mod -dir=1")
```

NOTE: the `-dir` option must always be specified when using NMproject

The `mod1` object here will contain information about the run.  If you have set up the `nmtran_exe_path` option configured (see FAQ below), you can do a quick test that your control stream and dataset pass NMTRAN checks:

```r
nm_tran(mod1)
```

To run `mod1`:

```r
run(mod1)
```

To view all runs and track progress:

```r
shiny_nm()
```

To do basic goodness of fit plots consider the `gof_xpose.R` function template in the PMX code library:

```r
copy_script("R/gof_xpose.R")
source("R/gof_xpose.R")
gof_xpose(mod1)
```


## FAQ

### + How can I quickly test to see if my control stream & dataset pass NMTRAN checks without running the NONMEM job?

This is especially useful to do on a cluster submission system where a job may take a long time to start and come back with an error.  However it is good practice on non-server setups too.

Add the following line to your `~/.Rprofile` with the location of your nmtran.exe file:.

```r
options(nmtran_exe_path = "path/to/nonmem/installation/tr/NMTRAN.exe")
```
You can now use `nm_tran(mod1)`


### + We already have directory of R/NONMEM scripts/templates, how can we also use these?

Append your directory location to the `code_library_path` option.  To do this add the following command to your `~/.Rprofile` (or `$R_HOME/etc/Rprofile.site`) configuration file:

```r
options(code_library_path = c("/path/to/PMXcodelibrary/","path/to/existing/repository"))
```

### + How can I contribute to the PMX code library?

Create/log in to github.  Fork the repository `tsahota/PMXcodelibrary`. Make your change.  Create a pull request detailing your change.  

### + My Rstudio Server is on a different server to my NONMEM cluster.  How can I set up NMproject to work with this?

You need to ensure your account has passwordless ssh set up.  Then you need to create a `system_nm()` option in your `~/.Rprofile` configuration file that should ssh to the NONMEM server and run the desired command, e.g.

```r
options(system_nm=function(cmd,...) {
        system(paste0("ssh -q clustername \"cd $(pwd); ",cmd,"\""),...)
})
```

### + I'm working on a windows laptop but want to use my NONMEM cluster for NONMEM jobs.  How can I set up NMproject to work with this?

This option is not recommended as you would have to have your R working set to a networked drive.  It is possible though by modifying the `system_nm()` option, as in the above FAQ question, to use plink to ssh to the server, change to the relevant working directory and submit a command.  This has not been tested however, and results are likely to be disappointing.

### + My organisation has a different control file convention to the runXX.mod convention.  Can I change this?

Yes, you need to modify the `model_file_sub` and `model_file_extn` options.  To do this add the following to your `~/.Rprofile` configuration file. E.g. to change the convention to nm.XX.con

```r
options(model_file_stub="nm.")
options(model_file_extn="con")

```

### + How do I submit a command directly to the NONMEM server?

```r
system_nm("command_to_run",dir="path/to/dir")
```

### + After having closed my session how to I recreate an nm object

Run the nm() statements again in the R console.  The objects will be recreated.

### + I want to repeat my model development script, how do I do this?

You need to make `run()` submit NONMEM jobs synchronously.  To do this:

```r
non_interactive_mode()
```

The `run()` function will now wait for each run to finish before moving onto the next R command.  To return to interactive mode (asynchronous NONMEM execution) run:

```r
interactive_mode()
```

### + How I get queue PsN jobs runs

Assuming you have the nm objects defined, You can get a limited queing ability by doing something like:

```r
run(mod1,wait=TRUE) ; run(mod1vpc,mod1sse)
```

This will run mod1, wait for it to finish and then execute mod1vpc and mod1see at the same time.  You will not be able to use the R console while mod1 is running however since it will be waiting.  This means you will not be able to code without launching a new R session.  To get around this consider the `future` package:

```r
library(future)
plan("multisession")
future({run(mod1,wait=TRUE) ; run(mod1vpc,mod1sse)})
```

### + There is functionality in PsN's runrecord, sumo or Pirana that I would like but is not currently available in NMproject.

NMproject doesn't change PsN's default directory structure, everything in the "Models"" directory is as if you lauched the jobs from the command line.  Therefore you can continue to use PsN functions on the command line.  You can also continue using Pirana by pointing it towards your models directory.

If it's something you think really should be part of NMproject, open a github "issue" and ask for the feature.

### + I don't want to use NMproject on my analysis project any more, can I go back to submitting runs on the command line

Yes, NMproject doesn't change PsN's default directory structure, so you can go back to running PsN via command line.  If there a bug or a a feature you think really should be part of NMproject, consider opening a github "issue" and asking.

### + My client doesn't have NMproject, how can send the project to them.

NMproject doesn't change PsN's default directory structure, so they will find model files and outputs in the locations they expect.  All R scripts not involving NMproject, e.g. exploratory plot generation, output processing,... will still work for the client as long as their version of R (and packages) is compatible.  It is recommended to run Renvironment_info() as a last step before sending the analysis directory.

The main R script you'll have involving NMproject will be the model development script.  Keeping a well commented model development script, will serve as a helpful, human readable record of your model development steps.  Your client also has the option of installing NMproject, so they can reproduce your analysis steps.
