# NMproject

[![Travis-CI Build Status](https://travis-ci.org/tsahota/NMproject.svg?branch=newinterface)](https://travis-ci.org/tsahota/NMproject)
[![Coverage Status](https://coveralls.io/repos/github/tsahota/NMproject/badge.svg?branch=newinterface)](https://coveralls.io/github/tsahota/NMproject?branch=newinterface)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tsahota/NMproject?branch=newinterface&svg=true)](https://ci.appveyor.com/project/tsahota/NMproject)

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
  
Two-minute Youtube summary: https://www.youtube.com/watch?v=b7oBb6QZub8

<img src=https://user-images.githubusercontent.com/18026277/26879195-79b6f4c0-4b90-11e7-8228-01b117e64a12.png width=24.6% /><img src=https://user-images.githubusercontent.com/18026277/26879231-a046cfc0-4b90-11e7-9dbf-666086f32b9d.png width=24.5% /><img src=https://user-images.githubusercontent.com/18026277/26879238-a4a94fc0-4b90-11e7-8e8f-1b12a03f912d.png width=24.5% /><img src=https://user-images.githubusercontent.com/18026277/26879240-a7a53ebe-4b90-11e7-80fa-74bef643db29.png width=24.5% />

History: NMproject was previously an AstraZeneca project.  It is being reimplemented here as a community version to be compatible with a variety of architectures (standalone NONMEM and a variety of grid submission systems)
 
## Installation

NONMEM, PsN, and Rstudio are required to be installed prior to these steps. 

```R
if(!require("devtools")) install.packages("devtools")
devtools::install_github("tsahota/NMproject")
```
Load the package with 

```R
library(NMproject)
```

### Configuration

NMproject also needs to know the path to your nmtran.exe file.  Do this by opening your `~/.Rprofile` - this can be done through R by:

```r
file.edit("~/.Rprofile")
```

and inserting the following option statement (adjusting the NONMEM installation path as necessary):

```r
options(nmtran_exe_path = "C:/nm741/tr/NMTRAN.exe")
```
There are many other options one can customise depending on your infrastructure and preferences.

To download and configure the PMXcodelibrary, go to the R console and run:

```r
get_PMX_code_library("/path/to/desired/location", config_file="~/.Rprofile")
```
This will have updated your `~/.Rprofile`.

If you are running NONMEM and R on a desktop/laptop this should suffice.  For more complicated set ups additional configuration may be required see FAQ (below) for details.

## Instructions

* NMprojects are directories where you can work on pharmacometric analysis.
* First, set up a tidyproject with the `make_project("/path/to/project/dir"")`.
   * See tutorial at https://github.com/tsahota/tidyproject for more information
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
code_library()
```

Preview code:

```r
preview("NONMEM/ADVAN2.mod")
```

Stage file into project

```r
ls_code_library("NONMEM/ADVAN2.mod") %>%
  stage()
```

Get started with a model development by using the following template from the PMX code library:

```r
ls_code_library("R/nm.log.R"") %>%
  stage() %>%
  import()
```

The following command will create an object of class `nm` with information about the run. This will not run the command yet.

```r
m1 <- nm(run_id = "m1") %>%
   based_on("staging/Models/ADVAN2.mod") %>%
   data_path("DerivedData/data.csv") %>%
   cmd("execute {ctl_name} -dir={run_dir}")
```

NOTE: the `-dir` option must always be specified when using NMproject

NOTE: If you have set up the `nmtran_exe_path` option configured (see FAQ below), you can run a quick test that your control stream and dataset pass NMTRAN checks without running NONMEM via:

```r
nm_tran(m1) 
```

This is especially useful to do on a cluster submission system where a job may take a while to come back with an error message.

To run `m1` use:

```r
m1 <- m1 %>% run_nm()
```

To view all runs and track progress:

```r
shiny_nm()
```

To do basic goodness of fit plots consider the `basic_gof.Rmd` function template in the PMX code library:

```r
ls_codelibrary("single_files/Scripts/basic_gof.Rmd") %>%
  stage()
m1 <- m1 %>% nm_render("Scripts/basic_gof.Rmd")
```


## FAQ

### + I want to repeat my model development script, how do I do this?

Just rerun the code in R

### + How can I queue multiple PsN jobs, forcing some to wait, and some not to.

Assuming you have the `nm` objects defined, you can queing jobs, by forcing some to wait, e.g.:

```r
mod1 <- run_nm(mod1) %>% wait_finish()
mod1vpc <- run_nm(mod1vpc)
mod1sse <- run_nm(mod1sse)
```

This will run mod1, wait for it to finish and then execute mod1vpc and mod1see at the same time.  You will not be able to use the R console while mod1 is running however since it will be waiting for mod1 to finish.  To get around this consider the `future` package to have a separate R process control the execution:

```r
future::plan("multisession")
mod1 %<-%{
  run(mod1,wait=TRUE) ; run(mod1vpc,mod1sse)})
```

### + After having closed my session how to I recreate my workspace

Just re-run all the code again.

If nothing has changed with regards to datasets and control files run_nm() will retrieve cached results without running nonmem.


### + I see the PMX code library has been updated, how can I update my local version of it?

The PMX code library is version controlled with `git`.  Ensure you have git installed, navigate to the local PMX code library repository and type `git pull`.  Git will complain if you have made changes to your local directory since downloading it.  If you have, you need to `commit` those changes before using `git pull`.  See git help for more information on how to do this.  Consider splitting your code library into multiple repositories for easier management (e.g. one public, one for your organisation, one for you)

### + We already have directory of R/NONMEM scripts/templates, how can we also use these?

Append your directory location to the `code_library_path` option.  To do this add the following command to your `~/.Rprofile` (or `$R_HOME/etc/Rprofile.site`) configuration file:

```r
options(code_library_path = c("/path/to/PMXcodelibrary/","path/to/existing/repository"))
```

### + How can I contribute to the PMX code library?

Log in to github (create an account if necessary).  Fork the repository `tsahota/PMXcodelibrary`. Make your change.  Create a pull request detailing your change.

### + My Rstudio Server is on a different linux server to my NONMEM cluster.  How can I set up NMproject to work with this?

You need to ensure your account has passwordless ssh set up.  Then create a `system_nm()` option in your `~/.Rprofile` configuration file which appends an ssh statement to the system call e.g. the following will set you up to connect to the host `clustername`:

```r
options(system_nm=function(cmd,...) {
        system(paste0("ssh -q clustername \"cd $(pwd); ",cmd,"\""),...)
})
```

### + I'm working on a windows laptop but want to use my NONMEM cluster for NONMEM jobs.  How can I set up NMproject to work with this?

This is not recommended as it requires R working directory being set to a networked drive.  This is very slow.  If you really want to though consider modifying the `system_nm()` option, as in the above FAQ question, to use `plink` to ssh to the server, change to the relevant working directory and submit a command.  This has not been tested however and results are likely to be disappointing.

### + My organisation has a different control file convention to the runXX.mod convention.  Can I change this?

Yes, you can specify the convention with the ctl_path() e.g. to change the convention to nm.XX.con

```r
m %>% ctl_path("Models/nm.{run_id}.com")

```

Consider making a wrapper function around nm() and ctl_path() and distributing to your users

### + How do I submit a command directly to the NONMEM server?

```r
system_nm("command_to_run",dir="path/to/dir")
```

### + There is functionality in PsN's runrecord, sumo or Pirana that I would like but is not currently available in NMproject.

NMproject doesn't change PsN's default directory structure, everything in the "Models" directory is as if you lauched the jobs from the command line.  Therefore you can continue to use PsN functions on the command line.  You can also continue using Pirana by pointing it towards your models directory.

If it's something you think really should be part of NMproject, open a github "issue" and ask for the feature.

### + I don't want to use NMproject on my analysis project any more, can I go back to submitting runs on the command line

Yes, NMproject doesn't change PsN's default directory structure, so you can go back to running PsN via command line.  If there a bug or a feature you think really should be part of NMproject, consider opening a github "issue" and asking.

### + I work for a CRO. My client doesn't have NMproject, how can send the analysis to them.

NMproject doesn't change PsN's default directory structure, and everything will work for them as long as their version of R (and package versions) are compatible.  It is recommended to run Renvironment_info() as a last step before sending the analysis directory so they can see the package versions you used.

Obviously they will not be able to run code that is dependent on NMproject unless they install it.  But your model development script can still serve as a helpful, human readable process description of your model development steps.
