# NMproject

[![Travis-CI Build Status](https://travis-ci.org/tsahota/NMproject.svg?branch=newinterface)](https://travis-ci.org/tsahota/NMproject)
[![Coverage Status](https://coveralls.io/repos/github/tsahota/NMproject/badge.svg?branch=newinterface)](https://coveralls.io/github/tsahota/NMproject?branch=newinterface)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tsahota/NMproject?branch=newinterface&svg=true)](https://ci.appveyor.com/project/tsahota/NMproject)

This is the newer beta interface.  To use the older interface, install v0.3.2 (see releases)

Script based NONMEM execution on tidyprojects with shiny interface.

* Tidy code:
  * Standardised, version controlled, directory structure for all NONMEM users.
  * Enforcement of basic NONMEM control stream coding conventions around parameter naming for human and machine readability
* PMX Code library:
  * Keep NONMEM template scripts to speed creation of NONMEM runs and facilitate adherence to best practices.
  * "Attach" community-wide, organisation-wide, or individual code repositories.
* Script based model development:
  * Code your model development process using end-to-end R workflows.
  * Private project R libraries for long term reproducibility and consistent running of R scripts between users.
* Shiny interface:
  * Table of runs
  * Real time run tracking
  * Run comparison
  
Two-minute Youtube summary (alpha interface): https://www.youtube.com/watch?v=b7oBb6QZub8

<img src=https://user-images.githubusercontent.com/18026277/26879195-79b6f4c0-4b90-11e7-8228-01b117e64a12.png width=24.6% /><img src=https://user-images.githubusercontent.com/18026277/26879231-a046cfc0-4b90-11e7-9dbf-666086f32b9d.png width=24.5% /><img src=https://user-images.githubusercontent.com/18026277/26879238-a4a94fc0-4b90-11e7-8e8f-1b12a03f912d.png width=24.5% /><img src=https://user-images.githubusercontent.com/18026277/26879240-a7a53ebe-4b90-11e7-80fa-74bef643db29.png width=24.5% />

History: NMproject was previously an AstraZeneca project.  It is being reimplemented here as a community version to be compatible with a variety of architectures (standalone NONMEM and a variety of grid submission systems)
 
## Installation

NONMEM, PsN, and Rstudio are required to be installed prior to these steps.  To install a specific release version (e.g. v0.5.1)

```R
if(!require("devtools")) install.packages("devtools")
devtools::install_github("tsahota/NMproject@v0.5.1")
```

To install the latests developmental version:

```R
devtools::install_github("tsahota/NMproject")
```

Load the package with 

```R
library(NMproject)
```

## Instructions

* NMprojects are directories where you can work on pharmacometric analysis.
* First, set up a tidyproject with the `make_project("/path/to/project/dir"")` or in RStudio File -> New Project -> New Directory -> New tidyproject
   * See tutorial at https://github.com/tsahota/tidyproject for more information

### Demo

The easiest way to familiarise your with NMproject is to follow through the demo.

To step through the theophylline example, create a new tidyproject and run the following

```r
library(NMproject)
setup_nm_demo()
```

open the `Scripts` directory and step through the scripts s01.....Rmd, s02....Rmd to familiarise yourself with the functions

### Basic commands

To get started with your own analysis, copy your dataset to `SourceData` directory.

Then open a generic Rmardown template (New Rmarkdown -> From Template -> NMproject generic), write a script to read in your SourceData and save a processed NONMEM version of this in `DerivedData`.

Then open a model development Rmarkdown template. First step is to find a NONMEM template control stream in the code library.  View it with: 

```r
code_library()
```

Preview code:

```r
preview("Models/ADVAN2.mod")
```

import file into project

```r
ls_code_library("Models/ADVAN2.mod") %>%
  stage()
```

The model will be in the staging area: `staging/Models/ADVAN2.mod`

The following command will create an object of class `nm` with information about the run. This will not run the command yet.

```r
m1 <- nm(run_id = "m1",
         based_on = "staging/Models/ADVAN2.mod",
         data_path = "DerivedData/data.csv",
         cmd = "execute {ctl_name} -dir={run_dir}")
```

Edit the control file manually by highlighting the entire command and starting a manual edit (addins -> manual edit).  Follow the instructions to edit the file to be fit for purpose.  This will the `apply_manual_edit` command to your code like so:

```r
m1 <- nm(run_id = "m1",
         based_on = "staging/Models/ADVAN2.mod",
         data_path = "DerivedData/data.csv",
         cmd = "execute {ctl_name} -dir={run_dir}") %>%
      apply_manual_edit("patch-tsdk2324-2021-01-01-02-42-28")
      
```

Add a `run_nm` command to make it run (at this point the control will be created on disk in the `run_in(m)` location.

```r
m1 <- nm(run_id = "m1",
         based_on = "staging/Models/ADVAN2.mod",
         data_path = "DerivedData/data.csv",
         cmd = "execute {ctl_name} -dir={run_dir}") %>%
      apply_manual_edit("patch-tsdk2324-2021-01-01-02-42-28") %>%
      run_nm()
      
```

Test out your run by running nm_tran add_in or typing the following into the console

```r
nm_tran(m1) 
```

Send this command to the console to get it running.

To view all runs in the workspace and track progress:

```r
shiny_nm()
```

To do basic goodness of fit open the post processing Rmarkdown template, follow instructions, customise your template save it (e.g. as `Scripts/basic_gof.Rmd`) and run in your log script with

```r
m1 <- m1 %>% nm_render("Scripts/basic_gof.Rmd")
```

It will create the output in `Results` (or results_dir(m1))

To create additional runs (child runs) use the child function and modify, e.g. to convert from `advan2 trans1` to `advan2 trans2` do the following

```r
m2 <- m1 %>% child(run_id = "m2") %>%
             subroutine(trans = 2) %>%
             run_nm()

```

To add a covariate:

```r
m3 <- m2 %>% child(run_id = "m3") %>%
             add_cov(param = "CL", cov = "WT", state = "linear") %>%
             run_nm()

```

Evaluate and compare runs on the fly with the following commands 

```r
rr(m2)
rr(c(m2, m3))
plot_iter(m2)
covariance_results(m2)
```

To create a simulation:

```r
m2s <- m2 %>% child(run_id = "m2s") %>%
              convert_to_simulation(subpr = 50) %>%
              run_nm()

```

For simulation runs, use the VPC and PPC templates together with `nm_render` like the basic goodness of template used above.

```r
m2s <- m2s %>% nm_render("Scripts/basic_vpc.Rmd")
m2s <- m2s %>% nm_render("Scripts/basic_ppc.Rmd")
```

Don't forget to comment your code with your decision making.

## FAQ

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

### + How do I run this on SGE?

There is a built in `sge_command` that's part of NMproject.  Use this to set your `cmd()` field of the nm object 

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
