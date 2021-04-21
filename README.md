
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NMproject

<!-- badges: start -->

[![Travis-CI Build
Status](https://travis-ci.org/tsahota/NMproject.svg?branch=master)](https://travis-ci.org/tsahota/NMproject)
[![Coverage
Status](https://coveralls.io/repos/github/tsahota/NMproject/badge.svg?branch=master)](https://coveralls.io/github/tsahota/NMproject?branch=master)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/tsahota/NMproject?branch=master&svg=true)](https://ci.appveyor.com/project/tsahota/NMproject)
<!-- badges: end -->

*This is the newer beta interface. To use the older interface, install
v0.3.2 (see releases)*

The intent of NMproject is to allow for fully script based NONMEM model
development:

-   Code management:
    -   Code library: NONMEM template scripts to speed creation of
        NONMEM runs and facilitate adherence to best practices.
    -   Standardised, version controlled, directory structure for all
        NONMEM users.
-   Script based model development:
    -   Instead of command line interface (CLI) or graphical user
        interface (GUI) use a script based interface
    -   End-to-end R workflows that can be repeated, reused and
        documented in R markdown
    -   Convenience functions for routine NONMEM control file edits
    -   Tracked manual edits for everything else to ensure full
        flexibility
    -   Fully customizable r markdown templates for goodness of fits,
        vpcs, ppcs, bootstraps
    -   Vectorized model objects - interact with groups of NONMEM runs
        as if they were one (no loops needed)
    -   Shiny interface for real time run tracking in grid environments

History: NMproject was previously an AstraZeneca project. It is being
reimplemented here as a community version to be compatible with a
variety of architectures (standalone NONMEM and a variety of grid
submission systems)

Two-minute Youtube summary (alpha interface):
<https://www.youtube.com/watch?v=b7oBb6QZub8>

<img src=https://user-images.githubusercontent.com/18026277/26879195-79b6f4c0-4b90-11e7-8228-01b117e64a12.png width=24.6% /><img src=https://user-images.githubusercontent.com/18026277/26879231-a046cfc0-4b90-11e7-9dbf-666086f32b9d.png width=24.5% /><img src=https://user-images.githubusercontent.com/18026277/26879238-a4a94fc0-4b90-11e7-8e8f-1b12a03f912d.png width=24.5% /><img src=https://user-images.githubusercontent.com/18026277/26879240-a7a53ebe-4b90-11e7-80fa-74bef643db29.png width=24.5% />

## Installation

NONMEM, PsN, and Rstudio are required to be installed prior to these
steps. To install a specific release version (e.g. v0.5.1) from
[GitHub](https://github.com/):

``` r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("tsahota/NMproject@v0.5.1")
```

To install the latest developmental version:

``` r
devtools::install_github("tsahota/NMproject")
```

Load the package with

``` r
library(NMproject)
```

## Instructions

-   NMprojects are directories where you can work on pharmacometric
    analysis.
-   First, set up a tidyproject with the
    `make_project("/path/to/project/dir"")` or in RStudio File -&gt; New
    Project -&gt; New Directory -&gt; New tidyproject
    -   See tutorial at <https://github.com/tsahota/tidyproject> for
        more information

### Demo

The easiest way to familiarise your with NMproject is to follow through
the demo.

First create a new tidyproject (`FILE` -&gt; `New Project` -&gt;
`New Directory` -&gt; `New tidyproject`). You’ll see a clean analysis
directory with empty subfolders. Run the following in the new R session:

``` r
library(NMproject)
setup_nm_demo()
```

This will population the `Scripts` directory with scripts, and deposit
the Theopp dataset into the `SourceData` subdirectory.

The scripts are numbered s01…..Rmd, s02….Rmd etc., and are designed to
be read and run in order. This is the best way to familiarize yourself.
The scripts can also be used as template for your own model development

## Example

If we already have a parent run `m1` using `ADVAN2 TRANS1`, we can
create a `child()` run that uses `TRANS2` using `subroutine()`:

``` r
m2 <- m1 %>% child() %>%
  subroutine(trans = 2) %>%
  run_nm()
```

View exactly what’s been changed by highlighting the above code in
RStudio and clicking the `nm_diff` addin to see what’s been changed
before running. Here, changes will be in the `$SUB`, `$PK` and `$THETA`.

Also use the `nm_tran` addin to test before running (useful on grid
environments).

To add a covariate using PsN coding conventions use `add_cov()`:

``` r
m3 <- m2 %>% child() %>%
  add_cov(param = "CL", cov = "WT", state = "linear") %>%
  run_nm()
```

Apply a fully editable goodness of fit R markdown template to both runs
`m2` and `m3`:

``` r
c(m2, m3) %>% nm_render("Scripts/basic_gof.Rmd")
## will produce two run specific html reports in the "Results" directory for run evaluation
```

`c(m1, m3)` is a vector object of 2 NONMEM runs. It can be embedded into
data.frames/tibbles.

Create a simulation run using `update_parameters()` and
`convert_to_simulation()`. Apply vpc and ppc diagnostics

``` r
m2s <- m2 %>% child(run_id = "m2s") %>%
              update_parameters(m2) %>%
              convert_to_simulation(subpr = 50) %>%
              run_nm()

m2s %>% nm_render("Scripts/basic_vpc.Rmd")
m2s %>% nm_render("Scripts/basic_ppc.Rmd")
```

See the vignette for more examples
