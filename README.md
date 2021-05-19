
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NMproject

<!-- badges: start -->

[![Travis-CI Build
Status](https://travis-ci.org/tsahota/NMproject.svg?branch=master)](https://travis-ci.org/tsahota/NMproject)
[![Coverage
Status](https://coveralls.io/repos/github/tsahota/NMproject/badge.svg?branch=master)](https://coveralls.io/github/tsahota/NMproject?branch=master)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/tsahota/NMproject?branch=master&svg=true)](https://ci.appveyor.com/project/tsahota/NMproject)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Codecov test
coverage](https://codecov.io/gh/tsahota/NMproject/branch/master/graph/badge.svg)](https://codecov.io/gh/tsahota/NMproject?branch=master)
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

History: NMproject was previously an internal AstraZeneca project. It is
being reimplemented here as a community version to be compatible with a
variety of architectures (standalone NONMEM and a variety of grid
submission systems)

Two-minute Youtube summary (alpha interface):
<https://www.youtube.com/watch?v=b7oBb6QZub8>

<!-- old pictures
<img src=https://user-images.githubusercontent.com/18026277/26879195-79b6f4c0-4b90-11e7-8228-01b117e64a12.png width=24.6% /><img src=https://user-images.githubusercontent.com/18026277/26879231-a046cfc0-4b90-11e7-9dbf-666086f32b9d.png width=24.5% /><img src=https://user-images.githubusercontent.com/18026277/26879238-a4a94fc0-4b90-11e7-8e8f-1b12a03f912d.png width=24.5% /><img src=https://user-images.githubusercontent.com/18026277/26879240-a7a53ebe-4b90-11e7-80fa-74bef643db29.png width=24.5% />
-->

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

## Getting started with NMproject

The easiest way to familiarise your with NMproject is to follow through
the demo. The
(vignette)\[<https://tsahota.github.io/NMproject/articles/NMproject.html>\]
gives instructions for how to get started.

## Example code

NMprojects works best with pipes `%>%`. Below we take a parent run,
`m1`, create a new child run with the first pipe, then convert it from
`ADVAN2 TRANS1` (KA, K, V) to `ADVAN2 TRANS2` (KA, CL, V) by piping into
the `subroutine()` function before running with the `run_nm()` function:

``` r
m2 <- m1 %>% child() %>%
  subroutine(trans = 2) %>%
  run_nm()
```

To generate diagnostics we create a diagnostic template with help of
pre-existing templates, here it’s `Scripts/basic_gof.Rmd` and run this
on m2. This will create a project specific run report to evaluate your
model

``` r
m2 %>% nm_render("Scripts/basic_gof.Rmd")

## run reports will be in "Results" directory of analysis project
```

Here’s a code snippet creating a running simulation so that vpc and ppc
diagnostics can be applied

``` r
m2s <- m2 %>% child(run_id = "m2s") %>%
              update_parameters(m2) %>%
              convert_to_simulation(subpr = 50) %>%
              run_nm()

m2s %>% nm_render("Scripts/basic_vpc.Rmd")
m2s %>% nm_render("Scripts/basic_ppc.Rmd")

## run reports will be in "Results" directory of analysis project
```

See the vignette for more examples
