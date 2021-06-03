
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NMproject

<!-- badges: start -->

[![Travis-CI Build
Status](https://travis-ci.org/tsahota/NMproject.svg?branch=master)](https://travis-ci.org/tsahota/NMproject)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/tsahota/NMproject?branch=master&svg=true)](https://ci.appveyor.com/project/tsahota/NMproject)
[![Coverage
Status](https://coveralls.io/repos/github/tsahota/NMproject/badge.svg?branch=master)](https://coveralls.io/github/tsahota/NMproject?branch=master)
[![Codecov test
coverage](https://codecov.io/gh/tsahota/NMproject/branch/master/graph/badge.svg)](https://codecov.io/gh/tsahota/NMproject?branch=master)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/NMproject)](https://CRAN.R-project.org/package=NMproject)
<!-- badges: end -->

Have you ever tried to continue some model development work (a
colleague’s or even your own) but struggled to remember what was
previously done and in which order? Have you ever wished you could
repeat your entire model building at the push of a button e.g. because
the source data has been updated, or because of a mistake carried
forward?

The end-to-end workflows you create with NMproject reduce the time it
takes to get a working model, drastically reduce the time to
update/repeat prior analyses, enable the repurposing of prior analyses
to new problems, and enable pre-programming of complex workflows before
data becomes available so that when it does become available we can
react to decision points in stats-like timelines.

History: NMproject was previously an internal AstraZeneca project. It is
reimplemented here as a community version.

[Two-minute Youtube
summary](https://www.youtube.com/watch?v=b7oBb6QZub8) *WARNING: this is
the prototype “alpha” interface. The new interface has a different
syntax. Video to be replaced*

<!-- old pictures
<img src=https://user-images.githubusercontent.com/18026277/26879195-79b6f4c0-4b90-11e7-8228-01b117e64a12.png width=24.6% /><img src=https://user-images.githubusercontent.com/18026277/26879231-a046cfc0-4b90-11e7-9dbf-666086f32b9d.png width=24.5% /><img src=https://user-images.githubusercontent.com/18026277/26879238-a4a94fc0-4b90-11e7-8e8f-1b12a03f912d.png width=24.5% /><img src=https://user-images.githubusercontent.com/18026277/26879240-a7a53ebe-4b90-11e7-80fa-74bef643db29.png width=24.5% />
-->

## Installation

NONMEM, PsN, and Rstudio are required to be installed prior to these
steps. To install the latest development version from
[GitHub](https://github.com/):

``` r
if(!require("devtools")) install.packages("devtools")
devtools::install_github("tsahota/NMproject")
```

To install a specific release (e.g. v0.5.1) use the following command:

``` r
devtools::install_github("tsahota/NMproject@v0.5.1")
```

Load the package with

``` r
library(NMproject)
```

## Getting started with NMproject

The easiest way to familiarise your with NMproject is to follow through
the
[demo](https://tsahota.github.io/NMproject/articles/NMproject.html#demo-learn-by-doing-1).

See the
[vignette](https://tsahota.github.io/NMproject/articles/NMproject.html)
for overall package documentation.

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
model. Can also run `Scripts/basic_gof.Rmd` interactively as a notebook
to customise your diagnostics to the problem.

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

See the
[vignette](https://tsahota.github.io/NMproject/articles/NMproject.html)
for more examples
