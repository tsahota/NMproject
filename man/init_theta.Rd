% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/init_funs.R
\name{init_theta}
\alias{init_theta}
\alias{init_omega}
\alias{init_sigma}
\title{Get/set initial parameters}
\usage{
init_theta(m, replace, ...)

init_omega(m, replace, ...)

init_sigma(m, replace, ...)
}
\arguments{
\item{m}{An nm object.}

\item{replace}{Optional \code{tibble} for replacement.}

\item{...}{Additional arguments for mutating initial estimate NONMEM
subroutines.  See examples.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

These functions are useful to obtain and modify initial values of \verb{$THETA},
\verb{$OMEGA} and \verb{$SIGMA}.
}
\examples{
\dontrun{

## set initial values

m1 <- new_nm(run_id = "m1",
             based_on = "staging/Models/ADVAN2.mod",
             data_path = "DerivedData/data.csv") \%>\%
  fill_input() \%>\%
  init_theta(init = c(-2, 0.5, 1)) \%>\%
  init_sigma(init = c(0.1, 0.1)) \%>\%
  run_nm()

init_theta(m1) ## display current $THETA in tibble-form
init_omega(m1) ## display current $OMEGA in tibble-form


## here we supply a named vector in a different order
m1 <- m1 \%>\% init_theta(init = c(KA = -2, V = 1))
m1 \%>\% dollar("THETA")

## can also manipulate other aspects (like the FIX column) similarly
m1 <- m1 \%>\% init_theta(init = c(KA = -2, V = 1),
                        FIX = c(KA = TRUE))
m1 \%>\% dollar("THETA")

## perturb all parameters by ~10\%
m1 <- m1 \%>\% init_theta(init = rnorm(length(init), mean = init, sd = 0.1))

## perturb only log transformed parameters by ~10\%
m1 <- m1 \%>\% init_theta(
  init = ifelse(
    trans \%in\% "LOG",
    rnorm(length(init), mean = init, sd = 0.1),
    init
  )
)
}
}
