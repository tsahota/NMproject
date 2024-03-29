% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim-diagnostics.R
\name{convert_to_simulation}
\alias{convert_to_simulation}
\title{Convert a NONMEM run to a simulation}
\usage{
convert_to_simulation(m, seed = 12345, subpr = 1)
}
\arguments{
\item{m}{A nm object.}

\item{seed}{Numeric (default = \code{12345}). seed value to include in $SIM.}

\item{subpr}{Numeric (default = \code{1}). SUBPR value to include in $SIM.}
}
\value{
An nm object with modified \code{ctl_contents} field.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Replaces $EST with $SIM.
}
\details{
Will only change $EST/$SIM, therefore it will not be sufficient to
change a categorical estimation control file to simulation. You will likely
need to perform a \verb{manual edit} for categorical data simulation.
}
\examples{

\dontrun{

## requires NONMEM to be installed

m1s <- m1 \%>\%
  child(run_id = "m1s") \%>\%
  update_parameters(m1) \%>\%
  convert_to_simulation(subpr = 50) \%>\%
  run_nm()

m1s \%>\% nm_render("Scripts/basic_vpc.Rmd")
m1s \%>\% nm_render("Scripts/basic_ppc.Rmd")
  
}

}
