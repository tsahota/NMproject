% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nm-gettersetters.R
\name{ctl_path}
\alias{ctl_path}
\title{Get and set path to NONMEM control file}
\usage{
ctl_path(m, text)
}
\arguments{
\item{m}{An nm object.}

\item{text}{Optional character. Name of path to control file (see details).
Typically, this file does not yet normally exist, but will house the code
code for this run.}
}
\value{
\code{character} with path to NONMEM control file to be copied immediately
prior to running (with \code{\link[=run_nm]{run_nm()}}).
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Similar to \code{\link[=ctl_name]{ctl_name()}} & \code{\link[=run_in]{run_in()}}, this allows you to retrieve and specify
the relative path to the control file that will be written by the \code{\link[=run_nm]{run_nm()}}.
}
\details{
Note that \code{text} can contain an \code{"{run_id}"} string.  E.g.
\code{"Models/run{run_id}.mod"} will use the name \code{"Models/runm1.mod"} if
\code{run_id(m1)} is \code{"m1"}.
}
\examples{

# create example object m1 from package demo files
exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
m1 <- new_nm(run_id = "m1", 
             based_on = file.path(exdir, "Models", "ADVAN2.mod"),
             data_path = file.path(exdir, "SourceData", "THEOPP.csv"))


ctl_name(m1)
ctl_path(m1)

m1 <- m1 \%>\% ctl_path("Models/nm_{run_id}.ctl")
ctl_path(m1)


}
