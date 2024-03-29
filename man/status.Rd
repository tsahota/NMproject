% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monitoring.R
\name{status}
\alias{status}
\title{Get status of NONMEM runs}
\usage{
status(x)
}
\arguments{
\item{x}{An nm object.}
}
\value{
A character with the status of the run with values \code{"non started"},
\code{"running"}, \code{"finished"}, or \code{"error"}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
}
\examples{

# create example object m1 from package demo files
exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
m1 <- new_nm(run_id = "m1", 
             based_on = file.path(exdir, "Models", "ADVAN2.mod"),
             data_path = file.path(exdir, "SourceData", "THEOPP.csv"))

status(m1)  ## not run

}
\seealso{
\code{\link[=status_table]{status_table()}}.
}
