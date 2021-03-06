% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_parameters.R
\name{update_parameters}
\alias{update_parameters}
\title{Update initial estimates to final estimates}
\usage{
update_parameters(ctl, from)
}
\arguments{
\item{ctl}{An nm object.}

\item{from}{Optional nm object. The completed object from which to extract results.  If not specified, \code{from} will be taken to be \code{ctl}.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
}
\examples{

\dontrun{

m1 \%>\% dollar("THETA")
m1 <- m1 \%>\% update_parameters()
m1 \%>\% dollar("THETA")
}
}
