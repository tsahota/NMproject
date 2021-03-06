% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subroutine.R
\name{dollar_subroutine}
\alias{dollar_subroutine}
\alias{advan}
\alias{trans}
\alias{tol}
\title{Get/set $SUBROUTINE values in control file}
\usage{
advan(m, text)

trans(m, text)

tol(m, text)
}
\arguments{
\item{m}{An nm object.}

\item{text}{Optional number/character number to set to.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

These are mostly back end functions used by \code{\link[=subroutine]{subroutine()}} and will make
simple ADVAN/TRANS/TOL adjustments to the NONMEM control file.  No other file
changes outside $SUBROUTINE will be made which makes \code{advan} and \code{trans} less
useful than the higher level \code{\link[=subroutine]{subroutine()}} function.
}
\examples{
\dontrun{

tol(m1) ## display current TOL value

## set tol to 12
m1 <- m1 \%>\% tol(12)
}
}
\seealso{
\code{\link[=subroutine]{subroutine()}}
}
