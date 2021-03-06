% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nm-gettersetters.R
\name{simple_field}
\alias{simple_field}
\title{Interface for getting and setting your own simple fields in nm objects}
\usage{
simple_field(m, ...)
}
\arguments{
\item{m}{An nm object.}

\item{...}{Arguments to get/set fields.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
}
\examples{
\dontrun{

mc <- mc \%>\% simple_field(stars = 3)
mc \%>\% simple_field(stars)
mc ## see that stars is a field of the nm object.
}
}
