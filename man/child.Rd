% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nm_object.R
\name{child}
\alias{child}
\title{Make child nm object from parent}
\usage{
child(m, run_id = NA_character_, type = "execute", parent = m, silent = FALSE)
}
\arguments{
\item{m}{Parent nm object.}

\item{run_id}{Character.  New \code{run_id} to assign to child object.}

\item{type}{Character (default = \code{"execute"}). Type of child object.}

\item{parent}{Optional nm object (default = \code{m}) . Parent object will by
default be \code{m}, but this argument will force parent to be a different
object.}

\item{silent}{Logical (default = \code{FALSE}). Should warn if conflicts detected.}
}
\value{
An new nm object with modified \verb{parent_*} fields updated to be the
\code{*} fields of the parent object, \code{m}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Child objects inherit attributes of parent but with a new \code{run_id}. The control
file will be inherited too with $TABLEs updated.
}
\details{
Specifying \code{parent} will force parent to be different from \code{m}. This
is useful in piping when a parent object is modified prior to being used in
the child object.
}
\examples{

exdir <- system.file("extdata", "examples", "theopp", package = "NMproject")
m1 <- new_nm(run_id = "m1", 
             based_on = file.path(exdir, "Models", "ADVAN2.mod"),
             data_path = file.path(exdir, "SourceData", "THEOPP.csv"))

m2 <- m1 \%>\% child("m2")

nm_diff(m2, m1)

}
