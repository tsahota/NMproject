% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/param-manipulate.R
\name{add_mixed_param}
\alias{add_mixed_param}
\title{Add a mixed effect parameter to $PK (or $PRED)}
\usage{
add_mixed_param(
  m,
  name,
  init = 1,
  unit = "",
  trans = c("LOG"),
  position = NA_integer_,
  after = character()
)
}
\arguments{
\item{m}{An nm object.}

\item{name}{Character. Name of NONMEM variable to create.}

\item{init}{Numeric (default = \code{1}). Initial value of fixed effect.}

\item{unit}{Character (default = \code{""}). Unit of variable.}

\item{trans}{Character (default = \code{"LOG"}). Transformation of the variable.}

\item{position}{Integer. Not used.}

\item{after}{Character. Pattern to match and include the mixed effect after.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Primarily an internal function. This will (by default) add a parameter (mixed
effect) to your code $PK/$PRED and $THETA/$OMEGA.
}
