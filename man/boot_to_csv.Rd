% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boot.R
\name{boot_to_csv}
\alias{boot_to_csv}
\title{Save bootstrap datasets to disk}
\usage{
boot_to_csv(
  d,
  rsplit,
  data_name,
  data_folder = "DerivedData/bootstrap_datasets",
  id_var = "ID",
  oob = FALSE,
  overwrite = FALSE
)
}
\arguments{
\item{d}{Dataset to merge.}

\item{rsplit}{An object from \code{rsample::bootstraps()}.}

\item{data_name}{Name of dataset.}

\item{data_folder}{Path to bootstrap datasets.}

\item{id_var}{Character (default = \code{"ID"}). Name of ID column.}

\item{oob}{Logical.  Should out of bag dataset be written (default = \code{FALSE}).}

\item{overwrite}{Should datasets be overwritten.}
}
\description{
Used by \code{make_boot_datasets()}, run once per bootstrap sample
}
\keyword{internal}
