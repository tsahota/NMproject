% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_project.R
\name{list_dirs}
\alias{list_dirs}
\title{List directories}
\usage{
list_dirs(
  path = ".",
  full.names = TRUE,
  recursive = FALSE,
  maxdepth = 1,
  pattern
)
}
\arguments{
\item{path}{same as list.dirs()}

\item{full.names}{same as list.dirs()}

\item{recursive}{same as list.dirs()}

\item{maxdepth}{integer (default = 1) maximum depth to search}

\item{pattern}{character (default = missing) regex pattern match on directory name}
}
\description{
Wrapper around list.dirs() but includes maxdepth and pattern arguments
 and removes full.names argument, always return full names.
}