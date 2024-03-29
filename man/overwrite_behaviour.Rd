% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NMproject-options.R
\name{overwrite_behaviour}
\alias{overwrite_behaviour}
\title{Overwrite behaviour of NMproject}
\usage{
overwrite_behaviour(txt = c("ask", "overwrite", "stop", "skip"))
}
\arguments{
\item{txt}{Character either \code{"run"}, \code{"stop"}, or \code{"skip"}.}
}
\value{
if \code{txt} is missing returns \verb{getOption(\\"nm.overwrite_behaviour\\")}
otherwise returns no value and is called for side effects (setting the
\code{nm.overwrite_behaviour} option).
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

This is best used via the "overwrite behaviour" RStudio 'Addin'.  Sets the
strategy for how to handle overwriting of previously executed control files.
}
