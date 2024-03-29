% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import-code.R
\name{search_raw}
\alias{search_raw}
\title{Search for files matching raw text search}
\usage{
search_raw(files, text, search_title = TRUE, search_contents = TRUE)
}
\arguments{
\item{files}{Vector string of files (either names or paths).}

\item{text}{String (can be regex) to search for.}

\item{search_title}{Logical (default=\code{TRUE}). Should matching occur in title.}

\item{search_contents}{Logical (default=\code{TRUE}). Should matching occur in
file contents.}
}
\value{
A subset of \code{files} with contents matching \code{text}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Searches through the list of supplied for matching strings of text.  Useful
in finding files that you know contain certain text snippets.
}
\examples{


ls_scripts("Scripts") \%>\% search_raw("AUC") ## finds all scripts containing string "AUC"

## regex match find instances of AUC() function being used
ls_scripts("Scripts") \%>\% search_raw("AUC\\\\(")

## requires NMproject directory structure to operate in
\dontrun{
## bring file(s) into project
ls_scripts("/path/to/other/analysis/scripts/dir") \%>\%
  search_raw("AUC\\\\(") \%>\%
  import()
}

}
\seealso{
\code{\link[=ls_scripts]{ls_scripts()}}, \code{\link[=ls_code_library]{ls_code_library()}}, \code{\link[=stage]{stage()}}
}
