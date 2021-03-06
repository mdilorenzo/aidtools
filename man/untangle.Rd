% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text-functions.R
\name{untangle}
\alias{untangle}
\title{A Function for Splitting Apart Character Separated Values}
\usage{
untangle(x, char = ",")
}
\arguments{
\item{x}{Character string with character-separated values.}

\item{char}{Character separating values.}
}
\description{
This function splits character-separated values.
}
\examples{
untangle(c("400|500|600"), char = "|")
}
