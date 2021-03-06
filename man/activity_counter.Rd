% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project-split-functions.R
\name{activity_counter}
\alias{activity_counter}
\title{Count Activities in an AidData Project}
\usage{
activity_counter(project)
}
\arguments{
\item{project}{Row and column indices of AidData project.}
}
\description{
Returns a count of unique activities tagged to a project.
}
\examples{
activity_counter(project = aiddata_core$aiddata_activity_codes[1])
}
