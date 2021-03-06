% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project-split-functions.R
\name{dollars2shape}
\alias{dollars2shape}
\title{A Function for Merging World Bank IBRD-IDA Project Amounts with a Shapefile}
\usage{
dollars2shape(shapefile, max_precision = 3, amt_var = "commitments",
  sectors = "all", project_status = c("Implementation", "Completion"),
  fetch = TRUE, projects = NA, locations = NA)
}
\arguments{
\item{shapefile}{Shapefile to append data slot with columns for financial amounts by year.}

\item{max_precision}{Maximum precision score of project locations to include.}

\item{amt_var}{Desired financial amounts: "commitments" or "disbursements".}

\item{project_status}{Vector containing "Implementation" and/or "Completion". Both by default.}

\item{fetch}{Defaults to TRUE. If TRUE, gets World Bank IBRD-IDA from AidData website. If FALSE, user needs to supply 2 data frames as objects: projects and locations.}

\item{sector}{Sectors to include. Default is "all" which returns all sectors.}
}
\description{
Get amount for equal splits of World Bank IBRD-IDA projects across locations. The returned object is the shapefile appended with a column counting dollar amounts of World Bank projects for every year.
}
\examples{
dollars2shape(shapefile = shape, max_precision = 2, amt_var = "commitments", sectors = c(120, 121))
}
