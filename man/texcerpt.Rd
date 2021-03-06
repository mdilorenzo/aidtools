% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text-functions.R
\name{texcerpt}
\alias{texcerpt}
\title{A Text Excerpting Function}
\usage{
texcerpt(text, words, n)
}
\arguments{
\item{text}{Character string to search.}

\item{words}{Character vector containing key search words.}

\item{n}{Return +/- n words around key search words.}
}
\description{
This function extracts words and their surrounding text. Note that this only words with individual words (or multiple individual words).
}
\examples{
texcerpt(example, words = c("data", "statistic"), n = 10)
}
