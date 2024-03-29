% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractResults.R
\name{extractResults}
\alias{extractResults}
\title{Extract Data From Search Results}
\usage{
extractResults(x)
}
\arguments{
\item{x}{a nested list containing search results. each main element is a parsed json response from google places api}
}
\value{
a data.frame/data.table (containing the extracted data from all objects in the response)
}
\description{
Extract all results from a response.list (all in one data.frame/data.table)
}
\details{
see extractJson for details on what key:value pairs of the parsed json responses are extracted.
}
\author{
Ulrich Matter <umatter@protonmail.com>
}
