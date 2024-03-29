% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractJsonRadar.R
\name{extractJsonRadar}
\alias{extractJsonRadar}
\title{Extract Data From Response of Radar Search}
\usage{
extractJsonRadar(response)
}
\arguments{
\item{response}{nested list, parsed json-object returned from the API}
}
\value{
a data.frame/data.table (containing the extracted data, or the error message from the response)
}
\description{
Internal function to extract the data from one API response based on a radar search (large area)
}
\details{
...
}
\author{
Ulrich Matter <umatter@protonmail.com>
}
