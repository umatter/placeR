% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/searchRadar.R
\name{searchRadar}
\alias{searchRadar}
\title{Search Large Areas}
\usage{
searchRadar(
  location,
  sensor = "false",
  types = NULL,
  saveDL = FALSE,
  chunksize = 10,
  pause = 0
)
}
\arguments{
\item{location}{character, containing the coordinates (long/lat separated by a comma) of where the map search shall be executed}

\item{sensor}{character, the map search sensor option (defaults to "false")}

\item{types}{character, types of objects to be searched (defaults to NULL)}

\item{saveDL}{logical, if TRUE chunks of the search results are cached on disk during download (defaults to FALSE)}

\item{chunksize}{integer, size of chunks if saveDL=TRUE (defaults to 10)}

\item{pause}{numeric, break to be taken between downloads of chunks (in seconds, defaults to 0)}
}
\value{
a data.frame
}
\description{
Main search function for large area searches ('radarsearch')
}
\details{
NOTE: THE RADAR SEARCH FUNCTION IS NOT ANYMORE SUPPORTED BY GOOGLE, SEE
https://cloud.google.com/blog/products/maps-platform/announcing-deprecation-of-place-add.
THE FUNCTION IS NOW IMPLEMENTED AS THE SUGGESTED REPLACEMENT, HOWEVER THIS DOES NOT ANYMORE SUPPORT RADIUS!
Takes parameter values according to google Places API
all parameter values are character strings, but the location value can be a
list of lat/long-character-strings (allowing for automated search of many locations)
}
\examples{
\dontrun{
searchRadar(location="51.503186,-0.126446", types="museum")
searchRadar(location="47.566667,7.6", types="restaurant")
}
}
\author{
Ulrich Matter <umatter@protonmail.com>
}
