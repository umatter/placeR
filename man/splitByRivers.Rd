% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splitByRivers.R
\name{splitByRivers}
\alias{splitByRivers}
\title{Split City Polygon By Rivers}
\usage{
splitByRivers(cities, rivers)
}
\arguments{
\item{rivers}{path to shapefile containing official river coordinates}

\item{country}{character, name of the country according to GADM database, or an object of class "SpatialPolygons"}
}
\value{
SpatialPolygonsDataFrame object
}
\description{
Adds for each city polygon a layer with sub-polygons that are divided by rivers crossing the city
}
\details{
For each subgeometry an additional id (sub_id) is added to the data-frame. The
}
\examples{
\dontrun{
PATH <- "_misc/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0.tif"
# get the cities in the country
cities <- getCountryCities(r = PATH, country = "SWITZERLAND", tol=0.05)
# get the rivers in the country
rivers <- "_misc/hydrosheds/eu_riv_15s/eu_riv_15s.shp"
cty_rivers <- getCountryRivers(rivers = rivers, country = "SWITZERLAND", tol=0.05)
cities2 <- splitByRivers(cities, cty_rivers)
}
}
\author{
Ulrich Matter <umatter@protonmail.com>
}
