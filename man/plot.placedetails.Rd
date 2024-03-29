% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.placedetails.R
\name{plot.placedetails}
\alias{plot.placedetails}
\title{Plot Method For Placedetails Objects}
\usage{
plot.placedetails(
  x,
  ggmap.zoom = "auto",
  ncircles = 1,
  col.grid = "white",
  col.loc = "#e55562",
  col.area = "#6b3a70",
  col.search = "#4db0d9",
  col.circles = "#4db0d9"
)
}
\arguments{
\item{x}{object of class placedetails}

\item{ggmap.zoom}{integer, zoom parameter for ggmap output}

\item{ncircles}{integer, the number of circles to be plotted (centered around search area, defaults to 1)}

\item{col.grid}{character, the color of the grid (in hexadecimal or name of R-color; default:"white")}

\item{col.loc}{character, the color of the locations found (in hexadecimal or name of R-color; default: "#e55562")}

\item{col.area}{character, the color of the rectangle indicating the search area (in hexadecimal or name of R-color; default: "#6b3a70")}

\item{col.search}{character, the color of the circles indicating the search radius (in hexadecimal or name of R-color; default: "#4db0d9")}
}
\value{
a ggplot object
}
\description{
Takes an object of class scan and plots the locations and search grid to a map
}
\author{
Ulrich Matter <umatter@protonmail.com>
}
