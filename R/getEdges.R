##' Get Polygon Edges
##'
##' Returns a 2x2 data frame containing the two extreme points (edges) of the polygon.
##' @usage getEdges(x, zoomfactor=1)
##' @param x object of class Polygon, Polygons, SpatialLinesDataFrame, SpatialPolygons, or SpatialPolygonsDataFrame
##' @param zoomfactor numeric, a number to multiply the resulting long/lat values in order to "zoom in" or "zoom out" (e.g. a zoomfactor biggar than 1 is widening the area, defaults to 1)
##' @return the function returns an object of class placesearch, containing the search results and information on the search parameters
##' @details
##' getAllEdges is the vectorized version
##' Given a map object of class Polygon, Polygons, SpatialLinesDataFrame, SpatialPolygons, or SpatialPolygonsDataFrame
##' the function returns a 2x2 data frame containing the two extreme points (edges) of the polygon with the x-coordinates (long)  in the first column and the
##' y-coordinates (lat) in the second column. This is used as input for a area search. (instead of manually providing such a data.frame)
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' # example:
##' cantons <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_CHE_1_sp.rds"))
##' ag <- cantons[cantons$NAME_1=="Aargau",]
##' edges <- getEdges(x=ag, zoomfactor=1.01)
##' several_cantons <- list(cantons[1,], cantons[2,])
##' several_edges <- getAllEdges(several_cantons)
##' @export
##' @import ggplot2
##'


getEdges <-
  function(x, zoomfactor=1) {

    df <- fortify(x)
    bottomleft <- data.frame(x=min(df$long), y=min(df$lat))
    topright <- data.frame(x=max(df$long), y=max(df$lat))

    adf <- rbind(bottomleft, topright)
    adf <- adf*zoomfactor

    return(adf)
  }



##' Get Polygon Edges
##'
##' Returns a list of 2x2 data frame containing the two extreme points (edges) of the polygon.
##' @usage getAllEdges(x, zoomfactor=1)
##' @param x a list containing objects of class Polygon, Polygons, SpatialLinesDataFrame, SpatialPolygons, or SpatialPolygonsDataFrame
##' @param zoomfactor numeric, a number to multiply the resulting long/lat values in order to "zoom in" or "zoom out" (e.g. a zoomfactor biggar than 1 is widening the area, defaults to 1)
##' @return the function returns an object of class placesearch, containing the search results and information on the search parameters
##' @details
##' getAllEdges is the vectorized version of getEdges
##' Given a map object of class Polygon, Polygons, SpatialLinesDataFrame, SpatialPolygons, or SpatialPolygonsDataFrame
##' the function returns a 2x2 data frame containing the two extreme points (edges) of the polygon with the x-coordinates (long)  in the first column and the
##' y-coordinates (lat) in the second column. This is used as input for a area search. (instead of manually providing such a data.frame)
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' # example:
##' cantons <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_CHE_1_sp.rds"))
##' ag <- cantons[cantons$NAME_1=="Aargau",]
##' edges <- getEdges(x=ag, zoomfactor=1.01)
##' several_cantons <- list(cantons[1,], cantons[2,])
##' several_edges <- getAllEdges(several_cantons)
##' @export
##' @import ggplot2
##'


getAllEdges <- Vectorize(getEdges, SIMPLIFY = FALSE)
