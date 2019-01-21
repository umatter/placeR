##' Add City Names
##' Matches coordinates (poins) of cities to urban center polygons.
##' @usage addCityNames(poly, cities)
##' @param poly SpatialPolygons object (identifying cities as features)
##' @param cities path to shapefile containing official city coordinates
##' @return SpatialPolygons object
##' @details ...
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' # first get the cities polygons
##' PATH <- "_misc/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0.tif"
##' poly <- getCountryCities(PATH, country="SWITZERLAND", tol=0.05)
##' # load cities from http://worldmap.harvard.edu/data/geonode:placemarks_edited_columns_yz6
##' cities <- "_misc/placemarks_edited_columns_yz6/placemarks_edited_columns_yz6.shp"
##' swiss_cities_named <- addCityNames(swiss_cities, citypath)
##' @export
##' @import sp rgeos rgdal spacialEco
##'

addCityNames <-
  function(poly, cities) {

    # ensure correct input
    if (class(cities)[1]=="character") {
      if (file.exists(cities)){
        cities <- readOGR(cities)
      }
    }

    stopifnot(class(rdat)=="RasterLayer")

    # cut out city-markers within polygons
    cities_data <- cities@data
    cities_dat <- over(cities_data, poly[,"id"])
    # does not work properly yet. Problem: poly is not a spatialPointsDataFrame,
    # better to improve this in getCountryCities!



  }



