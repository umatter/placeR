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
##' swiss_cities_named <- addCityNames(poly, cities)
##' @export
##' @import sp rgeos rgdal spatialEco data.table
##'

addCityNames <-
  function(poly, cities) {

    # ensure correct input
    if (class(cities)[1]=="character") {
      if (file.exists(cities)){
        cities <- readOGR(cities)
      }
    }

    stopifnot(class(cities)=="SpatialPointsDataFrame")

    # cut out city-markers within polygons and merge meta data
    cities_points <- SpatialPoints(cities@coords, cities@proj4string, cities@bbox)
    match_data <- na.omit(over(cities_points, poly))
    points_data <- cities@data[row.names(match_data),]
    points_data$match_id <- row.names(points_data)
    match_data$match_id <- row.names(match_data)
    match_data <- as.data.table(merge(match_data, points_data, by="match_id"))

    # filter/clean results (current goal: keep only major cities)
    match_data[,is_max:=max(population)==population, by=ID]
    match_data <- match_data[is_max==TRUE]
    match_data$match_id <- NULL
    match_data$is_max <- NULL

    # add to polygon data
    poly@data <- merge(poly@data,
                       match_data[,-2],
                       by="ID",
                       all.x = TRUE,
                       all.y=FALSE)

    return(poly)

}



