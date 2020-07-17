##' Add City Names
##' Matches coordinates (points) of cities to urban center polygons.
##' @usage addCityNames(poly, cities, expand="auto")
##' @param poly SpatialPolygonsDataFrame object (identifying cities as features)
##' @param cities path to shapefile containing official city coordinates
##' @param expand numeric or character ("auto"), the width to slightly expand the polygon for the overlay (helps in coast regions, where the overlay with points might fail). Defaults to "auto", the expand is computed based on the total area of the bounding box containing all polygons.
##' @return SpatialPolygonsDataFrame object
##' @details Note that if extend is larger than 0, the extension only affects the overlay with city ponits. The returned polygon is not extended.
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' # first get the cities polygons
##' PATH <- "_misc/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0.tif"
##' poly <- getCountryCities(PATH, country="SWITZERLAND", tol=0.001)
##' # load cities from http://worldmap.harvard.edu/data/geonode:placemarks_edited_columns_yz6
##' cities <- "_misc/placemarks_edited_columns_yz6/placemarks_edited_columns_yz6.shp"
##' swiss_cities_named <- addCityNames(poly, cities)
##' @export
##' @import sp rgeos rgdal spatialEco data.table
##'

addCityNames <-
  function(poly, cities, expand="auto") {

    # ensure correct input
    if (class(cities)[1]=="character") {
      if (file.exists(cities)){
        cities <- readOGR(cities, verbose = FALSE)
      }
    }

    if (expand=="auto"){
      expand <- area(rgeos::bbox2SP(bbox=poly@bbox))/1750000000 # Note: this is based on the Italy example, results in ~500 for Italy, which works well there
    }

    stopifnot(class(cities)=="SpatialPointsDataFrame")

    # cut out city-markers within polygons and merge meta data
    poly2 <- spTransform(x=poly, CRSobj=CRS("+init=epsg:32662"))
    poly2 <- gBuffer(poly2, byid = TRUE, width = expand)
    poly2 <- spTransform(x=poly2, CRSobj=poly@proj4string)
    cities <- spTransform(x=cities, CRSobj = poly@proj4string) # seems to be necessary on OSX but not on Linux, unclear why
    #cities_points <- SpatialPoints(cities@coords, cities@proj4string)
    match_data <- na.omit(over(cities, poly2))
    points_data <- cities@data[as.numeric(row.names(match_data)),]
    points_data$match_id <- row.names(match_data)
    match_data$match_id <- row.names(match_data)
    match_data <- as.data.table(merge(match_data, points_data, by="match_id", all=TRUE))

    # filter/clean results (current goal: keep only major cities)
    match_data[,is_max:=max(population)==population, by=ID]
    match_data <- match_data[is_max==TRUE]
    match_data$match_id <- NULL
    match_data$is_max <- NULL
    match_data$asciiname <- as.character(match_data$asciiname)

    # add to polygon data
    # NOTE: need to keep same order of rows!!! (sort==F)
    merged_dat <- merge(poly@data,
                       match_data[,-2:-3],
                       by="ID",
                       all.x = TRUE,
                       all.y = FALSE, sort=FALSE)
    poly@data <- merged_dat
    poly <- poly[!is.na(poly$asciiname),] # only keep matched polygons

    return(poly)

  }



