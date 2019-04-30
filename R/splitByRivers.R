##' Split City Polygon By Rivers
##' Adds for each city polygon a layer with sub-polygons that are divided by rivers crossing the city
##' @usage splitByRivers(country, rivers)
##' @param country character, name of the country according to GADM database, or an object of class "SpatialPolygons"
##' @param rivers path to shapefile containing official river coordinates
##' @return SpatialPolygonsDataFrame object
##' @details ...
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' PATH <- "_misc/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0.tif"
##' # get the cities in the country
##' cities <- getCountryCities(r = PATH, country = "SWITZERLAND", tol=0.05)
##' # get the rivers in the country
##' rivers <- "_misc/hydrosheds/eu_riv_15s/eu_riv_15s.shp"
##' cty_rivers <- getCountryRivers(rivers = rivers, country = "SWITZERLAND", tol=0.05)
##' cities2 <- splitByRivers(cities, cty_rivers)
##' @export
##' @import sp rgeos rgdal spatialEco data.table geobuffer
##'


splitByRivers <-
  function(cities, rivers) {

    # iterate through each city
    n_cities <- length(cities@polygons)
    for (i in 1:n_cities) {
      # extract the city polygons
      city_pol <- SpatialPolygons(cities@polygons[i], proj4string = CRS(proj4string(cities)))
      # intersect the river-lines with the river lines
      river_intersect <- gIntersection(city_pol, cty_rivers)

      # replace city polygon if it was split by rivers otherwhise keep original city polygon
      if (!is.null(river_intersect)) {
        river_intersect <- spTransform(river_intersect,  CRS(proj4string(cities)))
        # create polygon buffer of the intersected line
        polbuffer <- suppressWarnings(gBuffer(river_intersect, width = 0.000001)) # warning should not matter if zoomed in that much
        # split the city polygons by rivers
        cities_split <- gDifference(city_pol, polbuffer)
        # replace city polygon with splitted city polygon
        cities@polygons[i] <- cities_split@polygons
      }

    }
    return(cities)
  }








