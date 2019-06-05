##' Get River Polygons of a Country
##' Extracts river polygons from hydroshed shapefile data
##' @usage getCountryRivers(rivers, country, tol=0.1, intermediate=FALSE)
##' @param rivers SpatialLinesDataFrame object of imported hydroshed shapefile, or path to such a file
##' @param country character, name of the country according to GADM database, or an object of class "SpatialPolygons"
##' @param tol numeric, tolerance value to be used by the Douglas-Peuker algorithm to simplify the country-shapefile, defaults to .1
##' @return SpatialPolygonsDataFrame object (or a list of SpatialPolygonsDataFrame, raster and SpatialPolygons objects, see details)
##' @details By default, the function assumes that the river shapefile data is from this HydroSHEDS: https://hydrosheds.org/pages/overview.
##' The user has to select the right shapefile as input (they are devided into regions, EU, North America, etc.). NOTE: reading the rivers shapefile can
##' take a while!
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' # load river network shapefile from https://hydrosheds.org/downloads
##' rivers <- "_misc/hydrosheds/eu_riv_15s/eu_riv_15s.shp"
##' swiss_rivers <- getCountryRivers(rivers, country="SWITZERLAND", tol=0.05)
##' plot(swiss_cities)
##' @export
##' @import sp rgeos
##'


getCountryRivers <-
  function(rivers, country, tol=0.1){

    # ensure correct input
    if (class(rivers)[1]=="character") {
      if (file.exists(rivers)){


        message("Reading rivers shapefile...\n")
        rivers <- readOGR(rivers, verbose = FALSE)
      }
    }
    stopifnot(class(rivers)[1]=="SpatialLinesDataFrame")

    if (class(country)[1]!="SpatialPolygons" & class(country)[1]!="SpatialPolygonsDataFrame") {

      # import country shape from GADM via temporary file
      message("Importing country shapefile...\n")
      tempd <- tempdir()
      cty <- getData("GADM",
                     country=country,
                     level=0,
                     path = tempd)
      #file_path <- list.files(tempd)
      #unlink(tempd, recursive = TRUE)

    } else {
      cty <- country
    }
    stopifnot(class(cty)[1]=="SpatialPolygonsDataFrame")


    # simplify polygons
    message("Simplifying country shapefile...\n")
    cty <- gSimplify(cty, tol=tol, topologyPreserve=TRUE)

    # extract the country's part of the river data
    message("Cropping and masking country from river data...\n")
    r_cty_ext <- try(crop(rivers, cty)) # in rare cases crop fails with Error in RGEOSBinTopoFunc(spgeom1, spgeom2, byid, id, drop_lower_td, unaryUnion_if_byid_false,  : Geometry collections may not contain other geometry collections


    return(r_cty_ext)


  }

