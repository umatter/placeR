#' Get River Polygons of a Country
#' Extracts river polygons from hydroshed shapefile data
#' @usage getCountryRivers(rivers, country, tol=0.1)
#' @param rivers sf object of imported hydroshed shapefile, or path to such a file
#' @param country character, name of the country according to GADM database, or an object of class "SpatVector"
#' @param tol numeric, tolerance value to be used by the Douglas-Peuker algorithm to simplify the country-shapefile, defaults to .1
#' @return sf object representing the river polygons within the specified country
#' @details By default, the function assumes that the river shapefile data is from HydroSHEDS: https://hydrosheds.org/pages/overview.
#' The user has to select the right shapefile as input (they are divided into regions, EU, North America, etc.). NOTE: reading the rivers shapefile can
#' take a while!
#' @author Ulrich Matter <umatter@protonmail.com>
#' @examples
#' \dontrun{
#' # load river network shapefile from https://hydrosheds.org/downloads
#' rivers <- "_misc/hydrosheds/eu_riv_15s/eu_riv_15s.shp"
#' swiss_rivers <- getCountryRivers(rivers, country="SWITZERLAND", tol=0.05)
#' plot(swiss_cities)
#' }
#' @export
#' @import sf geodata
#'


getCountryRivers <-
  function(rivers, country, tol=0.1){
    requireNamespace("terra", quietly = TRUE)
    # ensure correct input
    if (class(rivers)[1]=="character") {
      if (file.exists(rivers)){


        message("Reading rivers shapefile...\n")
        rivers <- sf::st_read(rivers)
      }
    }
    stopifnot(class(rivers)[1]=="sf")

    if (class(country)[1]!="SpatialPolygons" & class(country)[1]!="SpatialPolygonsDataFrame") {

      # import country shape from GADM via temporary file
      message("Importing country shapefile...\n")
      tempd <- tempdir()
      cty <- geodata::gadm( country = country, level = 0, path = tempd)

    } else {
      cty <- country
    }
    stopifnot(class(cty)[1]=="SpatVector")

    # simplify polygons
    message("Simplifying country shapefile...\n")
    cty <- terra::simplifyGeom(cty, tolerance=tol, preserveTopology=TRUE)

    # extract the country's part of the river data
    message("Cropping and masking country from river data...\n")
    r_cty_ext <- suppressWarnings(try(sf::st_crop(rivers, cty)))

    return(r_cty_ext)
  }

