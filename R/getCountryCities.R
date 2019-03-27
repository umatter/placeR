##' Get City Polygons of a Country
##' Extracts polygon 'city' boundaries from GHS settlement grid raster data
##' @usage getCountryCities(r, country, min_value = 3, tol=0.1, intermediate=FALSE)
##' @param r RasterLayer object of imported GHS settlement grid raster file, or path to such a file
##' @param country character, name of the country according to GADM database
##' @param min_value integer, minimum cell value to consider (as 'city', defaults to 3, urban centers)
##' @param tol numeric, tolerance value to be used by the Douglas-Peuker algorithm to simplify the country-shapefile, defaults to .1
##' @param intermediate logical, if TRUE, intermediate results (raster data and country polygon are returned as well), defaults to FALSE
##' @return SpatialPolygonsDataFrame object (or a list of SpatialPolygonsDataFrame, raster and SpatialPolygons objects, see details)
##' @details The European Commission's GHS SETTLEMENT GRID (LDS) (https://ghslsys.jrc.ec.europa.eu/ghs_smod.php) provides raster data on settlements (urban centers/surroundings)
##' worldwide. This function takes their data as input, identifies all urban areas ('cities') in a given country, and extracts
##' the shape of the cities (polygons) as features of one SpatialPolygons object of the country's extent.
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' PATH <- "_misc/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0.tif"
##' swiss_cities <- getCountryCities(PATH, country="SWITZERLAND", tol=0.05)
##' plot(swiss_cities)
##' @export
##' @import sp rgeos
##' @importFrom raster getData
##' @importFrom raster crop
##' @importFrom raster mask
##' @importFrom raster projectRaster
##' @importFrom raster raster
##' @importFrom raster rasterToPolygons
##' @importFrom raster crs
##'


getCountryCities <-
  function(r, country, min_value = 3, tol=0.1, intermediate = FALSE){

    # ensure correct input
    if (class(r)[1]=="character") {
      if (file.exists(r)){
        rdat <- raster(r)
      }
    } else {
      rdat <- r
    }
    stopifnot(class(rdat)=="RasterLayer")


    # import country shape from GADM via temporary file
    message("Importing country shapefile...\n")
    tempd <- tempdir()
    cty <- getData("GADM",
                   country=country,
                   level=0,
                   path = tempd)
    #file_path <- list.files(tempd)
    #unlink(tempd, recursive = TRUE)

    # simplify polygons
    message("Simplifying country shapefile...\n")
    cty <- gSimplify(cty, tol=tol, topologyPreserve=TRUE)
    # change projection (to fit with raster)
    cty_moll <- spTransform(cty, CRSobj = CRS(proj4string(rdat)))

    # extract the country's part of the raster data
    message("Cropping and masking country from raster data...\n")
    r_cty_ext <- crop(rdat, cty_moll)
    r_cty <- mask(r_cty_ext, cty_moll)
    r_masked <- r_cty # keep for detailed output
    r_cty[r_cty < min_value] <- NA # only keep cells with min urban value

    # set projection to original country projection
    country_proj <- proj4string(cty)
    #country_proj <- "+proj=longlat +datum=WGS84"
    r_cty <- projectRaster(r_cty,  crs = country_proj)
    # convert to polygons
    message("Converting country raster to polygons...\n")
    r_cty_pol <- rasterToPolygons(r_cty,
                                  na.rm = TRUE,
                                  dissolve = TRUE)

    # assign polygons as features
    message("Extracting city polygons...\n")
    cities <- extractPolygonIslands(r_cty_pol)
    cities_meta <- lapply(cities@polygons,
                          FUN = function(i){
                            data.frame(ID=i@ID,
                                       area=i@area)
                          })
    cities_df <- do.call("rbind", cities_meta)
    cities <- SpatialPolygonsDataFrame(Sr = cities,
                                       data = cities_df)
    # keep the extent of the input file to facilitate plotting
    cities@bbox <- cty@bbox

    # return results in more or less detail
    if (intermediate) {
      return(list(cities=cities,
                  country=cty,
                  settlement_raster=r_cty_ext,
                  country_raster=r_masked,
                  city_raster=r_cty))
    } else {
      return(cities)

    }

  }
