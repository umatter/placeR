##' Get City Polygons of a Country
##' Extracts city polygon boundaries from the GHS settlement grid raster data
##' @param r A RasterLayer object of the imported GHS settlement grid raster file or the file path to such a file
##' @param country A character string representing the name of the country according to the GADM database, or a SpatialPolygonsDataFrame containing the country data
##' @param sub_country A character string representing the name of the subnational unit to select from the country (if NULL, the entire country is processed; default: NULL)
##' @param sub_country_var A character string representing the variable name in the country used for the variable that distinguishes subnational units (if NULL, the entire country is processed; default: NULL)
##' @param min_value An integer representing the minimum cell value to consider as a city (default is 3, representing urban centers)
##' @param tol A numeric value representing the tolerance value used by the Douglas-Peuker algorithm to simplify the country shapefile (default is 0.1)
##' @param intermediate A logical value; if TRUE, intermediate results (raster data and country polygon) are returned as well (default is FALSE)
##' @return A SpatialPolygonsDataFrame object (or a list containing SpatialPolygonsDataFrame, raster, and SpatialPolygons objects if intermediate is set to TRUE)
##' @details The European Commission's GHS SETTLEMENT GRID (LDS) (https://ghslsys.jrc.ec.europa.eu/ghs_smod.php) provides raster data on settlements (urban centers/surroundings) worldwide. This function takes their data as input, identifies all urban areas (cities) in a given country, and extracts the city shapes (polygons) as features of a SpatialPolygons object of the country's extent.
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' \dontrun{
##' PATH <- "_misc/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0.tif"
##' swiss_cities <- getCountryCities(PATH, country="SWITZERLAND", tol=0.005)
##' # contains by default the entire city polygons (also over the country border)
##' plot(swiss_cities)
##' # only consider parts within the country
##' plot(swiss_cities[swiss_cities$in_country,])
##'}
##' @export
##'
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
  function(r, country, sub_country = NULL, sub_country_var = NULL,
           min_value = 3, tol=0.05, intermediate = FALSE){

    requireNamespace("terra", quietly = TRUE)

    if (is.null(sub_country) & is.null(sub_country_var)){
      cty_level <- 0
    } else {
      cty_level <- 1
    }

    # ensure correct input: r
    if (class(r)[1]=="character") {
      if (file.exists(r)){
        rdat <- terra::rast(r)
      }
    } else {
      rdat <- r
    }
    stopifnot(class(rdat)=="SpatRaster")

    # ensure correct input: country
    if (class(country)[1]=="character") {
      # import country shape from GADM via temporary file
      message("Importing country shapefile...\n")
      tempd <- tempdir()
      cty <- geodata::gadm( country = country, level = cty_level, path = tempd)

    } else {
      cty <- country
    }
    #stopifnot(class(cty)=="SpatialPolygonsDataFrame")

    # select sub-national unit to process (if indicated)
    if (!is.null(sub_country) & !is.null(sub_country_var)) {
      cty <- cty[cty@data[,sub_country_var]==sub_country,]
    }


    # simplify polygons
    message("Simplifying country shapefile...\n")
    cty <- terra::simplifyGeom(cty, tolerance=tol, preserveTopology=TRUE)
    # change projection (to fit with raster)
    cty_moll <- terra::project(cty, terra::crs(rdat))

    # extract the country's part of the raster data
    message("Cropping and masking country from raster data...\n")
    r_cty_ext <- terra::crop(rdat, terra::ext(cty_moll)@ptr$vector)
    r_cty <- terra::mask(r_cty_ext, cty_moll)
    r_masked <- r_cty # keep for detailed output
    r_cty[r_cty < min_value] <- NA # only keep cells with min urban value
    r_cty_ext[r_cty_ext < min_value] <- NA # dito for extent

    # set projection to original country projection
    r_cty <- terra::project(r_cty,  terra::crs(cty))
    r_cty_ext <- terra::project(r_cty_ext,  terra::crs(cty))
    r_masked <- terra::project(r_masked, terra::crs(cty))

    # convert to polygons
    message("Converting country raster to polygons...\n")
    r_cty_pol <- terra::as.polygons(r_cty,
                                  na.rm = TRUE,
                                  dissolve = TRUE)
    r_cty_ext_pol <- terra::as.polygons(r_cty_ext,
                                      na.rm = TRUE,
                                      dissolve = TRUE)


    # assign polygons as features
    message("Extracting city polygons...\n")
    cty_sf_ext_pol <- sf::st_as_sf(r_cty_ext_pol) # Convert to sf object
    cty_sp_ext_pol <- as(cty_sf_ext_pol, "Spatial") # Convert to Spatial object
    cty_spdf_ext_pol <- as(cty_sp_ext_pol, "SpatialPolygonsDataFrame") # Convert to SpatialPolygonsDataFrame
    cities <- extractPolygonIslands(cty_spdf_ext_pol)
    cities_meta <- lapply(cities@polygons,
                          FUN = function(i){
                            data.frame(ID=i@ID,
                                       area=i@area)
                          })
    cities_df <- do.call("rbind", cities_meta)
    cities <- SpatialPolygonsDataFrame(Sr = cities,
                                       data = cities_df)
    # keep the extent of the input file to facilitate plotting
    cty_sf <- sf::st_as_sf(cty) # Convert to sf object
    cty_sp <- as(cty_sf, "Spatial") # Convert to Spatial object
    cty_spdf <- as(cty_sp, "SpatialPolygonsDataFrame") # Convert to SpatialPolygonsDataFrame
    cities@bbox <- cty_spdf@bbox
    raster::crs(cty_spdf) <- raster::crs(cities)
    # get rid of cities that are clearly outside the country
    cities <- cities[!is.na(over(cities, cty_spdf))[,1],]
    # rename the ids of polygons properly
    new_ids <- as.character(1:length(cities$ID))
    cities$ID <- new_ids
    row.names(cities@data) <- new_ids
    for (i in 1:length(cities$ID)){
      slot(slot(cities, "polygons")[[i]], "ID") <- new_ids[i]
    }


    # iterate through each city, replace polygon with polygon split by country-border
    n_cities <- length(cities@polygons)
    for (i in 1:n_cities) {
      # extract the city polygons
      city_pol <- SpatialPolygons(cities@polygons[i], proj4string = CRS(proj4string(cities)))
      # intersect the borders with the city lines
      border_intersect <- suppressWarnings(gIntersection(city_pol, cty_spdf)) # warnings are simply due to the rgeos function being retired soon (we keep it for the moment for simplicity)

      # replace city polygon if it was split by rivers otherwhise keep original city polygon
      if (!is.null(border_intersect)) {
        border_intersect <- spTransform(border_intersect,  CRS(proj4string(cities)))
        # create polygon buffer of the intersected line
        polbuffer <- suppressWarnings(gBuffer(border_intersect, width = 0.000001)) # warning should not matter if zoomed in that much
        # split the city polygons by borders
        cities_split <- suppressWarnings(gDifference(city_pol, polbuffer))
        if (!is.null(cities_split)){
        # unite the difference
        cities_split <- union(cities_split, border_intersect)
        # extend ids: 1 is out, and 2 is in the country
        slot(slot(cities_split, "polygons")[[1]], "ID") <- paste0(i, ".out")
        slot(slot(cities_split, "polygons")[[2]], "ID")  <- as.character(i)

        # replace city polygon with splitted city polygon
        # (technically: replace the current polygon with the inside country only polygon)
        cities@polygons[i] <- cities_split@polygons[2]
        # add the outside-part as a separate polygon
        cities@polygons <- c(cities@polygons, cities_split@polygons[[1]])
        }
      }

      # update the dataframe
      ids <- unlist(lapply(slot(cities, "polygons"), slot, "ID"))
      areas <- unlist(lapply(slot(cities, "polygons"), slot, "area"))
      in_country <- !grepl("\\.out", ids)
      slot(cities, "data") <- data.frame(ID=ids,
                                         area=areas,
                                         in_country=in_country,
                                         stringsAsFactors = FALSE)
      # update the plot order
      cities@plotOrder <- 1:length(ids)

    }


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

