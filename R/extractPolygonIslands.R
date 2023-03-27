##' Extract Polygon Islands
##'
##' Store each polygon (island) as individual feature of a SpatialPolygons object
##' @param spdf SpatialPolygonsDataFrame object
##' @return SpatialPolygons object
##' @details The input spdf contains several polygons, all in one feature (as a result of extracting polygons from a raster).
##' @author Ulrich Matter <umatter@protonmail.com>
##' @export
##' @import sp


extractPolygonIslands <-
  function(spdf) {

    # get number of polygons
    n_poly <- length(spdf@polygons[[1]]@Polygons)
    # set ids for each polygon
    ids <- as.character(1:n_poly)

    # extract each polygon
    polys <- lapply(1:n_poly,
                    FUN = function(i) {
                      Polygons(spdf@polygons[[1]]@Polygons[i], ID = ids[i])

    })

    # initiate SpatialPolygons object, keep same projection as input object
    spo <- SpatialPolygons(polys, proj4string = CRS(proj4string(spdf)))
    return(spo)


  }
