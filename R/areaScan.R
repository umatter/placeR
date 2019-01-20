##' Perform Radar Searches Over Large Area
##'
##' Divides the area into equally sized squares and performs a radar search in the center of each square.
##' @usage areaScan(area, radius, types, details=FALSE)
##' @param area a 2x2 data frame containing the x-coordinates (long) of two points in the first column and the
##' y-coordinates (lat) in the second column.
##' @param radius numeric, the radius for the google places radar search
##' @param types character, the location types to be searched for
##' @param details logical, indicating whether details on locations should be extracted
##' @return object of class "locations"
##' @details
##' The function divides the area into equally sized squares with a diagonal of 2*radius
##' and performs a radar search in the center of each square (and additionally
##' a details search for each radar search reference)
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' # example of search: scan all switzerland
##' cantons <- url("http://gadm.org/data/rda/CHE_adm1.RData") # get a shapefile with swiss admin. borders
##' print(load(cantons))
##' # extract map edges
##' edges <- getEdges(x=gadm)
##' # plot search area
##' area <- getPointsArea(edges, radius=20000)
##' plot(area)
##' scanresp <- areaScan(area=edges, radius=8000, types="restaurant", details=TRUE )
##' data <- placesData(scanresp)
##' data <- data[!duplicated(data$id),]
##' View(data)
##' @export
##'


areaScan <-
  function(area, radius, types, details=FALSE) {

    if (details == FALSE) {

      points <- getPointsArea(area, radius)
      locs <- locations(points)
      #refs <- searchRadar(location=locs, radius=radius, types=types) # original radar search is depreciated
      refs <- searchNearby(location=locs, radius=radius, types=types)
      #     latlong <- strsplit(x=refs$search.location, split=",", fixed=TRUE)
      #     latlong  <- do.call("rbind", latlong)
      #     refs$lat <- latlong[,1]
      #     refs$lng <- latlong[,2]

      scanresp <- new("scan", search.results = placesData(refs), locations = points)
      return(scanresp)

    } else {

      points <- getPointsArea(area, radius)
      locs <- locations(points)
      #refs1 <- searchRadar(location=locs, radius=radius, types=types) # original radar search is depreciated
      refs1 <- searchNearby(location=locs, radius=radius, types=types)
      refs2 <- unique(na.omit(as.character(refs1$reference))) #extract unique references for detailed search

      detail <- placesData(placeDetails(reference=refs2)) # search details to the references
      detail$lat <- as.numeric(detail$lat)
      detail$lng <- as.numeric(detail$lng)

      scanresp <- new("scan", search.results = detail, locations = points)
      return(scanresp)

    }
  }

