##' Perform Radar Searches Over Large Area
##'
##' Divides the area into equally sized squares and performs a radar search in the center of each square.
##' @usage areaScan(area, radius, types, details=FALSE, random=FALSE, random_breaks=NULL)
##' @param area a 2x2 data frame containing the x-coordinates (long) of two points in the first column and the
##' y-coordinates (lat) in the second column, or a list of such data frames.
##' @param radius numeric, the radius for the google places radar search
##' @param types character, the location types to be searched for
##' @param details logical, indicating whether details on locations should be extracted
##' @param random logical, indicating whether the order of points to search should be random (default:FALSE)
##' @param random_breaks numeric vector of length 2, indicating the interval [min,max] no. of seconds to wait
##' between calls to the api, defaults to NULL (no breaks)
##' @return object of class "locations" (if area is one dataframe) or a list of objects of class "locations" (if area is a list of dataframes)
##' @details
##' The function divides the area into equally sized squares with a diagonal of 2*radius
##' and performs a radar search in the center of each square (and additionally
##' a details search for each radar search reference)
##' NOTE: randomizations and list-input only works if details==FALSE!
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' # example of search: scan all switzerland
##' library(sp)
##' cantons <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_CHE_1_sp.rds"))
##' # extract map edges for two cantons
##' edges <- getAllEdges(list(cantons[1,], cantons[2,]))
##' # plot search area
##' area <- getAllPointsArea(edges, radius=20000)
##' plot(area[[1]])
##' plot(area[[2]])
##' scanresp <- areaScan(area=edges,radius=20000, types="church",details=FALSE, random=TRUE, random_breaks=c(0,0.3))
##' data <- placesData(scanresp)
##' data <- data[!duplicated(data$id),]
##' View(data)
##' @export
##' @importFrom httr parse_url
##' @import data.table


areaScan <-
  function(area, radius, types, details=FALSE, random=FALSE, random_breaks=NULL) {

    if (details == FALSE) {

      if (class(area)=="data.frame") {
        area <- list(area)
      }

      # get all search locations for all areas
      points <- getAllPointsArea(area, radius = radius)
      locs <- locations(points)
      names(locs) <- 1:length(locs)
      # stack locations
      all_locs <- unlist(locs)
      all_locs_df <- data.frame(loc=all_locs,
                                loc_id = substr(names(all_locs),
                                                 start = 1,
                                                 stop = 1))
      names(all_locs_df) <- c("loc", "loc_id")
      row.names(all_locs_df) <- NULL
      # randomize order if necessary
      if (random==TRUE) {
        all_locs_df <- all_locs_df[sample(1:nrow(all_locs_df),
                                          nrow(all_locs_df),
                                          replace = FALSE),]
      }

      # search all the places with or without breaks
      if (!is.null(random_breaks)){
        # prepare iteration with breaks
        refs_list <- list()
        n_locs <- nrow(all_locs_df)
        length(refs_list) <- n_locs
        # iterate over locs with random breaks in between
        for (i in 1:n_locs) {
          loc.i <- as.character(all_locs_df$loc[i])
          # issue query
          refs_list[[i]] <- as.data.table(placesData(searchNearby(location = loc.i,
                                         radius = radius,
                                         types = types)))
          # take a random break
          Sys.sleep(runif(1,
                          min = random_breaks[1],
                          max = random_breaks[2]))
        }

        # stack results
        refs <- rbindlist(refs_list, use.names = TRUE, fill = TRUE)
        refs <- refs[refs$status=="OK",] # only keep those responses with results


      } else {
        refs <- as.data.table(placesData(searchNearby(location=all_locs_df$loc,
                                        radius=radius,
                                        types=types)))
        refs <- refs[refs$status=="OK",] # only keep those responses with results


      }

      # recover the search location from the query url
      refs$loc <- unlist(lapply(refs$query_url,
                                function(x) {
                                  parse_url(x)$query$location
                                }
      ))
      # merge with the location ids to link with search areas
      refs <- merge(refs, all_locs_df, all=TRUE)

      # create and return scan object for each of the areas
      n_areas <- length(area)
      scanresp_list <- list()
      length(scanresp_list) <- n_areas
      for (i in 1:n_areas) {
        scanresp_list[[i]] <- new("scan",
                        search.results = refs[refs$loc_id==i,],
                        locations = points[[i]])
      }

      if (n_areas > 1) {
        return(scanresp_list)
      } else {
        return(scanresp_list[[1]])
      }



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

