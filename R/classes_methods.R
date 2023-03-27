########################
# placeR
# classes
########################


# set placesearch class (returned from  searchNearby, searchText)

setClass("placesearch",
         slots= c(search.results = "data.frame",
                  parameters = "data.frame"))

# set placedetails class (returned from placeDetails)

setClass("placedetails",
         slots= c(search.results = "data.frame",
                  parameters = "data.frame"))


# set locations class (returned from getPointsArea)

setClass("locations",
         slots= c(search = "list",
                  illustration = "list",
                  statistics = "data.frame"))

# set scan class (returned from areaScan)

setClass("scan",
         slots= c(search.results = "data.frame",
                  locations = "locations"))



# end classes
##############################################################################
# begin methods

#------------------------------
# METHODS FOR CLASS PLACESEARCH
#------------------------------
#------------------------------


#------------------------------------
# print method for class PLACESEARCH
#------------------------------------

# definition of the print function
print.placesearch <- function(x) {


  cat("\n Search Results \n===============================\n\n")
  print(summary(x@search.results))

}

# print method
setMethod(f = "print",
          signature = "placesearch",
          definition = print.placesearch
)


#----------------------------------
# show method for class PLACESEARCH
#----------------------------------

# definition of the show function
show.placesearch <- function(object) {


  cat("\n Search Results \n===============================\n\n")
  print(summary(object@search.results))

}

# show method
setMethod(f = "show",
          signature = "placesearch",
          definition = show.placesearch
)


#----------------------------------
# plot method for class placesearch:
#----------------------------------


#' Plot a placesearch object on a map
#'
#' This function takes a placesearch object and plots it on a map using ggmap.
#' The function allows for customization of colors and zoom level.
#'
#' @param x A placesearch object to be plotted on the map.
#' @param ggmap.zoom Zoom level for the map (default: "auto").
#' @param ncircles Number of circles to be plotted (default: 1).
#' @param col.grid Color for the grid (default: "white").
#' @param col.loc Color for the location points (default: "#e55562").
#' @param col.area Color for the area (default: "#6b3a70").
#' @param col.search Color for the search points (default: "#4db0d9").
#' @param col.circles Color for the circles (default: "#4db0d9").
#'
#' @return A ggplot object with the plotted placesearch object.
#' @export
#' @import ggmap
#' @import ggplot2
#' @examples
#' \dontrun{
#' # Assuming you have a placesearch object named 'ps_obj'
#' plot.placesearch(ps_obj)
#' }
plot.placesearch <- function(x, ggmap.zoom="auto", ncircles=1, col.grid="white", col.loc="#e55562", col.area="#6b3a70", col.search="#4db0d9", col.circles="#4db0d9" ) {


  if(class(x)!="placesearch") {

    cat(paste("Error: ",bquote(x), " is not an object of class placesearch.\n", sep=""))

  } else {

    # get necessary data

    param <- x@parameters
    coord <- as.numeric(unlist(strsplit(as.character(param$location), split=",", fixed=TRUE)))
    names(coord) <- c("lat", "lon")
    search.points <- as.data.frame(t(coord))

    search.data <- x@search.results # data related to the placesearch-search results (transformed response from the api)

    # compute center of the map:
    mlat <- mean(search.points$lat)
    mlon <- mean(search.points$lon)
    center <- c(lon=mlon, lat=mlat)

    # compute zoom of the map (as default):

    if (ggmap.zoom=="auto") {

      box <- make_bbox(lon=lng, lat=lat, data=search.data, f=0.15) # f is for extension of range, needed because bbox queries are experimental at google
      center_map <- get_map(location=box, source="google",  scale=2, color="bw")

    } else {

      center_map <- get_googlemap(center, zoom=ggmap.zoom, scale=2, color="bw")

    }


    # plot the placesearch object with ggmap/ggplot

    p <-  ggmap(center_map) +
      geom_point(aes(x = lng, y = lat), data = search.data, shape=1, size = 1, colour = col.loc)+ # add the search results in an extra layer
      geom_point(aes(x = lon, y = lat), data = search.points, shape=3, size=3, colour=col.search)


    p



  }

}

# set the method:
setMethod(f = "plot",
          signature = "placesearch",
          definition = plot.placesearch
)


#-------------------------------
# METHODS FOR CLASS PLACEDETAILS
#-------------------------------
#-------------------------------


#------------------------------------
# print method for class PLACEDETAILS
#------------------------------------

# definition of the print function
print.placedetails <- function(x) {


  cat("\n Search Results \n===============================\n\n")
  print(summary(x@search.results))

}

# print method
setMethod(f = "print",
          signature = "placedetails",
          definition = print.placedetails
)


#----------------------------------
# show method for class PLACEDETAILS
#----------------------------------

# definition of the show function
show.placedetails <- function(object) {


  cat("\n Search Results \n===============================\n\n")
  print(summary(object@search.results))

}

# show method
setMethod(f = "show",
          signature = "placedetails",
          definition = show.placedetails
)






#-----------------------------
# METHODS FOR CLASS LOCATIONS
#-----------------------------
#-----------------------------


#---------------------------------
# print method for class locations
#---------------------------------

# print and show methods for class locations (print alone did not work,
# see: https://stat.ethz.ch/pipermail/r-devel/2004-December/031649.html:

# definition of the printfunction
print.locations <- function(x) {

  cat("\ngetPointsArea: Results Overview \n===============================\n\n")
  print(x@statistics)

}

# print method
setMethod(f = "print",
          signature = "locations",
          definition = print.locations
          )


#---------------------------------
# show method for class locations
#---------------------------------

# definition of the show function
show.locations <- function(object) {

  cat("\ngetPointsArea: Results Overview \n===============================\n\n")
  print(object@statistics)

}

# show method
setMethod(f = "show",
          signature = "locations",
          definition = show.locations
)





#-----------------------------
# METHODS FOR CLASS SCAN
#-----------------------------
#-----------------------------


#---------------------------------
# print method for class SCAN
#---------------------------------

# definition of the print function
print.scan <- function(x) {

  cat("\ngetPointsArea: Search Input \n===============================\n\n")
  print(x@locations@statistics)

  cat("\n============================================================\n\n")


  cat("\n Scan Search Results \n===============================\n\n")
  print(summary(x@search.results))

}

# print method
setMethod(f = "print",
          signature = "scan",
          definition = print.scan
)


#---------------------------------
# show method for class locations
#---------------------------------

# definition of the show function
show.scan <- function(object) {

  cat("\ngetPointsArea: Search Input \n===============================\n\n")
  print(object@locations@statistics)

  cat("\n============================================================\n\n")


  cat("\n Scan Search Results \n===============================\n\n")
  print(summary(object@search.results))

}

# show method
setMethod(f = "show",
          signature = "scan",
          definition = show.scan
)



