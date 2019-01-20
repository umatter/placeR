##' Plot Method For Placedetails Objects
##'
##' Takes an object of class scan and plots the locations and search grid to a map
##' @usage plot.placedetails(x, ggmap.zoom="auto", ncircles=1, col.grid="white", col.loc="#e55562", col.area="#6b3a70", col.search="#4db0d9", col.circles="#4db0d9" )
##' @param x object of class placedetails
##' @param ggmap.zoom integer, zoom parameter for ggmap output
##' @param ncircles integer, the number of circles to be plotted (centered around search area, defaults to 1)
##' @param col.grid character, the color of the grid (in hexadecimal or name of R-color; default:"white")
##' @param col.loc character, the color of the locations found (in hexadecimal or name of R-color; default: "#e55562")
##' @param col.area character, the color of the rectangle indicating the search area (in hexadecimal or name of R-color; default: "#6b3a70")
##' @param col.search character, the color of the search area center (in hexadecimal or name of R-color; default: "#4db0d9")
##' @param col.search character, the color of the circles indicating the search radius (in hexadecimal or name of R-color; default: "#4db0d9")
##' @return a ggplot object
##' @author Ulrich Matter <umatter@protonmail.com>
##' @export
##' @import ggmap



# definition of the plot function
plot.placedetails <-
  function(x, ggmap.zoom="auto", ncircles=1, col.grid="white", col.loc="#e55562", col.area="#6b3a70", col.search="#4db0d9", col.circles="#4db0d9" ) {

    if(class(x)!="placedetails") {

      cat(paste("Error: ",bquote(x), " is not an object of class placedetails.\n", sep=""))
    } else {

      # register api key
      register_google(api.key)

      # get necessary data
      search.data <- x@search.results # data related to the placedetails-search results (transformed response from the api)
      search.data$lat <- as.numeric(search.data$lat)
      search.data$lng <- as.numeric(search.data$lng)

      # compute center of the map:
      mlat <- mean(search.data$lat)
      mlng <- mean(search.data$lng)
      center <- c(lng=mlng, lat=mlat)

      # compute zoom of the map (as default):

      if (ggmap.zoom=="auto") {

        box <- make_bbox(lon=lng, lat=lat, data=search.data, f=0.15) # f is for extension of range, needed because bbox queries are experimental at google
        center_map <- get_map(location=box, source="google",  scale=2, color="bw")
      } else {

        center_map <- get_googlemap(center, zoom=ggmap.zoom, scale=2, color="bw")
      }


      # plot the placedetails object with ggmap/ggplot
      p <-  ggmap(center_map) + # add the search results in an extra layer
        geom_point(aes(x = lng, y = lat), data = search.data, shape=1, size = 1, colour = col.loc)

      return(p)
    }
  }

# set the method:
setMethod(f = "plot",
          signature = "placedetails",
          definition = plot.placedetails
)
