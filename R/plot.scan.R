##' Plot Method For Scan Objects
##'
##' Takes an object of class scan and plots the locations and search grid to a map
##' @usage plot.scan(x, ggmap.zoom=7, ncircles=1, col.grid="white", col.loc="#e55562", col.area="#6b3a70", col.search="#4db0d9", col.circles="#4db0d9" )
##' @param x object of class scan
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
plot.scan <-
  function(x, ggmap.zoom=7, ncircles=1, col.grid="white", col.loc="#e55562", col.area="#6b3a70", col.search="#4db0d9", col.circles="#4db0d9" ) {

    if(class(x)!="scan") {

      cat(paste("Error: ",bquote(x), " is not an object of class scan.\n", sep=""))
    } else {

      # register api key
      register_google(api.key)

      # get necessary data
      data <- x@locations@illustration # data related to the search input/the area searched
      center <- data$map.center
      grid.vline <- data$grid.vline
      grid.hline <- data$grid.hline
      area.edges <- data$area.edges
      search.points <- data$search.points
      circles <- data$circles

      search.data <- x@search.results # data related to the scan-search results (transformed response from the api)

      # plot the scan object with ggmap/ggplot
      center_map <- get_googlemap(center, zoom=ggmap.zoom, scale=2, color="bw")

      p <-  ggmap(center_map) +
        geom_vline(aes(xintercept = x), data=grid.vline, colour=col.grid, alpha=.5) +
        geom_hline(aes(yintercept = y), data=grid.hline, colour=col.grid, alpha=.5)+
        geom_point(aes(x = lng, y = lat), data = search.data, shape=1, size = 1, colour = col.loc)+ # add the search results in an extra layer
        geom_point(aes(x = x, y = y), data = area.edges, shape=3, size = 3, colour = col.area)+
        geom_point(aes(x = x, y = y), data = search.points, shape=3, size=3, colour=col.search)+
        geom_line(aes(x=x, y=y), data= area.edges[1:2,], colour=col.area)+
        geom_line(aes(x=x, y=y), data= area.edges[c(1,3),], colour=col.area)+
        geom_line(aes(x=x, y=y), data= area.edges[c(2,4),], colour=col.area)+
        geom_line(aes(x=x, y=y), data= area.edges[3:4,], colour=col.area)

      if (ncircles>0){

        n <- ifelse(ncircles>length(circles), length(circles), ncircles)

        for (i in 1:n) {

          p <- p + geom_path( aes(x,y), data=circles[[i]], colour=col.circles)

        }
      }
      return(p)
    }
  }


# set the method:
setMethod(f = "plot",
          signature = "scan",
          definition = plot.scan
)
