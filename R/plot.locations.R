##' Plot Method For Location Objects
##'
##' Takes an object of class locations and plots the locations to a map
##' @usage locations(x)
##' @param x object of class location
##' @param ggmap.zoom integer, zoom parameter for ggmap output
##' @param ncircles integer, the number of circles to be plotted (centered around search area, defaults to 1)
##' @return NULL (output is plotted)
##' @author Ulrich Matter <umatter@protonmail.com>
##' @export
##' @import ggmap


# definition of the plot function
plot.locations <-
  function(x, ggmap.zoom=7, ncircles=1) {

    if(class(x)!="locations") {

      cat(paste("Error: ",bquote(x), " is not an object of class locations.\n", sep=""))

    } else {

      # register api key
      register_google(api.key)

      # get necessary data
      data <- x@illustration
      center <- data$map.center
      grid.vline <- data$grid.vline
      grid.hline <- data$grid.hline
      area.edges <- data$area.edges
      search.points <- data$search.points
      circles <- data$circles

      # plot the locations object with ggmap/ggplot
      center_map <- get_googlemap(center, zoom=ggmap.zoom, scale=2, color="bw")
      p <-  ggmap(center_map) +
        geom_vline(aes(xintercept = x), data=grid.vline, colour="white", alpha=.5) +
        geom_hline(aes(yintercept = y), data=grid.hline, colour="white", alpha=.5)+
        geom_point(aes(x = x, y = y), data = area.edges, shape=3, size = 3, colour = 'cyan')+
        geom_point(aes(x = x, y = y), data = search.points, shape=3, size=3, colour="orange")+
        geom_line(aes(x=x, y=y), data= area.edges[1:2,], colour="cyan")+
        geom_line(aes(x=x, y=y), data= area.edges[c(1,3),], colour="cyan")+
        geom_line(aes(x=x, y=y), data= area.edges[c(2,4),], colour="cyan")+
        geom_line(aes(x=x, y=y), data= area.edges[3:4,], colour="cyan")

      if (ncircles>0){

        n <- ifelse(ncircles>length(circles), length(circles), ncircles)
        for (i in 1:n) {
          p <- p + geom_path( aes(x,y), data=circles[[i]], colour="orange")
        }

      }

      return(p)
    }
  }


# set the method:
setMethod(f = "plot",
          signature = "locations",
          definition = plot.locations
)
