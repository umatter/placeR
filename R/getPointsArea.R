##' Get Cell Centers of large Area
##'
##' Divides an area into cells of the size with the diagonal
##' and returns the lng/lat coordinates of the cells' center
##' @usage getPointsArea(area, radius)
##' @param area a data.frame containing two lng/lat points
##' @param radius numeric, the radius of the individual search areas
##' @return object of class "locations"
##' @details
##' A function that divides an area (defined by two lng/lat points) into cells of the size with the diagonal
##' twice the given radius (in meters)  and returns the lng/lat coordinates of the cells' center
##' area: a 2x2 data frame containing the x-coordinates (long) of two points in the first column and the
##' y-coordinates (lat) in the second column.
##' The function returns an object of class "locations" (see classes_methods.R) containing
##' the lng/lat coordinates as well as additional data for print- and plot methods for location objects.
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' # example:
##' r2squareSide(5)
##' @export
##' @import Imap geosphere
##'

getPointsArea <-
  function(area, radius) {

    # format area input for plotting etc.
    a.df <- area
    names(a.df) <- c("x","y")
    a.df <- expand.grid(a.df)
    a.df <- data.frame(a.df[order(a.df$x,a.df$y),],row.names=NULL)
    a.df$edges <- c("b.left", "t.left", "b.right", "t.right")

    res_raster <- r2squareSide(radius) # the sidelenght of the square that covers a circle with radius. this will serve as the resolution of the raster

    # compute distance between AB, AC
    distance.h <- gdist(lon.1=a.df$x[a.df$edges=="t.left"],
                        lat.1=a.df$y[a.df$edges=="t.left"],
                        lon.2=a.df$x[a.df$edges=="t.right"],
                        lat.2=a.df$y[a.df$edges=="t.right"], units="m")

    distance.v <- gdist(lon.1=a.df$x[a.df$edges=="t.left"],
                        lat.1=a.df$y[a.df$edges=="t.left"],
                        lon.2=a.df$x[a.df$edges=="b.left"],
                        lat.2=a.df$y[a.df$edges=="b.left"], units="m")


    # compute raster (starting with upper left edge on map)

    # 1) compute distances to each of the (inner) gridlines on long (h) and lat (v) axis
    # starting from the upper left edge (point A)
    seq.h <- seq(from=res_raster/2, to=distance.h, by=res_raster)
    seq.v <- seq(from=res_raster/2, to=distance.v, by=res_raster)


    # 2) compute corresponding coordinates
    points.h <- sapply(seq.h, FUN=function(x){

      destPointRhumb(a.df[a.df$edges=="t.left", c("x","y")], 90, x)

    })
    points.h <- data.frame(t(points.h))
    names(points.h) <- c("x","y")

    points.v <- sapply(seq.v, FUN=function(x){

      destPointRhumb(a.df[a.df$edges=="t.left", c("x","y")], 180, x)

    })
    points.v <- data.frame(t(points.v))
    names(points.v) <- c("x","y")

    # 3) compute input coordinates for radar search (intersections of gridline coordinates from above)
    search.points <- expand.grid(list(points.h$x,points.v$y))
    names(search.points) <- c("x", "y")


    # format for radarSearch, keep search.points for illustration
    loc <- round(search.points,6)
    loc <- loc[,c("y","x")]
    names(loc)  <- c("lat","long")
    locations <- latlongString(loc)

    # Compute variables for plot method

    # (outer gridlines only for illustration)
    grid.h <- seq(from=res_raster, to=distance.h, by=res_raster)
    grid.v <- seq(from=res_raster, to=distance.v, by=res_raster)

    gp.h <- sapply(grid.h, FUN=function(x){

      destPointRhumb(a.df[a.df$edges=="t.left", c("x","y")], 90, x)

    })
    gp.h <- data.frame(t(gp.h))
    names(gp.h) <- c("x","y")

    gp.v <- sapply(grid.v, FUN=function(x){

      destPointRhumb(a.df[a.df$edges=="t.left", c("x","y")], 180, x)

    })
    gp.v <- data.frame(t(gp.v))
    names(gp.v) <- c("x","y")

    # compute coordinates for drawing circles (as illustration only):

    circle.dat <- as.data.frame(cbind(direction = seq(0, 2*pi,by=2*pi/100), magnitude = radius))
    circle.list <- lapply(1:nrow(search.points), FUN = function(i){

      df <- data.frame(vectordestination(as.numeric(search.points[i,]), circle.dat))
      names(df) <- c("x", "y")
      df

    }) # each list entry contains a df with the coordinates of points on the circles line (i.e. each list entry provides data to print one circle)


    # compute center of the map/search area (lies half-way between the two coordinates in the area variable)
    c.dist <- gdist(lon.1=a.df[a.df$edges=="b.left", "x"],
                    lat.1=a.df[a.df$edges=="b.left", "y"],
                    lon.2=a.df[a.df$edges=="t.right", "x"],
                    lat.2=a.df[a.df$edges=="t.right", "y"],,units="m")

    b <- bearingRhumb(p1= as.numeric(a.df[a.df$edges=="b.left",c("x","y")]),
                      p2= as.numeric(a.df[a.df$edges=="t.right",c("x","y")]))

    center <- destPointRhumb(p=as.numeric(a.df[a.df$edges=="b.left",c("x","y")]), b=b, d=c.dist/2)



    stats <-  data.frame( N.points = round(nrow(x=search.points), 0),
                          lon.min = min(search.points$x),
                          lon.max = max(search.points$x),
                          lat.min = min(search.points$y),
                          lat.max = max(search.points$y),
                          area.km2 = (distance.h/1000)*(distance.v/1000),
                          search.radius = radius)

    row.names(stats) <- ""


    # pack data in lists for locations object as response
    .s <- list(locations=locations, search.radius=radius)
    .i <- list(search.points=search.points,
               area.edges=a.df,
               map.center=center,
               grid.vline=gp.h,
               grid.hline=gp.v,
               circles=circle.list)

    resp <- new("locations", search = .s, illustration = .i, statistics=stats)
    return(resp)
  }

