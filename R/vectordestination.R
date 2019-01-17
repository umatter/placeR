##' Draw Circle On Map
##'
##' Draw a circle with radius of x meters
##' @param longlatpoint numeric vector with coordinates
##' @param travelvector direction of circle
##' @return the function returns an object of class placesearch, containing the search results and information on the search parameters
##' @details # Function written by Arien Lam
# https://stat.ethz.ch/pipermail/r-help/2006-November/116851.html
##' @author Arien Lam
##' @examples
##' # example: use this function to
##'
##' radius <- 10000 # in meter
##' NewHaven <- c(-72.9,41.3) #c(lon,lat)
##' circlevector <- as.data.frame(cbind(direction = seq(0, 2*pi,
##'                                                      by=2*pi/100), magnitude = radius))
##' mycircle <- vectordestination(NewHaven, circlevector)
##' plot(mycircle,type="l")##' @export
##'


vectordestination <-
  function(lonlatpoint, travelvector) {
    Rearth <- 6372795
    Dd <- travelvector$magnitude / Rearth
    Cc <- travelvector$direction

    if (class(lonlatpoint) == "SpatialPoints") {
      lata <- coordinates(lonlatpoint)[1,2] * (pi/180)
      lona <- coordinates(lonlatpoint)[1,1] * (pi/180)
    }
    else {
      lata <- lonlatpoint[2] * (pi/180)
      lona <- lonlatpoint[1] * (pi/180)
    }
    latb <- asin(cos(Cc) * cos(lata) * sin(Dd) + sin(lata)
                 * cos(Dd))
    dlon <- atan2(cos(Dd) - sin(lata) * sin(latb), sin(Cc)
                  * sin(Dd) * cos(lata))
    lonb <- lona - dlon + pi/2

    lonb[lonb >  pi] <- lonb[lonb >  pi] - 2 * pi
    lonb[lonb < -pi] <- lonb[lonb < -pi] + 2 * pi

    latb <- latb * (180 / pi)
    lonb <- lonb * (180 / pi)

    cbind(longitude = lonb, latitude = latb)
  }




