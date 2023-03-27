# Internal functions: manages requests, collects result-objects in a list for further processing
#-------------------

# generateURL: takes a searchtype and a combination of search parameters and gives the url back
# searchtype: is a character string, either "nearby", "text", or "radar", defining the type of search
generateURL <-
  function (searchtype, search.param) {
    api.key <- Sys.getenv("API_KEY")
    url <- paste("https://maps.googleapis.com/maps/api/place/", searchtype, "/json?",  search.param, "&key=", api.key, sep="") #generate url for request
    return(url)
  }

# area centroid
# x, area (such as in getPointsArea)
area_centroid <-
  function(x){
    distx <- max(x$x) - min(x$x)
    disty <- max(x$y) - min(x$y)
    cent_x <- min(x$x) + distx/2
    cent_y <- min(x$y) + disty/2

    return(data.frame(x=cent_x, y=cent_y))
  }

# generateParameters: takes parameter values and combines them in order to use them as inputs for generateURL
# all parameters according to https://developers.google.com/places/documentation/search

# for nearby search
generateParametersNearby <-
  function (location, radius, sensor="false", keyword=NULL, language=NULL, name=NULL, rankby=NULL, types=NULL) {


    url.list <- lapply(location, FUN= function(l.){

      params <- paste("&location=", l., "&radius=", radius, "&sensor=", sensor, "&keyword=", keyword, "&language=", language, "&name=", name, "&rankby=", rankby, "&types=", types, sep="")
      generateURL(searchtype="nearbysearch", search.param=params)

    })

    return(url.list)

  }

# for text search
generateParametersText <-
  function (location, radius, sensor="false", keyword=NULL, language=NULL, name=NULL, rankby=NULL, types=NULL) {


    url.list <- lapply(location, FUN= function(l.){

      params <- paste("location=", l., "radius=", radius, "sensor=", sensor, "keyword=", keyword, "language=", language, name, rankby, types, sep="&")

      generateURL(searchtype="text", search.param=params)

    })

    return(url.list)

  }


# for radar search
# example: generateParametersRadar(location="51.503186,-0.126446", radius=5000, types="museum")
# NOTE: original radar search has depreciated, see https://cloud.google.com/blog/products/maps-platform/announcing-deprecation-of-place-add
# "Nearby Search can work as an alternative for Radar Search, when used with rankby=distance and without keyword or name"
generateParametersRadar <-
  function (location, sensor="false", types=NULL) {

    url.list <- lapply(location, FUN= function(l.){
      params <- paste("location=", l., "&sensor=", sensor, "&types=", types,  "&rankby=distance", sep="")
      generateURL(searchtype="nearbysearch", search.param=params)

    })

    return(url.list)

  }


# internal function to generate parameters-part of url for a details request
generateParametersDetails <-
  function (reference, sensor="false", language=NULL) {

    url.list <- lapply(reference, FUN= function(ref){
      params <- paste("&reference=", ref, "&sensor=", sensor,  "&language=", language, sep="")
      generateURL(searchtype="details", search.param=params)

    })

    return(url.list)

  }


#-----------------------
# dlStatus(x, pause=0)
#-----------------------
# A function that shows the current id/object in a loop or vectorized function and pauses
# the loop for a certain time if needed
# x = the current id/object in the loop
# pause

dlStatus <-
  function(x, pause=0) {

    Sys.sleep(pause)
    cat("\r",x)
    flush.console()

  }

