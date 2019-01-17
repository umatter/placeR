# Internal functions: manages requests, collects result-objects in a list for further processing
#-------------------

# generateURL: takes a searchtype and a combination of search parameters and gives the url back
# searchtype: is a character string, either "nearby", "text", or "radar", defining the type of search
generateURL <-
  function (searchtype, search.param) {
    url <- paste("https://maps.googleapis.com/maps/api/place/", searchtype, "/json?",  search.param, "&key=", api.key, sep="") #generate url for request
    return(url)
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
generateParametersRadar <-
  function (location, radius, sensor="false", keyword=NULL, name=NULL, types=NULL) {

    url.list <- lapply(location, FUN= function(l.){
      params <- paste("location=", l., "&radius=", radius, "&sensor=", sensor, "&keyword=", keyword, "&name=", name, "&types=", types, sep="")
      generateURL(searchtype="radarsearch", search.param=params)

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
