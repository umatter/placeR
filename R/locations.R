##' Get Locations Vector
##'
##' Takes an object of class locations as an input and returns
##' the lng/lat-locations-vector of the object's search slot
##' @usage locations(x)
##' @param x object of class location
##' @return character vector with coordinate strings
##' @details
##' a function that takes an object of class locations as an input and returns
##' the lng/lat-locations-vector of the object's search slot.
##' @author Ulrich Matter <umatter@protonmail.com>
##' @export
##'

locations <-
  function(x) {

    if(class(x)=="locations") {

      return(x@search$locations)

    } else {

      cat(paste("Error: ",bquote(x), " is not an object of class locations.\n", sep=""))
    }


  }

##' Get Locations Vector
##'
##' Takes a list of objects of class locations as an input and returns
##' the lng/lat-locations-vector of the object's search slot
##' @usage locations(x)
##' @param x a list of objects of class location
##' @return character vector with coordinate strings
##' @details
##' The vectorized version of locations().
##' a function that takes an object of class locations as an input and returns
##' the lng/lat-locations-vector of the object's search slot.
##' @author Ulrich Matter <umatter@protonmail.com>
##' @export
##'
locations <- Vectorize(locations, SIMPLIFY = TRUE)

