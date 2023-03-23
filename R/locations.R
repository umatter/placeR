##' Get Locations Vector
##'
##' Takes an object of class locations or a list of objects of class locations as input and returns
##' the lng/lat-locations-vector of the object's search slot
##' @usage locations(x)
##' @param x object of class location or a list of objects of class location
##' @return character vector with coordinate strings
##' @details
##' a function that takes an object of class locations or a list of objects of class locations as an input and returns
##' the lng/lat-locations-vector of the object's search slot.
##' @author Ulrich Matter <umatter@protonmail.com>
##' @export
##'
locations <- function(x) {
  # Check if x is a list
  if (class(x) == "list") {
    # If x is a list, apply the locations function to each element and return the combined result
    locations_list <- lapply(x, function(location) {
      if (class(location) != "locations") {
        stop("All elements in the list must be of class 'locations'.")
      }
      location@search$locations
    })
    return(do.call(c, locations_list))
  } else if (class(x) == "locations") {
    # If x is a single object of class "locations", return the lng/lat-locations-vector of the object's search slot
    return(x@search$locations)
  } else {
    stop("Error: x is neither an object of class 'locations' nor a list of objects of class 'locations'.")
  }
}
