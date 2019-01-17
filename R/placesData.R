##' Get Places Data
##'
##' Extracts the search.results from a scan object
##' @usage placesData(x)
##' @param x object of class location
##' @return character vector with coordinate strings
##' @details A function that extracts the search.results from a scan object.
##' @author Ulrich Matter <umatter@protonmail.com>
##' @export
##'

placesData <-
  function(x) {

    x@search.results

  }
