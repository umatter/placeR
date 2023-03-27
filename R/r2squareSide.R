##' Get Square Side
##'
##' Compute the length of the square side of the square that has a diagonal equal to 2r
##' @param r numeric, radius of a circle
##' @return numeric
##' @details
##' Given a radius r of a circle, compute the length of the square
##' side of the square that has a diagonal equal to 2r
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' # example:
##' r2squareSide(5)
##' @export
##'


r2squareSide <-
  function(r) {

    return(sqrt(((2*r)^2)/2))

  }


