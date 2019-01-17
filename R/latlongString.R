##' Get Coordinate String
##'
##' Transforms coordinats from getPointsArea into character string for searchRadar
##' @usage latlongString(coords)
##' @param coords numeric, coordinates from getPointArea
##' @return character vector with coordinate strings
##' @details
##' a function that takes an x and y coordinate (numbers from a getPointsArea response (df)) and
##' returns each row as a character string for the searchRadar tpation parameter.
##' @author Ulrich Matter <umatter@protonmail.com>
##' @export
##'


latlongString <-
  function(coords) {

    coords[,1] <- as.character(coords[,1] )
    coords[,2] <- as.character(coords[,2])


    tps <-   sapply(1:nrow(coords), FUN=function(i){

      ri <- paste(coords[i,1],coords[i,2], sep=",")
      ri

    })
    return(tps)

  }

