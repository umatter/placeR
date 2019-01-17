##' Search Large Areas
##'
##' Main search function for large area searches ('radarsearch')
##' @param location character, containing the coordinates (long/lat separated by a comma) of where the map search shall be executed
##' @param sensor character, the map search sensor option (defaults to "false")
##' @param types character, types of objects to be searched (defaults to NULL)
##' @param saveDL logical, if TRUE chunks of the search results are cached on disk during download (defaults to FALSE)
##' @param chunksize integer, size of chunks if saveDL=TRUE (defaults to 10)
##' @param pause numeric, break to be taken between downloads of chunks (in seconds, defaults to 0)
##' @return a data.frame
##' @details NOTE: THE RADAR SEARCH FUNCTION IS NOT ANYMORE SUPPORTED BY GOOGLE, SEE
##' https://cloud.google.com/blog/products/maps-platform/announcing-deprecation-of-place-add.
##' THE FUNCTION IS NOW IMPLEMENTED AS THE SUGGESTED REPLACEMENT, HOWEVER THIS DOES NOT ANYMORE SUPPORT RADIUS!
##' Takes parameter values according to google Places API
##' all parameter values are character strings, but the location value can be a
##' list of lat/long-character-strings (allowing for automated search of many locations)
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' example (taken from google developers):
##' searchRadar(location="51.503186,-0.126446", types="museum")
##' example 2 (Basel)
##' searchRadar(location="47.566667,7.6", types="restaurant")
##' @export
##'


searchRadar <-
  function (location, sensor="false", types=NULL, saveDL=FALSE, chunksize=10, pause=0 ) {

    urls <- generateParametersRadar(location, sensor, types)

    if (saveDL==FALSE) {
      responses <- searchRequestRadar(url.list=urls)
    } else {
      responses <- saveDL(request.function=searchRequestRadar, request.id=urls, chunksize=chunksize, pause=pause )
    }

    results.df <- extractResultsRadar(x=responses, l=location)

    return(results.df)

  }
