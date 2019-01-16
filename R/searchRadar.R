##' Search Large Areas
##'
##' Main search function for large area searches ('radarsearch')
##' @param location character, containing the coordinates (long/lat separated by a comma) of where the map search shall be executed
##' @param radius numeric, giving radius to be used for the nearby search
##' @param sensor character, the map search sensor option (defaults to "false")
##' @param keyword character, keywords to be used (defaults to NULL)
##' @param name character, name parameter of search (defaults to NULL)
##' @param types character, types of objects to be searched (defaults to NULL)
##' @param saveDL logical, if TRUE chunks of the search results are cached on disk during download (defaults to FALSE)
##' @param chunksize integer, size of chunks if saveDL=TRUE (defaults to 10)
##' @param pause numeric, break to be taken between downloads of chunks (in seconds, defaults to 0)
##' @return a data.frame
##' @details Takes parameter values according to google Places API
##' all parameter values are character strings, but the location value can be a
##' list of lat/long-character-strings (allowing for automated search of many locations)
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' example (taken from google developers):
##' searchRadar(location="51.503186,-0.126446", radius=5000, types="museum")
##' example 2 (Basel)
##' searchRadar(location="47.566667,7.6", radius=2000, types="restaurant")
##' @export
##'


searchRadar <-
  function (location, radius, sensor="false", keyword=NULL, name=NULL, types=NULL, saveDL=FALSE, chunksize=10, pause=0 ) {

    urls <- generateParametersRadar(location, radius, sensor, keyword, name, types)

    if (saveDL==FALSE) {
      responses <- searchRequestRadar(url.list=urls)
    } else {
      responses <- saveDL(request.function=searchRequestRadar, request.id=urls, chunksize=chunksize, pause=pause )
    }

    results.df <- extractResultsRadar(x=responses, l=location)

    return(results.df)

  }
