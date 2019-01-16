##' Search Nearby
##'
##' Main search function
##' @param location character, containing the coordinates (long/lat separated by a comma) of where the map search shall be executed
##' @param radius numeric, giving radius to be used for the nearby search
##' @param sensor character, the map search sensor option (defaults to "false")
##' @param keyword character, keywords to be used (defaults to NULL)
##' @param language character, language parameter of search (defaults to NULL, default language will be used)
##' @param name character, name parameter of search (defaults to NULL)
##' @param rankby character, rank parameter for search results (defaults to NULL)
##' @param type character, type of objects to be searched (defaults to NULL)
##' @return the function returns an object of class placesearch, containing the search results and information on the search parameters
##' @details Takes parameter values according to google Places API
##' all parameter values are character strings, but the location value can be a
##' list of lat/long-character-strings (allowing for automated search of many locations)
##' NOTE the function automatically requests additional results
##' if available. This requires to suspend execution in order to
##' respect the time gap "between when a next_page_token is issued, and when it will become valid."
##' (see: https://developers.google.com/places/documentation/search)
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' example (taken from google developers):
##' searchNearby(location="-33.8670522,151.1957362", rankby="distance", types="food", name="harbour")
##' @export
##'

searchNearby <-
  function (location, radius, sensor="false", keyword=NULL, language=NULL, name=NULL, rankby=NULL, types=NULL) {

    if (missing(radius) & !is.null(rankby)) {radius <- NULL}

    urls <- generateParametersNearby(location, radius, sensor, keyword, language, name, rankby, types)

    responses <- searchRequestNearby(url.list=urls)

    results.df <- extractResults(responses)
    parameters.df <- data.frame(location=unlist(location), radius=radius, sensor=sensor,
                                keyword=ifelse(is.null(keyword),NA,keyword),
                                language=ifelse(is.null(language),NA,language),
                                name=ifelse(is.null(name),NA,name),
                                rankby=ifelse(is.null(rankby),NA,rankby),
                                types=ifelse(is.null(types),NA,types))

    resp <- new("placesearch", search.results = results.df, parameters = parameters.df)

    return(resp)

  }
