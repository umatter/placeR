##' Details Request
##'
##' Handles GET requests for the details method of the Google Places API
##' @usage detailsRequest(url.list)
##' @url.list list of urls (character strings)
##' @return a list with the parsed json objects
##' @details sends GET requests, extracts and parses response objects, returns a list with the parsed json objects
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' @export
##' @import httr

detailsRequest <-
  function (url.list) {

    response.list <- lapply(url.list, FUN=function(.url){
      dlStatus(.url)

      if (url_success(.url)) {

        response <- GET(url=.url) #GET the response as response-object (httr), maybe add later: , user_agent("GooglePlaceR")
        cont <- content(response, as="parsed") #parse the content (should automatically recognize json and parse it)
        return(cont)

      }else{

        response <- GET(url=.url) #GET the response as response-object (httr), maybe add: user_agent("GooglePlaceR")
        warn_for_status(response) #issues a warning if a http error occurs

        httperror <- http_status(response)
        return(httperror)

      }
      })

    return(response.list)
  }
