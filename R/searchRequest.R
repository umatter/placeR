##' Search Request
##'
##' sends GET requests, extracts and parses response objects,
##' @param url.list list of urls (character strings)
##' @return a list with the parsed json objects
##' @details NOTE the function automatically requests additional results if available. This requires to suspend execution in order to
##' respect the time gap "between when a next_page_token is issued, and when it will become valid." (see: https://developers.google.com/places/documentation/search)
##' @author Ulrich Matter <umatter@protonmail.com>
##' @export
##' @import httr


searchRequest <-
  function (url.list) {

    response.list <- lapply(url.list,
                            FUN=function(.url){

      if (!http_error(.url)) {

        response <- GET(url=.url) #GET the response as response-object (httr), maybe add later: , user_agent("GooglePlaceR")
        cont <- content(response, as="parsed") #parse the content (should automatically recognize json and parse it)
        resp.list <- list(cont)

        # check whether there are more results, i.e., does the response contain a next_page_token property?
        npt.index <- grep(pattern="next_page_token", x=labels(cont), fixed=TRUE, value=FALSE)
        n <- 1
        while (length(npt.index) > 0) { # while more results, get them (add to resp.list)

          Sys.sleep(2) # suspend execution for two seconds
          # reason: "There is a short delay between when a next_page_token is issued, and when it will become valid."
          # (https://developers.google.com/places/documentation/search)

          npt <- cont[[npt.index]]
          response <- GET(url=paste(.url,"&pagetoken=",npt,sep=""))
          cont <- content(response, as="parsed") #parse the content (should automatically recognize json and parse it)

          n <- n+1
          resp.list[[n]] <- cont

          npt.index <- grep(pattern="next_page_token", x=labels(cont), fixed=TRUE, value=FALSE)

        }

        return(resp.list)

      }else{

        response <- GET(url=.url) #GET the response as response-object (httr), maybe add , user_agent("GooglePlaceR")
        warn_for_status(response) #issues a warning if a http error occurs

        httperror <- http_status(response)
        return(httperror)

      }


    })

    res <- unlist(response.list, recursive=FALSE) # possibly an alternative: redlist
    return(res)
  }
