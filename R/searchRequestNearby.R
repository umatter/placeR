##' Handle Nearby Search
##'
##' Sends GET requests, extracts and parses response objects
##' @param url.list list of urls (character strings)
##' @return a list with the parsed json objects
##' @details NOTE the function automatically requests additional results if available. This requires to suspend execution in order to
##' respect the time gap "between when a next_page_token is issued, and when it will become valid." (see: https://developers.google.com/places/documentation/search)
##' @author Ulrich Matter <umatter@protonmail.com>
##' @export
##' @import httr


searchRequestNearby <-
  function (url.list) {

    response.list <- lapply(url.list, FUN=function(.url){

      response <- try(GET(url=.url, config = config(http_version = 0)),
                      silent = TRUE) #GET the response as response-object (httr), maybe add later: , user_agent("GooglePlaceR")
      trials_left <- 5
      while(class(response)[1]=="try-error" & 0 < trials_left ) {
        message("Error occurred while fetching data from API, let's take a short break and try it again!\n")
        Sys.sleep(2)
        response <- try(GET(url=.url, config = config(http_version = 0)))
        trials_left <- trials_left-1

      }

      cont <- content(response)
      if ("status" %in% names(cont)){
        zero <- cont$status=="ZERO_RESULTS"
      }


      if (!http_error(response) | !zero) {

        message(paste0(.url, " requested...\n"))
        cont <- content(response, as="parsed") #parse the content (should automatically recognize json and parse it)
        # add query url
        cont$results <- lapply(cont$results, function(x) {
          x$query_url <- .url
          return(x)})

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
          message(paste0(paste(.url,"&pagetoken=",npt,sep=""), " requested...\n"))
          cont <- content(response, as="parsed") #parse the content (should automatically recognize json and parse it)
          # add query url
          cont$results <- lapply(cont$results, function(x) {
            x$query_url <- .url
            return(x)})

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

    return(unlist(response.list, recursive=FALSE)) # possibly an alternative: redlist

  }
