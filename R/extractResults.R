##' Extract Data From Search Results
##'
##' Extract all results from a response.list (all in one data.frame/data.table)
##' @param x a nested list containing search results. each main element is a parsed json response from google places api
##' @return a data.frame/data.table (containing the extracted data from all objects in the response)
##' @details see extractJson for details on what key:value pairs of the parsed json responses are extracted.
##' @author Ulrich Matter <umatter@protonmail.com>
##'
##' @import data.table



extractResults <-
  function(x) {

    results.list <- lapply(x, FUN=function(r){

      results.df <- extractJson(r)
      return(results.df)

    })

    return(rbindlist(results.list, use.names = TRUE, fill = TRUE))
  }

