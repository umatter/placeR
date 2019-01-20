##' Extract Details Results
##'
##' Extract all results from a detail response.list (all in one data frame)
##' @usage extractDetailsResults(x)
##' @param response a nested list containing search results from a details search
##' @return a data frame
##' @details x: a nested list containing search results from a details search. each main element is a parsed json response from google places api
##' see extractJsonDetails for what key:value pairs of the parsed json responses are extracted.
##' @author Ulrich Matter <umatter@protonmail.com>
##' @import data.table

extractDetailsResults <-
  function(x) {

    results.list <- lapply(x, FUN=function(.r){

      results.df <- extractJsonDetails(.r)
      results.df

    })

    return(rbindlist(results.list, use.names = TRUE, fill = TRUE))

  }

