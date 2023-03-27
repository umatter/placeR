##' Extract Data From Radar Search Results
##'
##' Extract all results from a response.list (all in one data frame), adds the location-input (l) to the respective response
##' @param x a nested list containing search results. each main element is a parsed json response from google places api
##' @param l the location-input-list for the main search function
##' @return a data.frame/data.table (containing the extracted data from all objects in the response)
##' @details see extractJsonRadar for details on what key:value pairs of the parsed json responses are extracted.
##' @author Ulrich Matter <umatter@protonmail.com>
##'
#' @importFrom data.table rbindlist



extractResultsRadar <-
  function(x, l) {

    results.list <- lapply(1:length(x), FUN=function(i){

      r <- x[[i]]
      results.df <- extractJsonRadar(r)
      results.df$search.location <- l[[i]]
      results.df

    })

    return(data.table::rbindlist(results.list, fill = TRUE, use.names = TRUE))

  }
