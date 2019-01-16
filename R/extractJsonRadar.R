##' Extract Data From Response of Radar Search
##'
##' Internal function to extract the data from one API response based on a radar search (large area)
##' @param response nested list, parsed json-object returned from the API
##' @return a data.frame/data.table (containing the extracted data, or the error message from the response)
##' @details ...
##' @author Ulrich Matter <umatter@protonmail.com>
##'
##' @import data.table


extractJsonRadar <-
  function(response) {

    if (response[["status"]]=="OK") { # response valid?

      result.dfs <- lapply(response[["results"]],
                           FUN=function(r){ # extract the following key:value pairs and order in df

        r.df <- data.table(reference=r[["reference"]],
                           stringsAsFactors=FALSE )   # could be extended with photos etc.
        return(r.df)
        })

      results <- rbindlist(result.dfs, use.names = TRUE, fill = TRUE)
      results$status <- "OK"

    } else { # error occured: extract status message

      results <- data.table(status=response[["status"]], stringsAsFactors=FALSE)
    }

    return(results)
  }

