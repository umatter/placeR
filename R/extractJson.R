##' Extract Data From Response Object
##'
##' Internal function to extract the data from one API response
##' @param response nested list, parsed json-object returned from the API
##' @return a data.frame/data.table (containing the extracted data, or the error message from the response)
##' @details Takes parameter values according to google Places API
##' all parameter values are character strings, but the location value can be a
##' list of lat/long-character-strings (allowing for automated search of many locations)
##' @author Ulrich Matter <umatter@protonmail.com>
##'
##' @import data.table


extractJson <-
  function(response) {

    if (response[["status"]]=="OK") { # response valid?

      result.dfs <- lapply(response[["results"]],
                           FUN=function(r){ # extract the following key:value pairs and order in df

        r.df <- data.table(id=r[["id"]],
                           name=r[["name"]],
                           lat=r[["geometry"]][["location"]][["lat"]],
                           lng=r[["geometry"]][["location"]][["lng"]],
                           vicinity=r[["vicinity"]],
                           types=paste(r[["types"]], collapse=" "),
                           reference=r[["reference"]],
                           query_url=r[["query_url"]],
                           stringsAsFactors=FALSE )   # could be extended with photos etc.

        return(r.df)

      })
      results <- rbindlist(result.dfs, use.names = TRUE, fill = TRUE)

      if (length(response[["html_attributions"]]) != 0){
        results$html_attributions <- response[["html_attributions"]]

      } else {
        results$html_attributions <- NA
      }
      results$status <- "OK"

    } else { # error occured: extract status message
      results <- data.table(status=response[["status"]], stringsAsFactors=FALSE)
    }

    return(results)
  }

