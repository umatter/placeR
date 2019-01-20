##' Extract JSON Details
##'
##' Internal function to extract the data from one details search response
##' @usage extractJsonDetails(response)
##' @param response the response object of a detailsRequest response
##' @return a data frame
##' @details ...
##' @author Ulrich Matter <umatter@protonmail.com>
##'

extractJsonDetails <-
  function(response) {
    if (response[["status"]]=="OK") { # response valid?

      r <- response[["result"]]

      r.list <- list(id=r[["id"]],
                     name=r[["name"]],
                     lat=r[["geometry"]][["location"]][["lat"]],
                     lng=r[["geometry"]][["location"]][["lng"]],
                     vicinity=r[["vicinity"]],
                     types=paste(r[["types"]], collapse=" "),
                     reference=r[["reference"]],
                     formatted_address=r[["formatted_address"]],
                     rating=r[["rating"]],
                     price_level=r[["price_level"]],
                     url=r[["url"]],
                     utc_offset=r[["utc_offset"]],
                     website=r[["website"]],
                     reference.new=r[["reference"]] #reference in response (might be different than in request)
      )   # could be extended with opening hours etc.

      results1  <- data.frame(t(unlist(r.list)), stringsAsFactors=F)
      ac <- addressComponents(r)

      if (is.null(r$opening_hours)) {

        oh <- data.frame(day00=NA)
      } else {

        oh <- openingHours(r)
      }

      results <- cbind(results1, ac, oh)

      if(length(response[["html_attributions"]])>0){

        results$html_attributions <- response[["html_attributions"]]
      } else{

        results$html_attributions <- NA
      }
      results$status <- "OK"

      } else { # error occured: extract status message

      results <- data.frame(status=response[["status"]], stringsAsFactors=FALSE)
    }

    return(results)
  }


