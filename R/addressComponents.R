##' Extract Address Components
##'
##' Extract all address_components of a details search response
##' @usage addressComponents(r)
##' @param r the "results" part of a detailsRequest response
##' @return a data frame
##' @details
##' Used as internal function in extractJsonDetails()
##' @author Ulrich Matter <umatter@protonmail.com>
##'

addressComponents <-
  function(r) {

    ac <- r$address_components

    ac.list <- lapply(ac, FUN=function(x){

      type <- paste(x[["types"]], collapse=".")
      ln <- data.frame(x[["long_name"]])
      names(ln) <- type
      return(ln)

    })

    ac.df <- do.call("cbind", ac.list)
    return(ac.df)
  }

