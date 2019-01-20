##' Extract Opening Hours
##'
##' Extract opening hours of a details search response
##' @usage openingHours(r)
##' @param r the "results" part of a detailsRequest response
##' @return a data frame
##' @details
##' a function to extract all opening hours of a response and to transform them in a df with days as columns
##' containing times as values. Used as internal function in extractJsonDetails()
##' @author Ulrich Matter <umatter@protonmail.com>
##'

openingHours <-
  function(r) {

    oh <- r$opening_hours$periods
    oh.list <- lapply(oh, FUN=function(x){

      oday <- x$open$day
      cday <- x$close$day
      otime <- x$open$time
      ctime <- x$close$time

      df <- data.frame(paste(otime, ctime, sep="-"))
      names(df) <- paste("day",oday, "-", cday, sep="")
      return(df)
      })

    oh.df <- do.call("cbind", oh.list)
    return(oh.df)
  }
