#' Place Details Function
#'
#' This function retrieves detailed information about a place using its reference.
#' It can also save the results to disk and allows for customizing the request process.
#'
#' @param reference A character vector of reference(s) for the place(s) you want to retrieve details for.
#' @param language (Optional) A character string specifying the language to be used for the returned data. Default is NULL.
#' @param saveDL (Optional) A logical value indicating whether to save the responses to disk. Default is FALSE.
#' @param chunksize (Optional) An integer specifying the number of requests to process at a time when saving responses to disk. Default is 100.
#' @param pause (Optional) A numeric value indicating the time (in seconds) to pause between chunks when saving responses to disk. Default is 0.
#'
#' @return A "placedetails" S4 object containing the search results (results.df) and the parameters used for the request (parameters.df).
#' @export
#'
#' @examples
#' \dontrun{
#' reference <- "some_place_reference"
#' place_details <- placeDetails(reference, language="en", saveDL=FALSE)
#'}
placeDetails <- function(reference, language = NULL, saveDL = FALSE, chunksize = 100, pause = 0) {
  urls <- generateParametersDetails(reference, language)

  if (saveDL == FALSE) {
    responses <- detailsRequest(url.list = urls)
  } else {
    responses <- saveDL(request.function = detailsRequest, request.id = urls, chunksize = chunksize, pause = pause)
  }

  results.df <- extractDetailsResults(x = responses)
  parameters.df <- data.frame(
    reference = unlist(reference),
    language = ifelse(is.null(language), NA, language)
  )

  resp <- new("placedetails", search.results = results.df, parameters = parameters.df)
  return(resp)
}
