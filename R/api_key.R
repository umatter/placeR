#' Set up Google Places API Key
#'
#' This function helps users to set up their Google Places API key as an
#' environment variable. It allows the placeR package to access the API
#' for fetching place information and other services provided by the Google Places
#' API.
#'
#' @param key A character string representing the user's Google Places API key.
#'
#' @return No return value. This function sets the API key as an environment
#' variable for the current R session.
#'
#' @details
#' The Google Places API requires an API key to use its services. This function
#' is a convenient way to set up the API key as an environment variable,
#' allowing the placeR package to access the API services. The API key is
#' stored as an environment variable only for the current R session and will
#' not persist across sessions. To use the API key in future sessions, call
#' this function again with the appropriate key.
#'
#' @seealso
#' For more information on obtaining a Google Places API key, visit:
#' \url{https://developers.google.com/maps/documentation/places/web-service/get-api-key}
#'
#' @examples
#' \dontrun{
#'   # Set up your API key
#'   api_key("your_google_places_api_key")
#' }
#'
#' @export
#'
api_key <- function(key) {
  Sys.setenv(API_KEY = key)
}
