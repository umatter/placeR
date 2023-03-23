
test_that("detailsRequest function works correctly", {
  skip_if_not("API_KEY" %in% names(Sys.getenv()))

  # Prepare input data
  base_url <- "https://maps.googleapis.com/maps/api/place/details/json?"
  place_id <- "ChIJN1t_tDeuEmsRUsoyG83frY4" # Sydney Opera House
  api_key <- Sys.getenv("API_KEY")
  url <- paste0(base_url, "place_id=", place_id, "&key=", api_key)

  # Test the function
  details_result <- detailsRequest(list(url))

  # Check the output
  expect_is(details_result, "list")
  expect_length(details_result, 1)
  expect_is(details_result[[1]], "list")
  expect_equal(details_result[[1]]$result$name, "Sydney Opera House")
  expect_equal(details_result[[1]]$result$address_components[[1]]$short_name, "Bennelong Point")
})
