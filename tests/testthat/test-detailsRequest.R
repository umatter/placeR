
test_that("detailsRequest function works correctly", {
  skip_if_not("API_KEY" %in% names(Sys.getenv()))

  # Prepare input data
  base_url <- "https://maps.googleapis.com/maps/api/place/details/json?"
  place_id <- "ChIJN1t_tDeuEmsRUsoyG83frY4" # Google in Sidney
  api_key <- Sys.getenv("API_KEY")
  url <- paste0(base_url, "place_id=", place_id, "&key=", api_key)

  # Test the function
  details_result <- detailsRequest(list(url))

  # Check the output
  expect_type(details_result, "list")
  expect_length(details_result, 1)
  expect_type(details_result[[1]], "list")
  expect_equal(details_result[[1]]$result$name, "Google Workplace 6")
  expect_equal(details_result[[1]]$result$address_components[[2]]$short_name, "Pirrama Rd")
})
