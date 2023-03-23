
test_that("areaScan function works correctly", {
  skip_if_not("API_KEY" %in% names(Sys.getenv()))

  # Prepare input data
  area <- data.frame(
    x = c(-122.5000, -121.5000),
    y = c(37.0000, 38.0000)
  )
  radius <- 5000
  types <- "restaurant"

  # Test the function
  area_scan_result <- areaScan(area, radius, types, details = FALSE, random = FALSE, random_breaks = NULL)

  # Check the output
  expect_s3_class(area_scan_result, "scan")
  expect_is(area_scan_result$search.results, "data.table")
  expect_true("name" %in% names(area_scan_result$search.results))
  expect_true("vicinity" %in% names(area_scan_result$search.results))
  expect_true("place_id" %in% names(area_scan_result$search.results))
})
