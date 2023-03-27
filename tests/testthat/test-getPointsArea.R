test_that("getPointsArea returns correct output", {
  # Define a test area
  test_area <- data.frame(x = c(8.540001, 8.560000), y = c(47.370001, 47.390000))

  # Set test radius
  test_radius <- 500

  # Call the getPointsArea function
  result <- getPointsArea(test_area, test_radius)

  # Check if result is of class "locations"
  expect_true(inherits(result, "locations"))

  # Check if result contains the correct components
  expect_named(result@search, c("locations", "search.radius"))
  expect_named(result@illustration, c("search.points", "area.edges", "map.center", "grid.vline", "grid.hline", "circles"))
  expect_named(result@statistics, c("N.points", "lon.min", "lon.max", "lat.min", "lat.max", "area.km2", "search.radius"))
})
