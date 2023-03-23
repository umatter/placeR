
test_that("getCountryRivers function works correctly", {
  # Provide the path to the hydroshed shapefile
  path_to_rivers <- "path/to/hydrosheds/eu_riv_15s/eu_riv_15s.shp"
  skip_if_not(file.exists(path_to_rivers))

  # Test the function
  swiss_rivers <- getCountryRivers(path_to_rivers, country = "SWITZERLAND", tol = 0.05)

  # Check the output
  expect_is(swiss_rivers, "SpatialLinesDataFrame")
  expect_true(nrow(swiss_rivers) > 0)
})
