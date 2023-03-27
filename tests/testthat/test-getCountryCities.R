
test_that("getCountryCities function works correctly", {
  # Provide the path to the GHS settlement grid raster file
  path_to_raster <- "../../_misc/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0.tif"
  skip_if_not(file.exists(path_to_raster))

  # Test the function
  swiss_cities <- getCountryCities(path_to_raster, country = "SWITZERLAND", tol = 0.005)

  # Check the output
  expect_s4_class(swiss_cities, "SpatialPolygonsDataFrame")
  expect_true(any(swiss_cities$in_country))
  expect_true(any(!swiss_cities$in_country))
})
