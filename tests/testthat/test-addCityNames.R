test_that("addCityNames function works correctly", {

  # Prepare input data
  PATH <- "_misc/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0.tif"
  poly <- getCountryCities(PATH, country="SWITZERLAND", tol=0.001)
  cities <- "_misc/placemarks_edited_columns_yz6/placemarks_edited_columns_yz6.shp"

  # Test the function
  swiss_cities_named <- addCityNames(poly, cities)

  # Check the output
  expect_s3_class(swiss_cities_named, "SpatialPolygonsDataFrame")
  expect_true(nrow(swiss_cities_named@data) > 0)
  expect_true(any(!is.na(swiss_cities_named@data$asciiname)))

  # Test with expand parameter set to a numeric value
  swiss_cities_named_expand <- addCityNames(poly, cities, expand=500)

  # Check the output
  expect_s3_class(swiss_cities_named_expand, "SpatialPolygonsDataFrame")
  expect_true(nrow(swiss_cities_named_expand@data) > 0)
  expect_true(any(!is.na(swiss_cities_named_expand@data$asciiname)))
})
