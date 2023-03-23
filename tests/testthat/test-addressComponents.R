
test_that("addressComponents function works correctly", {
  skip_if_not("API_KEY" %in% names(Sys.getenv()))

  # Prepare input data
  r <- list(
    address_components = list(
      list(
        long_name = "123 Main Street",
        short_name = "123 Main St",
        types = c("street_address")
      ),
      list(
        long_name = "New York",
        short_name = "NY",
        types = c("locality", "political")
      )
    )
  )

  # Test the function
  ac_df <- addressComponents(r)

  # Check the output
  expect_is(ac_df, "data.frame")
  expect_named(ac_df, c("street_address", "locality.political"))
  expect_equal(ac_df$street_address, "123 Main Street")
  expect_equal(ac_df$locality.political, "New York")
})
