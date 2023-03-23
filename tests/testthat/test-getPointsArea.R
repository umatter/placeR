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

Save the following test file as test-getAllPointsArea.R:

  R

library(testthat)
library(Imap)
library(geosphere)

source("path/to/your/functions.R") # Replace with the path to the file containing getPointsArea() and getAllPointsArea() functions

test_that("getAllPointsArea returns correct output", {
  # Define a list of test areas
  test_areas <- list(
    data.frame(x = c(8.540001, 8.560000), y = c(47.370001, 47.390000)),
    data.frame(x = c(8.570001, 8.590000), y = c(47.400001, 47.420000))
  )

  # Set test radius
  test_radius <- 500

  # Call the getAllPointsArea function
  results <- getAllPointsArea(test_areas, test_radius)

  # Check if results is a list
  expect_true(is.list(results))

  # Check if each result is of class "locations"
  for (result in results) {
    expect_true(inherits(result, "locations"))
  }

  # Check if each result contains the correct components
  for (result in results) {
    expect_named(result@search, c("locations", "search.radius"))
    expect_named(result@illustration, c("search.points", "area.edges", "map.center", "grid.vline", "grid.hline", "circles"))
    expect_named(result@statistics, c("N.points", "lon.min", "lon.max", "lat.min", "lat.max", "area.km2", "search.radius"))
  }
})

To run the tests, you can use the testthat::test_dir() function in your R console:

  R

library(testthat)
test_dir("path/to/your/test/directory") # Replace with the path to the directory containing the test files

This command will run all test files in the specified directory and display the results. Make sure to replace the paths in the source() function
umatter@protonmail.com
document()
ℹ Updating placeR documentation
ℹ Loading placeR
Warning message:
  replacing previous import ‘data.table::shift’ by ‘spatialEco::shift’ when loading ‘placeR’
>

  It looks like you are trying to update the documentation of your package using the document() function from the devtools package. The warning message you received indicates there is a naming conflict between two packages you are using, data.table and `
