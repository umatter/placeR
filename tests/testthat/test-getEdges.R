
test_that("getEdges function works correctly", {
  # Create a simple square polygon
  square <- Polygon(cbind(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0)))
  square_polygons <- Polygons(list(square), "square")
  square_sp <- SpatialPolygons(list(square_polygons))

  # Test the function
  edges <- getEdges(square_sp)

  # Check the output
  expect_is(edges, "data.frame")
  expect_equal(nrow(edges), 2)
  expect_equal(ncol(edges), 2)
  expect_equal(edges[1,], c(0, 0))
  expect_equal(edges[2,], c(1, 1))
})

test_that("getAllEdges function works correctly", {
  # Create two simple square polygons
  square1 <- Polygon(cbind(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0)))
  square2 <- Polygon(cbind(c(1, 1, 2, 2, 1), c(0, 1, 1, 0, 0)))
  square_polygons1 <- Polygons(list(square1), "square1")
  square_polygons2 <- Polygons(list(square2), "square2")
  square_sp1 <- SpatialPolygons(list(square_polygons1))
  square_sp2 <- SpatialPolygons(list(square_polygons2))

  # Test the function
  all_edges <- getAllEdges(list(square_sp1, square_sp2))

  # Check the output
  expect_is(all_edges, "list")
  expect_equal(length(all_edges), 2)

  edges1 <- all_edges[[1]]
  edges2 <- all_edges[[2]]

  expect_is(edges1, "data.frame")
  expect_equal(nrow(edges1), 2)
  expect_equal(ncol(edges1), 2)
  expect_equal(edges1[1,], c(0, 0))
  expect_equal(edges1[2,], c(1, 1))

  expect_is(edges2, "data.frame")
  expect_equal(nrow(edges2), 2)
  expect_equal(ncol(edges2), 2)
  expect_equal(edges2[1,], c(1, 0))
  expect_equal(edges2[2,], c(2, 1))
})
