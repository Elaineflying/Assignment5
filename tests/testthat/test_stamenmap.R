
test_that("generateStamenMap rejects errounous input", {
  expect_error(generateStamenMap("New York", "toners", 10))
  expect_error(generateStamenMap("New York", "watercolor", 21))
  expect_error(generateStamenMap(100, "watercolor", 15))
})


test_that("Geocodes the address(latitude and longitude) are correct", {
  maps <- generateStamenMap("New York", "watercolor", 15)
  expect_equal(unlist(round(unname(maps$lat_longs[c(2,3)]),2)), c(40.71,-74.01))
})

test_that("map_image is a valid image object", {
  maps <- generateStamenMap("New York", "watercolor", 15) 
  # Use image_read to read image object
  map_image <- image_read(maps$map_image)
  # Check if map_image is a valid image object
  expect_true(inherits(map_image, "magick-image"))
})
