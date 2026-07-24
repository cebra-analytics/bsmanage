context("ManageDivisions")

test_that("initializes with planar CRS raster", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  expect_silent(parts <- ManageDivisions(template))
  expect_is(parts, "ManageDivisions")
  expect_equal(parts$get_type(), "grid")
  expect_equal(parts$get_parts(), length(which(is.finite(template[]))))
  expect_true(parts$is_compatible(parts$get_template()))
  expect_equal(parts$get_indices(), which(is.finite(template[])))
  expect_equal(parts$get_res(), 1000)
  expect_equal(parts$is_included(349:351), c(TRUE, FALSE, TRUE))
  expect_silent(features <- parts$get_feat())
  expect_is(features, "SpatVector")
  expect_length(features, parts$get_parts())
})

test_that("initializes with lonlat raster", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb_wgs84.tif"))
  expect_silent(parts <- ManageDivisions(template))
  coord <- terra::xyFromCell(template, 1113)
  mid_res <- terra::distance(coord, coord + 0.025, lonlat = TRUE)/sqrt(2)
  expect_true(abs(parts$get_res() - mid_res)/mid_res < 0.05)
  expect_equal(parts$is_included(1113:1116), c(TRUE, FALSE, FALSE, TRUE))
})

test_that("gets a raster with specified values", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  expect_silent(parts <- ManageDivisions(template))
  expect_error(parts$get_rast(1:10),
               paste("Values should be a single value or vector of length",
                     "matching the number of region locations."))
  expect_silent(value_rast <- parts$get_rast(10))
  expect_equal(value_rast[parts$get_indices()][,1],
               rep(10, parts$get_parts()))
  expect_silent(value_rast <- parts$get_rast(1:parts$get_parts()))
  expect_equal(value_rast[parts$get_indices()][,1], 1:parts$get_parts())
  expect_true(terra::is.int(value_rast))
  parts <- ManageDivisions(terra::as.int(template*10))
  expect_true(terra::is.int(parts$get_template()))
  expect_silent(
    value_rast <- parts$get_rast(round(runif(parts$get_parts()), 1)))
  expect_false(terra::is.int(value_rast))
  expect_true(all(
    unique(value_rast[parts$get_indices()][,1]) %in% ((0:10)/10)))
  expect_true(length(unique(value_rast[parts$get_indices()][,1])) > 2)
  cat_values <- factor(c("a", rep(c("b", "c"), 198)), c("a", "b", "c", "d"))
  expect_silent(value_rast <- parts$get_rast(cat_values))
  expect_true(terra::is.factor(value_rast))
  expect_equal(terra::cats(value_rast)[[1]],
               data.frame(ID = 1:4, category = c("a", "b", "c", "d")))
  expect_equal(value_rast[parts$get_indices()][,1], cat_values)
})

test_that("initializes with CSV data", {
  TEST_DIRECTORY <- test_path("test_inputs")
  locations <- utils::read.csv(file.path(TEST_DIRECTORY, "vic_cities.csv"))
  expect_silent(parts <- ManageDivisions(locations))
  expect_equal(parts$get_type(), "patch")
  expect_equal(parts$get_parts(), nrow(locations))
  expect_true(parts$is_compatible(1:nrow(locations)))
  expect_equal(parts$get_data(), locations)
  expect_equal(parts$get_coords(), locations[, c("lon", "lat")])
  expect_equal(parts$get_coords(extra_cols = TRUE),
               locations[, c("lon", "lat", "name")])
  expect_silent(features <- parts$get_feat())
  expect_is(features, "SpatVector")
  expect_length(features, parts$get_parts())
  categories <- data.frame(categ = c("a", "b", "c"), other = 1:3)
  expect_silent(parts <- ManageDivisions(categories))
  expect_equal(parts$get_type(), "other")
})
