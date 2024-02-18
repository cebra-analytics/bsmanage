context("ManageRemovals")

test_that("initializes with region, population, and other parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  template_vect <- template[region$get_indices()][,1]
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  expect_error(manage_removals <- ManageRemovals(region, population_model,
                                                 removal_pr = 1:10),
               paste("Removal probability should be a vector with a value for",
                     "each region location."))
  expect_error(manage_removals <- ManageRemovals(region, population_model,
                                                 radius = -1),
               "The radius (m) parameter must be numeric and >= 0.",
               fixed = TRUE)
  expect_silent(manage_removals <- ManageRemovals(region, population_model))
  expect_silent(manage_removals <- ManageRemovals(region, population_model,
                                                  removal_pr = template_vect,
                                                  radius = 2000))
  expect_is(manage_removals, "ManageRemovals")
  expect_s3_class(manage_removals, "ManageActions")
  expect_named(manage_removals, c(c("get_type", "apply")))
  expect_equal(manage_removals$get_type(), "removal")
})
