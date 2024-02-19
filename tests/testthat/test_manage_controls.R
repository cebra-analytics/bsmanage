context("ManageControls")

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
  expect_silent(manage_controls <- ManageControls(region, population_model,
                                                  stages = 2:3))
  expect_is(manage_controls, "ManageControls")
  expect_s3_class(manage_controls, "ManageActions")
  expect_named(manage_controls, c(c("get_type", "get_stages", "apply")))
  expect_equal(manage_controls$get_type(), "control")
  expect_equal(manage_controls$get_stages(), 2:3)
})
