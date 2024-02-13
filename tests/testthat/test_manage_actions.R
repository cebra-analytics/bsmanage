context("ManageActions")

test_that("initializes with region, population model and apply stages", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  expect_error(manage_actions <- ManageActions(region, "population_model",
                                               apply_stages = 2:3),
               paste("Population model must be a 'Population' or inherited",
                     "class object."))
  template2 <- template*1
  template2[1] <- NA
  expect_error(manage_actions <-
                 ManageActions(bsspread::Region(template2), population_model,
                               apply_stages = 2:3),
               "Population model must be compatible with the region object.")
  expect_error(manage_actions <- ManageActions(region, population_model,
                                               apply_stages = 3:4),
               paste("Apply stages must be a vector of stage indices",
                     "consistent with the population model."))
  manage_actions <- ManageActions(region, population_model,
                                  apply_stages = 2:3) # silent
  expect_is(manage_actions, "ManageActions")
  expect_equal(manage_actions$get_type(), "detection")
  expect_equal(manage_actions$apply(1:10), 1:10) # returns n
})
