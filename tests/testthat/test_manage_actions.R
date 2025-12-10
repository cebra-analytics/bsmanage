context("ManageActions")

test_that("initializes with region, population model, stages & schedule", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  expect_error(manage_actions <- ManageActions(region, "population_model",
                                               stages = 2:3),
               paste("Population model must be a 'Population' or inherited",
                     "class object."))
  template2 <- template*1
  template2[1] <- NA
  expect_error(manage_actions <-
                 ManageActions(bsspread::Region(template2), population_model,
                               stages = 2:3),
               "Population model must be compatible with the region object.")
  expect_error(manage_actions <- ManageActions(region, population_model,
                                               stages = 3:4),
               paste("Stages must be a vector of stage indices consistent",
                     "with the population model."))
  expect_error(manage_actions <- ManageActions(region, population_model,
                                               schedule = "2"),
               paste("The schedule for applying actions should be a vector of",
                     "numeric simulation time steps."))
  expect_silent(manage_actions <- ManageActions(region, population_model,
                                                stages = 2:3, schedule = 4:6))
  expect_is(manage_actions, "ManageActions")
  expect_named(manage_actions,
               c(c("get_type", "get_id", "set_id", "get_label", "get_stages",
                   "get_schedule", "include_cost", "get_cost_unit",
                   "clear_attributes", "apply")))
  expect_equal(manage_actions$get_type(), "detection")
  expect_equal(manage_actions$get_label(), "action")
  expect_equal(manage_actions$get_stages(), 2:3)
  expect_equal(manage_actions$get_schedule(), 4:6)
  expect_false(manage_actions$include_cost())
  expect_null(manage_actions$get_cost_unit())
  expect_equal(manage_actions$clear_attributes(1:10), 1:10) # returns n
  expect_equal(manage_actions$apply(1:10, 4), 1:10) # returns n
  expect_silent(manage_actions$set_id("a1"))
  expect_equal(manage_actions$get_id(), "a1")
  expect_equal(manage_actions$get_label(), "a1_action")
})
