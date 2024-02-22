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
  expect_message(manage_removals <- ManageRemovals(region, population_model,
                                                   detected_only = TRUE,
                                                   radius = 2000),
                 paste("Radius is not used when only detected individuals are",
                       "removed."))

  expect_silent(manage_removals <- ManageRemovals(region, population_model))
  expect_silent(manage_removals <- ManageRemovals(region, population_model,
                                                  removal_pr = template_vect,
                                                  radius = 2000,
                                                  stages = 2:3))
  expect_is(manage_removals, "ManageRemovals")
  expect_s3_class(manage_removals, "ManageActions")
  expect_named(manage_removals, c(c("get_type", "get_label", "get_stages",
                                    "apply")))
  expect_equal(manage_removals$get_type(), "removal")
  expect_equal(manage_removals$get_label(), "removed")
  expect_equal(manage_removals$get_stages(), 2:3)
})

test_that("applies stochastic removals to invasive population", {
  TEST_DIRECTORY <- test_path("test_inputs")

  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  template[region$get_indices()][5916:5922,] <- c(rep(0.5, 4), 0.5, 0.75, 1)
  template_vect <- template[region$get_indices()][,1]
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  initial_n <- rep(0, region$get_locations())
  initial_n[5920:5922] <- (10:12)*10
  initializer <- bsspread::Initializer(initial_n, region = region,
                                       population_model = population_model)
  # without detected
  n <- initializer$initialize()
  set.seed(1234)
  expected_removals <- array(c(rep(0, 3),
                               stats::rbinom(6, size = n[5920:5922,2:3],
                                             c(0.5, 0.75, 1))), c(3, 3))
  expect_silent(manage_removals <- ManageRemovals(region, population_model,
                                                  removal_pr = template_vect,
                                                  detected_only = FALSE,
                                                  radius = NULL,
                                                  stages = 2:3))
  set.seed(1234)
  expect_silent(new_n <- manage_removals$apply(n))
  expect_equal(attr(new_n, "removed")[5920:5922,], expected_removals)
  expect_equal(new_n[5920:5922,], n[5920:5922,] - expected_removals)
  expect_silent(manage_removals <- ManageRemovals(region, population_model,
                                                  removal_pr = template_vect,
                                                  detected_only = TRUE,
                                                  radius = NULL,
                                                  stages = 2:3))
  expect_silent(new_n <- manage_removals$apply(n))
  expect_equal(attr(new_n, "removed")[5920:5922,], n[5920:5922,]*0)
  expect_equal(new_n[5920:5922,], n[5920:5922,])
  # with detected
  attr(n, "detected") <- n*0
  attr(n, "detected")[5920:5922,2:3] <- trunc(n[5920:5922,2:3]*c(0, 0.5, 1))
  set.seed(1234)
  expected_removals <- array(
    c(rep(0, 3), stats::rbinom(6, size = attr(n, "detected")[5920:5922,2:3],
                               c(0.5, 0.75, 1))), c(3, 3))
  set.seed(1234)
  expect_silent(new_n <- manage_removals$apply(n))
  expect_equal(attr(new_n, "removed")[5920:5922,], expected_removals)
  expect_equal(new_n[5920:5922,], n[5920:5922,] - expected_removals)
  expect_silent(manage_removals <- ManageRemovals(region, population_model,
                                                  removal_pr = template_vect,
                                                  detected_only = FALSE,
                                                  radius = NULL,
                                                  stages = 2:3))
  n_detected <- n*(attr(n, "detected") > 0)
  set.seed(1234)
  expected_removals <- array(
    c(rep(0, 3), stats::rbinom(6, size = n_detected[5920:5922,2:3],
                               c(0.5, 0.75, 1))), c(3, 3))
  set.seed(1234)
  expect_silent(new_n <- manage_removals$apply(n))
  expect_equal(attr(new_n, "removed")[5920:5922,], expected_removals)
  expect_equal(new_n[5920:5922,], n[5920:5922,] - expected_removals)
  # with radius
  expect_silent(manage_removals <- ManageRemovals(region, population_model,
                                                  removal_pr = template_vect,
                                                  detected_only = FALSE,
                                                  radius = 3000,
                                                  stages = 2:3))
  n[5916:5919,] <- rep(n[5920,] - 6, each = 4)
  set.seed(1234)
  expected_removals <- rbind(array(0, c(2, 3)), array(
    c(rep(0, 5), stats::rbinom(10, size = n[5918:5922,2:3],
                               template_vect[5918:5922])), c(5, 3)))
  set.seed(1234)
  expect_silent(new_n <- manage_removals$apply(n))
  expect_equal(attr(new_n, "removed")[5916:5922,], expected_removals)
  expect_equal(new_n[5916:5922,], n[5916:5922,] - expected_removals)
})
