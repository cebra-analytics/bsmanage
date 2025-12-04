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
  expect_error(ManageRemovals(region, population_model,
                              removal_pr = (0:10)/10),
               paste("Removal probability should be a vector with a value 0-1",
                     "for each region location."))
  expect_error(ManageRemovals(region, population_model,
                              removal_pr = 2),
               paste("Removal probability should be a vector with a value 0-1",
                     "for each region location."))
  expect_error(manage_removals <- ManageRemovals(region, population_model,
                                                 removal_pr = 0.5,
                                                 removal_cost = 1:5),
               paste("The removal cost parameter must be a numeric vector",
                     "with values for each location."))
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
                                                  removal_cost = 2,
                                                  radius = 2000,
                                                  stages = 2:3,
                                                  schedule = 4:6))
  expect_is(manage_removals, "ManageRemovals")
  expect_s3_class(manage_removals, "ManageActions")
  expect_named(manage_removals,
               c(c("get_type", "get_label", "get_stages", "get_schedule",
                   "include_cost", "get_cost_unit", "clear_attributes",
                   "apply")))
  expect_equal(manage_removals$get_type(), "removal")
  expect_equal(manage_removals$get_label(), "removed")
  expect_equal(manage_removals$get_stages(), 2:3)
  expect_equal(manage_removals$get_schedule(), 4:6)
  expect_true(manage_removals$include_cost())
  expect_named(manage_removals$include_cost(), "removal_cost")
  expect_null(manage_removals$get_cost_unit())
  removal_cost <- 2
  attr(removal_cost, "unit") <- "beans"
  manage_removals <- ManageRemovals(region, population_model,
                                    removal_pr = template_vect,
                                    removal_cost = removal_cost) # silent
  expect_equal(manage_removals$get_cost_unit(), "beans")
})

test_that("applies stochastic removals to invasive population", {
  TEST_DIRECTORY <- test_path("test_inputs")

  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  idx1 <- 5916:5922
  region <- bsspread::Region(template)
  template[region$get_indices()][idx1,] <- c(rep(0.5, 4), 0.5, 0.75, 1)
  idx <- idx1[5:7]
  template_vect <- template[region$get_indices()][,1]
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  initial_n <- rep(0, region$get_locations())
  initial_n[idx] <- (10:12)*10
  initializer <- bsspread::Initializer(initial_n, region = region,
                                       population_model = population_model)
  # without detected
  set.seed(1234)
  n <- initializer$initialize()
  set.seed(1234)
  expected_removals1 <- array(c(rep(0, 3),
                                stats::rbinom(6, size = n[idx,2:3],
                                              c(0.5, 0.75, 1))), c(3, 3))
  colnames(expected_removals1) <- colnames(n)
  new_n <- n[idx,] - expected_removals1
  set.seed(1234)
  expected_removals2 <- array(c(rep(0, 3),
                                stats::rbinom(6, size = new_n[,2:3],
                                              c(0.5, 0.75, 1))), c(3, 3))
  colnames(expected_removals2) <- colnames(n)
  removal_cost <- 2
  attr(removal_cost, "unit") <- "$"
  expect_silent(manage_removals <- ManageRemovals(region, population_model,
                                                  removal_pr = template_vect,
                                                  detected_only = FALSE,
                                                  removal_cost = removal_cost,
                                                  radius = NULL,
                                                  stages = 2:3,
                                                  schedule = 4:6))
  expected_removal_cost <- rep(0, region$get_locations())
  attr(expected_removal_cost, "unit") <- "$"
  expect_silent(new_n <- manage_removals$apply(n, 2)) # not scheduled
  expect_equal(attr(new_n, "removed")[idx,], expected_removals1*0)
  expect_equal(new_n[idx,], n[idx,])
  expect_equal(attr(new_n, "removal_cost"), expected_removal_cost)
  set.seed(1234)
  expect_silent(new_n <- manage_removals$apply(n, 4))
  expect_equal(attr(new_n, "removed")[idx,], expected_removals1)
  expect_equal(new_n[idx,], n[idx,] - expected_removals1)
  expected_removal_cost[idx] <- 2
  expect_equal(attr(new_n, "removal_cost"), expected_removal_cost)
  expect_equal(attr(attr(new_n, "removal_cost"), "unit"), "$")
  set.seed(1234)
  expect_silent(new_n2 <- manage_removals$apply(new_n, 4))
  expect_equal(attr(new_n2, "removed")[idx,],
               expected_removals1 + expected_removals2)
  expected_removal_cost <-
    expected_removal_cost + (rowSums(new_n2[,2:3]) > 0)*2
  expect_equal(attr(new_n2, "removal_cost"), expected_removal_cost)
  set.seed(1234)
  expect_silent(new_n <- manage_removals$clear_attributes(new_n))
  expect_null(attr(new_n, "removed"))
  expect_null(attr(new_n, "removal_cost"))
  expect_silent(new_n2 <- manage_removals$apply(new_n, 5))
  expect_equal(attr(new_n2, "removed")[idx,], expected_removals2)
  expected_removal_cost <-
    expected_removal_cost*0 + (rowSums(new_n2[,2:3]) > 0)*2
  expect_equal(attr(new_n2, "removal_cost"), expected_removal_cost)
  # single value removal_pr
  set.seed(1234)
  expected_removals <- array(c(rep(0, 3),
                               stats::rbinom(6, size = n[idx,2:3],
                                             0.65)), c(3, 3))
  colnames(expected_removals) <- colnames(n)
  expected_removal_cost[] <- 0
  expected_removal_cost[idx] <- 2
  expect_silent(manage_removals <- ManageRemovals(region, population_model,
                                                  removal_pr = 0.65,
                                                  detected_only = FALSE,
                                                  removal_cost = removal_cost,
                                                  radius = NULL,
                                                  stages = 2:3, schedule = 4:6))
  set.seed(1234)
  expect_silent(new_n <- manage_removals$apply(n, 4))
  expect_equal(attr(new_n, "removed")[idx,], expected_removals)
  expect_equal(attr(new_n, "removal_cost"), expected_removal_cost)
  # detection only
  expect_silent(manage_removals <- ManageRemovals(region, population_model,
                                                  removal_pr = template_vect,
                                                  detected_only = TRUE,
                                                  removal_cost = removal_cost,
                                                  radius = NULL,
                                                  stages = 2:3,
                                                  schedule = 4:6))
  expect_silent(new_n <- manage_removals$apply(n, 4))
  expect_equal(attr(new_n, "removed")[idx,], n[idx,]*0)
  expect_equal(new_n[idx,], n[idx,])
  expect_equal(attr(new_n, "removal_cost"), expected_removal_cost*0)
  # with detected/undetected
  detected <- n*0
  detected[idx,2:3] <- trunc(n[idx,2:3]*c(0, 0.5, 1))
  undetected <- n - detected
  attr(n, "detected") <- detected
  attr(n, "undetected") <- undetected
  set.seed(1234)
  expected_removals <- array(
    c(rep(0, 3), stats::rbinom(6, size = attr(n, "detected")[idx,2:3],
                               c(0.5, 0.75, 1))), c(3, 3))
  colnames(expected_removals) <- colnames(n)
  attr(n, "attachment") <- "extra"
  set.seed(1234)
  expect_silent(new_n <- manage_removals$apply(n, 4))
  expect_equal(attr(new_n, "removed")[idx,], expected_removals)
  expect_equal(new_n[idx,], n[idx,] - expected_removals)
  expect_equal(attr(new_n, "attachment"), "extra")
  expect_equal(attr(new_n, "detected"), attr(n, "detected"))
  expect_equal(attr(new_n, "undetected"), attr(n, "undetected"))
  expect_equal(attr(new_n, "removal_cost"),
               expected_removal_cost*(rowSums(attr(n, "detected")) > 0))
  attr(n, "attachment") <- NULL
  expect_silent(manage_removals <- ManageRemovals(region, population_model,
                                                  removal_pr = template_vect,
                                                  detected_only = FALSE,
                                                  removal_cost = removal_cost,
                                                  radius = NULL,
                                                  stages = 2:3))
  n_apply <- array(as.numeric(n*(attr(n, "detected") > 0)), dim(n))
  n_undetected <- array(as.numeric(attr(n, "undetected")*(n_apply > 0)),
                        dim(n))
  n_apply <- list(detected = n_apply - n_undetected, undetected = n_undetected)
  set.seed(1234)
  expected_removals <- lapply(n_apply, function(a) {
    removed <- array(c(rep(0, 3),
                       stats::rbinom(6, size = a[idx,2:3], c(0.5, 0.75, 1))),
                     c(3, 3))
    colnames(removed) <- colnames(n)
    return(removed)
  })
  set.seed(1234)
  expect_silent(new_n <- manage_removals$apply(n, 4))
  expect_equal(attr(new_n, "removed")[idx,],
               expected_removals$detected + expected_removals$undetected)
  expect_equal(new_n[idx,], n[idx,] - (expected_removals$detected +
                                         expected_removals$undetected))
  expect_equal(attr(new_n, "detected"), attr(n, "detected"))
  expect_equal(attr(new_n, "undetected")[idx,],
               attr(n, "undetected")[idx,] - expected_removals$undetected)
  expect_equal(attr(new_n, "removal_cost"),
               expected_removal_cost*(rowSums(attr(n, "detected")) > 0))
  # remove always (as per without detected)
  expect_silent(manage_removals <-
                  ManageRemovals(region, population_model,
                                 removal_pr = template_vect,
                                 remove_always = TRUE,
                                 removal_cost = removal_cost,
                                 radius = NULL,
                                 stages = 2:3, schedule = 4:6))
  n_apply <- array(as.numeric(n), dim(n))
  n_undetected <- array(as.numeric(attr(n, "undetected")*(n_apply > 0)),
                        dim(n))
  n_apply <- list(detected = n_apply - n_undetected, undetected = n_undetected)
  set.seed(1234)
  expected_removals <- lapply(n_apply, function(a) {
    removed <- array(c(rep(0, 3),
                       stats::rbinom(6, size = a[idx,2:3], c(0.5, 0.75, 1))),
                     c(3, 3))
    colnames(removed) <- colnames(n)
    return(removed)
  })
  set.seed(1234)
  expect_silent(new_n <- manage_removals$apply(n, 4))
  expect_equal(attr(new_n, "removed")[idx,],
               expected_removals$detected + expected_removals$undetected)
  expect_equal(new_n[idx,], n[idx,] - (expected_removals$detected +
                                         expected_removals$undetected))
  expect_equal(attr(new_n, "detected"), attr(n, "detected"))
  expect_equal(attr(new_n, "undetected")[idx,],
               attr(n, "undetected")[idx,] - expected_removals$undetected)
  expect_equal(attr(new_n, "removal_cost"),
               expected_removal_cost*(rowSums(n) > 0))
  # with radius
  expect_silent(manage_removals <- ManageRemovals(region, population_model,
                                                  removal_pr = template_vect,
                                                  detected_only = FALSE,
                                                  removal_cost = removal_cost,
                                                  radius = 3000,
                                                  stages = 2:3))
  n[idx1[1:4],] <- rep(n[idx1[5],] - 6, each = 4)
  attr(n, "undetected")[idx1[1:4],] <- n[idx1[1:4],]
  n_apply <- array(as.numeric(n), dim(n))
  n_undetected <- array(as.numeric(attr(n, "undetected")*(n_apply > 0)),
                        dim(n))
  n_apply <- list(detected = n_apply - n_undetected, undetected = n_undetected)
  set.seed(1234)
  expected_removals <- lapply(n_apply, function(a) {
    removed <- rbind(array(0, c(2, 3)), array(
      c(rep(0, 5), stats::rbinom(10, size = a[idx1[3:7], 2:3],
                                 template_vect[idx1[3:7]])), c(5, 3)))
    colnames(removed) <- colnames(n)
    return(removed)
  })
  set.seed(1234)
  expect_silent(new_n <- manage_removals$apply(n, 4))
  expect_equal(attr(new_n, "removed")[idx1,],
               expected_removals$detected + expected_removals$undetected)
  expect_equal(new_n[idx1,], n[idx1,] - (expected_removals$detected +
                                           expected_removals$undetected))
  expect_equal(attr(new_n, "detected"), attr(n, "detected"))
  expect_equal(attr(new_n, "undetected")[idx1,],
               attr(n, "undetected")[idx1,] - expected_removals$undetected)
  expected_removal_cost[] <- 0
  expected_removal_cost[rowSums(attr(new_n, "removed")) > 0] <- 2
  expect_equal(attr(new_n, "removal_cost"), expected_removal_cost)
})
