context("ManageDetection")

test_that("initializes with region, population, and surveillance", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  template_vect <- template[region$get_indices()][,1]
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  surveillance <-
    bsdesign::SpatialSurvDesign(context = bsdesign::Context("test"),
                                divisions = bsdesign::Divisions(template),
                                establish_pr = template_vect*0,
                                lambda = 1,
                                optimal = "none",
                                exist_sens = template_vect)
  expect_error(manage_detection <- ManageDetection(
    region, population_model, "surveillance", stages = 2:3),
    paste("Surveillance object must be a 'SurveillanceDesign' or inherited",
          "class object."))
  template2 <- template*1
  template2[1] <- NA
  region2 <- bsspread::Region(template2)
  expect_error(manage_detection <- ManageDetection(
    region2, bsspread::StagedPopulation(region2, stage_matrix), surveillance,
    stages = 2:3),
    "Surveillance object must be compatible with the region object.")
  expect_error(manage_detection <- ManageDetection(
    region, population_model, surveillance, surv_cost = 1:5, stages = 2:3,
    schedule = 4:6),
    paste("The surveillance cost parameter must be a numeric vector with",
          "values for each location."))
  expect_silent(manage_detection <- ManageDetection(
    region, population_model, surveillance, surv_cost = 1, stages = 2:3,
    schedule = 4:6))
  expect_is(manage_detection, "ManageDetection")
  expect_s3_class(manage_detection, "ManageActions")
  expect_named(manage_detection,
               c(c("get_type", "get_id", "set_id", "get_label", "get_stages",
                   "get_schedule", "include_cost", "get_cost_label",
                   "get_cost_unit", "clear_attributes", "apply",
                   "get_surveillance")))
  expect_equal(manage_detection$get_type(), "detection")
  expect_equal(manage_detection$get_label(), "detected")
  expect_is(manage_detection$get_surveillance(), "SpatialSurvDesign")
  expect_equal(manage_detection$get_stages(), 2:3)
  expect_equal(manage_detection$get_schedule(), 4:6)
  expect_true(manage_detection$include_cost())
  expect_equal(manage_detection$get_cost_label(), "surv_cost")
  expect_equal(manage_detection$get_cost_unit(), "$")
  expect_silent(manage_detection$set_id("a1"))
  expect_equal(manage_detection$get_id(), "a1")
  expect_equal(manage_detection$get_label(), "a1_detected")
  expect_equal(manage_detection$get_label(include_id = FALSE), "detected")
  expect_equal(manage_detection$get_cost_label(), "a1_surv_cost")
  expect_equal(manage_detection$get_cost_label(include_id = FALSE),
               "surv_cost")
})

test_that("applies stochastic detection to invasive population", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  idx <- 5916:5922
  region <- bsspread::Region(template)
  template[region$get_indices()][idx,] <- c(rep(0.5, 4), 0.5, 0.75, 1)
  idx <- idx[5:7]
  template_vect <- template[region$get_indices()][,1]
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  surveillance <-
    bsdesign::SpatialSurvDesign(context = bsdesign::Context("test"),
                                divisions = bsdesign::Divisions(template),
                                establish_pr = template_vect*0,
                                lambda = 1,
                                optimal = "none",
                                exist_sens = template_vect)
  initial_n <- rep(0, region$get_locations())
  initial_n[idx] <- (10:12)*5
  initializer <- bsspread::Initializer(initial_n, region = region,
                                       population_model = population_model)
  set.seed(1212)
  n <- initializer$initialize()
  set.seed(1234)
  expected_detected <- array(c(rep(0, 3),
                               stats::rbinom(6, size = n[idx, 2:3],
                                             c(0.5, 0.75, 1))), c(3, 3))
  colnames(expected_detected) <- colnames(n)
  expect_silent(manage_detection <- ManageDetection(
    region, population_model, surveillance, surv_cost = 2, stages = 2:3,
    schedule = 4:6))
  expect_silent(new_n <- manage_detection$apply(n, 2))
  expect_equal(attr(new_n, "detected")[idx,], expected_detected*0)
  expect_equal(attr(new_n, "undetected"), n)
  expect_equal(as.numeric(attr(new_n, "surv_cost")),
               rep(0, region$get_locations()))
  expect_equal(attr(attr(new_n, "surv_cost"), "unit"), "$")
  set.seed(1234)
  expect_silent(new_n <- manage_detection$apply(n, 4))
  expect_equal(attr(new_n, "detected")[idx,], expected_detected)
  expect_equal(attr(new_n, "undetected"), n - attr(new_n, "detected"))
  expect_equal(as.numeric(attr(new_n, "surv_cost")),
               2*(surveillance$get_sensitivity() > 0))
  expect_equal(attr(attr(new_n, "surv_cost"), "unit"), "$")
  set.seed(1234)
  expect_silent(new_n2 <- manage_detection$apply(new_n, 4))

  expect_true(all(attr(new_n2, "detected") <= attr(new_n, "undetected")))
  expect_equal(attr(new_n2, "undetected"),
               n - attr(new_n, "detected") - attr(new_n2, "detected"))
  expect_equal(as.numeric(attr(new_n2, "surv_cost")),
               2*(surveillance$get_sensitivity() > 0))
  expect_equal(attr(attr(new_n2, "surv_cost"), "unit"), "$")
  expect_silent(new_n2 <- manage_detection$clear_attributes(new_n2))
  expect_null(attr(new_n2, "detected"))
  expect_null(attr(new_n2, "undetected"))
  expect_null(attr(new_n2, "surv_cost"))
  expect_equal(new_n2, n)
  expect_silent(manage_detection$set_id("a1"))
  set.seed(1234)
  expect_silent(new_n <- manage_detection$apply(n, 4))
  expect_equal(attr(new_n, "a1_detected")[idx,], expected_detected)
  expect_equal(attr(new_n, "undetected"), n - attr(new_n, "a1_detected"))
  expect_equal(as.numeric(attr(new_n, "a1_surv_cost")),
               2*(surveillance$get_sensitivity() > 0))
  expect_equal(attr(attr(new_n, "a1_surv_cost"), "unit"), "$")
  # unstructured population
  population_model <- bsspread::UnstructPopulation(region, growth = 1.2)
  n <- rowSums(n)
  set.seed(1234)
  expected_detected <- stats::rbinom(3, size = n[idx], c(0.5, 0.75, 1))
  expect_silent(
    manage_detection <- ManageDetection(region, population_model, surveillance,
                                        surv_cost = 2, schedule = 4:6))
  set.seed(1234)
  expect_silent(new_n <- manage_detection$apply(n, 4))
  expect_equal(attr(new_n, "detected")[idx], expected_detected)
  expect_equal(attr(new_n, "undetected"), n - attr(new_n, "detected"))
  expect_equal(as.numeric(attr(new_n, "surv_cost")),
               2*(surveillance$get_sensitivity() > 0))
  expect_equal(attr(attr(new_n, "surv_cost"), "unit"), "$")
  set.seed(1234)
  expect_silent(new_n2 <- manage_detection$apply(new_n, 4))
  expect_true(all(attr(new_n2, "detected") <= attr(new_n, "undetected")))
  expect_equal(attr(new_n2, "undetected"),
               n - attr(new_n, "detected") - attr(new_n2, "detected"))
  expect_equal(as.numeric(attr(new_n2, "surv_cost")),
               2*(surveillance$get_sensitivity() > 0))
  expect_equal(attr(attr(new_n2, "surv_cost"), "unit"), "$")
  # presence-only population
  population_model <- bsspread::PresencePopulation(region)
  n <- n > 0
  set.seed(1234)
  expected_detected <- stats::rbinom(3, size = n[idx], c(0.5, 0.75, 1))
  expect_silent(
    manage_detection <- ManageDetection(region, population_model, surveillance,
                                        surv_cost = 2, schedule = 4:6))
  set.seed(1234)
  expect_silent(new_n <- manage_detection$apply(n, 4))
  expect_equal(attr(new_n, "detected")[idx], as.logical(expected_detected))
  expect_equal(attr(new_n, "undetected"), n & !attr(new_n, "detected"))
  expect_equal(as.numeric(attr(new_n, "surv_cost")),
               2*(surveillance$get_sensitivity() > 0))
  expect_equal(attr(attr(new_n, "surv_cost"), "unit"), "$")
  set.seed(1234)
  expect_silent(new_n2 <- manage_detection$apply(new_n, 4))
  expect_true(sum(attr(new_n2, "detected")) <= sum(attr(new_n, "undetected")))
  expect_equal((attr(new_n, "detected") | attr(new_n2, "detected") |
                  attr(new_n2, "undetected")), n)
  expect_equal(as.numeric(attr(new_n2, "surv_cost")),
               2*(surveillance$get_sensitivity() > 0))
  expect_equal(attr(attr(new_n2, "surv_cost"), "unit"), "$")
})
