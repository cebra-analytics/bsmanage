context("ManageControls")

test_that("initializes with region, population, and other parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- bsdesign::Divisions(template)
  region <- bsspread::Region(template)
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  set.seed(1234)
  exist_manage_pr <- runif(divisions$get_parts())
  control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = 1,
    lambda = 1,
    optimal = "none",
    exist_manage_pr = exist_manage_pr)
  expect_error(manage_controls <- ManageControls(region, population_model,
                                                 "control_design"),
               paste("Control design object must be a 'ControlDesign',",
                     "'ManageDesign', or inherited class object."))
  expect_silent(manage_controls <- ManageControls(
    region, population_model, control_design,
    control_type = "search_destroy",
    stages = 2:3, schedule = 4:6))
  expect_is(manage_controls, "ManageControls")
  expect_s3_class(manage_controls, "ManageActions")
  expect_named(manage_controls, c(c("get_type", "get_label", "get_stages",
                                    "get_schedule", "apply")))
  expect_equal(manage_controls$get_type(), "control")
  expect_equal(manage_controls$get_label(), "control_search_destroy")
  expect_equal(manage_controls$get_stages(), 2:3)
  expect_equal(manage_controls$get_schedule(), 4:6)
})

test_that("applies stochastic controls to invasive population", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- bsdesign::Divisions(template)
  region <- bsspread::Region(template)
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  initial_n <- rep(0, region$get_locations())
  initial_n[101:150] <- (11:60)*10
  initializer <- bsspread::Initializer(initial_n, region = region,
                                       population_model = population_model)
  n <- initializer$initialize()
  set.seed(1234)
  exist_manage_pr <- runif(divisions$get_parts())
  control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = 1,
    lambda = 1,
    optimal = "none",
    exist_manage_pr = exist_manage_pr)
  # search and destroy
  set.seed(1234)
  expected_controls <- array(0, dim(n))
  colnames(expected_controls) <- colnames(n)
  expected_controls[101:150, 2:3] <- stats::rbinom(100, size = n[101:150, 2:3],
                                                   exist_manage_pr[101:150])
  expected_n <- n - expected_controls
  attr(expected_n, "control_search_destroy") <- expected_controls
  expect_silent(manage_controls <- ManageControls(
    region, population_model, control_design,
    control_type = "search_destroy",
    stages = 2:3, schedule = 4:6))
  set.seed(1234)
  expect_silent(new_n <- manage_controls$apply(n, 4))
  expect_equal(new_n, expected_n)
  expect_equal(attr(new_n, "control_search_destroy"), expected_controls)
  # growth, spread, or establishment
  expected_n <- n
  attr(expected_n, "control_growth") <- exist_manage_pr
  expect_silent(manage_controls <- ManageControls(
    region, population_model, control_design,
    control_type = "growth",
    stages = 2:3, schedule = 4:6))
  set.seed(1234)
  expect_silent(new_n <- manage_controls$apply(n, 4))
  expect_equal(new_n, expected_n)
  expect_equal(attr(new_n, "control_growth"), exist_manage_pr)
})
