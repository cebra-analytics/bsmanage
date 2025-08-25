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
  expect_error(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "search_destroy",
                                      control_design = "control_design"),
    paste("Control design object must be a 'ControlDesign', 'ManageDesign',",
          "or inherited class object."))
  expect_error(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "search_destroy"),
    "Control design object is required for 'search & destroy' control.")
  expect_silent(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "search_destroy",
                                      control_design = control_design,
                                      stages = 2:3, schedule = 4:6))
  expect_is(manage_controls, "ManageControls")
  expect_s3_class(manage_controls, "ManageActions")
  expect_named(manage_controls, c(c("get_type", "get_label", "get_stages",
                                    "get_schedule", "apply")))
  expect_equal(manage_controls$get_type(), "control")
  expect_equal(manage_controls$get_label(), "control_search_destroy")
  expect_equal(manage_controls$get_stages(), 2:3)
  expect_equal(manage_controls$get_schedule(), 4:6)
  expect_error(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth"),
    paste("Suppression multiplier is required for growth, spread, and",
          "establishment control."))
  expect_error(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth",
                                      suppress_mult = (0:10)/10),
    paste("Suppression multiplier should be a vector with a value 0-1 for",
          "each region location."))
  expect_error(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth",
                                      suppress_mult = 2),
    paste("Suppression multiplier should be a vector with a value 0-1 for",
          "each region location."))
  expect_error(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth",
                                      suppress_mult = 0.7,
                                      radius = -1),
    "The radius (m) parameter must be numeric and >= 0.", fixed = TRUE)
  expect_error(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth",
                                      suppress_mult = 0.7,
                                      apply_to = "dummy"),
    paste("Growth control 'apply to' attribute should be 'reproductions' or",
          "'survivals'."))
  expect_silent(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth",
                                      control_design = control_design,
                                      radius = 1000,
                                      suppress_mult = 0.7,
                                      stages = 2:3,
                                      apply_to = "survivals",
                                      schedule = 4:6))
  expect_equal(manage_controls$get_label(), "control_growth")
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
  expect_silent(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "search_destroy",
                                      control_design = control_design,
                                      stages = 2:3, schedule = 4:6))
  set.seed(1234)
  expect_silent(new_n <- manage_controls$apply(n, 4))
  expect_equal(new_n, expected_n)
  expect_equal(attr(new_n, "control_search_destroy"), expected_controls)
  # growth, spread, or establishment
  control_design <- ManageDesign(
    context = ManageContext("test"),
    divisions = divisions,
    optimal = "none",
    exist_alloc = +(exist_manage_pr > 0.95))
  exist_mask <- rowSums(n[,2:3])*control_design$get_allocation() > 0
  detected <- n*0
  detected[,2:3] <- round((n[,2:3] > 80)*n[,2:3]*0.7)
  attr(n, "detected") <- detected
  detected_mask <- +(rowSums(detected) > 0 | NA)
  detected_mask <- rowSums(n[,2:3])*terra::buffer(
    region$get_rast(detected_mask), width = 1500)[region$get_indices()][,1] > 0
  expect_silent(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth",
                                      control_design = control_design,
                                      radius = 1500,
                                      suppress_mult = 0.7,
                                      stages = 2:3,
                                      apply_to = "survivals",
                                      schedule = 4:6))
  expect_silent(new_n <- manage_controls$apply(n, 4))
  expect_equal(as.numeric(new_n), as.numeric(n))
  expect_equal(dim(new_n), dim(n))
  expect_named(new_n, names(n))
  expect_equal(attr(new_n, "detected"), detected)
  expect_equal(as.numeric(attr(new_n, "control_growth")),
               (exist_mask | detected_mask)*0.7)
  expect_equal(attr(attr(new_n, "control_growth"), "stages"), 2:3)
  expect_equal(attr(attr(new_n, "control_growth"), "apply_to"), "survivals")
})
