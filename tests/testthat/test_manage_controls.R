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
  expect_error(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "search_destroy",
                                      control_design = control_design,
                                      control_cost = 1:5),
    paste("The control cost parameter must be a numeric vector with values",
          "for each location."))
  expect_silent(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "search_destroy",
                                      control_design = control_design,
                                      control_cost = 2, stages = 2:3,
                                      schedule = 4:6))
  expect_is(manage_controls, "ManageControls")
  expect_s3_class(manage_controls, "ManageActions")
  expect_named(manage_controls,
               c(c("get_type", "get_id", "set_id", "get_label", "get_stages",
                   "get_schedule", "include_cost", "get_cost_label",
                   "get_cost_unit", "clear_attributes", "apply")))
  expect_equal(manage_controls$get_type(), "control")
  expect_equal(manage_controls$get_label(), "control_search_destroy")
  expect_equal(manage_controls$get_stages(), 2:3)
  expect_equal(manage_controls$get_schedule(), 4:6)
  expect_true(manage_controls$include_cost())
  expect_equal(manage_controls$get_cost_label(), "control_search_destroy_cost")
  expect_equal(manage_controls$get_cost_unit(), "$")
  expect_silent(manage_controls$set_id("a1"))
  expect_equal(manage_controls$get_id(), "a1")
  expect_equal(manage_controls$get_label(), "a1_control_search_destroy")
  expect_equal(manage_controls$get_label(include_id = FALSE),
               "control_search_destroy")
  expect_equal(manage_controls$get_cost_label(),
               "a1_control_search_destroy_cost")
  expect_equal(manage_controls$get_cost_label(include_id = FALSE),
               "control_search_destroy_cost")
  expect_error(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth"),
    paste("Control multiplier is required for growth, spread, and",
          "establishment control."))
  expect_error(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth",
                                      control_mult = (0:10)/10),
    paste("Control multiplier should be a vector with a value 0-1 for",
          "each region location."))
  expect_error(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth",
                                      control_mult = 2),
    paste("Control multiplier should be a vector with a value 0-1 for",
          "each region location."))
  expect_error(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth",
                                      control_mult = 0.7,
                                      radius = -1),
    "The radius (m) parameter must be numeric and >= 0.", fixed = TRUE)
  expect_error(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth",
                                      control_mult = 0.7,
                                      apply_to = "dummy"),
    paste("Growth control 'apply to' attribute should be 'reproduction' or",
          "'survival'."))
  expect_silent(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth",
                                      control_design = control_design,
                                      radius = 1000,
                                      control_mult = 0.7,
                                      control_cost = 2,
                                      stages = 2:3,
                                      apply_to = "survival",
                                      schedule = 4:6))
  expect_equal(manage_controls$get_label(), "control_growth")
  expect_true(manage_controls$include_cost())
  expect_equal(manage_controls$get_cost_label(), "control_growth_cost")
  expect_equal(manage_controls$get_cost_unit(), "$")
  expect_silent(manage_controls$set_id("a2"))
  expect_equal(manage_controls$get_id(), "a2")
  expect_equal(manage_controls$get_label(), "a2_control_growth")
  expect_equal(manage_controls$get_label(include_id = FALSE), "control_growth")
  expect_equal(manage_controls$get_cost_label(), "a2_control_growth_cost")
  expect_equal(manage_controls$get_cost_label(include_id = FALSE),
               "control_growth_cost")
})

test_that("applies stochastic search and destroy controls to population", {
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
  initial_n[101:150] <- 11:60
  initializer <- bsspread::Initializer(initial_n, region = region,
                                       population_model = population_model)
  set.seed(1234)
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
  idx <- 1:region$get_locations()
  set.seed(1234)
  expected_controls <- array(0, dim(n))
  colnames(expected_controls) <- colnames(n)
  expected_controls[101:150, 2:3] <- stats::rbinom(100, size = n[101:150, 2:3],
                                                   exist_manage_pr[101:150])
  attr(n, "attachment") <- "extra"
  expected_n <- n - expected_controls
  attr(expected_n, "control_search_destroy") <- expected_controls
  expected_costs <- 2*(exist_manage_pr > 0)
  attr(expected_costs, "unit") <- "$"
  attr(expected_n, "control_search_destroy_cost") <- expected_costs
  expect_silent(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "search_destroy",
                                      control_design = control_design,
                                      control_cost = 2,
                                      stages = 2:3,
                                      schedule = 4:6))
  set.seed(1234)
  expect_silent(new_n <- manage_controls$apply(n, 4))
  expect_equal(new_n[idx,], expected_n[idx,])
  expect_equal(attr(new_n, "undetected")[idx,],
               n[idx,] - expected_controls[idx,])
  expect_equal(attr(new_n, "control_search_destroy")[idx,],
               expected_controls[idx,])
  expect_equal(attr(new_n, "control_search_destroy_cost")[idx],
               expected_costs[idx])
  expect_equal(attr(new_n, "attachment"), "extra")
  attr(n, "attachment") <- NULL
  # duplicate with extra detections and removals
  attr(new_n, "undetected")[101:110, 2:3] <- 0
  rm_idx <- which(new_n[101:110,] > 1)
  new_n[101:110,][rm_idx] <- new_n[101:110,][rm_idx] - 1
  attr(new_n, "undetected")[101:110, 1] <- new_n[101:110, 1]
  n_undetected <- attr(new_n, "undetected")[,]
  n_apply <- list(detected = new_n[,] - n_undetected,
                  undetected = n_undetected)
  idx1 <- which(rowSums(new_n[,2:3]) > 0)
  set.seed(1234)
  expected_controls2 <- lapply(n_apply, function(a) {
    controlled <- expected_controls*0
    controlled[idx1, 2:3] <- stats::rbinom(length(idx1)*2, size = a[idx1, 2:3],
                                           exist_manage_pr[idx1])
    return(controlled)
  })
  expected_controls_plus <-
    expected_controls2$detected + expected_controls2$undetected
  set.seed(1234)
  expect_silent(new_n2 <- manage_controls$apply(new_n, 4))
  expect_equal(new_n2[idx,], new_n[idx,] - expected_controls_plus[idx,])
  expect_equal(attr(new_n2, "undetected")[idx,],
               (attr(new_n, "undetected")[idx,] -
                  expected_controls2$undetected[idx,]))
  expect_equal(attr(new_n2, "control_search_destroy")[idx,],
               expected_controls_plus[idx,])
  expect_equal(attr(new_n2, "control_search_destroy_cost")[idx],
               expected_costs[idx])

  expect_silent(manage_controls$set_id("a2"))
  set.seed(1234)
  expect_silent(new_n <- manage_controls$apply(n, 4))
  new_n[idx,] ; expected_n[idx,]
  expect_equal(attr(new_n, "undetected")[idx,],
               n[idx,] - expected_controls[idx,])
  expect_equal(attr(new_n, "a2_control_search_destroy")[idx,],
               expected_controls[idx,])
  expect_equal(attr(new_n, "a2_control_search_destroy_cost")[idx],
               expected_costs[idx])

  # unstructured
  population_model <- bsspread::UnstructPopulation(region, growth = 1.2)
  n <- rowSums(n)
  set.seed(1234)
  expected_controls <- n*0
  expected_controls[101:150] <- stats::rbinom(50, size = n[101:150],
                                              exist_manage_pr[101:150])
  expected_n <- n - expected_controls
  expected_costs <- 2*(exist_manage_pr > 0)
  attr(expected_costs, "unit") <- "$"
  expect_silent(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "search_destroy",
                                      control_design = control_design,
                                      control_cost = 2,
                                      schedule = 4:6))
  set.seed(1234)
  expect_silent(new_n <- manage_controls$apply(n, 4))
  expect_equal(as.numeric(new_n), expected_n)
  expect_equal(attr(new_n, "undetected"), n - expected_controls)
  expect_equal(attr(new_n, "control_search_destroy"), expected_controls)
  expect_equal(attr(new_n, "control_search_destroy_cost"), expected_costs)
  # duplicate with extra detections and removals
  attr(new_n, "undetected")[101:110] <- 0
  rm_idx <- which(new_n[101:110] > 1)
  new_n[101:110][rm_idx] <- new_n[101:110][rm_idx] - 1
  n_undetected <- as.numeric(attr(new_n, "undetected"))
  n_apply <- list(detected = as.numeric(new_n) - n_undetected,
                  undetected = n_undetected)
  set.seed(1234)
  idx1 <- which(new_n > 0)
  expected_controls2 <- lapply(n_apply, function(a) {
    controlled <- expected_controls*0
    controlled[idx1] <- stats::rbinom(length(idx1), size = a[idx1],
                                      exist_manage_pr[idx1])
    return(controlled)
  })
  expected_controls_plus <-
    expected_controls2$detected + expected_controls2$undetected
  set.seed(1234)
  expect_silent(new_n2 <- manage_controls$apply(new_n, 4))
  expect_equal(as.numeric(new_n2), as.numeric(new_n) - expected_controls_plus)
  expect_equal(attr(new_n2, "undetected"),
               attr(new_n, "undetected") - expected_controls2$undetected)
  expect_equal(attr(new_n2, "control_search_destroy"), expected_controls_plus)
  expect_equal(attr(new_n2, "control_search_destroy_cost")[idx],
               expected_costs[idx])
  # presence-only
  population_model <- bsspread::PresencePopulation(region)
  n <- n > 0
  set.seed(1234)
  expected_controls <- n*0
  expected_controls[101:150] <- stats::rbinom(50, size = n[101:150],
                                              exist_manage_pr[101:150])
  expected_n <- n - expected_controls
  expected_costs <- 2*(exist_manage_pr > 0)
  attr(expected_costs, "unit") <- "$"
  expect_silent(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "search_destroy",
                                      control_design = control_design,
                                      control_cost = 2,
                                      schedule = 4:6))
  set.seed(1234)
  expect_silent(new_n <- manage_controls$apply(n, 4))
  expect_equal(as.numeric(new_n), expected_n)
  expect_equal(attr(new_n, "undetected"), as.logical(n - expected_controls))
  expect_equal(attr(new_n, "control_search_destroy"),
               as.logical(expected_controls))
  expect_equal(attr(new_n, "control_search_destroy_cost"), expected_costs)
  # duplicate with extra detections and removals
  attr(new_n, "undetected")[101:110] <- FALSE
  new_n[101] <- FALSE
  n_undetected <- as.numeric(attr(new_n, "undetected"))
  n_apply <- list(detected = as.numeric(new_n) - n_undetected,
                  undetected = n_undetected)
  set.seed(1234)
  idx1 <- which(new_n > 0)
  expected_controls2 <- lapply(n_apply, function(a) {
    controlled <- expected_controls*0
    controlled[idx1] <- stats::rbinom(length(idx1), size = a[idx1],
                                      exist_manage_pr[idx1])
    return(controlled)
  })
  expected_controls_plus <-
    expected_controls2$detected + expected_controls2$undetected
  set.seed(1234)
  expect_silent(new_n2 <- manage_controls$apply(new_n, 4))
  expect_equal(as.numeric(new_n2), as.numeric(new_n) - expected_controls_plus)
  expect_equal(attr(new_n2, "undetected"),
               attr(new_n, "undetected") & !expected_controls2$undetected)
  expect_equal(attr(new_n2, "control_search_destroy"),
               as.logical(expected_controls_plus))
  expect_equal(attr(new_n2, "control_search_destroy_cost")[idx],
               expected_costs[idx])
})

test_that("applies stochastic growth/spread/establishment controls", {
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
  initial_n[101:150] <- 11:60
  initializer <- bsspread::Initializer(initial_n, region = region,
                                       population_model = population_model)
  set.seed(1234)
  n <- initializer$initialize()
  set.seed(1234)
  exist_manage_pr <- runif(divisions$get_parts())
  expected_costs <- rep(2, region$get_locations())
  attr(expected_costs, "unit") <- "$"
  idx <- 1:region$get_locations() # idx <- 101:120; idx <- 101:150
  control_design <- ManageDesign(
    context = ManageContext("test"),
    divisions = divisions,
    optimal = "none",
    exist_alloc = +(exist_manage_pr > 0.95))
  exist_mask <- rowSums(n[,2:3])*control_design$get_allocation() > 0
  detected <- n*0
  detected[,2:3] <- round((n[,2:3] > 8)*n[,2:3]*0.7)
  attr(n, "undetected") <- n - detected
  detected_mask <- +(rowSums(detected) > 0 | NA)
  detected_mask <- rowSums(n[,2:3])*terra::buffer(
    region$get_rast(detected_mask), width = 1500)[region$get_indices()][,1] > 0
  expect_silent(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth",
                                      control_design = control_design,
                                      radius = 1500,
                                      control_mult = 0.7,
                                      control_cost = 2,
                                      stages = 2:3,
                                      apply_to = "survival",
                                      schedule = 4:6))
  expect_silent(new_n <- manage_controls$apply(n, 4))
  expect_equal(new_n[idx,], n[idx,])
  expect_equal(attr(new_n, "undetected")[idx,], (n - detected)[idx,])
  expect_equal(as.numeric(attr(new_n, "control_growth")),
               ((exist_mask | detected_mask)*0.7 +
                  !(exist_mask | detected_mask)))
  expect_equal(attr(attr(new_n, "control_growth"), "stages"), 2:3)
  expect_equal(attr(attr(new_n, "control_growth"), "apply_to"), "survival")
  expect_equal(attr(new_n, "control_growth_cost"),
               (expected_costs*(control_design$get_allocation() > 0 |
                                  detected_mask)))
  establish_mask <- +(rowSums(detected) > 0 | NA)
  establish_mask <- terra::buffer(region$get_rast(establish_mask),
                                  width = 3000)[region$get_indices()][,1] > 0
  expect_silent(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "establishment",
                                      control_design = control_design,
                                      radius = 3000,
                                      control_mult = 0.7,
                                      control_cost = 2,
                                      stages = 2:3,
                                      apply_to = "survival",
                                      schedule = 4:6))
  expect_silent(new_n <- manage_controls$apply(n, 4))
  expect_equal(attr(new_n, "control_establishment"),
               ((exist_mask | establish_mask)*0.7 +
                  !(exist_mask | establish_mask)))
  expect_equal(attr(new_n, "control_establishment_cost"),
               (expected_costs*(control_design$get_allocation() > 0 |
                                  establish_mask)))
  # additional detection and removals
  detected[,2:3] <- round((n[,2:3] > 6)*n[,2:3]*0.7)
  new_n[which(rowSums(detected) >= 8),] <- 0
  attr(new_n, "undetected") <- new_n[,] - detected
  exist_mask <- rowSums(new_n[,2:3])*control_design$get_allocation() > 0
  detected_mask <- +(rowSums(detected) > 0 | NA)
  detected_mask <- terra::buffer(region$get_rast(detected_mask),
                                 width = 3000)[region$get_indices()][,1] > 0
  expect_silent(new_n2 <- manage_controls$apply(new_n, 4))
  expected_mult <- rep(1, region$get_locations())
  expected_mult[which(exist_mask | detected_mask)] <-
    expected_mult[which(exist_mask | detected_mask)]*0.7
  expect_equal(attr(new_n2, "control_establishment"), expected_mult)
  expect_equal(
    attr(new_n2, "control_establishment_cost"),
    expected_costs*(control_design$get_allocation() > 0 | detected_mask))
  expect_silent(manage_controls$set_id("a3"))
  expect_silent(new_n <- manage_controls$apply(n, 4))
  detected[,2:3] <- round((n[,2:3] > 8)*n[,2:3]*0.7)
  expect_equal(attr(new_n, "undetected")[idx,], (n - detected)[idx,])
  expect_equal(attr(new_n, "a3_control_establishment"),
               ((exist_mask | establish_mask)*0.7 +
                  !(exist_mask | establish_mask)))
  expect_equal(attr(new_n, "a3_control_establishment_cost"),
               expected_costs*(control_design$get_allocation() > 0 |
                                 establish_mask))
  # spatially-implicit
  region <- bsspread::Region()
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  initializer <- bsspread::Initializer(50, region = region,
                                       population_model = population_model)
  set.seed(1234)
  n <- initializer$initialize()
  detected <- n*0
  detected[,2:3] <- round(n[,2:3]*0.7)
  attr(n, "undetected") <- n - detected
  expect_silent(
    manage_controls <- ManageControls(region, population_model,
                                      control_type = "growth",
                                      control_design = NULL,
                                      radius = NULL,
                                      control_mult = 0.7,
                                      control_cost = 2,
                                      stages = 2:3,
                                      apply_to = "survival",
                                      schedule = 4:6))
  expect_error(new_n <- manage_controls$apply(n, 4),
               paste("Cannot calculate spatially implicit control cost",
                     "without area occupied."))
  attr(n, "spread_area") <- 300
  set.seed(1234)
  expect_silent(new_n <- manage_controls$apply(n, 4))
  expect_equal(new_n[,], n[,])
  expect_equal(attr(new_n, "undetected")[,], (n - detected)[,])
  expect_equal(as.numeric(attr(new_n, "control_growth")), 0.7)
  expect_equal(attr(attr(new_n, "control_growth"), "stages"), 2:3)
  expect_equal(attr(attr(new_n, "control_growth"), "apply_to"), "survival")
  expect_equal(attr(new_n, "control_growth_cost"), 2*300)
})
