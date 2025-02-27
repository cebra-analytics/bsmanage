context("ManageDesign")

test_that("initializes with context, divisions, and valid parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  divisions <- bsdesign::Divisions(template)
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = "invalid"),
               paste("Divisions parameter must be a 'bsdesign::Divisions' or",
                     "inherited class object."))
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             establish_pr = 1:5),
               paste("The establishment probability must be a numeric vector",
                     "with values >= 0 for each division part."))
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             benefit = 1:5),
               paste("The benefit parameter must be a numeric vector with",
                     "values for each division part."))
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             overall_pr = 1.5),
               paste("The overall probability/effectiveness parameter must be",
                     "numeric, >= 0 and <= 1."))
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             optimal = "saving",
                                             benefit = NULL),
               "The benefit parameter must be specified for optimal saving.")
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             optimal = "benefit",
                                             benefit = NULL),
               "The benefit parameter must be specified for optimal benefit.")
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             optimal = "benefit",
                                             benefit = 1,
                                             budget = NULL,
                                             overall_pr = NULL),
               paste("Either the budget or overall probability/effectiveness",
                     "parameter must be specified for optimal benefit."))
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             optimal = "successes",
                                             budget = NULL,
                                             overall_pr = NULL),
               paste("Either the budget or overall probability/effectiveness",
                     "parameter must be specified for optimal management",
                     "successes."))
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             optimal = "effectiveness",
                                             budget = NULL,
                                             overall_pr = NULL),
               paste("Either the budget or overall probability/effectiveness",
                     "parameter must be specified for optimal effectiveness."))
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             optimal = "saving",
                                             benefit = 1,
                                             alloc_cost = 1:5),
               paste("The allocation cost parameter must be a numeric vector",
                     "with values for each division part."))
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             optimal = "effectiveness",
                                             budget = 0),
               "The budget parameter must be numeric and > 0.")
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             optimal = "effectiveness",
                                             budget = 1,
                                             exist_alloc = 1),
               paste("The existing allocation parameter should only be",
                     "specified when the optimal parameter is 'none'."))
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             optimal = "none",
                                             exist_alloc = 1),
               paste("The existing allocation parameter must be a numeric",
                     "vector with values for each division part."))
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             optimal = "none",
                                             exist_manage_pr = 1:5),
               paste("The existing management probability/effectiveness",
                     "parameter must be a numeric vector, or list of numeric",
                     "vectors, with values for each division part."))
  expect_error(manage_design <- ManageDesign(context = ManageContext("test"),
                                             divisions = divisions,
                                             optimal = "none",
                                             exist_manage_pr = list(a = 1,
                                                                    b = 1:5)),
               paste("The existing management probability/effectiveness",
                     "parameter must be a numeric vector, or list of numeric",
                     "vectors, with values for each division part."))
  expect_silent(manage_design <- ManageDesign(context = ManageContext("test"),
                                              divisions = divisions,
                                              optimal = "none"))
  expect_is(manage_design, "ManageDesign")
  expect_named(manage_design, c("get_context", "get_divisions", "get_dim_type",
                                "get_allocation", "get_manage_pr",
                                "get_overall_pr", "save_design"))
  expect_is(manage_design$get_context(), "ManageContext")
  expect_is(manage_design$get_divisions(), "Divisions")
  expect_equal(manage_design$get_dim_type(), "spatial")
  expect_null(manage_design$get_allocation())
  expect_null(manage_design$get_manage_pr())
  expect_null(manage_design$get_overall_pr())
  expect_silent(manage_design <- ManageDesign(context = ManageContext("test"),
                                              divisions = divisions,
                                              dim_type = "user_defined",
                                              optimal = "none"))
  expect_equal(manage_design$get_dim_type(), "user_defined")
})

test_that("combines existing management probability/effectiveness via union", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  divisions <- bsdesign::Divisions(template)
  set.seed(1234)
  exist_manage_pr <- lapply(1:3, function(i) runif(divisions$get_parts(), 0, 1))
  expect_silent(manage_design <- ManageDesign(
    context = ManageContext("test"),
    divisions = divisions,
    optimal = "none",
    exist_manage_pr = exist_manage_pr))
  expected_manage_pr <-
    1 - apply(cbind(exist_manage_pr[[1]], exist_manage_pr[[2]],
                    exist_manage_pr[[3]]), 1,
              function(s) prod(1 - s))
  expect_equal(round(manage_design$get_manage_pr(), 8),
               round(expected_manage_pr, 8))
  expect_null(manage_design$get_overall_pr())
})
