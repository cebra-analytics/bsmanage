context("ControlDesign")

test_that("initializes with context, divisions, and valid parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- bsdesign::Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  establish_pr <- test_ref$establish_pr
  expect_error(control_design <- ControlDesign(context = ManageContext("test"),
                                               divisions = "invalid"),
               paste("Divisions parameter must be a 'bsdesign::Divisions' or",
                     "inherited class object."))
  expect_error(control_design <- ControlDesign(context = ManageContext("test"),
                                               divisions = divisions,
                                               establish_pr = establish_pr,
                                               lambda = 1:5,
                                               optimal = "none"),
               paste("The lambda parameter must be numeric, >= 0, and match",
                     "the number of division parts."))
  expect_error(control_design <- ControlDesign(context = ManageContext("test"),
                                               divisions = divisions,
                                               establish_pr = establish_pr,
                                               lambda = 1,
                                               optimal = "saving",
                                               benefit = 1,
                                               fixed_cost = 1:5),
               paste("The fixed cost parameter must be a numeric vector with",
                     "values for each division part."))
  expect_error(control_design <- ControlDesign(context = ManageContext("test"),
                                               divisions = divisions,
                                               establish_pr = establish_pr,
                                               lambda = 1,
                                               optimal = "effectiveness",
                                               budget = 1,
                                               min_alloc = 1:5),
               paste("The minimum allocation parameter must be a numeric",
                     "vector with values for each division part."))
  expect_error(control_design <- ControlDesign(context = ManageContext("test"),
                                               divisions = divisions,
                                               establish_pr = establish_pr,
                                               lambda = 1,
                                               optimal = "effectiveness",
                                               budget = 1,
                                               discrete_alloc = NULL),
               "The discrete allocation indicator parameter must be logical.")
  expect_silent(control_design <- ControlDesign(context = ManageContext("test"),
                                                divisions = divisions,
                                                establish_pr = establish_pr,
                                                lambda = 1,
                                                optimal = "none"))
  expect_is(control_design, "ControlDesign")
  expect_s3_class(control_design, "ManageDesign")
  expect_named(control_design,
               c("get_context", "get_divisions", "get_dim_type",
                 "get_cost_unit", "get_allocation", "get_manage_pr",
                 "get_average_pr", "get_overall_pr", "save_design"))
  expect_is(control_design$get_context(), "ManageContext")
  expect_is(control_design$get_divisions(), "Divisions")
  expect_equal(control_design$get_dim_type(), "spatial")
  expect_null(control_design$get_allocation())
  expect_null(control_design$get_manage_pr())
  expect_null(control_design$get_average_pr())
  expect_null(control_design$get_overall_pr())
})

test_that("allocates resources consistently with reference method", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- bsdesign::Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "saving",
    benefit = test_ref$cost_undetected - test_ref$cost_detected,
    budget = NULL))
  expect_silent(no_budget_alloc <- control_design$get_allocation())
  expect_equal(round(no_budget_alloc, 8),
               round(test_ref$surv_effort$no_budget, 8))
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "saving",
    benefit = test_ref$cost_undetected - test_ref$cost_detected,
    budget = test_ref$budget))
  expect_silent(with_saving_alloc <- control_design$get_allocation())
  expect_equal(round(with_saving_alloc, 6),
               round(test_ref$surv_effort$with_budget, 6))
  expect_equal(sum(with_saving_alloc), test_ref$budget)
  expected_manage_pr <- 1 - exp(-1*test_ref$lambda*with_saving_alloc)
  expect_silent(manage_pr <- control_design$get_manage_pr())
  expect_equal(manage_pr, expected_manage_pr)
  expected_overall_pr <-
    1 - ((1 - prod(1 - test_ref$establish_pr*(1 - manage_pr)))/
           (1 - prod(1 - test_ref$establish_pr)))
  expect_silent(overall_pr <- control_design$get_overall_pr())
  expect_equal(overall_pr, expected_overall_pr)
})

test_that("facilitates existing allocations and management probabilities", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- bsdesign::Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  expected_manage_pr <- 1 - exp(-1*test_ref$lambda*
                                  test_ref$surv_effort$no_budget)
  exist_alloc <- c(test_ref$surv_effort$no_budget[1:198], rep(0, 199))
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "none",
    mgmt_cost = list(),
    budget = NULL,
    exist_alloc = exist_alloc))
  expect_silent(exist_manage_pr <- control_design$get_manage_pr())
  expect_equal(exist_manage_pr, c(expected_manage_pr[1:198], rep(0, 199)))
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "none",
    exist_manage_pr = exist_manage_pr))
  expect_equal(control_design$get_manage_pr(),
               c(expected_manage_pr[1:198], rep(0, 199)))
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "saving",
    benefit = test_ref$cost_undetected - test_ref$cost_detected,
    budget = NULL,
    exist_manage_pr = exist_manage_pr))
  expect_silent(half_alloc <- control_design$get_allocation())
  expect_equal(round(half_alloc, 8),
               round(c(rep(0, 198), test_ref$surv_effort$no_budget[199:397]),
                     8))
  expect_silent(manage_pr <- control_design$get_manage_pr())
  expect_equal(manage_pr, expected_manage_pr)
})

test_that("allocates for optimal number of successes via constraints", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- bsdesign::Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "benefit",
    benefit = 1,
    budget = 30))
  expect_silent(const_benefit_alloc <- control_design$get_allocation())
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "successes",
    budget = 30,
    average_pr = 0.9995,
    overall_pr = 0.995))
  expect_silent(successes_alloc <- control_design$get_allocation())
  expect_equal(successes_alloc, const_benefit_alloc)
  expect_equal(sum(successes_alloc), 30)
  expect_true(control_design$get_average_pr() < 0.9995)
  expect_true(control_design$get_overall_pr() < 0.995)
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "benefit",
    benefit = 1,
    average_pr = 0.998))
  expect_silent(const_benefit_alloc <- control_design$get_allocation())
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "successes",
    budget = 35,
    average_pr = 0.998,
    overall_pr = 0.99))
  expect_silent(successes_alloc <- control_design$get_allocation())
  expect_equal(successes_alloc, const_benefit_alloc)
  expect_true(sum(successes_alloc) < 35)
  expect_equal(control_design$get_average_pr(), 0.998)
  expect_true(control_design$get_overall_pr() < 0.99)
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "benefit",
    benefit = 1,
    overall_pr = 0.99))
  expect_silent(const_benefit_alloc <- control_design$get_allocation())
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "successes",
    budget = 35,
    average_pr = 0.9995,
    overall_pr = 0.99))
  expect_silent(successes_alloc <- control_design$get_allocation())
  expect_equal(successes_alloc, const_benefit_alloc)
  expect_true(sum(successes_alloc) < 35)
  expect_true(control_design$get_average_pr() < 0.9995)
  expect_equal(control_design$get_overall_pr(), 0.99)
})

test_that("allocates for optimal effectiveness via constraints", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- bsdesign::Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  benefit <- test_ref$cost_undetected - test_ref$cost_detected
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "none", # saving
    exist_alloc = test_ref$surv_effort$with_budget))
  expect_silent(saving_budget_manage_pr <- control_design$get_manage_pr())
  expect_silent(saving_budget_overall_pr <- control_design$get_overall_pr())
  saving_budget_tot <- sum(test_ref$establish_pr*benefit*
                             saving_budget_manage_pr)
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "effectiveness",
    budget = test_ref$budget))
  expect_silent(effect_budget_alloc <- control_design$get_allocation())
  expect_silent(effect_budget_manage_pr <- control_design$get_manage_pr())
  expect_silent(effect_budget_overall_pr <- control_design$get_overall_pr())
  effect_budget_tot <- sum(test_ref$establish_pr*benefit*
                             effect_budget_manage_pr)
  expect_equal(sum(effect_budget_alloc), test_ref$budget)
  expect_true(effect_budget_overall_pr > saving_budget_overall_pr)
  expect_true(effect_budget_tot < saving_budget_tot)
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "saving",
    benefit = benefit,
    overall_pr = 0.95))
  expect_silent(saving_95_alloc <- control_design$get_allocation())
  expect_silent(saving_95_mgmt_pr <- control_design$get_manage_pr())
  expect_silent(saving_95_oval_pr <- control_design$get_overall_pr())
  saving_95_tot <- sum(test_ref$establish_pr*benefit*saving_95_mgmt_pr)
  expect_equal(saving_95_oval_pr, 0.95)
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "effectiveness",
    overall_pr = 0.95))
  expect_silent(effect_95_alloc <- control_design$get_allocation())
  expect_silent(effect_95_mgmt_pr <- control_design$get_manage_pr())
  expect_silent(effect_95_oval_pr <- control_design$get_overall_pr())
  effect_95_tot <- sum(test_ref$establish_pr*benefit*effect_95_mgmt_pr)
  expect_equal(effect_95_oval_pr, 0.95)
  expect_true(sum(effect_95_alloc) < sum(saving_95_alloc))
  expect_true(effect_95_tot < saving_95_tot)
})

test_that("handles establishment probabilities of one", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- bsdesign::Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  establish_pr <- test_ref$establish_pr/max(test_ref$establish_pr)
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    lambda = test_ref$lambda,
    optimal = "successes",
    budget = test_ref$budget))
  expect_silent(successes_alloc <- control_design$get_allocation())
  expect_equal(sum(successes_alloc), test_ref$budget)
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    lambda = test_ref$lambda,
    optimal = "successes",
    overall_pr = 0.99))
  expect_silent(successes_alloc <- control_design$get_allocation())
  expect_equal(control_design$get_overall_pr(), 0.99)
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    lambda = test_ref$lambda,
    optimal = "effectiveness",
    budget = test_ref$budget))
  expect_silent(effective_alloc <- control_design$get_allocation())
  expect_equal(sum(effective_alloc), test_ref$budget)
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    lambda = test_ref$lambda,
    optimal = "effectiveness",
    overall_pr = 0.99))
  expect_silent(effective_alloc <- control_design$get_allocation())
  expect_equal(control_design$get_overall_pr(), 0.99)
})

test_that("resource allocation utilises previous control efforts", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- bsdesign::Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  previous_control <- +(test_ref$surv_effort$no_budget > 0.1)
  repeats <- +previous_control + (test_ref$surv_effort$no_budget > 0.15)
  previous_control <- previous_control*0.5
  mod_establish_pr <- test_ref$establish_pr*(1 - previous_control)^repeats
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = mod_establish_pr,
    lambda = test_ref$lambda,
    optimal = "saving",
    benefit = test_ref$cost_undetected - test_ref$cost_detected,
    budget = NULL,
    previous_control = NULL))
  expect_silent(expected_alloc <- control_design$get_allocation())
  attr(previous_control, "repeats") <- repeats
  expect_silent(control_design <- ControlDesign(
    context = ManageContext("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "saving",
    benefit = test_ref$cost_undetected - test_ref$cost_detected,
    budget = NULL,
    previous_control = previous_control))
  expect_named(control_design,
               c("get_context", "get_divisions", "get_dim_type",
                 "get_cost_unit", "get_allocation", "get_manage_pr",
                 "get_average_pr", "get_overall_pr", "save_design",
                 "get_mod_establish_pr"))
  expect_equal(control_design$get_mod_establish_pr(), mod_establish_pr)
  expect_silent(mod_alloc <- control_design$get_allocation())
  expect_equal(mod_alloc, expected_alloc)
  expect_true(all(mod_alloc[which(repeats > 0)] <
                    test_ref$surv_effort$no_budget[which(repeats > 0)]))
})
