context("ManageImpacts")

test_that("initializes with impacts, populations and impact stages", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  context <- bsimpact::Context("My species",
                               impact_scope = c("aspect1", "aspect2"))
  incursion <- bsimpact::Incursion(template*0, region)
  impact_layers <- list(aspect1 = 100*(template > 0.1 & template < 0.3),
                        aspect2 = 200*(template > 0.2 & template < 0.4))
  impacts <- bsimpact::ImpactAnalysis(context, region, incursion,
                                      impact_layers)
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  expect_error(manage_impacts <- ManageImpacts("impacts", population_model),
               "Impacts must be a 'ImpactAnalysis' or inherited class object.")
  expect_error(manage_impacts <- ManageImpacts(impacts, "population_model"),
               paste("Population model must be a 'Population' or inherited",
                     "class object."))
  expect_error(manage_impacts <- ManageImpacts(impacts, population_model,
                                               impact_stages = 3:4),
               paste("Impact stages must be a vector of stage indices",
                     "consistent with the population model."))
  expect_error(manage_impacts <- ManageImpacts(impacts, population_model,
                                               calc_total = 1),
               "Calculate total indicator should be logical.")
  expect_silent(manage_impacts <- ManageImpacts(impacts, population_model,
                                                impact_stages = 2:3))
  expect_is(manage_impacts, "ManageImpacts")
  expect_named(manage_impacts, c("get_context", "get_calc_total",
                                 "includes_combined", "calculate"))
  expect_is(manage_impacts$get_context(), "Context")
  expect_true(manage_impacts$includes_combined())
  expect_true(manage_impacts$get_calc_total())
  expect_silent(manage_impacts <- ManageImpacts(impacts, population_model,
                                                calc_total = FALSE))
  expect_false(manage_impacts$get_calc_total())
  context <- bsimpact::Context("My species",
                               impact_scope = c("aspect1", "aspect2"),
                               valuation_type = "non-monetary")
  impacts <- bsimpact::ImpactAnalysis(context, region, incursion,
                                      impact_layers)
  expect_silent(manage_impacts <- ManageImpacts(impacts, population_model))
  expect_false(manage_impacts$get_calc_total())
  impacts <- bsimpact::ImpactAnalysis(context, region, incursion,
                                      impact_layers, combine_function = "none")
  expect_silent(manage_impacts <- ManageImpacts(impacts, population_model,
                                               calc_total = TRUE))
  expect_silent(manage_impacts <- ManageImpacts(impacts, population_model,
                                                calc_total = FALSE))
  expect_false(manage_impacts$get_calc_total())
  context <- bsimpact::Context("My species", impact_scope = "aspect1",
                               valuation_type = "non-monetary")
  impacts <- bsimpact::ImpactAnalysis(context, region, incursion,
                                      impact_layers[1],
                                      combine_function = "none")
  expect_silent(manage_impacts <- ManageImpacts(impacts, population_model,
                                                calc_total = TRUE))
  expect_true(manage_impacts$get_calc_total())
})

test_that("calculates impacts including combined", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  template[region$get_indices()][5922,] <- 0.25
  context <- bsimpact::Context("My species",
                               impact_scope = c("aspect1", "aspect2"))
  incursion <- bsimpact::Incursion(template*0, region, type = "density",
                                   multiplier = 0.2)
  aspects <- list(aspect1 = "aspect1", aspect2 = "aspect2")
  impact_layers <- list(aspect1 = 100*(template > 0.1 & template < 0.3),
                        aspect2 = 200*(template > 0.2 & template < 0.4))
  loss_rates <- c(aspect1 = 0.3, aspect2 = 0.4)
  impacts <- bsimpact::ValueImpacts(context, region, incursion,
                                    impact_layers, loss_rates = loss_rates)
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  initial_n <- rep(0, region$get_locations())
  initial_n[5920:5922] <- 10:12
  initializer <- bsspread::Initializer(initial_n, region = region,
                                       population_model = population_model)
  n <- initializer$initialize()
  expect_silent(manage_impacts <- ManageImpacts(impacts, population_model,
                                                impact_stages = 2:3))
  expected_impacts <- lapply(aspects, function(l) {
    impact_incursion <- rowSums(n[,2:3])*0.2
    impact_incursion[which(impact_incursion > 1)] <- 1
    (impact_incursion*impact_layers[[l]][region$get_indices()][,1]*
        loss_rates[l])
  })
  expected_impacts$combined <-
    expected_impacts$aspect1 + expected_impacts$aspect2
  expect_silent(calc_impacts <- manage_impacts$calculate(n))
  expect_named(calc_impacts, c("aspect1", "aspect2", "combined"))
  expect_equal(calc_impacts, expected_impacts)
})

test_that("calculates spatially implicit impacts via area occupied", {
  context <- bsimpact::Context("My species",
                               impact_scope = c("aspect1", "aspect2"))
  incursion <- bsimpact::Incursion(0, bsimpact::Region(), type = "area")
  aspects <- list(aspect1 = "aspect1", aspect2 = "aspect2")
  impact_layers <- list(aspect1 = 100, aspect2 = 200)
  loss_rates <- c(aspect1 = 0.3, aspect2 = 0.4)
  impacts <- bsimpact::ValueImpacts(context, bsimpact::Region(), incursion,
                                    impact_layers, loss_rates = loss_rates)
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  region <- bsspread::Region()
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  initializer <- bsspread::Initializer(20, region = region,
                                       population_model = population_model)
  n <- initializer$initialize()
  expect_silent(manage_impacts <- ManageImpacts(impacts, population_model,
                                                impact_stages = 2:3))
  expect_error(calc_impacts <- manage_impacts$calculate(n),
               paste("Cannot calculate spatially implicit impacts without",
                     "area occupied."))
  attr(n, "spread_area") <- 50
  expect_silent(calc_impacts <- manage_impacts$calculate(n))
  expect_named(calc_impacts, c("aspect1", "aspect2", "combined"))
  expect_equal(lapply(calc_impacts, as.numeric),
               list(aspect1 = 100*50*0.3,
                    aspect2 = 200*50*0.4,
                    combined = 100*50*0.3 + 200*50*0.4))
})


