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
  expect_silent(manage_impacts <- ManageImpacts(impacts, population_model,
                                                impact_stages = 2:3))
  expect_is(manage_impacts, "ManageImpacts")
  expect_is(manage_impacts$get_context(), "Context")
  expect_true(manage_impacts$includes_combined())
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
  expected_impacts <- lapply(context$get_impact_scope(), function(l) {
    (rowSums(n[,2:3])*impact_layers[[l]][region$get_indices()][,1]*0.2*
       loss_rates[l])
  })
  names(expected_impacts) <- context$get_impact_scope()
  expected_impacts$combined <-
    expected_impacts$aspect1 + expected_impacts$aspect2
  expect_silent(calc_impacts <- manage_impacts$calculate(n))
  expect_named(calc_impacts, c("aspect1", "aspect2", "combined"))
  expect_equal(calc_impacts, expected_impacts)
})
