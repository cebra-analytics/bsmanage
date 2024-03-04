context("ManageSimulator")

test_that("initializes with components and parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  template_vect <- template[region$get_indices()][,1]
  expect_error(simulator <- ManageSimulator(region, time_steps = 0),
               paste("Time step and replicate parameters should be numeric",
                     "values > 0."))
  expect_error(simulator <- ManageSimulator(region, time_steps = 1,
                                            impacts = list(1:2)),
               "Impacts must be a list of 'ManageImpacts' objects.")
  expect_error(simulator <- ManageSimulator(region, time_steps = 1,
                                            actions = list(1:2)),
               "Actions must be a list of 'ManageActions' objects.")
  population_model <- bsspread::UnstructPopulation(region)
  initializer <- bsspread::Initializer(rep(0, region$get_locations()),
                                       region = region,
                                       population_model = population_model)
  dispersal <- bsspread::Dispersal(region, population_model)
  context <- bsimpact::Context("My species", impact_scope = "aspect1")
  incursion <- bsimpact::Incursion(template*0, region, type = "density")
  impacts <- list(ManageImpacts(
    bsimpact::ValueImpacts(context, region, incursion,
                           impact_layers = list(template > 0.5),
                           loss_rates = c(aspect1 = 0.5)),
    population_model))
  surveillance <-
    bsdesign::SpatialSurvDesign(context = bsdesign::Context("test"),
                                divisions = bsdesign::Divisions(template),
                                establish_pr = template_vect*0,
                                lambda = 1,
                                optimal = "none",
                                exist_sens = template_vect)
  actions <- list(ManageDetection(region, population_model, surveillance),
                  ManageRemovals(region, population_model,
                                 removal_pr = template_vect))
  expect_silent(simulator <- ManageSimulator(
    region,
    time_steps = 4,
    initializer = initializer,
    population_model = population_model,
    dispersal_models = list(dispersal),
    impacts = impacts,
    actions = actions))
  class(simulator) # "ManageSimulator" "Simulator"
  expect_is(simulator, "ManageSimulator")
  expect_s3_class(simulator, "Simulator")
  expect_named(simulator, c("set_initializer", "set_population_model",
                            "set_dispersal_models", "run"))
})

test_that("runs simulator with correct configuration", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  template[region$get_indices()][5922,] <- 0.25
  template_vect <- template[region$get_indices()][,1]
  population_model <- bsspread::UnstructPopulation(region, growth = 2)
  population_model_grow <- population_model$grow
  population_model$grow <- function(n) {
    attr(n, "growth") <- c(attr(n, "growth"), max(c(attr(n, "growth"), 0) + 1))
    population_model_grow(n)
  }
  initial_n <- rep(0, region$get_locations())
  initial_n[5920:5922] <- (10:12)*5
  initializer <- bsspread::Initializer(initial_n, region = region,
                                       population_model = population_model)
  dispersal <- bsspread::Dispersal(region, population_model)
  dispersal_disperse <- dispersal$disperse
  dispersal$disperse <- function(n) {
    attr(n$relocated, "dispersal") <-
      c(attr(n$relocated, "dispersal"),
        max(c(attr(n$relocated, "dispersal"), 0) + 1))
    dispersal_disperse(n)
  }
  incursion <- bsimpact::Incursion(template*0, region, type = "density")
  impacts <- list(ManageImpacts(
    bsimpact::ValueImpacts(bsimpact::Context("test",
                                             impact_scope = "aspect1"),
                           region, incursion,
                           impact_layers = list(aspect1 = 100*template),
                           loss_rates = c(aspect1 = 0.5)),
    population_model),
    ManageImpacts(
      bsimpact::ValueImpacts(bsimpact::Context("test",
                                               impact_scope = "aspect2"),
                             region, incursion,
                             impact_layers = list(aspect2 = 200*template),
                             loss_rates = c(aspect2 = 0.7)),
      population_model))
  surveillance <-
    bsdesign::SpatialSurvDesign(context = bsdesign::Context("test"),
                                divisions = bsdesign::Divisions(template),
                                establish_pr = template_vect*0,
                                lambda = 1,
                                optimal = "none",
                                exist_sens = template_vect)
  actions <- list(ManageDetection(region, population_model, surveillance),
                  ManageRemovals(region, population_model,
                                 removal_pr = template_vect))
  expect_silent(simulator <- ManageSimulator(
    region,
    time_steps = 4,
    collation_steps = 2,
    replicates = 1,
    initializer = initializer,
    population_model = population_model,
    dispersal_models = list(dispersal),
    impacts = impacts,
    actions = actions,
    user_function = function(n) {
      attr(n, "user") <- c(attr(n, "user"), max(c(attr(n, "user"), 0) + 1))
      n
    }))
  expect_silent(results <- simulator$run())
  expect_silent(results_list <- results$get_list())
  expect_named(results_list, c("collated", "total", "area", "occupancy",
                               "impacts", "actions"))
  expect_named(results_list$collated, as.character(seq(0, 4, 2)))
  expect_equal(unname(sapply(results_list$collated, length)),
               rep(region$get_locations(), 3))
  expect_equal(attributes(results_list$collated[["2"]]),
               list(growth = 1:2, dispersal = 1:2, user = 1:2))
  expect_equal(attributes(results_list$collated[["4"]]),
               list(growth = 1:4, dispersal = 1:4, user = 1:4))
  expect_named(results_list$total, as.character(0:4))
  expect_named(results_list$area, as.character(0:4))
  expect_named(results_list$occupancy, as.character(seq(0, 4, 2)))
  expect_length(results_list$impacts, 2)
  expect_equal(lapply(results_list$impacts, function(i) lapply(i, length)),
               list(list(aspect1 = 3, combined = 3, total = 5),
                    list(aspect2 = 3, combined = 3, total = 5)))
  expect_named(results_list$impacts[[1]]$aspect1, as.character(seq(0, 4, 2)))
  expect_equal(unname(results_list$impacts[[1]]$aspect1),
               lapply(1:3, function (n) (initial_n > 0)*template_vect*100*0.5))
  expect_named(results_list$impacts[[2]]$aspect2, as.character(seq(0, 4, 2)))
  expect_equal(unname(results_list$impacts[[2]]$aspect2),
               lapply(1:3, function (n) (initial_n > 0)*template_vect*200*0.7))
  expect_length(results_list$actions, 2)
  expect_equal(lapply(results_list$actions, function(i) lapply(i, length)),
               list(list(detected = 3, total = 5),
                    list(removed = 3, total = 5)))
  expect_named(results_list$actions[[1]]$detected, as.character(seq(0, 4, 2)))
  expect_equal(unname(lapply(results_list$actions[[1]]$detected,
                             function(n) n > 0)),
               lapply(1:3, function (n) (initial_n > 0)))
  expect_named(results_list$actions[[2]]$removed, as.character(seq(0, 4, 2)))
  expect_equal(unname(lapply(results_list$actions[[2]]$removed,
                             function(n) n > 0)),
               lapply(1:3, function (n) (initial_n > 0)))
  expect_equal(results_list$collated[["0"]],
               initial_n - results_list$actions[[2]]$removed[["0"]])
})
