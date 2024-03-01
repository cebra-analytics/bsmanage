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

# test_that("runs simulator with correct configuration", {
#   TEST_DIRECTORY <- test_path("test_inputs")
# })
