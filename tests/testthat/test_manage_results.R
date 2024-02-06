context("ManageResults")

test_that("initializes inherited object with impacts", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  pops <- region$get_locations()
  population_model <- bsspread::UnstructPopulation(region, growth = 1.2)
  context <- list(bsimpact::Context("My species",
                                    impact_scope = c("aspect1", "aspect2")),
                  bsimpact::Context("My species",
                                    impact_scope = c("aspect3", "aspect4")))
  incursion <- bsimpact::Incursion(template*0, region, type = "density",
                                   multiplier = 0.2)
  impact_layers <- list(aspect1 = 100*(template > 0.1 & template < 0.3),
                        aspect2 = 200*(template > 0.2 & template < 0.4),
                        aspect3 = 300*(template > 0.1 & template < 0.3),
                        aspect4 = 400*(template > 0.2 & template < 0.4))
  loss_rates <- c(aspect1 = 0.1, aspect2 = 0.2, aspect3 = 0.3, aspect4 = 0.4)
  impacts <- list(
    a1 = ManageImpacts(
      bsimpact::ValueImpacts(context[[1]], region, incursion,
                             impact_layers[1:2], loss_rates = loss_rates[1:2]),
      population_model),
    a2 = ManageImpacts(
      bsimpact::ValueImpacts(context[[2]], region, incursion,
                             impact_layers[3:4], loss_rates = loss_rates[3:4]),
      population_model))

  expect_silent(results <- ManageResults(region,
                                         population_model = population_model))
  expect_error(results <- ManageResults(region,
                                        population_model = population_model,
                                        impacts = as.list(1:2)),
               paste("Impacts must be a list of 'ManageImpacts' or inherited",
                     "class objects."))
  expect_silent(results <- ManageResults(region,
                                         population_model = population_model,
                                         impacts = impacts, time_steps = 10,
                                         collation_steps = 2, replicates = 1))
  expect_is(results, "ManageResults")
  expect_s3_class(results, "Results")
  expect_named(results, c("collate", "finalize", "get_list", "get_params",
                          "save_rasters", "save_csv", "save_plots"))
  expect_equal(results$get_params(),
               list(time_steps = 10, step_duration = 1, step_units = "years",
                    collation_steps = 2, replicates = 1, stages = NULL,
                    combine_stages = NULL))
  expect_silent(result_list <- results$get_list())
  expect_named(result_list,
               c("collated", "total", "area", "occupancy", "impacts"))
  expect_equal(lapply(result_list$impacts, function(i) lapply(i, length)),
               list(a1 = list(aspect1 = 6, aspect2 = 6, combined = 6,
                              total = 11),
                    a2 = list(aspect3 = 6, aspect4 = 6, combined = 6,
                              total = 11)))
  collated <- rep(pops, 6)
  names(collated) <- as.character(seq(0, 10, 2))
  totals <- rep(1, 11)
  names(totals) <- as.character(0:10)
  expect_equal(lapply(result_list$impacts,
                      function(i) lapply(i, function(j) sapply(j, length))),
               list(a1 = list(aspect1 = collated, aspect2 = collated,
                              combined = collated, total = totals),
                    a2 = list(aspect3 = collated, aspect4 = collated,
                              combined = collated, total = totals)))
})

test_that("initializes inherited object with impacts", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  template[region$get_indices()][5922,] <- 0.25
  population_model <- bsspread::UnstructPopulation(region, growth = 1.2)
  n <- rep(0, region$get_locations())
  n[5920:5922] <- 7:9
  context <- list(bsimpact::Context("My species",
                                    impact_scope = c("aspect1", "aspect2")),
                  bsimpact::Context("My species",
                                    impact_scope = c("aspect3", "aspect4")))
  incursion <- bsimpact::Incursion(template*0, region, type = "density",
                                   multiplier = 0.1)
  aspects <- list(aspect1 = "aspect1", aspect2 = "aspect2",
                  aspect3 = "aspect3", aspect4 = "aspect4")
  impact_layers <- list(aspect1 = 100*(template > 0.1 & template < 0.3),
                        aspect2 = 200*(template > 0.2 & template < 0.4),
                        aspect3 = 300*(template > 0.1 & template < 0.3),
                        aspect4 = 400*(template > 0.2 & template < 0.4))
  impact_layer_vals <- lapply(impact_layers,
                              function(l) l[region$get_indices()][5920:5922,])
  loss_rates <- c(aspect1 = 0.1, aspect2 = 0.2, aspect3 = 0.3, aspect4 = 0.4)
  impact_names <- list(a1 = "a1", a2 = "a2")
  impacts <- list(
    a1 = ManageImpacts(
      bsimpact::ValueImpacts(context[[1]], region, incursion,
                             impact_layers[1:2], loss_rates = loss_rates[1:2]),
      population_model),
    a2 = ManageImpacts(
      bsimpact::ValueImpacts(context[[2]], region, incursion,
                             impact_layers[3:4], loss_rates = loss_rates[3:4]),
      population_model))
  calc_impacts <- lapply(impacts, function(impacts_i) impacts_i$calculate(n))
  expected_collated <- lapply(calc_impacts, function(i) {
    lapply(i, function(j) j[5920:5922])
  })
  # single replicate
  expect_silent(results <- ManageResults(region,
                                         population_model = population_model,
                                         impacts = impacts, time_steps = 4,
                                         collation_steps = 2, replicates = 1))
  expect_silent(results$collate(r = 1, tm = 2, n = n, calc_impacts))
  expect_silent(result_list <- results$get_list())
  expect_equal(lapply(result_list$impacts, function(i) {
    lapply(i[1:3], function(j) j[["2"]][5920:5922])
  }), expected_collated)
  expect_equal(lapply(result_list$impacts, function(i) {
    lapply(i[4], function(j) j[["2"]])
  }), lapply(expected_collated, function(i) list(total = sum(i$combined))))
  # multiple replicates
  expect_silent(results <- ManageResults(region,
                                         population_model = population_model,
                                         impacts = impacts, time_steps = 4,
                                         collation_steps = 2, replicates = 3))
  calc_impacts <- list()
  n[5920:5922] <- 5:7
  calc_impacts[[1]] <- lapply(impacts,
                              function(impacts_i) impacts_i$calculate(n))
  expect_silent(results$collate(r = 1, tm = 2, n = n, calc_impacts[[1]]))
  n[5920:5922] <- 7:9
  calc_impacts[[2]] <- lapply(impacts,
                              function(impacts_i) impacts_i$calculate(n))
  expect_silent(results$collate(r = 2, tm = 2, n = n, calc_impacts[[2]]))
  n[5920:5922] <- 8:10
  calc_impacts[[3]] <- lapply(impacts,
                              function(impacts_i) impacts_i$calculate(n))
  expect_silent(results$collate(r = 3, tm = 2, n = n, calc_impacts[[3]]))
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  calc_impacts_vals <- lapply(calc_impacts, function(i) {
    lapply(i, function(a) lapply(a, function(v) v[5920:5922]))
  })
  calc_impacts_r <- calc_impacts_vals[[1]]
  for (i in 2:3) {
    for (j in impact_names) {
      for (a in names(calc_impacts_r[[j]])) {
        calc_impacts_r[[j]][[a]] <- rbind(calc_impacts_r[[j]][[a]],
                                          calc_impacts_vals[[i]][[j]][[a]])
      }
    }
  }
  expect_equal(lapply(result_list$impacts, function(i) {
    lapply(i[1:3], function(j) j[["2"]]$mean[5920:5922])
  }), lapply(calc_impacts_r,
             function(i) lapply(i, function(a) colMeans(a))))
  expect_equal(lapply(result_list$impacts, function(i) {
    lapply(i[1:3], function(j) j[["2"]]$sd[5920:5922])
    }), lapply(calc_impacts_r,
               function(i) lapply(i, function(a) apply(a, 2, sd))))
  expect_equal(lapply(result_list$impacts, function(i) {
    lapply(i[4], function(j) j[["2"]]$mean)
  }), lapply(calc_impacts_r,
             function(i) list(total = mean(rowSums(i$combined)))))
  expect_equal(lapply(result_list$impacts, function(i) {
    lapply(i[4], function(j) j[["2"]]$sd)
  }), lapply(calc_impacts_r,
             function(i) list(total = sd(rowSums(i$combined)))))
})
