context("ManageResults")

test_that("initializes inherited object with impacts and actions", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  template_vect <- template[region$get_indices()][,1]
  pops <- region$get_locations()
  population_model <- bsspread::UnstructPopulation(region, growth = 1.2)
  context <- list(bsimpact::Context("My species",
                                    impact_scope = c("aspect1", "aspect2")),
                  bsimpact::Context("My species", impact_scope = "aspect3",
                                    valuation_type = "non-monetary"))
  incursion <- bsimpact::Incursion(template*0, region, type = "density",
                                   multiplier = 0.2)
  impact_layers <- list(aspect1 = 100*(template > 0.1 & template < 0.3),
                        aspect2 = 200*(template > 0.2 & template < 0.4),
                        aspect3 = 300*(template > 0.1 & template < 0.3))
  loss_rates <- c(aspect1 = 0.1, aspect2 = 0.2, aspect3 = 0.3)
  impacts <- list(
    a1 = ManageImpacts(
      bsimpact::ValueImpacts(context[[1]], region, incursion,
                             impact_layers[1:2], loss_rates = loss_rates[1:2]),
      population_model),
    a2 = ManageImpacts(
      bsimpact::ValueImpacts(context[[2]], region, incursion,
                             impact_layers[3], loss_rates = loss_rates[3],
                             combine_function = "none"),
      population_model, calc_total = TRUE))
  surveillance <-
    bsdesign::SpatialSurvDesign(context = bsdesign::Context("test"),
                                divisions = bsdesign::Divisions(template),
                                establish_pr = template_vect*0,
                                lambda = 1,
                                optimal = "none",
                                exist_sens = template_vect)
  actions <- list(
    a3 = ManageDetection(region, population_model, surveillance),
    a4 = ManageRemovals(region, population_model))
  expect_silent(results <- ManageResults(region,
                                         population_model = population_model))
  expect_error(results <- ManageResults(region,
                                        population_model = population_model,
                                        impacts = as.list(1:2)),
               paste("Impacts must be a list of 'ManageImpacts' or inherited",
                     "class objects."))
  expect_error(results <- ManageResults(region,
                                        population_model = population_model,
                                        actions = as.list(1:2)),
               paste("Actions must be a list of 'ManageActions' or inherited",
                     "class objects."))
  expect_silent(results <- ManageResults(region,
                                         population_model = population_model,
                                         impacts = impacts, actions = actions,
                                         time_steps = 10, collation_steps = 2,
                                         replicates = 1))
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
               c("collated", "total", "area", "occupancy", "impacts",
                 "actions"))
  expect_equal(lapply(result_list$impacts, function(i) lapply(i, length)),
               list(a1 = list(aspect1 = 6, aspect2 = 6, combined = 6,
                              total = 11),
                    a2 = list(aspect3 = 6, total = 11)))
  expect_equal(lapply(result_list$actions, function(i) lapply(i, length)),
               list(a3 = list(detected = 6, total = 11),
                    a4 = list(removed = 6, total = 11)))
  collated <- rep(pops, 6)
  names(collated) <- as.character(seq(0, 10, 2))
  totals <- rep(1, 11)
  names(totals) <- as.character(0:10)
  expect_equal(lapply(result_list$impacts,
                      function(i) lapply(i, function(j) sapply(j, length))),
               list(a1 = list(aspect1 = collated, aspect2 = collated,
                              combined = collated, total = totals),
                    a2 = list(aspect3 = collated, total = totals)))
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i, function(j) sapply(j, length))),
               list(a3 = list(detected = collated, total = totals),
                    a4 = list(removed = collated, total = totals)))
})

test_that("collates and finalizes impact results", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  template[region$get_indices()][5922,] <- 0.25
  population_model <- bsspread::UnstructPopulation(region, growth = 1.2)
  n <- rep(0, region$get_locations())
  n[5920:5922] <- 7:9
  context <- list(bsimpact::Context("My species",
                                    impact_scope = c("aspect1", "aspect2")),
                  bsimpact::Context("My species", impact_scope = "aspect3",
                                    valuation_type = "non-monetary"))
  incursion <- bsimpact::Incursion(template*0, region, type = "density",
                                   multiplier = 0.1)
  aspects <- list(aspect1 = "aspect1", aspect2 = "aspect2",
                  aspect3 = "aspect3")
  impact_layers <- list(aspect1 = 100*(template > 0.1 & template < 0.3),
                        aspect2 = 200*(template > 0.2 & template < 0.4),
                        aspect3 = 300*(template > 0.1 & template < 0.3))
  impact_layer_vals <- lapply(impact_layers,
                              function(l) l[region$get_indices()][5920:5922,])
  loss_rates <- c(aspect1 = 0.1, aspect2 = 0.2, aspect3 = 0.3)
  impact_names <- list(a1 = "a1", a2 = "a2")
  impacts <- list(
    a1 = ManageImpacts(
      bsimpact::ValueImpacts(context[[1]], region, incursion,
                             impact_layers[1:2], loss_rates = loss_rates[1:2]),
      population_model),
    a2 = ManageImpacts(
      bsimpact::ValueImpacts(context[[2]], region, incursion,
                             impact_layers[3], loss_rates = loss_rates[3],
                             combine_function = "none"),
      population_model, calc_total = TRUE))
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

  expect_equal(lapply(result_list$impacts,
                      function(i) lapply(i[names(i) != "total"],
                                         function(j) j[["2"]][5920:5922])),
               expected_collated)
  expect_equal(lapply(result_list$impacts,
                      function(i) lapply(i[names(i) == "total"],
                                         function(j) j[["2"]])),
               lapply(expected_collated,
                      function(i) list(total = sum(
                        unlist(i[names(i) != "combined"])))))
  # multiple replicates
  expect_silent(results <- ManageResults(region,
                                         population_model = population_model,
                                         impacts = impacts, time_steps = 4,
                                         collation_steps = 2, replicates = 3))
  calc_impacts <- list()
  n[5920:5922] <- 5:7
  calc_impacts[[1]] <- lapply(impacts,
                              function(impacts_i) impacts_i$calculate(n))
  n[5920:5922] <- 7:9
  calc_impacts[[2]] <- lapply(impacts,
                              function(impacts_i) impacts_i$calculate(n))
  n[5920:5922] <- 8:10
  calc_impacts[[3]] <- lapply(impacts,
                              function(impacts_i) impacts_i$calculate(n))
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
  expect_silent(results$collate(r = 1, tm = 2, n = n, calc_impacts[[1]]))
  expect_silent(results$collate(r = 2, tm = 2, n = n, calc_impacts[[2]]))
  expect_silent(results$collate(r = 3, tm = 2, n = n, calc_impacts[[3]]))
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) != "total"],
                              function(j) j[["2"]]$mean[5920:5922])),
    lapply(calc_impacts_r, function(i) lapply(i, function(a) colMeans(a))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) != "total"],
                              function(j) j[["2"]]$sd[5920:5922])),
    lapply(calc_impacts_r, function(i) lapply(i, function(a) apply(a, 2, sd))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"],
                              function(j) j[["2"]]$mean)),
    list(a1 = list(total = mean(rowSums(calc_impacts_r$a1$combined))),
         a2 = list(total = mean(rowSums(calc_impacts_r$a2$aspect3)))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"],
                              function(j) j[["2"]]$sd)),
    list(a1 = list(total = sd(rowSums(calc_impacts_r$a1$combined))),
         a2 = list(total = sd(rowSums(calc_impacts_r$a2$aspect3)))))
})

test_that("collates and finalizes action results", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  template[region$get_indices()][5916:5922,] <- c(rep(0.5, 4), 0.5, 0.75, 1)
  template_vect <- template[region$get_indices()][,1]
  population_model <- bsspread::UnstructPopulation(region, growth = 1.2)
  n <- rep(0, region$get_locations())
  n[5920:5922] <- (10:12)*5
  surveillance <-
    bsdesign::SpatialSurvDesign(context = bsdesign::Context("test"),
                                divisions = bsdesign::Divisions(template),
                                establish_pr = template_vect*0,
                                lambda = 1,
                                optimal = "none",
                                exist_sens = template_vect)
  actions <- list(
    a3 = ManageDetection(region, population_model, surveillance,
                         schedule = 2:3),
    a4 = ManageRemovals(region, population_model, removal_pr = template_vect,
                        schedule = 2:3))
  n <- actions$a3$apply(n, 2)
  n <- actions$a4$apply(n, 2)
  # single replicate
  expected_collated <- lapply(actions, function(a) {
    n_a <- attr(n, a$get_label())
    collated <- list(n_a, total = sum(n_a))
    names(collated)[1] <- a$get_label()
    collated
  })
  expect_silent(results <- ManageResults(region,
                                         population_model = population_model,
                                         actions = actions, time_steps = 4,
                                         collation_steps = 2, replicates = 1))
  expect_silent(results$collate(r = 1, tm = 2, n = n))
  attributes(n) <- NULL
  n <- actions$a3$apply(n, 4)
  n <- actions$a4$apply(n, 4)
  expect_silent(results$collate(r = 1, tm = 4, n = n))
  expect_silent(result_list <- results$get_list())
  expect_equal(lapply(result_list$collated, function(i) attributes(i)),
               lapply(result_list$collated, function(i) NULL)) # no attr
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i, function(j) j[["2"]])),
               expected_collated)
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i, function(j) j[["4"]])),
               lapply(expected_collated,
                      function(i) lapply(i, function(j) j*0)))
  # multiple replicates
  n_r <- list(); n_r2 <- list()
  n[5920:5922] <- (9:11)*5
  n <- actions$a3$apply(n, 2); n <- actions$a4$apply(n, 2); n_r[[1]] <- n
  attributes(n) <- NULL
  n <- actions$a3$apply(n, 4); n <- actions$a4$apply(n, 4); n_r2[[1]] <- n
  n[5920:5922] <- (10:12)*5
  n <- actions$a3$apply(n, 2); n <- actions$a4$apply(n, 2); n_r[[2]] <- n
  attributes(n) <- NULL
  n <- actions$a3$apply(n, 4); n <- actions$a4$apply(n, 4); n_r2[[2]] <- n
  n[5920:5922] <- (11:13)*5
  n <- actions$a3$apply(n, 2); n <- actions$a4$apply(n, 2); n_r[[3]] <- n
  attributes(n) <- NULL
  n <- actions$a3$apply(n, 4); n <- actions$a4$apply(n, 4); n_r2[[3]] <- n
  collated_r <- lapply(n_r, function(n) lapply(actions, function(a) {
    n_a <- attr(n, a$get_label())
    collated <- list(n_a, total = sum(n_a))
    names(collated)[1] <- a$get_label()
    collated
  }))
  collated_arrays <- collated_r[[1]]
  for (i in 2:3) {
    for (j in names(actions)) {
      for (a in names(collated_r[[i]][[j]])) {
        collated_arrays[[j]][[a]] <- rbind(collated_arrays[[j]][[a]],
                                           collated_r[[i]][[j]][[a]])
      }
    }
  }
  expect_silent(results <- ManageResults(region,
                                         population_model = population_model,
                                         actions = actions, time_steps = 4,
                                         collation_steps = 2, replicates = 3))
  expect_silent(zero_results <- results$get_list()$actions)
  expect_silent(results$collate(r = 1, tm = 2, n = n_r[[1]]))
  expect_silent(results$collate(r = 1, tm = 4, n = n_r2[[1]]))
  expect_silent(results$collate(r = 2, tm = 2, n = n_r[[2]]))
  expect_silent(results$collate(r = 2, tm = 4, n = n_r2[[2]]))
  expect_silent(results$collate(r = 3, tm = 2, n = n_r[[3]]))
  expect_silent(results$collate(r = 3, tm = 4, n = n_r2[[3]]))
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i, function(j) j[["2"]]$mean)),
               lapply(collated_arrays,
                      function(i) lapply(i, function(a) colMeans(a))))
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i, function(j) j[["2"]]$sd)),
               lapply(collated_arrays, function(i)
                 lapply(i, function(a) apply(a, 2, sd))))
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i, function(j) j[["4"]]$mean)),
               lapply(zero_results,
                      function(i) lapply(i, function(j) j[["4"]]$mean)))
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i, function(j) j[["4"]]$sd)),
               lapply(zero_results,
                      function(i) lapply(i, function(j) j[["4"]]$sd)))
})
