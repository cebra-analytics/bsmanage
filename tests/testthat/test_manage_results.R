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
  incursion <- bsimpact::Incursion(template*0, region, multiplier = 0.2)
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
               c("population", "total_pop", "occupancy", "total_occup", "area",
                 "impacts", "actions"))
  expect_equal(lapply(result_list$impacts, function(i) lapply(i, length)),
               list(a1 = list(aspect1 = 6, aspect2 = 6, combined = 6,
                              total = 11, cumulative = 4),
                    a2 = list(aspect3 = 6, total = 11)))
  expect_equal(lapply(result_list$impacts$a1$cumulative, length),
               list(aspect1 = 6, aspect2 = 6, combined = 6, total = 11))
  expect_equal(lapply(result_list$actions, function(i) lapply(i, length)),
               list(a3 = list(detected = 6, total = 11),
                    a4 = list(removed = 6, total = 11)))
  collated <- rep(pops, 6)
  names(collated) <- as.character(seq(0, 10, 2))
  totals <- rep(1, 11)
  names(totals) <- as.character(0:10)
  cumulative <- c(aspect1 = 6, aspect2 = 6, combined = 6, total = 11)
  expect_equal(lapply(result_list$impacts,
                      function(i) lapply(i, function(j) sapply(j, length))),
               list(a1 = list(aspect1 = collated, aspect2 = collated,
                              combined = collated, total = totals,
                              cumulative = cumulative),
                    a2 = list(aspect3 = collated, total = totals)))
  expect_equal(lapply(result_list$impacts$a1$cumulative,
                      function(i) sapply(i, length)),
               list(aspect1 = collated, aspect2 = collated,
                    combined = collated, total = totals))
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i, function(j) sapply(j, length))),
               list(a3 = list(detected = collated, total = totals),
                    a4 = list(removed = collated, total = totals)))
  # with action costs and monetary-only impacts
  actions <- list(
    a3 = ManageDetection(region, population_model, surveillance,
                         surv_cost = 3),
    a4 = ManageControls(region, population_model, control_type = "growth",
                        control_mult = 0.4, control_cost = 4),
    a5 = ManageControls(region, population_model, control_type = "spread",
                        control_mult = 0.5, control_cost = 5),
    a6 = ManageRemovals(region, population_model, removal_cost = 6))
  expect_silent(
    results <- ManageResults(region, population_model = population_model,
                             impacts = impacts[1], actions = actions,
                             time_steps = 10, collation_steps = 2,
                             replicates = 1))
  expect_silent(result_list <- results$get_list())
  expect_named(result_list, c("population", "total_pop", "occupancy",
                              "total_occup", "area", "impacts", "actions",
                              "total_cost", "total_cumulative_cost"))
  expect_equal(sapply(result_list$total_cost, length), totals)
  expect_equal(sapply(result_list$total_cumulative_cost, length), totals)
  expect_named(result_list$actions,
               c("a3", "a4", "a5", "a6", "total_cost",
                 "total_cumulative_cost"))
  expect_equal(sapply(result_list$actions$total_cost, length), totals)
  expect_equal(sapply(result_list$actions$total_cumulative_cost, length),
               totals)
  expect_equal(lapply(result_list$actions[1:4],
                      function(i) lapply(i, length)),
               lapply(actions, function(a) {
                 l = list(6, total = 11, cost = 3)
                 names(l)[1] <- a$get_label()
                 l
               }))
  expect_equal(lapply(result_list$actions[1:4],
                      function(i) lapply(i$cost, length)),
               lapply(actions, function(a) {
                 l = list(6, total = 11, cumulative = 2)
                 names(l)[1] <- a$get_label()
                 l
               }))
  expect_equal(lapply(result_list$actions[1:4],
                      function(i) lapply(i$cost$cumulative, length)),
               lapply(actions, function(a) {
                 l = list(6, total = 11)
                 names(l)[1] <- a$get_label()
                 l
               }))
  expect_equal(lapply(result_list$actions[1:4],
                      function(i) lapply(i[-which(names(i) == "cost")],
                                         function(j) sapply(j, length))),
               lapply(actions, function(a) {
                 l = list(collated, total = totals)
                 names(l)[1] <- a$get_label()
                 l
               }))
  expect_equal(lapply(result_list$actions[1:4],
                      function(i) {
                        lapply(i$cost[-which(names(i$cost) == "cumulative")],
                               function(j) sapply(j, length))
                      }),
               lapply(actions, function(a) {
                 l = list(collated, total = totals)
                 names(l)[1] <- a$get_label()
                 l
               }))
  expect_equal(lapply(result_list$actions[1:4],
                      function(i) lapply(i$cost$cumulative,
                                         function(j) sapply(j, length))),
               lapply(actions, function(a) {
                 l = list(collated, total = totals)
                 names(l)[1] <- a$get_label()
                 l
               }))
})

test_that("collates and finalizes impact results", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  idx <- 5920:5922 # idx <- 11:13
  template[region$get_indices()][idx[3],] <- 0.25
  population_model <- bsspread::UnstructPopulation(region, growth = 1.2)
  n <- rep(0, region$get_locations())
  n[idx] <- 7:9
  context <- list(bsimpact::Context("My species",
                                    impact_scope = c("aspect1", "aspect2")),
                  bsimpact::Context("My species", impact_scope = "aspect3",
                                    valuation_type = "non-monetary"))
  incursion <- bsimpact::Incursion(template*0, region, multiplier = 0.1)
  aspects <- list(aspect1 = "aspect1", aspect2 = "aspect2",
                  aspect3 = "aspect3")
  impact_layers <- list(aspect1 = 100*(template > 0.1 & template < 0.3),
                        aspect2 = 200*(template > 0.2 & template < 0.4),
                        aspect3 = 300*(template > 0.1 & template < 0.3))
  impact_layer_vals <- lapply(impact_layers,
                              function(l) l[region$get_indices()][idx,])
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
  calc_impacts <- lapply(
    impacts, function(impacts_i) attr(impacts_i$calculate(n), "impacts"))
  expected_collated <- lapply(calc_impacts, function(i) {
    lapply(i, function(j) j[idx])
  })
  # single replicate
  expect_silent(results <- ManageResults(region,
                                         population_model = population_model,
                                         impacts = impacts, time_steps = 4,
                                         collation_steps = 2, replicates = 1))
  for (tm in 0:4) {
    results$collate(r = 1, tm = tm, n = n, calc_impacts)
  }
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[!names(i) %in% c("cumulative","total")],
                              function(j) j[["0"]][idx])),
    expected_collated)
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[!names(i) %in% c("cumulative","total")],
                              function(j) j[["2"]][idx])),
    expected_collated)
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[!names(i) %in% c("cumulative","total")],
                              function(j) j[["4"]][idx])),
    expected_collated)
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"], function(j) j[["0"]])),
    lapply(expected_collated,
           function(i) list(total = sum(unlist(i[names(i) != "combined"])))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"], function(j) j[["1"]])),
    lapply(expected_collated,
           function(i) list(total = sum(unlist(i[names(i) != "combined"])))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"], function(j) j[["2"]])),
    lapply(expected_collated,
           function(i) list(total = sum(unlist(i[names(i) != "combined"])))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"], function(j) j[["3"]])),
    lapply(expected_collated,
           function(i) list(total = sum(unlist(i[names(i) != "combined"])))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"], function(j) j[["4"]])),
    lapply(expected_collated,
           function(i) list(total = sum(unlist(i[names(i) != "combined"])))))
  cum_names <- names(result_list$impacts$a1$cumulative)
  expect_equal(cum_names, c("aspect1", "aspect2", "combined", "total"))
  expected_cumulative <- c(expected_collated$a1,
                           total = sum(unlist(expected_collated$a1[1:2])))
  expect_true(is.list(result_list$impacts$a1$cumulative))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[1:3]],
           function(i) i[["0"]][idx]),
    expected_cumulative[cum_names[1:3]])
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[1:3]],
           function(i) i[["2"]][idx]),
    lapply(expected_cumulative[cum_names[1:3]], function(i) i*3))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[1:3]],
           function(i) i[["4"]][idx]),
    lapply(expected_cumulative[cum_names[1:3]], function(i) i*5))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["0"]]),
    expected_cumulative[cum_names[4]])
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["1"]]),
    lapply(expected_cumulative[cum_names[4]], function(i) i*2))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["2"]]),
    lapply(expected_cumulative[cum_names[4]], function(i) i*3))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["3"]]),
    lapply(expected_cumulative[cum_names[4]], function(i) i*4))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["4"]]),
    lapply(expected_cumulative[cum_names[4]], function(i) i*5))
  expect_null(result_list$impacts$a2$cumulative)
  # multiple replicates
  expect_silent(results <- ManageResults(region,
                                         population_model = population_model,
                                         impacts = impacts, time_steps = 4,
                                         collation_steps = 2, replicates = 3))
  calc_impacts <- list()
  n[idx] <- 5:7
  calc_impacts[[1]] <- lapply(
    impacts, function(impacts_i) attr(impacts_i$calculate(n), "impacts"))
  n[idx] <- 7:9
  calc_impacts[[2]] <- lapply(
    impacts, function(impacts_i) attr(impacts_i$calculate(n), "impacts"))
  n[idx] <- 8:10
  calc_impacts[[3]] <- lapply(
    impacts, function(impacts_i) attr(impacts_i$calculate(n), "impacts"))
  calc_impacts_vals <-
    lapply(calc_impacts,
           function(i) lapply(i, function(a) lapply(a, function(v) v[idx])))
  calc_impacts_r <- calc_impacts_vals[[1]]
  for (i in 2:3) {
    for (j in impact_names) {
      for (a in names(calc_impacts_r[[j]])) {
        calc_impacts_r[[j]][[a]] <- rbind(calc_impacts_r[[j]][[a]],
                                          calc_impacts_vals[[i]][[j]][[a]])
      }
    }
  }
  for (i in 1:3) {
    for (tm in 0:4) {
      results$collate(r = i, tm = tm, n = n, calc_impacts[[i]])
    }
  }
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[!names(i) %in% c("cumulative","total")],
                              function(j) j[["0"]]$mean[idx])),
    lapply(calc_impacts_r, function(i) lapply(i, function(a) colMeans(a))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[!names(i) %in% c("cumulative","total")],
                              function(j) j[["2"]]$mean[idx])),
    lapply(calc_impacts_r, function(i) lapply(i, function(a) colMeans(a))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[!names(i) %in% c("cumulative","total")],
                              function(j) j[["4"]]$mean[idx])),
    lapply(calc_impacts_r, function(i) lapply(i, function(a) colMeans(a))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[!names(i) %in% c("cumulative","total")],
                              function(j) j[["0"]]$sd[idx])),
    lapply(calc_impacts_r, function(i) lapply(i, function(a) apply(a, 2, sd))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[!names(i) %in% c("cumulative","total")],
                              function(j) j[["2"]]$sd[idx])),
    lapply(calc_impacts_r, function(i) lapply(i, function(a) apply(a, 2, sd))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[!names(i) %in% c("cumulative","total")],
                              function(j) j[["4"]]$sd[idx])),
    lapply(calc_impacts_r, function(i) lapply(i, function(a) apply(a, 2, sd))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"],
                              function(j) j[["0"]]$mean)),
    list(a1 = list(total = mean(rowSums(calc_impacts_r$a1$combined))),
         a2 = list(total = mean(rowSums(calc_impacts_r$a2$aspect3)))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"],
                              function(j) j[["1"]]$mean)),
    list(a1 = list(total = mean(rowSums(calc_impacts_r$a1$combined))),
         a2 = list(total = mean(rowSums(calc_impacts_r$a2$aspect3)))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"],
                              function(j) j[["2"]]$mean)),
    list(a1 = list(total = mean(rowSums(calc_impacts_r$a1$combined))),
         a2 = list(total = mean(rowSums(calc_impacts_r$a2$aspect3)))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"],
                              function(j) j[["3"]]$mean)),
    list(a1 = list(total = mean(rowSums(calc_impacts_r$a1$combined))),
         a2 = list(total = mean(rowSums(calc_impacts_r$a2$aspect3)))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"],
                              function(j) j[["4"]]$mean)),
    list(a1 = list(total = mean(rowSums(calc_impacts_r$a1$combined))),
         a2 = list(total = mean(rowSums(calc_impacts_r$a2$aspect3)))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"],
                              function(j) j[["0"]]$sd)),
    list(a1 = list(total = sd(rowSums(calc_impacts_r$a1$combined))),
         a2 = list(total = sd(rowSums(calc_impacts_r$a2$aspect3)))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"],
                              function(j) j[["1"]]$sd)),
    list(a1 = list(total = sd(rowSums(calc_impacts_r$a1$combined))),
         a2 = list(total = sd(rowSums(calc_impacts_r$a2$aspect3)))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"],
                              function(j) j[["2"]]$sd)),
    list(a1 = list(total = sd(rowSums(calc_impacts_r$a1$combined))),
         a2 = list(total = sd(rowSums(calc_impacts_r$a2$aspect3)))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"],
                              function(j) j[["3"]]$sd)),
    list(a1 = list(total = sd(rowSums(calc_impacts_r$a1$combined))),
         a2 = list(total = sd(rowSums(calc_impacts_r$a2$aspect3)))))
  expect_equal(
    lapply(result_list$impacts,
           function(i) lapply(i[names(i) == "total"],
                              function(j) j[["4"]]$sd)),
    list(a1 = list(total = sd(rowSums(calc_impacts_r$a1$combined))),
         a2 = list(total = sd(rowSums(calc_impacts_r$a2$aspect3)))))
  cum_names <- names(result_list$impacts$a1$cumulative)
  expect_equal(cum_names,c("aspect1", "aspect2", "combined", "total"))
  expect_true(is.list(result_list$impacts$a1$cumulative))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[1:3]],
           function(i) i[["0"]]$mean[idx]),
    lapply(calc_impacts_r$a1, function(a) colMeans(a)))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[1:3]],
           function(i) i[["0"]]$sd[idx]),
    lapply(calc_impacts_r$a1, function(a) apply(a, 2, sd)))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[1:3]],
           function(i) i[["2"]]$mean[idx]),
    lapply(calc_impacts_r$a1, function(a) colMeans(a*3)))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[1:3]],
           function(i) i[["2"]]$sd[idx]),
    lapply(calc_impacts_r$a1, function(a) apply(a*3, 2, sd)))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[1:3]],
           function(i) i[["4"]]$mean[idx]),
    lapply(calc_impacts_r$a1, function(a) colMeans(a*5)))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[1:3]],
           function(i) i[["4"]]$sd[idx]),
    lapply(calc_impacts_r$a1, function(a) apply(a*5, 2, sd)))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["0"]]$mean),
    list(total = mean(rowSums(calc_impacts_r$a1$combined))))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["0"]]$sd),
    list(total = sd(rowSums(calc_impacts_r$a1$combined))))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["1"]]$mean),
    list(total = mean(rowSums(calc_impacts_r$a1$combined*2))))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["1"]]$sd),
    list(total = sd(rowSums(calc_impacts_r$a1$combined*2))))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["2"]]$mean),
    list(total = mean(rowSums(calc_impacts_r$a1$combined*3))))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["2"]]$sd),
    list(total = sd(rowSums(calc_impacts_r$a1$combined*3))))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["3"]]$mean),
    list(total = mean(rowSums(calc_impacts_r$a1$combined*4))))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["3"]]$sd),
    list(total = sd(rowSums(calc_impacts_r$a1$combined*4))))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["4"]]$mean),
    list(total = mean(rowSums(calc_impacts_r$a1$combined*5))))
  expect_equal(
    lapply(result_list$impacts$a1$cumulative[cum_names[4]],
           function(i) i[["4"]]$sd),
    list(total = sd(rowSums(calc_impacts_r$a1$combined*5))))
  expect_null(result_list$impacts$a2$cumulative)
})

test_that("collates and finalizes action results", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  template[region$get_indices()][5916:5922,] <- c(rep(0.5, 4), 0.5, 0.75, 1)
  template_vect <- template[region$get_indices()][,1]
  population_model <- bsspread::UnstructPopulation(region, growth = 1.2)
  n <- rep(0, region$get_locations())
  idx <- 5920:5922
  n[idx] <- (10:12)*5
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
                        schedule = 2:3),
    a5 = ManageControls(region, population_model, control_type = "growth",
                        control_mult = 0.7, schedule = 2:3))
  n <- actions$a3$apply(n, 2)
  n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2)
  # single replicate
  expected_collated <- lapply(actions, function(a) { # HERE
    if (a$get_label() == "control_growth") {
      n_a <- +(attr(n, a$get_label()) < 1)
    } else {
      n_a <- attr(n, a$get_label())
    }
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
  n <- actions$a5$apply(n, 4)
  expect_silent(results$collate(r = 1, tm = 4, n = n))
  expect_silent(result_list <- results$get_list())
  expect_equal(lapply(result_list$population, function(i) attributes(i)),
               lapply(result_list$population, function(i) NULL)) # no attr
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i, function(j) j[["2"]])),
               expected_collated)
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i, function(j) j[["4"]])),
               lapply(expected_collated,
                      function(i) lapply(i, function(j) j*0)))
  # multiple replicates
  n_r <- list(); n_r2 <- list()
  n[idx] <- (9:11)*5
  n <- actions$a3$apply(n, 2); n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2); n_r[[1]] <- n
  attributes(n) <- NULL
  n <- actions$a3$apply(n, 4); n <- actions$a4$apply(n, 4)
  n <- actions$a5$apply(n, 4); n_r2[[1]] <- n
  n[idx] <- (10:12)*5
  n <- actions$a3$apply(n, 2); n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2); n_r[[2]] <- n
  attributes(n) <- NULL
  n <- actions$a3$apply(n, 4); n <- actions$a4$apply(n, 4)
  n <- actions$a5$apply(n, 4); n_r2[[2]] <- n
  n[idx] <- (11:13)*5
  n <- actions$a3$apply(n, 2); n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2); n_r[[3]] <- n
  attributes(n) <- NULL
  n <- actions$a3$apply(n, 4); n <- actions$a4$apply(n, 4)
  n <- actions$a5$apply(n, 4); n_r2[[3]] <- n
  collated_r <- lapply(n_r, function(n) lapply(actions, function(a) {
    if (a$get_label() == "control_growth") {
      n_a <- +(attr(n, a$get_label()) < 1)
    } else {
      n_a <- attr(n, a$get_label())
    }
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
  expect_equal(lapply(result_list$actions[1:2],
                      function(i) lapply(i, function(j) j[["2"]]$sd)),
               lapply(collated_arrays[1:2], function(i)
                 lapply(i, function(a) apply(a, 2, sd))))
  expect_equal(lapply(result_list$actions[[3]], function(j) j[["2"]]$sd),
               list(control_growth = NULL, total = 0))
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i, function(j) j[["4"]]$mean)),
               lapply(zero_results,
                      function(i) lapply(i, function(j) j[["4"]]$mean)))
  expect_equal(lapply(result_list$actions[1:2],
                      function(i) lapply(i, function(j) j[["4"]]$sd)),
               lapply(zero_results[1:2],
                      function(i) lapply(i, function(j) j[["4"]]$sd)))
  expect_equal(lapply(result_list$actions[[3]], function(j) j[["4"]]$sd),
               list(control_growth = NULL, total = 0))
})

test_that("collates and finalizes staged action results", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  idx <- 5916:5922
  region <- bsspread::Region(template)
  template[region$get_indices()][idx,] <- c(rep(0.5, 4), 0.5, 0.75, 1)
  idx <- idx[5:7]
  template_vect <- template[region$get_indices()][,1]
  surveillance <-
    bsdesign::SpatialSurvDesign(context = bsdesign::Context("test"),
                                divisions = bsdesign::Divisions(template),
                                establish_pr = template_vect*0,
                                lambda = 1,
                                optimal = "none",
                                exist_sens = template_vect)
  # staged single replicate
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  initial_n <- rep(0, region$get_locations())
  n <- rep(0, region$get_locations())
  n[idx] <- (10:12)*5
  n <- population_model$make(initial = n)
  actions <- list(
    a3 = ManageDetection(region, population_model, surveillance, stages = 2:3,
                         schedule = 2:3),
    a4 = ManageRemovals(region, population_model, removal_pr = template_vect,
                        stages = 2:3, schedule = 2:3),
    a5 = ManageControls(region, population_model, control_type = "growth",
                        control_mult = 0.7, stages = 2:3, schedule = 2:3))
  n <- actions$a3$apply(n, 2)
  n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2)
  expect_silent(results <- ManageResults(region,
                                         population_model = population_model,
                                         actions = actions, time_steps = 4,
                                         collation_steps = 2, replicates = 1))
  expect_silent(results$collate(r = 1, tm = 2, n = n))
  expect_silent(result_list <- results$get_list())
  action_results <- lapply(result_list$actions,
                           function(i) lapply(i, function(j) j[["2"]]))
  expect_equal(lapply(action_results$a3, function(a) dim(a)),
               list(detected = c(region$get_locations(), 3), total = c(1, 3)))
  expect_equal(lapply(action_results$a3, function(a) dim(a)),
               list(detected = c(region$get_locations(), 3), total = c(1, 3)))
  expect_equal(lapply(action_results$a4, function(a) dim(a)),
               list(removed = c(region$get_locations(), 3), total = c(1, 3)))
  expect_equal(lapply(action_results$a5, function(a) length(a)),
               list(control_growth = region$get_locations(), total = 1))
  expect_equal(as.logical(action_results$a3$total > 0), c(FALSE, TRUE, TRUE))
  expect_equal(as.logical(action_results$a4$total > 0), c(FALSE, TRUE, TRUE))
  expect_equal(action_results$a5$total, 3)
  # staged multiple replicates
  n_r <- list(); n_r2 <- list()
  n <- rep(0, region$get_locations())
  n[idx] <- (9:11)*5
  n <- population_model$make(initial = n)
  n <- actions$a3$apply(n, 2); n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2); n_r[[1]] <- n
  attributes(n)[c("detected", "removed", "control_growth")] <- NULL
  n <- actions$a3$apply(n, 4); n <- actions$a4$apply(n, 4)
  n <- actions$a5$apply(n, 4); n_r2[[1]] <- n
  n <- rep(0, region$get_locations())
  n[idx] <- (10:12)*5
  n <- population_model$make(initial = n)
  n <- actions$a3$apply(n, 2); n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2); n_r[[2]] <- n
  attributes(n)[c("detected", "removed", "control_growth")] <- NULL
  n <- actions$a3$apply(n, 4); n <- actions$a4$apply(n, 4)
  n <- actions$a5$apply(n, 4); n_r2[[2]] <- n
  n <- rep(0, region$get_locations())
  n[idx] <- (11:13)*5
  n <- population_model$make(initial = n)
  n <- actions$a3$apply(n, 2); n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2); n_r[[3]] <- n
  attributes(n)[c("detected", "removed", "control_growth")] <- NULL
  n <- actions$a3$apply(n, 4); n <- actions$a4$apply(n, 4)
  n <- actions$a5$apply(n, 4); n_r2[[3]] <- n
  collated_r <- lapply(n_r, function(n) lapply(actions, function(a) {
    if (a$get_label() == "control_growth") {
      n_a <- +(attr(n, a$get_label()) < 1)
    } else {
      n_a <- as.numeric(attr(n, a$get_label()))
    }
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
  expect_silent(results$collate(r = 1, tm = 2, n = n_r[[1]]))
  expect_silent(results$collate(r = 1, tm = 4, n = n_r2[[1]]))
  expect_silent(results$collate(r = 2, tm = 2, n = n_r[[2]]))
  expect_silent(results$collate(r = 2, tm = 4, n = n_r2[[2]]))
  expect_silent(results$collate(r = 3, tm = 2, n = n_r[[3]]))
  expect_silent(results$collate(r = 3, tm = 4, n = n_r2[[3]]))
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  action_results <- lapply(result_list$actions, function(i) lapply(i, function(j) j[["2"]]))
  locs <- region$get_locations()
  expect_equal(lapply(action_results$a3, function(a) lapply(a, dim)),
               list(detected = list(mean = c(locs, 3), sd = c(locs, 3)),
                    total = list(mean = c(1, 3), sd = c(1, 3))))
  expect_equal(lapply(action_results$a4, function(a) lapply(a, dim)),
               list(removed = list(mean = c(locs, 3), sd = c(locs, 3)),
                    total = list(mean = c(1, 3), sd = c(1, 3))))
  expect_equal(lapply(action_results$a5, function(a) lapply(a, length)),
               list(control_growth = list(mean = locs),
                    total = list(mean = 1, sd = 1)))
  expect_equal(lapply(action_results$a3$total,
                      function(tot) as.logical(tot > 0)),
               list(mean = c(FALSE, TRUE, TRUE), sd = c(FALSE, TRUE, TRUE)))
  expect_equal(lapply(action_results$a4$total,
                      function(tot) as.logical(tot > 0)),
               list(mean = c(FALSE, TRUE, TRUE), sd = c(FALSE, TRUE, TRUE)))
  expect_equal(action_results$a5$total, list(mean = 3, sd = 0))
})
