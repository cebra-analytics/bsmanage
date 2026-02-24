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
    a3 = ManageDetection(region, population_model, surveillance,
                         sensitivity_type = "presence"),
    a4 = ManageRemovals(region, population_model,
                        removal_pr_type = "population"))
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
  expect_equal(lapply(result_list$impacts, function(i) attr(i, "unit")),
               list(a1 = "$", a2 = "$"))
  # include individual numbers
  actions <- list(
    a3 = ManageDetection(region, population_model, surveillance,
                         sensitivity_type = "individual"),
    a4 = ManageRemovals(region, population_model,
                        removal_pr_type = "individual"))
  results <- ManageResults(region, population_model = population_model,
                           impacts = impacts, actions = actions,
                           time_steps = 10, collation_steps = 2,
                           replicates = 1) # silent
  result_list <- results$get_list()
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i, length)),
               list(a3 = list(detected = 6, total = 11, number = 2),
                    a4 = list(removed = 6, total = 11, number = 2)))
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i, function(j) sapply(j, length))),
               list(a3 = list(detected = collated, total = totals,
                              number = c(detected = 6, total = 11)),
                    a4 = list(removed = collated, total = totals,
                              number = c(removed = 6, total = 11))))
  expect_equal(lapply(result_list$actions,
                      function(i) lapply(i$number,
                                         function(j) sapply(j, length))),
               list(a3 = list(detected = collated, total = totals),
                    a4 = list(removed = collated, total = totals)))
  # with action costs and monetary-only impacts
  control_cost1 <- 4; attr(control_cost1, "unit") <- "$"
  control_cost2 <- 5; attr(control_cost2, "unit") <- "$"
  removal_cost <- 6; attr(removal_cost, "unit") <- "$"
  actions <- list(
    a3 = ManageDetection(region, population_model, surveillance,
                         sensitivity_type = "presence", surv_cost = 3),
    a4 = ManageControls(region, population_model, control_type = "growth",
                        control_mult = 0.4, control_cost = control_cost1),
    a5 = ManageControls(region, population_model, control_type = "spread",
                        control_mult = 0.5, control_cost = control_cost2),
    a6 = ManageRemovals(region, population_model,
                        removal_pr_type = "population",
                        removal_cost = removal_cost))
  expect_silent(
    results <- ManageResults(region, population_model = population_model,
                             impacts = impacts[1], actions = actions,
                             time_steps = 10, collation_steps = 2,
                             replicates = 1))
  expect_silent(result_list <- results$get_list())
  expect_named(result_list, c("population", "total_pop", "occupancy",
                              "total_occup", "area", "impacts", "actions",
                              "cost"))
  expect_equal(sapply(result_list$cost$combined, length), collated)
  expect_equal(sapply(result_list$cost$cumulative$combined, length), collated)
  expect_equal(sapply(result_list$cost$total, length), totals)
  expect_equal(sapply(result_list$cost$cumulative$total, length), totals)
  expect_equal(attr(result_list$cost, "unit"), "$")
  expect_named(result_list$actions,
               c("a3", "a4", "a5", "a6", "cost"))
  expect_equal(sapply(result_list$actions$cost$combined, length), collated)
  expect_equal(sapply(result_list$actions$cost$cumulative$combined, length),
               collated)
  expect_equal(sapply(result_list$actions$cost$total, length), totals)
  expect_equal(sapply(result_list$actions$cost$cumulative$total, length),
               totals)
  expect_equal(attr(result_list$actions$cost, "unit"), "$")
  expect_equal(lapply(result_list$actions[1:4],
                      function(i) lapply(i, length)),
               lapply(actions, function(a) {
                 l = list(6, total = 11, cost = 3)
                 names(l)[1] <- a$get_label(include_id = FALSE)
                 l
               }))
  expect_equal(lapply(result_list$actions[1:4],
                      function(i) lapply(i$cost, length)),
               lapply(actions, function(a) {
                 l = list(6, total = 11, cumulative = 2)
                 names(l)[1] <- a$get_label(include_id = FALSE)
                 l
               }))
  expect_equal(lapply(result_list$actions[1:4],
                      function(i) lapply(i$cost$cumulative, length)),
               lapply(actions, function(a) {
                 l = list(6, total = 11)
                 names(l)[1] <- a$get_label(include_id = FALSE)
                 l
               }))
  expect_equal(lapply(result_list$actions[1:4],
                      function(i) lapply(i[-which(names(i) == "cost")],
                                         function(j) sapply(j, length))),
               lapply(actions, function(a) {
                 l = list(collated, total = totals)
                 names(l)[1] <- a$get_label(include_id = FALSE)
                 l
               }))
  expect_equal(lapply(result_list$actions[1:4],
                      function(i) {
                        lapply(i$cost[-which(names(i$cost) == "cumulative")],
                               function(j) sapply(j, length))
                      }),
               lapply(actions, function(a) {
                 l = list(collated, total = totals)
                 names(l)[1] <- a$get_label(include_id = FALSE)
                 l
               }))
  expect_equal(lapply(result_list$actions[1:4],
                      function(i) lapply(i$cost$cumulative,
                                         function(j) sapply(j, length))),
               lapply(actions, function(a) {
                 l = list(collated, total = totals)
                 names(l)[1] <- a$get_label(include_id = FALSE)
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
  expect_equal(lapply(result_list$impacts, function(i) attr(i, "unit")),
               list(a1 = "$", a2 = "$"))
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
  expect_equal(lapply(result_list$impacts, function(i) attr(i, "unit")),
               list(a1 = "$", a2 = "$"))
})

test_that("collates and finalizes spatially implicit impact results", {
  context <- list(bsimpact::Context("My species",
                                    impact_scope = c("aspect1", "aspect2")),
                  bsimpact::Context("My species",
                                    impact_scope = "aspect3",
                                    valuation_type = "non-monetary"))
  aspects <- list(aspect1 = "aspect1", aspect2 = "aspect2",
                  aspect3 = "aspect3")
  loss_rates <- c(aspect1 = 0.1, aspect2 = 0.2, aspect3 = 0.3)
  region <- bsspread::Region()
  population_model <- bsspread::UnstructPopulation(region, growth = 1.2)
  n <- 8
  attr(n, "spread_area") <- 10000
  incursion <- bsimpact::Incursion(8, region, type = "area", multiplier = 0.1)
  impact_layers <- list(aspect1 = 100, aspect2 = 200, aspect3 = 300)
  tmc_list <- as.list(1:5)
  names(tmc_list) <- as.character(0:4)
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
  calc_impacts <- lapply(impacts,
                         function(impacts_i)
                           attr(impacts_i$calculate(n), "impacts"))
  # single replicate
  expect_silent(
    results <- ManageResults(region, population_model = population_model,
                             impacts = impacts, time_steps = 4,
                             collation_steps = 2, replicates = 1))
  for (tm in 0:4) {
    results$collate(r = 1, tm = tm, n = n, calc_impacts)
  }
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  expect_equal(
    lapply(result_list$impacts, names),
    list(a1 = c("aspect1", "aspect2", "combined", "cumulative"),
         a2 = "aspect3"))
  expect_equal(
    result_list$impacts$a1[1:3],
    lapply(calc_impacts$a1, function(i) lapply(tmc_list, function(tmc) i)))
  expect_equal(
    result_list$impacts$a1$cumulative,
    lapply(calc_impacts$a1, function(i) lapply(tmc_list, function(tmc) i*tmc)))
  expect_equal(attr(result_list$impacts$a1, "unit"), "$")
  expect_equal(
    result_list$impacts$a2[1],
    lapply(calc_impacts$a2, function(i) lapply(tmc_list, function(tmc) i)))
  expect_equal(attr(result_list$impacts$a2, "unit"), "$")
  # multiple replicates
  expect_silent(
    results <- ManageResults(region, population_model = population_model,
                             impacts = impacts, time_steps = 4,
                             collation_steps = 2, replicates = 3))
  expect_silent(result_list_0 <- results$get_list())
  calc_impacts <- list()
  n <- 6; attr(n,"spread_area") <- 8000
  calc_impacts[[1]] <-
    lapply(impacts,
           function(impacts_i) attr(impacts_i$calculate(n), "impacts"))
  n <- 8; attr(n,"spread_area") <- 10000
  calc_impacts[[2]] <-
    lapply(impacts,
           function(impacts_i) attr(impacts_i$calculate(n), "impacts"))
  n <- 10; attr(n,"spread_area") <- 12000
  calc_impacts[[3]] <-
    lapply(impacts,
           function(impacts_i) attr(impacts_i$calculate(n), "impacts"))
  for (i in 1:3) {
    for (tm in 0:4) {
      results$collate(r = i, tm = tm, n = n, calc_impacts[[i]])
    }
  }
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  expect_collated <- result_list_0$impacts[[1]][[1]]
  generate_summary <- function(rep_list, summary_list) {
    for (i in 1:length(summary_list)) {
      nrow <- length(summary_list[[i]][[1]])
      rep_values <- matrix(sapply(rep_list, function(l) l[[i]]), nrow = nrow)
      summary_list[[i]]$mean <- rowMeans(rep_values)
      if (any(sapply(summary_list, function(sl) "sd" %in% names(sl)))) {
        summary_list[[i]]$sd <- apply(rep_values, 1, sd)
      }
    }
    return(summary_list)
  }
  expect_equal(
    lapply(result_list$impacts, names),
    list(a1 = c("aspect1", "aspect2", "combined", "cumulative"),
         a2 = "aspect3"))
  expect_equal(
    result_list$impacts$a1$aspect1,
    generate_summary(
      lapply(calc_impacts,
             function(i) lapply(tmc_list, function(tmc) i$a1$aspect1)),
      expect_collated))
  expect_equal(
    result_list$impacts$a1$aspect2,
    generate_summary(
      lapply(calc_impacts,
             function(i) lapply(tmc_list, function(tmc) i$a1$aspect2)),
      expect_collated))
  expect_equal(
    result_list$impacts$a1$combined,
    generate_summary(
      lapply(calc_impacts,
             function(i) lapply(tmc_list, function(tmc) i$a1$combined)),
      expect_collated))
  expect_equal(
    result_list$impacts$a1$cumulative$aspect1,
    generate_summary(
      lapply(calc_impacts,
             function(i) lapply(tmc_list, function(tmc) i$a1$aspect1*tmc)),
      expect_collated))
  expect_equal(
    result_list$impacts$a1$cumulative$aspect2,
    generate_summary(
      lapply(calc_impacts,
             function(i) lapply(tmc_list, function(tmc) i$a1$aspect2*tmc)),
      expect_collated))
  expect_equal(
    result_list$impacts$a1$cumulative$combined,
    generate_summary(
      lapply(calc_impacts,
             function(i) lapply(tmc_list, function(tmc) i$a1$combined*tmc)),
      expect_collated))
  expect_equal(attr(result_list$impacts$a1, "unit"), "$")
  expect_equal(
    result_list$impacts$a2$aspect3,
    generate_summary(
      lapply(calc_impacts,
             function(i) lapply(tmc_list, function(tmc) i$a2$aspect3)),
      expect_collated))
  expect_equal(attr(result_list$impacts$a2, "unit"), "$")
})

test_that("collates and finalizes action results with costs", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  idx <- 5916:5922
  region <- bsspread::Region(template)
  template[region$get_indices()][idx,] <- c(rep(0.5, 4), 0.5, 0.75, 1)
  template_vect <- template[region$get_indices()][,1]
  population_model <- bsspread::UnstructPopulation(region, growth = 1.2)
  n <- rep(0, region$get_locations())
  idx <- idx[5:7]
  n_list <- list(n, n, n)
  n_list[[1]][idx] <- (10:12)*5
  n_list[[2]][idx] <- c(9, 0, 11)*5
  n_list[[3]][idx] <- c(0, 12, 13)*5
  context <- list(bsimpact::Context("My species",
                                    impact_scope = c("aspect1", "aspect2")))
  incursion <- bsimpact::Incursion(template*0, region, multiplier = 0.1)
  aspects <- list(aspect1 = "aspect1", aspect2 = "aspect2")
  impact_layers <- list(aspect1 = 100*(template < 0.9),
                        aspect2 = 200*(template > 0.6))
  impact_layer_vals <- lapply(impact_layers,
                              function(l) l[region$get_indices()][idx,])
  loss_rates <- c(aspect1 = 0.1, aspect2 = 0.2)
  impact_names <- list(a1 = "a1", a2 = "a2")
  impacts <- list(
    a1 = ManageImpacts(
      bsimpact::ValueImpacts(context[[1]], region, incursion,
                             impact_layers[1:2],
                             loss_rates = loss_rates[1:2]),
      population_model))
  surveillance <-
    bsdesign::SpatialSurvDesign(context = bsdesign::Context("test"),
                                divisions = bsdesign::Divisions(template),
                                establish_pr = template_vect*0,
                                lambda = 1,
                                optimal = "none",
                                exist_sens = template_vect)
  removal_cost <- 4; attr(removal_cost, "unit") <- "$"
  control_cost <- 5; attr(control_cost, "unit") <- "$"
  actions <- list(
    a3 = ManageDetection(region, population_model, surveillance,
                         schedule = 2:3, surv_cost = 3),
    a4 = ManageRemovals(region, population_model, removal_pr = template_vect,
                        schedule = 2:3, removal_cost = removal_cost),
    a5 = ManageControls(region, population_model, control_type = "growth",
                        control_mult = 0.7, schedule = 2:3,
                        control_cost = control_cost))
  # single replicate
  expect_silent(
    results <- ManageResults(region, population_model = population_model,
                             impacts = impacts, actions = actions,
                             time_steps = 4, collation_steps = 2,
                             replicates = 1))
  expect_silent(action_results_0 <- results$get_list()$actions)
  action_results_0$combined_cost_plus_impacts <- action_results_0$cost$combined
  action_results_0$combined_cum_cost_plus_impacts <-
    action_results_0$cost$combined
  action_results_0$total_cost_plus_impacts <- action_results_0$cost$total
  action_results_0$total_cum_cost_plus_impacts <- action_results_0$cost$total
  expect_list <- list(action_results_0, action_results_0, action_results_0)
  set.seed(1234)
  for (r in 1:3) { # collect expected for single and multiple replicates
    n <- n_list[[r]]
    for (tm in 0:4) {
      tmc <- as.character(tm)
      tmc_prev <- as.character(max(tm - 1, 0))
      calc_impacts <- lapply(impacts, function(impacts_i)
        attr(impacts_i$calculate(n), "impacts"))
      for (a in c("a3", "a4", "a5")) {
        n <- actions[[a]]$clear_attributes(n) # clear
      }
      for (a in c("a3", "a4", "a5")) {
        n <- actions[[a]]$apply(n, tm)
      }
      if (tm %in% c(0, 2, 4)) {
        expect_list[[r]]$a3$detected[[tmc]] <- attr(n, "a3_detected")
        expect_list[[r]]$a4$removed[[tmc]] <- attr(n, "a4_removed")
        expect_list[[r]]$a5$control_growth[[tmc]] <-
          +(attr(n, "a5_control_growth") < 1)
        expect_list[[r]]$a4$cost$removed[[tmc]] <-
          as.numeric(attr(n, "a4_removal_cost"))
        expect_list[[r]]$a3$cost$detected[[tmc]] <-
          as.numeric(attr(n, "a3_surv_cost"))
        expect_list[[r]]$a5$cost$control_growth[[tmc]] <-
          as.numeric(attr(n, "a5_control_growth_cost"))
      }
      expect_list[[r]]$a3$total[[tmc]] <- sum(attr(n, "a3_detected"))
      expect_list[[r]]$a4$total[[tmc]] <- sum(attr(n, "a4_removed"))
      expect_list[[r]]$a5$total[[tmc]] <- sum(attr(n, "a5_control_growth") < 1)
      expect_list[[r]]$a3$cost$total[[tmc]] <- sum(attr(n, "a3_surv_cost"))
      expect_list[[r]]$a4$cost$total[[tmc]] <- sum(attr(n, "a4_removal_cost"))
      expect_list[[r]]$a5$cost$total[[tmc]] <-
        sum(attr(n, "a5_control_growth_cost"))
      expect_list[[r]]$a3$cost$cumulative$total[[tmc]] <-
        (expect_list[[r]]$a3$cost$cumulative$total[[tmc_prev]] +
           sum(attr(n, "a3_surv_cost")))
      expect_list[[r]]$a4$cost$cumulative$total[[tmc]] <-
        (expect_list[[r]]$a4$cost$cumulative$total[[tmc_prev]] +
           sum(attr(n, "a4_removal_cost")))
      expect_list[[r]]$a5$cost$cumulative$total[[tmc]] <-
        (expect_list[[r]]$a5$cost$cumulative$total[[tmc_prev]] +
           sum(attr(n, "a5_control_growth_cost")))
      combined_cost <-
        as.numeric(attr(n, "a3_surv_cost") + attr(n, "a4_removal_cost") +
                     attr(n, "a5_control_growth_cost"))
      combined_cost_plus_impacts <- combined_cost + calc_impacts$a1$combined
      if (tm %in% c(0, 2, 4)) {
        expect_list[[r]]$cost$combined[[tmc]] <- combined_cost
        expect_list[[r]]$combined_cost_plus_impacts[[tmc]] <-
          combined_cost_plus_impacts
      }
      expect_list[[r]]$cost$total[[tmc]] <- sum(combined_cost)
      expect_list[[r]]$cost$cumulative$total[[tmc]] <-
        (expect_list[[r]]$cost$cumulative$total[[tmc_prev]] +
           sum(combined_cost))
      expect_list[[r]]$total_cost_plus_impacts[[tmc]] <-
        sum(combined_cost_plus_impacts)
      expect_list[[r]]$total_cum_cost_plus_impacts[[tmc]] <-
        (expect_list[[r]]$total_cum_cost_plus_impacts[[tmc_prev]] +
           sum(combined_cost_plus_impacts))
      if (tm %in% c(0, 2, 4)) {
        tmc_prev <- as.character(tm)
      } else {
        tmc <- as.character(tm + 1)
      }
      expect_list[[r]]$a3$cost$cumulative$detected[[tmc]] <-
        (expect_list[[r]]$a3$cost$cumulative$detected[[tmc_prev]] +
           as.numeric(attr(n, "a3_surv_cost")))
      expect_list[[r]]$a4$cost$cumulative$removed[[tmc]] <-
        (expect_list[[r]]$a4$cost$cumulative$removed[[tmc_prev]] +
           as.numeric(attr(n, "a4_removal_cost")))
      expect_list[[r]]$a5$cost$cumulative$control_growth[[tmc]] <-
        (expect_list[[r]]$a5$cost$cumulative$control_growth[[tmc_prev]] +
           as.numeric(attr(n, "a5_control_growth_cost")))
      expect_list[[r]]$cost$cumulative$combined[[tmc]] <-
        expect_list[[r]]$cost$cumulative$combined[[tmc_prev]] + combined_cost
      expect_list[[r]]$combined_cum_cost_plus_impacts[[tmc]] <-
        (expect_list[[r]]$combined_cum_cost_plus_impacts[[tmc_prev]] +
           combined_cost_plus_impacts)
      if (r == 1) {
        results$collate(r = 1, tm = tm, n = n, calc_impacts)
      }
    }
  }
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  expect_equal(lapply(result_list$population, function(i) attributes(i)),
               lapply(result_list$population, function(i) NULL))
  expect_equal(result_list$actions$a3[c("detected", "total")],
               expect_list[[1]]$a3[c("detected", "total")])
  expect_equal(result_list$actions$a3$cost[c("detected", "total")],
               expect_list[[1]]$a3$cost[c("detected", "total")])
  expect_equal(result_list$actions$a3$cost$cumulative[c("detected", "total")],
               expect_list[[1]]$a3$cost$cumulative[c("detected", "total")])
  expect_equal(result_list$actions$a4[c("removed", "total")],
               expect_list[[1]]$a4[c("removed", "total")])
  expect_equal(result_list$actions$a4$cost[c("removed", "total")],
               expect_list[[1]]$a4$cost[c("removed", "total")])
  expect_equal(result_list$actions$a4$cost$cumulative[c("removed", "total")],
               expect_list[[1]]$a4$cost$cumulative[c("removed", "total")])
  expect_equal(result_list$actions$a5[c("control_growth", "total")],
               expect_list[[1]]$a5[c("control_growth", "total")])
  expect_equal(result_list$actions$a5$cost[c("control_growth", "total")],
               expect_list[[1]]$a5$cost[c("control_growth", "total")])
  expect_equal(result_list$actions$a5$cost$cumulative[c("control_growth",
                                                        "total")],
               expect_list[[1]]$a5$cost$cumulative[c("control_growth",
                                                     "total")])
  expect_equal(result_list$actions$cost$combined,
               expect_list[[1]]$cost$combined)
  expect_equal(result_list$actions$cost$cumulative$combined,
               expect_list[[1]]$cost$cumulative$combined)
  expect_equal(result_list$actions$cost$total,
               expect_list[[1]]$cost$total)
  expect_equal(result_list$actions$cost$cumulative$total,
               expect_list[[1]]$cost$cumulative$total)
  expect_equal(attr(result_list$actions$cost, "unit"), "$")
  expect_equal(result_list$cost$combined,
               expect_list[[1]]$combined_cost_plus_impacts)
  expect_equal(result_list$cost$cumulative$combined,
               expect_list[[1]]$combined_cum_cost_plus_impacts)
  expect_equal(result_list$cost$total,
               expect_list[[1]]$total_cost_plus_impacts)
  expect_equal(result_list$cost$cumulative$total,
               expect_list[[1]]$total_cum_cost_plus_impacts)
  expect_equal(attr(result_list$cost, "unit"), "$")
  # multiple replicates
  expect_silent(
    results <- ManageResults(region, population_model = population_model,
                             impacts = impacts, actions = actions,
                             time_steps = 4, collation_steps = 2,
                             replicates = 3))
  expect_silent(result_list_0 <- results$get_list())
  set.seed(1234)
  for (r in 1:3) {
    n <- n_list[[r]]
    for (tm in 0:4) {
      calc_impacts <- lapply(impacts, function(impacts_i)
        attr(impacts_i$calculate(n), "impacts"))
      for (j in c("a3", "a4", "a5")) {
        n <- actions[[j]]$clear_attributes(n) # clear
      }
      for (j in c("a3", "a4", "a5")) {
        n <- actions[[j]]$apply(n, tm)
      }
      results$collate(r = r, tm = tm, n = n, calc_impacts)
    }
  }
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  expect_collated <- result_list_0$actions[[1]][[1]]
  expect_collated_no_sd <- lapply(expect_collated, function(ec) ec["mean"])
  expect_totals <- result_list_0$actions[[1]]$total
  generate_summary <- function(rep_list, summary_list) {
    for (i in 1:length(summary_list)) {
      nrow <- length(summary_list[[i]][[1]])
      rep_values <- matrix(sapply(rep_list, function(l) l[[i]]), nrow = nrow)
      summary_list[[i]]$mean <- rowMeans(rep_values)
      if (any(sapply(summary_list, function(sl) "sd" %in% names(sl)))) {
        summary_list[[i]]$sd <- apply(rep_values, 1, sd)
      }
    }
    return(summary_list)
  }
  expect_equal(
    lapply(result_list$population,
           function(i) lapply(i, function(j) attributes(j))),
    lapply(expect_collated, function(i) lapply(i, function(j) NULL)))
  expect_equal(
    result_list$actions$a3$detected,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a3$detected),
                     expect_collated))
  expect_equal(
    result_list$actions$a3$total,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a3$total),
                     expect_totals))
  expect_equal(
    result_list$actions$a3$cost$detected,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a3$cost$detected),
                     expect_collated))
  expect_equal(
    result_list$actions$a3$cost$total,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a3$cost$total),
                     expect_totals))
  expect_equal(
    result_list$actions$a3$cost$cumulative$detected,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$a3$cost$cumulative$detected),
      expect_collated))
  expect_equal(
    result_list$actions$a3$cost$cumulative$total,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$a3$cost$cumulative$total),
      expect_totals))
  expect_equal(
    result_list$actions$a4$removed,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a4$removed),
                     expect_collated))
  expect_equal(
    result_list$actions$a4$total,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a4$total),
                     expect_totals))
  expect_equal(
    result_list$actions$a4$cost$removed,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a4$cost$removed),
                     expect_collated))
  expect_equal(
    result_list$actions$a4$cost$total,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a4$cost$total),
                     expect_totals))
  expect_equal(
    result_list$actions$a4$cost$cumulative$removed,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$a4$cost$cumulative$removed),
      expect_collated))
  expect_equal(
    result_list$actions$a4$cost$cumulative$total,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$a4$cost$cumulative$total),
      expect_totals))
  expect_equal(
    result_list$actions$a5$control_growth,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a5$control_growth),
                     expect_collated_no_sd))
  expect_equal(
    result_list$actions$a5$total,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a5$total),
                     expect_totals))
  expect_equal(
    result_list$actions$a5$cost$control_growth,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$a5$cost$control_growth),
      expect_collated))
  expect_equal(
    result_list$actions$a5$cost$total,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a5$cost$total),
                     expect_totals))
  expect_equal(
    result_list$actions$a5$cost$cumulative$control_growth,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$a5$cost$cumulative$control_growth),
      expect_collated))
  expect_equal(
    result_list$actions$a5$cost$cumulative$total,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$a5$cost$cumulative$total),
      expect_totals))
  expect_equal(
    result_list$actions$cost$combined,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$cost$combined),
                     expect_collated))
  expect_equal(
    result_list$actions$cost$cumulative$combined,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$cost$cumulative$combined),
      expect_collated))
  expect_equal(
    result_list$actions$cost$total,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$cost$total),
                     expect_totals))
  expect_equal(
    result_list$actions$cost$cumulative$total,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$cost$cumulative$total),
      expect_totals))
  expect_equal(attr(result_list$actions$cost, "unit"), "$")
  expect_equal(
    result_list$cost$combined,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$combined_cost_plus_impacts),
      expect_collated))
  expect_equal(
    result_list$cost$cumulative$combined,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$combined_cum_cost_plus_impacts),
      expect_collated))
  expect_equal(
    result_list$cost$total,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$total_cost_plus_impacts),
      expect_totals))
  expect_equal(
    result_list$cost$cumulative$total,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$total_cum_cost_plus_impacts),
      expect_totals))
  expect_equal(attr(result_list$cost, "unit"), "$")
})

test_that("collates and finalizes spatially implicit results with costs", {
  region <- bsspread::Region()
  population_model <- bsspread::UnstructPopulation(region, growth = 1.2)
  n_list <- list()
  n_list[[1]] <- 60; attr(n_list[[1]], "spread_area") <- 8000
  n_list[[2]] <- 80; attr(n_list[[2]], "spread_area") <- 10000
  n_list[[3]] <- 100; attr(n_list[[3]],"spread_area") <- 12000
  context <- list(bsimpact::Context("My species",
                                    impact_scope = c("aspect1", "aspect2")))
  incursion <- bsimpact::Incursion(0, region, type = "area")
  aspects <- list(aspect1 = "aspect1", aspect2 = "aspect2")
  impact_layers <- list(aspect1 = 0.01, aspect2 = 0.02)
  loss_rates <- c(aspect1 = 0.1, aspect2 = 0.2)
  impact_names <- list(a1 = "a1", a2 = "a2")
  impacts <- list(
    a1 = ManageImpacts(
      bsimpact::ValueImpacts(context[[1]], region, incursion,
                             impact_layers, loss_rates = loss_rates),
      population_model))

  surveillance <-
    bsdesign::SpatialSurvDesign(context = bsdesign::Context("test"),
                                divisions = bsdesign::Divisions(matrix(1)),
                                establish_pr = 0, lambda = 1,
                                optimal = "none",
                                exist_sens = 0.6)
  removal_cost <- 4; attr(removal_cost, "unit") <- "$"
  control_cost <- 5; attr(control_cost, "unit") <- "$"
  actions <- list(
    a3 = ManageDetection(region, population_model, surveillance,
                         schedule = 2:3, surv_cost = 3),
    a4 = ManageRemovals(region, population_model, removal_pr = 0.3,
                        schedule = 2:3, removal_cost = removal_cost),
    a5 = ManageControls(region, population_model, control_type = "growth",
                        control_mult = 0.9, schedule = 2:3,
                        control_cost = control_cost))
  # single replicate
  expect_silent(
    results <- ManageResults(region, population_model = population_model,
                             impacts = impacts, actions = actions,
                             time_steps = 4, collation_steps = 2,
                             replicates = 1))
  expect_silent(action_results_0 <- results$get_list()$actions)
  action_results_0$combined_cost_plus_impacts <- action_results_0$cost$combined
  action_results_0$combined_cum_cost_plus_impacts <-
    action_results_0$cost$combined
  expect_list <- list(action_results_0, action_results_0, action_results_0)
  set.seed(1234)
  for (r in 1:3) { # collect expected for single and multiple replicates
    n <- n_list[[r]]
    for (tm in 0:4) {
      tmc <- as.character(tm)
      tmc_prev <- as.character(max(tm - 1, 0))
      calc_impacts <- lapply(impacts, function(impacts_i)
        attr(impacts_i$calculate(n), "impacts"))
      for (a in c("a3", "a4", "a5")) {
        n <- actions[[a]]$clear_attributes(n) # clear
      }
      for (a in c("a3", "a4", "a5")) {
        n <- actions[[a]]$apply(n, tm)
      }
      expect_list[[r]]$a3$detected[[tmc]] <- attr(n, "a3_detected")
      expect_list[[r]]$a4$removed[[tmc]] <- attr(n, "a4_removed")
      expect_list[[r]]$a5$control_growth[[tmc]] <-
        +(attr(n, "a5_control_growth") < 1)
      expect_list[[r]]$a4$cost$removed[[tmc]] <-
        as.numeric(attr(n, "a4_removal_cost"))
      expect_list[[r]]$a3$cost$detected[[tmc]] <-
        as.numeric(attr(n, "a3_surv_cost"))
      expect_list[[r]]$a5$cost$control_growth[[tmc]] <-
        as.numeric(attr(n, "a5_control_growth_cost"))
      combined_cost <- (as.numeric(attr(n, "a3_surv_cost") +
                                     attr(n, "a4_removal_cost") +
                                     attr(n, "a5_control_growth_cost")))
      combined_cost_plus_impacts <- combined_cost + calc_impacts$a1$combined
      expect_list[[r]]$cost$combined[[tmc]] <- combined_cost
      expect_list[[r]]$combined_cost_plus_impacts[[tmc]] <-
        combined_cost_plus_impacts
      expect_list[[r]]$a3$cost$cumulative$detected[[tmc]] <-
        (expect_list[[r]]$a3$cost$cumulative$detected[[tmc_prev]] +
           as.numeric(attr(n, "a3_surv_cost")))
      expect_list[[r]]$a4$cost$cumulative$removed[[tmc]] <-
        (expect_list[[r]]$a4$cost$cumulative$removed[[tmc_prev]] +
           as.numeric(attr(n, "a4_removal_cost")))
      expect_list[[r]]$a5$cost$cumulative$control_growth[[tmc]] <-
        (expect_list[[r]]$a5$cost$cumulative$control_growth[[tmc_prev]] +
           as.numeric(attr(n, "a5_control_growth_cost")))
      expect_list[[r]]$cost$cumulative$combined[[tmc]] <-
        (expect_list[[r]]$cost$cumulative$combined[[tmc_prev]] + combined_cost)
      expect_list[[r]]$combined_cum_cost_plus_impacts[[tmc]] <-
        (expect_list[[r]]$combined_cum_cost_plus_impacts[[tmc_prev]] +
           combined_cost_plus_impacts)
      if (r == 1) {
        results$collate(r = 1, tm = tm, n = n, calc_impacts)
      }
    }
  }
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  expect_equal(result_list$actions$a3, expect_list[[1]]$a3)
  expect_equal(result_list$actions$a4, expect_list[[1]]$a4)
  expect_equal(result_list$actions$a5, expect_list[[1]]$a5)
  expect_equal(result_list$actions$cost, expect_list[[1]]$cost)
  expect_equal(result_list$cost$combined,
               expect_list[[1]]$combined_cost_plus_impacts)
  expect_equal(result_list$cost$cumulative$combined,
               expect_list[[1]]$combined_cum_cost_plus_impacts)
  # multiple replicates
  expect_silent(
    results <- ManageResults(region, population_model = population_model,
                             impacts = impacts, actions = actions,
                             time_steps = 4, collation_steps = 2,
                             replicates = 3))
  expect_silent(result_list_0 <- results$get_list())
  set.seed(1234)
  for (r in 1:3) {
    n <- n_list[[r]]
    for (tm in 0:4) {
      calc_impacts <- lapply(impacts, function(impacts_i)
        attr(impacts_i$calculate(n), "impacts"))
      for (j in c("a3", "a4", "a5")) {
        n <- actions[[j]]$clear_attributes(n) # clear
      }
      for (j in c("a3", "a4", "a5")) {
        n <- actions[[j]]$apply(n, tm)
      }
      results$collate(r = r, tm = tm, n = n, calc_impacts)
    }
  }
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  expect_collated <- result_list_0$actions[[1]][[1]]
  expect_collated_no_sd <- lapply(expect_collated, function(ec) ec["mean"])
  generate_summary <- function(rep_list, summary_list) {
    for (i in 1:length(summary_list)) {
      nrow <- length(summary_list[[i]][[1]])
      rep_values <- matrix(sapply(rep_list, function(l) l[[i]]), nrow = nrow)
      summary_list[[i]]$mean <- rowMeans(rep_values)
      if (any(sapply(summary_list, function(sl) "sd" %in% names(sl)))) {
        summary_list[[i]]$sd <- apply(rep_values, 1, sd)
      }
    }
    return(summary_list)
  }
  expect_equal(
    result_list$actions$a3$detected,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a3$detected),
                     expect_collated))
  expect_equal(
    result_list$actions$a3$cost$detected,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a3$cost$detected),
                     expect_collated))
  expect_equal(
    result_list$actions$a3$cost$cumulative$detected,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$a3$cost$cumulative$detected),
      expect_collated))
  expect_equal(
    result_list$actions$a4$removed,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a4$removed),
                     expect_collated))
  expect_equal(
    result_list$actions$a4$cost$removed,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a4$cost$removed),
                     expect_collated))
  expect_equal(
    result_list$actions$a4$cost$cumulative$removed,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$a4$cost$cumulative$removed),
      expect_collated))
  expect_equal(
    result_list$actions$a5$control_growth,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$a5$control_growth),
                     expect_collated_no_sd))
  expect_equal(
    result_list$actions$a5$cost$control_growth,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$a5$cost$control_growth),
      expect_collated))
  expect_equal(
    result_list$actions$a5$cost$cumulative$control_growth,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$a5$cost$cumulative$control_growth),
      expect_collated))
  expect_equal(
    result_list$actions$cost$combined,
    generate_summary(lapply(as.list(1:3),
                            function(i) expect_list[[i]]$cost$combined),
                     expect_collated))
  expect_equal(
    result_list$actions$cost$cumulative$combined,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$cost$cumulative$combined),
      expect_collated))
  expect_equal(attr(result_list$actions$cost, "unit"), "$")
  expect_equal(
    result_list$cost$combined,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$combined_cost_plus_impacts),
      expect_collated))
  expect_equal(
    result_list$cost$cumulative$combined,
    generate_summary(
      lapply(as.list(1:3),
             function(i) expect_list[[i]]$combined_cum_cost_plus_impacts),
      expect_collated))
  expect_equal(attr(result_list$cost, "unit"), "$")
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
  expect_silent(results <- ManageResults(region,
                                         population_model = population_model,
                                         actions = actions, time_steps = 4,
                                         collation_steps = 2, replicates = 1))
  set.seed(1234)
  n <- actions$a3$apply(n, 2)
  n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2)
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
  expect_equal(action_results$a5$total, sum(action_results$a5$control_growth))
  # staged multiple replicates
  set.seed(1234)
  n_r <- list(); n_r2 <- list()
  n <- rep(0, region$get_locations())
  n[idx] <- (9:11)*5
  n <- population_model$make(initial = n)
  n <- actions$a3$apply(n, 2); n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2); n_r[[1]] <- n
  attributes(n)[c("a3_detected", "undetected", "a4_removed",
                  "a5_control_growth")] <- NULL
  n <- actions$a3$apply(n, 4); n <- actions$a4$apply(n, 4)
  n <- actions$a5$apply(n, 4); n_r2[[1]] <- n
  n <- rep(0, region$get_locations())
  n[idx] <- (10:12)*5
  n <- population_model$make(initial = n)
  n <- actions$a3$apply(n, 2); n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2); n_r[[2]] <- n
  attributes(n)[c("a3_detected", "undetected", "a4_removed",
                  "a5_control_growth")] <- NULL
  n <- actions$a3$apply(n, 4); n <- actions$a4$apply(n, 4)
  n <- actions$a5$apply(n, 4); n_r2[[2]] <- n
  n <- rep(0, region$get_locations())
  n[idx] <- (11:13)*5
  n <- population_model$make(initial = n)
  n <- actions$a3$apply(n, 2); n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2); n_r[[3]] <- n
  attributes(n)[c("a3_detected", "undetected", "a4_removed",
                  "a5_control_growth")] <- NULL
  n <- actions$a3$apply(n, 4); n <- actions$a4$apply(n, 4)
  n <- actions$a5$apply(n, 4); n_r2[[3]] <- n
  collated_r <- lapply(n_r, function(n) lapply(actions, function(a) {
    if (a$get_label(include_id = FALSE) == "control_growth") {
      n_a <- +(attr(n, a$get_label()) < 1)
    } else {
      n_a <- as.numeric(attr(n, a$get_label()))
    }
    collated <- list(n_a, total = sum(n_a))
    names(collated)[1] <- a$get_label(include_id = FALSE)
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
  action_results <- lapply(result_list$actions,
                           function(i) lapply(i, function(j) j[["2"]]))
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
  expect_equal(action_results$a5$total,
               list(mean = sum(action_results$a5$control_growth$mean), sd = 0))
})

test_that("collates and finalizes spatially implicit staged action results", {
  surveillance <-
    bsdesign::SpatialSurvDesign(context = bsdesign::Context("test"),
                                divisions = bsdesign::Divisions(data.frame(1)),
                                establish_pr = 0,
                                lambda = 1,
                                optimal = "none",
                                exist_sens = 0.7)
  region <- bsspread::Region()
  stage_matrix <- matrix(c(0.0, 2.0, 5.0,
                           0.3, 0.0, 0.0,
                           0.0, 0.6, 0.8),
                         nrow = 3, ncol = 3, byrow = TRUE)
  population_model <- bsspread::StagedPopulation(region, stage_matrix)
  actions <- list(
    a3 = ManageDetection(region, population_model, surveillance, stages = 2:3,
                         schedule = 2:3),
    a4 = ManageRemovals(region, population_model, removal_pr = 0.8,
                        stages = 2:3, schedule = 2:3),
    a5 = ManageControls(region, population_model, control_type = "growth",
                        control_mult = 0.7, stages = 2:3, schedule = 2:3))
  # staged single replicate
  expect_silent(
    results <- ManageResults(region, population_model = population_model,
                             actions = actions, time_steps = 4,
                             collation_steps = 2, replicates = 1))
  n <- population_model$make(initial = 50)
  n <- actions$a3$apply(n, 2)
  n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2)
  expect_silent(results$collate(r = 1, tm = 2, n = n))
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  action_results <- lapply(result_list$actions,
                           function(i) lapply(i, function(j) j[["2"]]))
  expect_equal(lapply(action_results$a3, function(a) dim(a)),
               list(detected = c(1, 3)))
  expect_equal(lapply(action_results$a4, function(a) dim(a)),
               list(removed = c(1, 3)))
  expect_equal(lapply(action_results$a5, function(a) length(a)),
               list(control_growth = 1))
  # staged multiple replicates
  n_r <- list()
  n <- population_model$make(initial = 40)
  n <- actions$a3$apply(n, 2)
  n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2)
  n_r[[1]] <- n
  n <- population_model$make(initial = 50)
  n <- actions$a3$apply(n, 2)
  n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2)
  n_r[[2]] <- n
  n <- population_model$make(initial = 60)
  n <- actions$a3$apply(n, 2)
  n <- actions$a4$apply(n, 2)
  n <- actions$a5$apply(n, 2)
  n_r[[3]] <- n
  collated_r <- lapply(n_r, function(n) lapply(actions, function(a) {
    if (a$get_label(include_id = FALSE) == "control_growth") {
      n_a <- +(attr(n, a$get_label()) > 0)
    } else {
      n_a <- as.numeric(attr(n, a$get_label()))
    }
    collated <- list(n_a)
    names(collated)[1] <- a$get_label(include_id = FALSE)
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
  colnames(collated_arrays$a3$detected) <-
    attr(population_model$get_growth(), "labels")
  colnames(collated_arrays$a4$removed) <-
    attr(population_model$get_growth(), "labels")
  expect_silent(
    results <- ManageResults(region, population_model = population_model,
                             actions = actions, time_steps = 4,
                             collation_steps = 2, replicates = 3))
  expect_silent(zero_results <- results$get_list()$actions)
  expect_silent(results$collate(r = 1, tm = 2, n = n_r[[1]]))
  expect_silent(results$collate(r = 2, tm = 2, n = n_r[[2]]))
  expect_silent(results$collate(r = 3, tm = 2, n = n_r[[3]]))
  expect_silent(results$finalize())
  expect_silent(result_list <- results$get_list())
  action_results <- lapply(result_list$actions,
                           function(i) lapply(i, function(j) j[["2"]]))
  expect_equal(
    action_results$a3$detected,
    list(mean = t(as.matrix(colMeans(collated_arrays$a3$detected))),
         sd = t(as.matrix(apply(collated_arrays$a3$detected, 2, sd)))))
  expect_equal(
    action_results$a4$removed,
    list(mean = t(as.matrix(colMeans(collated_arrays$a4$removed))),
         sd = t(as.matrix(apply(collated_arrays$a4$removed, 2, sd)))))
  expect_equal(
    action_results$a5$control_growth,
    list(mean = colMeans(collated_arrays$a5$control_growth)))
})
