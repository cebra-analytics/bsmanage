#' Manage results class builder
#'
#' Builds a class for encapsulating, calculating, and collating incursion
#' management simulation results, including the population at each location at
#' each collation time step, the total population size and area occupied at
#' each time step, as well as spatio-temporal impacts and management action
#' quantities applied (e.g. removals), plus costs when configured. When
#' simulations are replicated, summary results (means and standard deviations)
#' are produced. Extends the \code{bsspread::Results} package module.
#'
#' @param region A \code{bsspread::Region} or inherited class object
#'   representing the spatial region (template) for the incursion management
#'   simulations.
#' @param population_model A \code{bsspread::Population} or inherited class
#'   object defining the population representation and growth functionality for
#'   the incursion management simulations.
#' @param impacts A list of \code{ManageImpacts} class objects specifying
#'   various impacts of the simulated population. Each impact object
#'   encapsulates a \code{bsimpact::ImpactAnalysis} or inherited class object.
#' @param actions A list of \code{ManageActions} or inherited class objects
#'   specifying simulated management actions, such as detection, control, and
#'   removal.
#' @param time_steps The number of discrete time steps to simulate. Default is
#'   1.
#' @param step_duration The duration of the simulation time steps in units
#'   specified by \code{step_units}. Default is 1.
#' @param step_units The units for the simulation step duration
#'   (\code{step_duration}) as a character string. Default is "years".
#' @param collation_steps The interval in time steps for collating results.
#'   Default is 1, that is, results are collated at every time step.
#' @param replicates The number of replicate or repeated simulations to be run.
#'   Default is 1. Note that replicate simulations results are collated as
#'   summary statistics across simulations.
#' @param combine_stages Optionally combine (sum) specified stages (a vector of
#'   stage indices) of stage-based population results. The default
#'   (\code{NULL}) maintains results for each stage.
#' @param ... Additional parameters.
#' @return A \code{ManageResults} class object (list) containing functions for
#'   calculating and collating results, as well as accessing lists of results
#'   and the simulation parameters used to produce them:
#'   \describe{
#'     \item{\code{collate(r, tm, n, calc_impacts)}}{Collate the results at
#'       simulation replicate \code{r} and time step \code{tm} using the
#'       current vector or array \code{n} representing the population at each
#'       location, as well as calculated impacts (list).}
#'     \item{\code{finalize()}}{Finalize the results collation (summary
#'       calculations).}
#'     \item{\code{get_list()}}{Return the results as a list (collated
#'       populations and/or occupancy, totals, area occupied, impacts, and
#'       actions).}
#'     \item{\code{get_params()}}{Get the simulation parameters used.}
#'     \item{\code{save_rasters(...)}}{Save the collated results as raster TIF
#'       files when the region is grid-based. \code{Terra} raster write options
#'       may be passed to the function. Returns a list of saved \code{Terra}
#'       raster layers with attached attributes indicating if each layer
#'       contains non-zero values.}
#'     \item{\code{save_csv()}}{Save the collated results as comma-separated
#'       values (CSV) files when the region is patch-based. Also saves the
#'       population totals, area occupied, as well as impact and action totals
#'       (where applicable) to CSV files for both grid and patch-based region
#'       types.}
#'     \item{\code{save_plots(width = 480, height = 480)}}{Save plots of the
#'       population (staged) totals, the area occupied, total impacts (where
#'       applicable), and total actions as PNG files having specified
#'       \code{width} and \code{height} in pixels.}
#'   }
#' @references
#'   Baker, C. M., Bower, S., Tartaglia, E., Bode, M., Bower, H., & Pressey,
#'   R. L. (2018). Modelling the spread and control of cherry guava on Lord
#'   Howe Island. \emph{Biological Conservation}, 227, 252–258.
#'   \doi{10.1016/j.biocon.2018.09.017}
#'
#'   Bradhurst, R., Spring, D., Stanaway, M., Milner, J., & Kompas, T. (2021).
#'   A generalised and scalable framework for modelling incursions,
#'   surveillance and control of plant and environmental pests.
#'   \emph{Environmental Modelling & Software}, 139, N.PAG.
#'   \doi{10.1016/j.envsoft.2021.105004}
#'
#'   Cacho, O. J., & Hester, S. M. (2022). Modelling biocontrol of invasive
#'   insects: An application to European Wasp (Vespula germanica) in Australia.
#'   \emph{Ecological Modelling}, 467. \doi{10.1016/j.ecolmodel.2022.109939}
#'
#'   García Adeva, J. J., Botha, J. H., & Reynolds, M. (2012). A simulation
#'   modelling approach to forecast establishment and spread of Bactrocera
#'   fruit flies. \emph{Ecological Modelling}, 227, 93–108.
#'   \doi{10.1016/j.ecolmodel.2011.11.026}
#'
#'   Gormley, A. M., Holland, E. P., Barron, M. C., Anderson, D. P., & Nugent,
#'   G. (2016). A modelling framework for predicting the optimal balance
#'   between control and surveillance effort in the local eradication of
#'   tuberculosis in New Zealand wildlife.
#'   \emph{Preventive Veterinary Medicine}, 125, 10–18.
#'   \doi{10.1016/j.prevetmed.2016.01.007}
#'
#'   Krug, R. M., Roura-Pascual, N., & Richardson, D. M. (2010). Clearing of
#'   invasive alien plants under different budget scenarios: using a
#'   simulation model to test efficiency. \emph{Biological Invasions}, 12(12),
#'   4099–4112. \doi{10.1007/s10530-010-9827-3}
#'
#'   Lustig, A., James, A., Anderson, D., & Plank, M. (2019). Pest control at a
#'   regional scale: Identifying key criteria using a spatially explicit,
#'   agent‐based model. \emph{Journal of Applied Ecology}, 56(7 pp.1515–1527),
#'   1527–1515. \doi{10.1111/1365-2664.13387}
#'
#'   Rout, T. M., Moore, J. L., & McCarthy, M. A. (2014). Prevent, search or
#'   destroy? A partially observable model for invasive species management.
#'   \emph{Journal of Applied Ecology}, 51(3), 804–813.
#'   \doi{10.1111/1365-2664.12234}
#'
#'   Spring, D., Croft, L., & Kompas, T. (2017). Look before you treat:
#'   increasing the cost effectiveness of eradication programs with aerial
#'   surveillance. \emph{Biological Invasions}, 19(2), 521.
#'   \doi{10.1007/s10530-016-1292-1}
#'
#'   Wadsworth, R. A., Collingham, Y. C., Willis, S. G., Huntley, B., & Hulme,
#'   P. E. (2000). Simulating the Spread and Management of Alien Riparian
#'   Weeds: Are They Out of Control? \emph{Journal of Applied Ecology}, 37,
#'   28–38. \doi{10.1046/j.1365-2664.2000.00551.x}
#'
#'   Warburton, B., & Gormley, A. M. (2015). Optimising the Application of
#'   Multiple-Capture Traps for Invasive Species Management Using Spatial
#'   Simulation. \emph{PLoS ONE}, 10(3), 1–14.
#'   \doi{10.1371/journal.pone.0120373}
#'
#'   Zub, K., García-Díaz, P., Sankey, S., Eisler, R., & Lambin, X. (2022).
#'   Using a Modeling Approach to Inform Progress Towards Stoat Eradication
#'   From the Orkney Islands. \emph{Frontiers in Conservation Science}, 2.
#'   \doi{10.3389/fcosc.2021.780102}
#' @export
ManageResults <- function(region, population_model,
                          impacts = list(),
                          actions = list(),
                          time_steps = 1,
                          step_duration = 1,
                          step_units = "years",
                          collation_steps = 1,
                          replicates = 1,
                          combine_stages = NULL, ...) {
  UseMethod("ManageResults")
}

#' @name ManageResults
#' @export
ManageResults.Region <- function(region, population_model,
                                 impacts = list(),
                                 actions = list(),
                                 time_steps = 1,
                                 step_duration = 1,
                                 step_units = "years",
                                 collation_steps = 1,
                                 replicates = 1,
                                 combine_stages = NULL, ...) {

  # Inherit a bsspread::Results class structure (includes validation)
  self <- bsspread::Results(region, population_model,
                            time_steps = time_steps,
                            step_duration = step_duration,
                            step_units = step_units,
                            collation_steps = collation_steps,
                            replicates = replicates,
                            combine_stages = combine_stages,
                            class = "ManageResults", ...)

  # List of inherited functions to be extended
  super <- list(collate = self$collate, finalize = self$finalize,
                get_list = self$get_list, save_rasters = self$save_rasters,
                save_csv = self$save_csv, save_plots = self$save_plots)

  # Validate impact objects
  if (length(impacts) > 0 &&
      (!is.list(impacts) ||
       !all(sapply(impacts, function(i) inherits(i, "ManageImpacts"))))) {
    stop(paste("Impacts must be a list of 'ManageImpacts' or inherited class",
               "objects."), call. = FALSE)
  }

  # Validate action objects
  if (length(actions) > 0 &&
      (!is.list(actions) ||
       !all(sapply(actions, function(i) inherits(i, "ManageActions"))))) {
    stop(paste("Actions must be a list of 'ManageActions' or inherited class",
               "objects."), call. = FALSE)
  }

  # Population stages (NULL or number of stages)
  stages <- population_model$get_stages()

  # Stage labels
  if (is.numeric(stages) && is.null(combine_stages)) {
    stage_labels <- attr(population_model$get_growth(), "labels")
  } else if (is.numeric(combine_stages)) {
    stage_labels <- "combined"
  }

  # Include collated and totals?
  include_collated <- (region$get_locations() > 1)

  # Initialize additional incursion management result lists
  results <- list()
  if (length(impacts) > 0) {
    results$impacts <- lapply(impacts, function(impacts_i) {
      zeros <- list()
      zeros$impact <- rep(0L, region$get_locations())
      if (include_collated) {
        zeros$total_impact <- 0L
      }
      if (replicates > 1) { # summaries
        zeros$impact <- list(mean = zeros$impact, sd = zeros$impact)
        if (include_collated) {
          zeros$total_impact <- list(mean = zeros$total_impact,
                                     sd = zeros$total_impact)
        }
      }
      zeros$impact_steps <- list()
      zeros$total_impact_steps <- list()
      if (include_collated) {
        for (tm in as.character(c(0, seq(collation_steps, time_steps,
                                         by = collation_steps)))) {
          zeros$impact_steps[[tm]] <- zeros$impact
        }
        for (tm in as.character(0:time_steps)) {
          zeros$total_impact_steps[[tm]] <- zeros$total_impact
        }
      } else {
        for (tm in as.character(0:time_steps)) {
          zeros$impact_steps[[tm]] <- zeros$impact
        }
      }
      impact_aspects <- list()
      for (a in impacts_i$get_context()$get_impact_scope()) {
        impact_aspects[[a]] <- zeros$impact_steps
      }
      if (impacts_i$includes_combined()) {
        impact_aspects$combined <- zeros$impact_steps
      }
      if (include_collated && impacts_i$get_calc_total()) {
        impact_aspects$total <- zeros$total_impact_steps
      }
      if (impacts_i$get_context()$get_valuation_type() == "monetary") {
        impact_aspects$cumulative <- impact_aspects
      }
      attr(impact_aspects, "unit") <-
        impacts_i$get_context()$get_impact_measures()
      impact_aspects
    })
  }
  if (length(actions) > 0) {
    results$actions <- lapply(actions, function(actions_i) {
      zeros <- list()
      direct_action <- (actions_i$get_label() %in%
                          c("detected", "control_search_destroy", "removed"))
      if (is.numeric(stages) && direct_action) {
        if (is.numeric(combine_stages)) {
          zeros$action <- array(0L, c(region$get_locations(), 1))
          colnames(zeros$action) <- stage_labels
          if (include_collated) {
            zeros$total_action <- zeros$action[1,, drop = FALSE]
          }
        } else {
          zeros$action <- population_model$make(initial = 0L)
          if (include_collated) {
            zeros$total_action <- zeros$action[1,, drop = FALSE]
          }
        }
      } else {
        zeros$action <- rep(0L, region$get_locations())
        if (include_collated) {
          zeros$total_action <- 0L
        }
      }
      if (actions_i$include_cost()) {
        zeros$action_cost <- rep(0L, region$get_locations())
        if (include_collated) {
          zeros$total_action_cost <- 0L
        }
      }
      if (replicates > 1) { # summaries
        zeros$action <- list(mean = zeros$action)
        if (direct_action) {
          zeros$action$sd <- zeros$action$mean
        }
        if (include_collated) {
          zeros$total_action <- list(mean = zeros$total_action,
                                     sd = zeros$total_action)
        }
        if (actions_i$include_cost()) {
          zeros$action_cost <- list(mean = zeros$action_cost,
                                    sd = zeros$action_cost)
          if (include_collated) {
            zeros$total_action_cost <- list(mean = zeros$total_action_cost,
                                            sd = zeros$total_action_cost)
          }
        }
      }
      actions_list <- list()
      actions_list[[actions_i$get_label()]] <- list()
      if (include_collated) {
        for (tm in as.character(c(0, seq(collation_steps, time_steps,
                                         by = collation_steps)))) {
          actions_list[[actions_i$get_label()]][[tm]] <- zeros$action
        }
        actions_list$total <- list()
        for (tm in as.character(0:time_steps)) {
          actions_list$total[[tm]] <- zeros$total_action
        }
      } else {
        for (tm in as.character(0:time_steps)) {
          actions_list[[actions_i$get_label()]][[tm]] <- zeros$action
        }
      }
      if (actions_i$include_cost()) {
        actions_list$cost <- list()
        actions_list$cost[[actions_i$get_label()]] <- list()
        if (include_collated) {
          for (tm in as.character(c(0, seq(collation_steps, time_steps,
                                           by = collation_steps)))) {
            actions_list$cost[[actions_i$get_label()]][[tm]] <-
              zeros$action_cost
          }
          actions_list$cost$total <- list()
          for (tm in as.character(0:time_steps)) {
            actions_list$cost$total[[tm]] <- zeros$total_action_cost
          }
        } else {
          for (tm in as.character(0:time_steps)) {
            actions_list$cost[[actions_i$get_label()]][[tm]] <-
              zeros$action_cost
          }
        }
        actions_list$cost$cumulative <- actions_list$cost
        attr(actions_list$cost, "unit") <- actions_i$get_cost_unit()
      }
      actions_list
    })

    # Combined cost for multiple actions
    if (length(actions) > 1 &&
        all(sapply(actions, function(a) a$include_cost())) &&
        all(sapply(actions, function(a) a$get_cost_unit()) ==
            actions[[1]]$get_cost_unit())) {
      results$actions$cost <- list(combined = results$actions[[1]]$cost[[1]])
      results$actions$cost$cumulative <-
        list(combined = results$actions[[1]]$cost$cumulative[[1]])
      attr(results$actions$cost, "unit") <- actions[[1]]$get_cost_unit()
      if (include_collated) {
        results$actions$cost$total <- results$actions[[1]]$cost$total
        results$actions$cost$cumulative$total <-
          results$actions[[1]]$cost$cumulative$total
      }
    }
  }

  # Combined monetary impacts and action costs
  if (length(impacts) > 0 &&
      all(sapply(impacts, function(i) {
        i$get_context()$get_valuation_type() == "monetary" })) &&
      length(actions) > 0 && is.list(results$actions$cost) &&
      all(sapply(results$impacts, function(i) attr(i, "unit")) ==
          actions[[1]]$get_cost_unit())) {
    results$cost <- results$actions$cost
  }

  # Extended collate results
  self$collate <- function(r, tm, n, calc_impacts) {

    # Collate population results (without additional attributes)
    n_no_attr <- n
    attr(n_no_attr, "recovery_delay") <- NULL
    attr(n_no_attr, "dynamic_mult") <- NULL
    for (a in actions) {
      attr(n_no_attr, a$get_label()) <- NULL
      if (a$include_cost()) {
        attr(n_no_attr, names(a$include_cost())) <- NULL
      }
    }
    super$collate(r, tm, n_no_attr)

    # Use character list index (allows initial time = 0 and collated times)
    tmc <- as.character(tm)

    # Collate impacts
    if (length(impacts) > 0) {

      # Combined monetary impacts and action costs
      if (is.list(results$cost$combined)) {
        combined_cost <- rep(0, region$get_locations())
        combined_cumulative_cost <- rep(0, region$get_locations())
        if (include_collated) {
          total_cost <- 0
          total_cumulative_cost <- 0
        }
      }

      # Place calculated impacts in existing results structure
      for (i in 1:length(calc_impacts)) {

        # Calculate total impact
        if ("total" %in% names(results$impacts[[i]]) ||
            is.list(results$cost$total)) {
          total_impact <- 0
          if ("combined" %in% names(calc_impacts[[i]])) {
            total_impact <- sum(calc_impacts[[i]]$combined)
          } else if (length(calc_impacts[[i]]) == 1) {
            total_impact <- sum(calc_impacts[[i]][[1]])
          }
        }

        # Current cumulative impacts
        if ("cumulative" %in% names(results$impacts[[i]])) {
          for (a in names(calc_impacts[[i]])) {
            if (tm == 0) {
              results$impacts[[i]]$cumulative[[a]]$current <<-
                unname(calc_impacts[[i]][[a]])
            } else {
              results$impacts[[i]]$cumulative[[a]]$current <<-
                (results$impacts[[i]]$cumulative[[a]]$current +
                   unname(calc_impacts[[i]][[a]]))
            }
          }
          if ("total" %in% names(results$impacts[[i]]$cumulative)) {
            if (tm == 0) {
              results$impacts[[i]]$cumulative$total$current <<- total_impact
            } else {
              results$impacts[[i]]$cumulative$total$current <<-
                (results$impacts[[i]]$cumulative$total$current +
                   total_impact)
            }
          }
        }

        # Add to combined monetary impacts and action costs when present
        if (is.list(results$cost$combined)) {
          if ("combined" %in% names(results$impacts[[i]])) {
            combined_cost <- combined_cost + unname(calc_impacts[[i]]$combined)
            combined_cumulative_cost <-
              (combined_cumulative_cost +
                 results$impacts[[i]]$cumulative$combined$current)
          } else if (length(results$impacts[[i]]) == 1) {
            combined_cost <- combined_cost + unname(calc_impacts[[i]][[1]])
            combined_cumulative_cost <-
              (combined_cumulative_cost +
                 results$impacts[[i]]$cumulative[[1]]$current)
          }
          if (is.list(results$cost$total)) {
            total_cost <- total_cost + total_impact
            if ("total" %in% names(results$impacts[[i]]$cumulative)) {
              total_cumulative_cost <-
                (total_cumulative_cost +
                   results$impacts[[i]]$cumulative$total$current)
            }
          }
        }

        if (replicates > 1) { # summaries

          # Calculates running mean and standard deviation (note: variance*r is
          # stored as SD and transformed at the final replicate and time step)

          # All calculated impact aspects recorded in specified time steps
          if (!include_collated || tm %% collation_steps == 0) {
            for (a in names(calc_impacts[[i]])) {
              previous_mean <- results$impacts[[i]][[a]][[tmc]]$mean
              results$impacts[[i]][[a]][[tmc]]$mean <<-
                previous_mean + (calc_impacts[[i]][[a]] - previous_mean)/r
              previous_sd <- results$impacts[[i]][[a]][[tmc]]$sd
              results$impacts[[i]][[a]][[tmc]]$sd <<-
                (previous_sd + ((calc_impacts[[i]][[a]] - previous_mean)*
                                  (calc_impacts[[i]][[a]] -
                                     results$impacts[[i]][[a]][[tmc]]$mean)))
            }
          }

          # Total aspects at every time step
          if ("total" %in% names(results$impacts[[i]])) {
            previous_mean <- results$impacts[[i]]$total[[tmc]]$mean
            results$impacts[[i]]$total[[tmc]]$mean <<-
              previous_mean + (total_impact - previous_mean)/r
            previous_sd <- results$impacts[[i]]$total[[tmc]]$sd
            results$impacts[[i]]$total[[tmc]]$sd <<-
              (previous_sd + ((total_impact - previous_mean)*
                                (total_impact -
                                   results$impacts[[i]]$total[[tmc]]$mean)))
          }

          # Cumulative impacts
          if ("cumulative" %in% names(results$impacts[[i]])) {

            # Collated cumulative aspects
            if (!include_collated || tm %% collation_steps == 0) {
              for (a in names(calc_impacts[[i]])) {
                previous_mean <-
                  results$impacts[[i]]$cumulative[[a]][[tmc]]$mean
                results$impacts[[i]]$cumulative[[a]][[tmc]]$mean <<-
                  (previous_mean +
                     (results$impacts[[i]]$cumulative[[a]]$current
                      - previous_mean)/r)
                previous_sd <- results$impacts[[i]]$cumulative[[a]][[tmc]]$sd
                results$impacts[[i]]$cumulative[[a]][[tmc]]$sd <<-
                  (previous_sd +
                     ((results$impacts[[i]]$cumulative[[a]]$current -
                         previous_mean)*
                        (results$impacts[[i]]$cumulative[[a]]$current -
                           results$impacts[[i]]$cumulative[[a]][[tmc]]$mean)))
              }
            }

            # Cumulative totals at every time step
            if ("total" %in% names(results$impacts[[i]]$cumulative) &&
                "total" %in% names(results$impacts[[i]])) {
              previous_mean <-
                results$impacts[[i]]$cumulative$total[[tmc]]$mean
              results$impacts[[i]]$cumulative$total[[tmc]]$mean <<-
                (previous_mean +
                   (results$impacts[[i]]$cumulative$total$current -
                      previous_mean)/r)
              previous_sd <- results$impacts[[i]]$cumulative$total[[tmc]]$sd
              results$impacts[[i]]$cumulative$total[[tmc]]$sd <<-
                (previous_sd +
                   ((results$impacts[[i]]$cumulative$total$current -
                       previous_mean)*
                      (results$impacts[[i]]$cumulative$total$current -
                         results$impacts[[i]]$cumulative$total[[tmc]]$mean)))
            }
          }

        } else {

          # All calculated impact aspects recorded in specified time steps
          if (!include_collated || tm %% collation_steps == 0) {
            for (a in names(calc_impacts[[i]])) {
              results$impacts[[i]][[a]][[tmc]] <<-
                unname(calc_impacts[[i]][[a]])
            }
          }

          # Total aspects at every time step
          if ("total" %in% names(results$impacts[[i]])) {
            results$impacts[[i]]$total[[tmc]] <<- total_impact
          }

          # Cumulative impacts
          if ("cumulative" %in% names(results$impacts[[i]])) {

            # Collated cumulative aspects
            if (!include_collated || tm %% collation_steps == 0) {
              for (a in names(calc_impacts[[i]])) {
                results$impacts[[i]]$cumulative[[a]][[tmc]] <<-
                  results$impacts[[i]]$cumulative[[a]]$current
              }
            }

            # Cumulative totals at every time step
            if ("total" %in% names(results$impacts[[i]]$cumulative) &&
                "total" %in% names(results$impacts[[i]])) {
              results$impacts[[i]]$cumulative$total[[tmc]] <<-
                results$impacts[[i]]$cumulative$total$current
            }
          }
        }
      }
    }

    # Collate management actions
    if (length(actions) > 0) {

      # Place applied actions in existing results structure
      if (is.list(results$actions$cost$combined) ||
          is.list(results$cost$combined)) {
        combined_actions_cost <- rep(0, region$get_locations())
        combined_actions_cumulative_cost <- rep(0, region$get_locations())
        if (include_collated) {
          total_actions_cost <- 0
          total_actions_cumulative_cost <- 0
        }
      }
      for (i in 1:length(actions)) {

        # Get attribute from n
        a <- actions[[i]]$get_label()
        n_a <- attr(n, a)*1
        direct_action <-
          (a %in% c("detected", "control_search_destroy", "removed"))

        # Combine stages when required
        if (population_model$get_type() == "stage_structured" &&
            direct_action && is.numeric(combine_stages)) {
          n_a <- matrix(rowSums(n_a[,combine_stages, drop = FALSE]), ncol = 1)
          colnames(n_a) <- stage_labels
        }

        # Binarize growth, spread, & establishment control (indirect actions)
        if (a %in%  c("control_growth", "control_spread",
                      "control_establishment")) {
          n_a <- +(n_a < 1)
        }

        # Shape total when population is staged
        if (include_collated && is.numeric(stages) && direct_action) {
          total_n_a <- array(colSums(n_a), c(1, ncol(n_a)))
          colnames(total_n_a) <- stage_labels
        } else {
          total_n_a <- sum(n_a)
        }

        # Get attached action cost
        if (actions[[i]]$include_cost()) {
          a_cost <- as.numeric(attr(n, names(actions[[i]]$include_cost())))

          # Add to current cumulative
          if (tm == 0) {
            results$actions[[i]]$cost$cumulative[[a]]$current <<- a_cost
          } else {
            results$actions[[i]]$cost$cumulative[[a]]$current <<-
              results$actions[[i]]$cost$cumulative[[a]]$current + a_cost
          }

          # Add to combined action costs and combined costs when present
          if (is.list(results$actions$cost$combined) ||
              is.list(results$cost$combined)) {
            combined_actions_cost <- combined_actions_cost + a_cost
            combined_actions_cumulative_cost <-
              (combined_actions_cumulative_cost +
                 results$actions[[i]]$cost$cumulative[[a]]$current)
            if (include_collated) {
              total_actions_cost <- total_actions_cost + sum(a_cost)
              total_actions_cumulative_cost <-
                (total_actions_cumulative_cost +
                   sum(results$actions[[i]]$cost$cumulative[[a]]$current))
            }

            # Add to combined monetary impacts and action costs
            if (is.list(results$cost$combined)) {
              combined_cost <- combined_cost + a_cost
              combined_cumulative_cost <-
                (combined_cumulative_cost +
                   results$actions[[i]]$cost$cumulative[[a]]$current)
              if (include_collated) {
                total_cost <- total_cost + sum(a_cost)
                total_cumulative_cost <-
                  (total_cumulative_cost +
                     sum(results$actions[[i]]$cost$cumulative[[a]]$current))
              }
            }
          }
        }

        if (replicates > 1) { # summaries

          # Calculates running mean and standard deviation (note: variance*r is
          # stored as SD and transformed at the final replicate and time step)

          # All applied actions recorded in specified time steps
          if (!include_collated || tm %% collation_steps == 0) {
            previous_mean <- results$actions[[i]][[a]][[tmc]]$mean
            results$actions[[i]][[a]][[tmc]]$mean <<-
              previous_mean + (n_a - previous_mean)/r
            if (direct_action) {
              previous_sd <- results$actions[[i]][[a]][[tmc]]$sd
              results$actions[[i]][[a]][[tmc]]$sd <<-
                (previous_sd +
                   ((n_a - previous_mean)*
                      (n_a - results$actions[[i]][[a]][[tmc]]$mean)))
            }
          }

          # Total applied actions at every time step
          if ("total" %in% names(results$actions[[i]])) {
            previous_mean <- results$actions[[i]]$total[[tmc]]$mean
            results$actions[[i]]$total[[tmc]]$mean <<-
              previous_mean + (total_n_a - previous_mean)/r
            previous_sd <- results$actions[[i]]$total[[tmc]]$sd
            results$actions[[i]]$total[[tmc]]$sd <<-
              (previous_sd + ((total_n_a - previous_mean)*
                                (total_n_a -
                                   results$actions[[i]]$total[[tmc]]$mean)))
          }

          # Record action costs and cumulative costs
          if ("cost" %in% names(results$actions[[i]])) {
            if (!include_collated || tm %% collation_steps == 0) {

              # Costs
              previous_mean <- results$actions[[i]]$cost[[a]][[tmc]]$mean
              results$actions[[i]]$cost[[a]][[tmc]]$mean <<-
                previous_mean + (a_cost - previous_mean)/r
              previous_sd <- results$actions[[i]]$cost[[a]][[tmc]]$sd
              results$actions[[i]]$cost[[a]][[tmc]]$sd <<-
                (previous_sd +
                   ((a_cost - previous_mean)*
                      (a_cost - results$actions[[i]]$cost[[a]][[tmc]]$mean)))

              # Cumulative costs
              previous_mean <-
                results$actions[[i]]$cost$cumulative[[a]][[tmc]]$mean
              results$actions[[i]]$cost$cumulative[[a]][[tmc]]$mean <<-
                (previous_mean +
                   (results$actions[[i]]$cost$cumulative[[a]]$current -
                      previous_mean)/r)
              previous_sd <-
                results$actions[[i]]$cost$cumulative[[a]][[tmc]]$sd
              results$actions[[i]]$cost$cumulative[[a]][[tmc]]$sd <<-
                (previous_sd +
                   ((results$actions[[i]]$cost$cumulative[[a]]$current -
                       previous_mean)*
                      (results$actions[[i]]$cost$cumulative[[a]]$current -
                         results$actions[[i]]$cost$cumulative[[a]][[tmc]]$mean)
                   ))
            }
            if ("total" %in% names(results$actions[[i]]$cost)) {

              # Total costs
              previous_mean <- results$actions[[i]]$cost$total[[tmc]]$mean
              results$actions[[i]]$cost$total[[tmc]]$mean <<-
                previous_mean + (sum(a_cost) - previous_mean)/r
              previous_sd <- results$actions[[i]]$cost$total[[tmc]]$sd
              results$actions[[i]]$cost$total[[tmc]]$sd <<-
                (previous_sd +
                   ((sum(a_cost) - previous_mean)*
                      (sum(a_cost) -
                         results$actions[[i]]$cost$total[[tmc]]$mean)))

              # Total cumulative costs
              previous_mean <-
                results$actions[[i]]$cost$cumulative$total[[tmc]]$mean
              results$actions[[i]]$cost$cumulative$total[[tmc]]$mean <<-
                (previous_mean +
                   (sum(results$actions[[i]]$cost$cumulative[[a]]$current) -
                      previous_mean)/r)
              previous_sd <-
                results$actions[[i]]$cost$cumulative$total[[tmc]]$sd
              results$actions[[i]]$cost$cumulative$total[[tmc]]$sd <<-
                (previous_sd +
                   ((sum(results$actions[[i]]$cost$cumulative[[a]]$current) -
                       previous_mean)*
                      (sum(results$actions[[i]]$cost$cumulative[[a]]$current) -
                         results$actions[[i]]$cost$cumulative$total[[tmc]]$mean)
                   ))
            }
          }

        } else {

          # All applied actions recorded in specified time steps
          if (!include_collated || tm %% collation_steps == 0) {
            results$actions[[i]][[a]][[tmc]] <<- n_a
          }

          # Total combined aspects at every time step
          if ("total" %in% names(results$actions[[i]])) {
            results$actions[[i]]$total[[tmc]] <<- total_n_a
          }

          # Record action costs and cumulative costs
          if ("cost" %in% names(results$actions[[i]])) {
            if (!include_collated || tm %% collation_steps == 0) {
              results$actions[[i]]$cost[[a]][[tmc]] <<- a_cost
              results$actions[[i]]$cost$cumulative[[a]][[tmc]] <<-
                results$actions[[i]]$cost$cumulative[[a]]$current
            }
            if ("total" %in% names(results$actions[[i]]$cost)) {
              results$actions[[i]]$cost$total[[tmc]] <<- sum(a_cost)
              results$actions[[i]]$cost$cumulative$total[[tmc]] <<-
                sum(results$actions[[i]]$cost$cumulative[[a]]$current)
            }
          }
        }
      }

      # Collate combined action costs when present
      if (is.list(results$actions$cost)) {
        if (replicates > 1) { # summaries

          # Calculates running mean and standard deviation (note: variance*r is
          # stored as SD and transformed at the final replicate and time step)

          if (!include_collated || tm %% collation_steps == 0) {

            # Combined costs
            previous_mean <- results$actions$cost$combined[[tmc]]$mean
            results$actions$cost$combined[[tmc]]$mean <<-
              previous_mean + (combined_actions_cost - previous_mean)/r
            previous_sd <- results$actions$cost$combined[[tmc]]$sd
            results$actions$cost$combined[[tmc]]$sd <<-
              (previous_sd +
                 ((combined_actions_cost - previous_mean)*
                    (combined_actions_cost -
                       results$actions$cost$combined[[tmc]]$mean)))

            # Combined cumulative costs
            previous_mean <-
              results$actions$cost$cumulative$combined[[tmc]]$mean
            results$actions$cost$cumulative$combined[[tmc]]$mean <<-
              (previous_mean +
                 (combined_actions_cumulative_cost - previous_mean)/r)
            previous_sd <-
              results$actions$cost$cumulative$combined[[tmc]]$sd
            results$actions$cost$cumulative$combined[[tmc]]$sd <<-
              (previous_sd +
                 ((combined_actions_cumulative_cost - previous_mean)*
                    (combined_actions_cumulative_cost -
                       results$actions$cost$cumulative$combined[[tmc]]$mean)
                 ))
          }

          if (include_collated) {

            # Total costs
            previous_mean <- results$actions$cost$total[[tmc]]$mean
            results$actions$cost$total[[tmc]]$mean <<-
              previous_mean + (total_actions_cost - previous_mean)/r
            previous_sd <- results$actions$cost$total[[tmc]]$sd
            results$actions$cost$total[[tmc]]$sd <<-
              (previous_sd +
                 ((total_actions_cost - previous_mean)*
                    (total_actions_cost -
                       results$actions$cost$total[[tmc]]$mean)))

            # Total cumulative costs
            previous_mean <-
              results$actions$cost$cumulative$total[[tmc]]$mean
            results$actions$cost$cumulative$total[[tmc]]$mean <<-
              (previous_mean +
                 (total_actions_cumulative_cost - previous_mean)/r)
            previous_sd <-
              results$actions$cost$cumulative$total[[tmc]]$sd
            results$actions$cost$cumulative$total[[tmc]]$sd <<-
              (previous_sd +
                 ((total_actions_cumulative_cost - previous_mean)*
                    (total_actions_cumulative_cost -
                       results$actions$cost$cumulative$total[[tmc]]$mean)
                 ))
          }
        } else {
          if (!include_collated || tm %% collation_steps == 0) {
            results$actions$cost$combined[[tmc]] <<- combined_actions_cost
            results$actions$cost$cumulative$combined[[tmc]] <<-
              combined_actions_cumulative_cost
          }
          if (include_collated) {
            results$actions$cost$total[[tmc]] <<- total_actions_cost
            results$actions$cost$cumulative$total[[tmc]] <<-
              total_actions_cumulative_cost
          }
        }
      }
    }

    # Collate total combined monetary impacts and action costs when present
    if (is.list(results$cost)) {
      if (replicates > 1) { # summaries

        # Calculates running mean and standard deviation (note: variance*r is
        # stored as SD and transformed at the final replicate and time step)

        if (!include_collated || tm %% collation_steps == 0) {

          # Combined costs
          previous_mean <- results$cost$combined[[tmc]]$mean
          results$cost$combined[[tmc]]$mean <<-
            previous_mean + (combined_cost - previous_mean)/r
          previous_sd <- results$cost$combined[[tmc]]$sd
          results$cost$combined[[tmc]]$sd <<-
            (previous_sd +
               ((combined_cost - previous_mean)*
                  (combined_cost -
                     results$cost$combined[[tmc]]$mean)))

          # Combined cumulative costs
          previous_mean <-
            results$cost$cumulative$combined[[tmc]]$mean
          results$cost$cumulative$combined[[tmc]]$mean <<-
            (previous_mean +
               (combined_cumulative_cost - previous_mean)/r)
          previous_sd <-
            results$cost$cumulative$combined[[tmc]]$sd
          results$cost$cumulative$combined[[tmc]]$sd <<-
            (previous_sd +
               ((combined_cumulative_cost - previous_mean)*
                  (combined_cumulative_cost -
                     results$cost$cumulative$combined[[tmc]]$mean)
               ))
        }

        if (include_collated) {

          # Total costs
          previous_mean <- results$cost$total[[tmc]]$mean
          results$cost$total[[tmc]]$mean <<-
            previous_mean + (total_cost - previous_mean)/r
          previous_sd <- results$cost$total[[tmc]]$sd
          results$cost$total[[tmc]]$sd <<-
            (previous_sd +
               ((total_cost - previous_mean)*
                  (total_cost - results$cost$total[[tmc]]$mean)))

          # Total cumulative costs
          previous_mean <-
            results$cost$cumulative$total[[tmc]]$mean
          results$cost$cumulative$total[[tmc]]$mean <<-
            (previous_mean +
               (total_cumulative_cost - previous_mean)/r)
          previous_sd <-
            results$cost$cumulative$total[[tmc]]$sd
          results$cost$cumulative$total[[tmc]]$sd <<-
            (previous_sd +
               ((total_cumulative_cost - previous_mean)*
                  (total_cumulative_cost -
                     results$cost$cumulative$total[[tmc]]$mean)
               ))
        }
      } else {
        if (!include_collated || tm %% collation_steps == 0) {
          results$cost$combined[[tmc]] <<- combined_cost
          results$cost$cumulative$combined[[tmc]] <<- combined_cumulative_cost
        }
        if (include_collated) {
          results$cost$total[[tmc]] <<- total_cost
          results$cost$cumulative$total[[tmc]] <<- total_cumulative_cost
        }
      }
    }
  }

  # Extended finalize the results collation
  self$finalize <- function() {

    # Finalize population spread results
    super$finalize()

    # Finalize impact results
    if (length(impacts) > 0) {

      # Clear working memory for current cumulative impacts
      for (i in 1:length(results$impacts)) {
        if ("cumulative" %in% names(results$impacts[[i]])) {
          for (a in names(results$impacts[[i]]$cumulative)) {
            results$impacts[[i]]$cumulative[[a]]$current <<- NULL
          }
          if ("total" %in% names(results$impacts[[i]]$cumulative)) {
            results$impacts[[i]]$cumulative$total$current <<- NULL
          }
        }
      }

      # Transform impact standard deviations
      if (replicates > 1) { # summaries
        for (i in 1:length(results$impacts)) {
          i_names <- names(results$impacts[[i]])
          for (a in i_names[i_names != "cumulative"]) {
            for (tmc in names(results$impacts[[i]][[a]])) {
              results$impacts[[i]][[a]][[tmc]]$sd <<-
                sqrt(results$impacts[[i]][[a]][[tmc]]$sd/(replicates - 1))
            }
          }
          if ("cumulative" %in% names(results$impacts[[i]])) {
            for (a in names(results$impacts[[i]]$cumulative)) {
              for (tmc in names(results$impacts[[i]]$cumulative[[a]])) {
                results$impacts[[i]]$cumulative[[a]][[tmc]]$sd <<-
                  sqrt(results$impacts[[i]]$cumulative[[a]][[tmc]]$sd/
                         (replicates - 1))
              }
            }
          }
        }
      }
    }

    # Finalize action results
    if (length(actions) > 0) {

      # Clear working memory for current cumulative costs
      for (i in 1:length(actions)) {
        if ("cost" %in% names(results$actions[[i]]) &&
            "cumulative" %in% names(results$actions[[i]]$cost)) {
          for (a in names(results$actions[[i]]$cost$cumulative)) {
            results$actions[[i]]$cost$cumulative[[a]]$current <<- NULL
          }
        }
      }

      # Transform action standard deviations
      if (replicates > 1) { # summaries
        for (i in 1:length(actions)) {
          i_names <- names(results$actions[[i]])
          for (a in i_names[i_names != "cost"]) {
            if (include_collated && a == "total" ||
                a %in% c("detected", "control_search_destroy", "removed")) {
              for (tmc in names(results$actions[[i]][[a]])) {
                results$actions[[i]][[a]][[tmc]]$sd <<-
                  sqrt(results$actions[[i]][[a]][[tmc]]$sd/(replicates - 1))
              }
            }
          }
          if ("cost" %in% names(results$actions[[i]])) {
            a <- actions[[i]]$get_label()
            for (tmc in names(results$actions[[i]]$cost[[a]])) {
              results$actions[[i]]$cost[[a]][[tmc]]$sd <<-
                sqrt(results$actions[[i]]$cost[[a]][[tmc]]$sd/(replicates - 1))
              results$actions[[i]]$cost$cumulative[[a]][[tmc]]$sd <<-
                sqrt(results$actions[[i]]$cost$cumulative[[a]][[tmc]]$sd/
                       (replicates - 1))
            }
            if ("total" %in% names(results$actions[[i]]$cost)) {
              for (tmc in names(results$actions[[i]]$cost$total)) {
                results$actions[[i]]$cost$total[[tmc]]$sd <<-
                  sqrt(results$actions[[i]]$cost$total[[tmc]]$sd/
                         (replicates - 1))
                results$actions[[i]]$cost$cumulative$total[[tmc]]$sd <<-
                  sqrt(results$actions[[i]]$cost$cumulative$total[[tmc]]$sd/
                         (replicates - 1))
              }
            }
          }
        }

        # Combined action costs
        if (is.list(results$actions$cost)) {
          for (tmc in names(results$actions$cost$combined)) {
            results$actions$cost$combined[[tmc]]$sd <<-
              sqrt(results$actions$cost$combined[[tmc]]$sd/
                     (replicates - 1))
            results$actions$cost$cumulative$combined[[tmc]]$sd <<-
              sqrt(results$actions$cost$cumulative$combined[[tmc]]$sd/
                     (replicates - 1))
          }
          if (include_collated) {
            for (tmc in names(results$actions$cost$total)) {
              results$actions$cost$total[[tmc]]$sd <<-
                sqrt(results$actions$cost$total[[tmc]]$sd/
                       (replicates - 1))
              results$actions$cost$cumulative$total[[tmc]]$sd <<-
                sqrt(results$actions$cost$cumulative$total[[tmc]]$sd/
                       (replicates - 1))
            }
          }
        }
      }
    }

    # Finalise summary combined monetary impacts and action costs
    if (is.list(results$cost$combined) && replicates > 1) {
      for (tmc in names(results$cost$combined)) {
        results$cost$combined[[tmc]]$sd <<-
          sqrt(results$cost$combined[[tmc]]$sd/(replicates - 1))
        results$cost$cumulative$combined[[tmc]]$sd <<-
          sqrt(results$cost$cumulative$combined[[tmc]]$sd/(replicates - 1))
      }
      if (include_collated) {
        for (tmc in names(results$cost$total)) {
          results$cost$total[[tmc]]$sd <<-
            sqrt(results$cost$total[[tmc]]$sd/(replicates - 1))
          results$cost$cumulative$total[[tmc]]$sd <<-
            sqrt(results$cost$cumulative$total[[tmc]]$sd/(replicates - 1))
        }
      }
    }

    # Add labels to staged populations actions (again)
    if (population_model$get_type() == "stage_structured") {
      if (replicates > 1) {
        summaries <- c("mean", "sd")
      } else {
        summaries <- ""
      }
      if (length(actions) > 0) {
        for (i in 1:length(results$actions)) {
          for (a in names(results$actions[[i]])) {
            if (a %in%  c("detected", "control_search_destroy", "removed")) {
              for (tmc in names(results$actions[[i]][[a]])) {
                for (s in summaries) {
                  if (replicates > 1) {
                    colnames(results$actions[[i]][[a]][[tmc]][[s]]) <<-
                      stage_labels
                  } else {
                    colnames(results$actions[[i]][[a]][[tmc]]) <<- stage_labels
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  # Get results list
  self$get_list <- function() {
    return(c(super$get_list(), results))
  }

  # Convert first letter of title to upper-case
  title_first <- function(title_str) {
    return(paste0(toupper(substr(title_str, 1, 1)),
                  substr(title_str, 2, nchar(title_str))))
  }

  # Save collated results as raster files
  if (region$get_type() == "grid") {
    self$save_rasters  <- function(...) {

      # Save population spread results
      spread_rast_list <- super$save_rasters(...)

      # Output and non-zero indicator lists
      output_list <- list()
      nonzero_list <- list()

      # Save impacts
      if (length(impacts) > 0) {

        # Replicate summaries or single replicate
        if (replicates > 1) {
          summaries <- c("mean", "sd")
        } else {
          summaries <- ""
        }

        # Save rasters for each impact aspect at each time step
        if (!is.null(names(results$impacts))) {
          impact_i <- names(results$impacts)    # named impacts
        } else {
          impact_i <- 1:length(results$impacts) # indices
        }
        for (i in impact_i) {

          # Impacts post-fix
          if (length(impact_i) == 1 && is.numeric(i)) {
            ic <- ""
          } else {
            ic <- paste0("_", i)
          }

          # Impact aspects and their cumulative when present
          aspects <- names(results$impacts[[i]])
          aspects <- aspects[which(!aspects %in% c("total", "cumulative"))]
          unit_map <- rep("", length(aspects))
          names(unit_map) <- aspects
          if (!is.null(attr(results$impacts[[i]], "unit"))) {
            unit <- attr(results$impacts[[i]], "unit")
            if (length(unit) > 1 && length(aspects) >= length(unit)) {
              unit_map[1:length(unit)] <- unit
              if (length(aspects) > length(unit)) { # assume same for combined
                unit_map[(length(unit) + 1):length(aspects)] <- unit[1]
              }
            } else {
              unit_map[] <- unit
            }
          }
          if ("cumulative" %in% names(results$impacts[[i]])) {
            cum_aspects <- names(results$impacts[[i]]$cumulative)
            cum_aspects <- cum_aspects[which(cum_aspects != "total")]
            names(cum_aspects) <- paste0("cum_", cum_aspects)
            aspects <- c(aspects, names(cum_aspects))
            unit_map_cum <- unit_map[cum_aspects]
            names(unit_map_cum) <- names(cum_aspects)
            unit_map <- c(unit_map, unit_map_cum)
          } else {
            cum_aspects <- NULL
          }
          for (a in aspects) {
            for (s in summaries) {

              # Summary post-fix
              if (replicates > 1) {
                sc <- paste0("_", s)
              } else {
                sc <- s
              }

              # Add nested list to output list
              if (a %in% names(cum_aspects)) {
                output_key <- paste0("cumulative_impacts", ic, "_",
                                     cum_aspects[a], sc)
              } else {
                output_key <- paste0("impacts", ic, "_", a, sc)
              }
              output_list[[output_key]] <- list()

              # Initialise non-zero indicator
              nonzero_list[[output_key]] <- FALSE

              # Collated time steps
              if (a %in% names(cum_aspects)) {
                collated_tmc <-
                  names(results$impacts[[i]]$cumulative[[cum_aspects[a]]])
              } else {
                collated_tmc <- names(results$impacts[[i]][[a]])
              }
              for (tmc in collated_tmc) {

                # Copy impacts into a raster & update non-zero indicator
                if (replicates > 1) {
                  if (a %in% names(cum_aspects)) {
                    output_rast <-
                      region$get_rast(results$impacts[[i]]$cumulative[[
                        cum_aspects[a]]][[tmc]][[s]])
                    nonzero_list[[output_key]] <-
                      (nonzero_list[[output_key]] |
                         sum(results$impacts[[i]]$cumulative[[
                           cum_aspects[a]]][[tmc]][[s]]) > 0)
                  } else {
                    output_rast <-
                      region$get_rast(results$impacts[[i]][[a]][[tmc]][[s]])
                    nonzero_list[[output_key]] <-
                      (nonzero_list[[output_key]] |
                         sum(results$impacts[[i]][[a]][[tmc]][[s]]) > 0)
                  }
                } else {
                  if (a %in% names(cum_aspects)) {
                    output_rast <-
                      region$get_rast(results$impacts[[i]]$cumulative[[
                        cum_aspects[a]]][[tmc]])
                    nonzero_list[[output_key]] <-
                      (nonzero_list[[output_key]] |
                         sum(results$impacts[[i]]$cumulative[[
                           cum_aspects[a]]][[tmc]]) > 0)
                  } else {
                    output_rast <-
                      region$get_rast(results$impacts[[i]][[a]][[tmc]])
                    nonzero_list[[output_key]] <-
                      (nonzero_list[[output_key]] |
                         sum(results$impacts[[i]][[a]][[tmc]]) > 0)
                  }
                }

                # Write raster to file
                if (a %in% names(cum_aspects)) {
                  filename <- sprintf(
                    paste0("cumulative_impacts%s_%s_t%0",
                           nchar(as.character(time_steps)), "d%s.tif"),
                    ic, cum_aspects[a], as.integer(tmc), sc)
                } else {
                  filename <- sprintf(
                    paste0("impacts%s_%s_t%0", nchar(as.character(time_steps)),
                           "d%s.tif"),
                    ic, a, as.integer(tmc), sc)
                }
                output_list[[output_key]][[tmc]] <-
                  terra::writeRaster(output_rast, filename, ...)
              }

              # Add list of metadata as an attribute
              if (impacts[[i]]$get_impacts()$get_is_dynamic()) {
                impact_type <- "dynamic"
              } else {
                impact_type <- impacts[[i]]$get_context()$get_valuation_type()
              }
              incursion_type <-
                impacts[[i]]$get_impacts()$get_incursion()$get_type()
              if (a %in% names(cum_aspects)) {
                aspect <- unname(cum_aspects[a])
                cumulative <- TRUE
              } else {
                aspect <- a
                cumulative <- FALSE
              }
              attr(output_list[[output_key]], "metadata") <- list(
                category = "impact",
                type = impact_type,
                name = aspect,
                incursion = incursion_type,
                cost = FALSE,
                cumulative = cumulative,
                summary = s,
                unit = unname(unit_map[a]),
                nonzero = nonzero_list[[output_key]]
              )
            }
          }
        }
      }

      # Save actions
      if (length(actions) > 0) {

        # Save rasters for each impact aspect at each time step
        if (!is.null(names(actions))) {
          action_i <- names(actions)    # named actions
        } else {
          action_i <- 1:length(actions) # indices
        }
        for (i in action_i) {

          # Actions post-fix
          if (length(action_i) == 1 && is.numeric(i)) {
            ic <- ""
          } else {
            ic <- paste0("_", i)
          }

          # Action key/name. Direct action?
          a <- actions[[i]]$get_label()
          direct_action <-
            (a %in%  c("detected", "control_search_destroy", "removed"))
          a_name <- a_key <- sub("control_", "", a, fixed = TRUE)
          if (actions[[i]]$get_type() == "control") {
            if (a_key == "search_destroy") {
              a_name <- "search & destroy"
            }
            a_key <- paste0(a_key, "_control")
            a_name <- paste(a_name, "control")
          }

          # Action cost and cumulative cost key/name
          if ("cost" %in% names(results$actions[[i]])) {
            if (a_key == "detected") {
              cost_name <- cost_key <- "detection"
            } else if (a_key == "removed") {
              cost_name <- cost_key <- "removal"
            } else {
              cost_key <- a_key
              cost_name <- a_name
            }
          }

          # Create and save an action results raster per stage
          result_stages <- stages
          if (is.null(stages) || is.numeric(combine_stages) ||
              !direct_action) {
            result_stages <- 1
          }
          for (j in 1:result_stages) {

            # Stage post-fix
            if (population_model$get_type() == "stage_structured" &&
                is.null(combine_stages) && direct_action) {
              jc <- paste0("_stage_", j)
            } else {
              jc <- ""
            }

            # Replicate summaries or single replicate
            if (replicates > 1) {
              if (direct_action) {
                summaries <- c("mean", "sd")
              } else {
                summaries <- c("mean")
              }
            } else {
              summaries <- ""
            }

            for (s in summaries) {

              # Summary post-fix
              if (replicates > 1) {
                sc <- paste0("_", s)
              } else {
                sc <- s
              }

              # Add nested list to output list
              output_key <- paste0("actions", ic, "_", a_key, jc, sc)
              output_list[[output_key]] <- list()

              # Initialise non-zero indicator
              nonzero_list[[output_key]] <- FALSE

              for (tmc in names(results$actions[[i]][[a]])) {

                # Copy actions into a raster and update non-zero indicator
                if (population_model$get_type() == "stage_structured" &&
                    direct_action) {
                  if (replicates > 1) {
                    output_rast <-
                      region$get_rast(
                        results$actions[[i]][[a]][[tmc]][[s]][,j])
                    nonzero_list[[output_key]] <-
                      (nonzero_list[[output_key]] |
                         sum(results$actions[[i]][[a]][[tmc]][[s]][,j]) > 0)
                  } else {
                    output_rast <-
                      region$get_rast(results$actions[[i]][[a]][[tmc]][,j])
                    nonzero_list[[output_key]] <-
                      (nonzero_list[[output_key]] |
                         sum(results$actions[[i]][[a]][[tmc]][,j]) > 0)
                  }
                  if (is.null(combine_stages)) {
                    names(output_rast) <- stage_labels[j]
                  } else if (is.numeric(combine_stages)) {
                    names(output_rast) <- "combined"
                  }
                } else {
                  if (replicates > 1) {
                    output_rast <-
                      region$get_rast(results$actions[[i]][[a]][[tmc]][[s]])
                    nonzero_list[[output_key]] <-
                      (nonzero_list[[output_key]] |
                         sum(results$actions[[i]][[a]][[tmc]][[s]]) > 0)
                  } else {
                    output_rast <-
                      region$get_rast(results$actions[[i]][[a]][[tmc]])
                    nonzero_list[[output_key]] <-
                      (nonzero_list[[output_key]] |
                         sum(results$actions[[i]][[a]][[tmc]]) > 0)
                  }
                }

                # Write raster to file
                filename <- sprintf(
                  paste0("actions%s_%s%s_t%0",
                         nchar(as.character(time_steps)), "d%s.tif"),
                  ic, a_key, jc, as.integer(tmc), sc)
                output_list[[output_key]][[tmc]] <-
                  terra::writeRaster(output_rast, filename, ...)
              }

              # Add list of metadata as an attribute
              if (is.null(stages)) {
                stage_attr <- NULL
              } else {
                stage_attr <- j
              }
              attr_list <- list(
                category = "action",
                type = actions[[i]]$get_type(),
                name = a_name,
                stage = stage_attr,
                cost = FALSE,
                cumulative = FALSE,
                summary = s,
                unit = "",
                nonzero = nonzero_list[[output_key]]
              )
              attr(output_list[[output_key]], "metadata") <- attr_list
            }
          }

          # Action cost and cumulative cost
          if ("cost" %in% names(results$actions[[i]])) {

            # Replicate summaries or single replicate
            if (replicates > 1) {
              summaries <- c("mean", "sd")
            } else {
              summaries <- ""
            }
            for (s in summaries) {

              # Summary post-fix
              if (replicates > 1) {
                sc <- paste0("_", s)
              } else {
                sc <- s
              }

              # Add nested list to output list
              output_cost_key <- paste0("action_costs", ic, "_", cost_key, sc)
              output_list[[output_cost_key]] <- list()
              nonzero_list[[output_cost_key]] <- FALSE
              if ("cumulative" %in% names(results$actions[[i]]$cost)) {
                output_cum_cost_key <- paste0("cumulative_action_costs", ic,
                                              "_", cost_key, sc)
                output_list[[output_cum_cost_key]] <- list()
                nonzero_list[[output_cum_cost_key]] <- FALSE
              }

              # Write cost and cumulative cost to raster files
              for (tmc in names(results$actions[[i]][[a]])) {
                if (replicates > 1) {
                  output_rast <- region$get_rast(
                    results$actions[[i]]$cost[[a]][[tmc]][[s]])
                  nonzero_list[[output_cost_key]] <-
                    (nonzero_list[[output_cost_key]] |
                       sum(results$actions[[i]]$cost[[a]][[tmc]][[s]]) > 0)
                } else {
                  output_rast <-
                    region$get_rast(results$actions[[i]]$cost[[a]][[tmc]])
                  nonzero_list[[output_cost_key]] <-
                    (nonzero_list[[output_cost_key]] |
                       sum(results$actions[[i]]$cost[[a]][[tmc]]) > 0)
                }
                filename <- sprintf(
                  paste0("action_costs%s_%s_t%0",
                         nchar(as.character(time_steps)), "d%s.tif"),
                  ic, cost_key, as.integer(tmc), sc)
                output_list[[output_cost_key]][[tmc]] <-
                  terra::writeRaster(output_rast, filename, ...)
                if ("cumulative" %in% names(results$actions[[i]]$cost)) {
                  if (replicates > 1) {
                    output_rast <- region$get_rast(
                      results$actions[[i]]$cost$cumulative[[a]][[
                        tmc]][[s]])
                    nonzero_list[[output_cum_cost_key]] <-
                      (nonzero_list[[output_cum_cost_key]] |
                         (sum(results$actions[[i]]$cost$cumulative[[a]][[
                           tmc]][[s]]) > 0))
                  } else {
                    output_rast <- region$get_rast(
                      results$actions[[i]]$cost$cumulative[[a]][[tmc]])
                    nonzero_list[[output_cum_cost_key]] <-
                      (nonzero_list[[output_cum_cost_key]] |
                         sum(results$actions[[i]]$cost$cumulative[[a]][[
                           tmc]]) > 0)
                  }
                  filename <- sprintf(
                    paste0("cumulative_action_costs%s_%s_t%0",
                           nchar(as.character(time_steps)), "d%s.tif"),
                    ic, cost_key, as.integer(tmc), sc)
                  output_list[[output_cum_cost_key]][[tmc]] <-
                    terra::writeRaster(output_rast, filename, ...)
                }
              }

              # Add list of metadata as an attribute
              cost_unit <- attr(results$actions[[i]]$cost, "unit")
              attr_list <- list(
                category = "action",
                type = actions[[i]]$get_type(),
                name = cost_name,
                stage = NULL,
                cost = TRUE,
                cumulative = FALSE,
                summary = s,
                unit = cost_unit,
                nonzero = nonzero_list[[output_cost_key]]
              )
              attr(output_list[[output_cost_key]], "metadata") <- attr_list
              if ("cumulative" %in% names(results$actions[[i]]$cost)) {
                attr_list$cumulative <- TRUE
                attr(output_list[[output_cum_cost_key]], "metadata") <-
                  attr_list
              }
            }
          }
        }

        # Combined action costs and/or monetary impacts
        if ("cost" %in% names(results$actions) || "cost" %in% names(results)) {

          # Replicate summaries or single replicate
          if (replicates > 1) {
            summaries <- c("mean", "sd")
          } else {
            summaries <- ""
          }

          for (s in summaries) {

            # Summary post-fix
            if (replicates > 1) {
              sc <- paste0("_", s)
            } else {
              sc <- s
            }

            # Combined action costs
            if ("cost" %in% names(results$actions)) {

              # Add cost and cumulative cost to output list
              output_cost_key <- paste0("action_costs_combined", sc)
              output_list[[output_cost_key]] <- list()
              nonzero_list[[output_cost_key]] <- FALSE
              if ("cumulative" %in% names(results$actions$cost)) {
                output_cum_cost_key <-
                  paste0("cumulative_action_costs_combined", sc)
                output_list[[output_cum_cost_key]] <- list()
                nonzero_list[[output_cum_cost_key]] <- FALSE
              }

              # Write combined actions cost to raster files
              for (tmc in names(results$actions$cost$combined)) {
                if (replicates > 1) {
                  output_rast <- region$get_rast(
                    results$actions$cost$combined[[tmc]][[s]])
                  nonzero_list[[output_cost_key]] <-
                    (nonzero_list[[output_cost_key]] |
                       sum(results$actions$cost$combined[[tmc]][[s]]) > 0)
                } else {
                  output_rast <-
                    region$get_rast(results$actions$cost$combined[[tmc]])
                  nonzero_list[[output_cost_key]] <-
                    (nonzero_list[[output_cost_key]] |
                       sum(results$actions$cost$combined[[tmc]]) > 0)
                }
                filename <- sprintf(
                  paste0("action_costs_combined_t%0",
                         nchar(as.character(time_steps)), "d%s.tif"),
                  as.integer(tmc), sc)
                output_list[[output_cost_key]][[tmc]] <-
                  terra::writeRaster(output_rast, filename, ...)
                if ("cumulative" %in% names(results$actions$cost)) {
                  if (replicates > 1) {
                    output_rast <- region$get_rast(
                      results$actions$cost$cumulative$combined[[tmc]][[s]])
                    nonzero_list[[output_cum_cost_key]] <-
                      (nonzero_list[[output_cum_cost_key]] |
                         (sum(results$actions$cost$cumulative$combined[[
                           tmc]][[s]]) > 0))
                  } else {
                    output_rast <- region$get_rast(
                      results$actions$cost$cumulative$combined[[tmc]])
                    nonzero_list[[output_cum_cost_key]] <-
                      (nonzero_list[[output_cum_cost_key]] |
                         sum(results$actions$cost$cumulative$combined[[
                           tmc]]) > 0)
                  }
                  filename <- sprintf(
                    paste0("cumulative_action_costs_combined_t%0",
                           nchar(as.character(time_steps)), "d%s.tif"),
                    as.integer(tmc), sc)
                  output_list[[output_cum_cost_key]][[tmc]] <-
                    terra::writeRaster(output_rast, filename, ...)
                }
              }

              # Add list of metadata as an attribute
              cost_unit <- attr(results$actions$cost, "unit")
              attr_list <- list(
                category = "action",
                type = "combined",
                name = "combined",
                cost = TRUE,
                cumulative = FALSE,
                summary = s,
                unit = cost_unit,
                nonzero = nonzero_list[[output_cost_key]]
              )
              attr(output_list[[output_cost_key]], "metadata") <- attr_list
              if ("cumulative" %in% names(results$actions$cost)) {
                attr_list$cumulative <- TRUE
                attr(output_list[[output_cum_cost_key]], "metadata") <-
                  attr_list
              }
            }

            # Combined monetary impacts and action costs
            if ("cost" %in% names(results)) {

              # Add cost and cumulative cost to output list
              output_cost_key <- paste0("combined_costs", sc)
              output_list[[output_cost_key]] <- list()
              nonzero_list[[output_cost_key]] <- FALSE
              if ("cumulative" %in% names(results$actions$cost)) {
                output_cum_cost_key <- paste0("cumulative_combined_costs", sc)
                output_list[[output_cum_cost_key]] <- list()
                nonzero_list[[output_cum_cost_key]] <- FALSE
              }

              # Write combined cost to raster files
              for (tmc in names(results$cost$combined)) {
                if (replicates > 1) {
                  output_rast <- region$get_rast(
                    results$cost$combined[[tmc]][[s]])
                  nonzero_list[[output_cost_key]] <-
                    (nonzero_list[[output_cost_key]] |
                       sum(results$cost$combined[[tmc]][[s]]) > 0)
                } else {
                  output_rast <-
                    region$get_rast(results$cost$combined[[tmc]])
                  nonzero_list[[output_cost_key]] <-
                    (nonzero_list[[output_cost_key]] |
                       sum(results$cost$combined[[tmc]]) > 0)
                }
                filename <- sprintf(
                  paste0("combined_costs_t%0",
                         nchar(as.character(time_steps)), "d%s.tif"),
                  as.integer(tmc), sc)
                output_list[[output_cost_key]][[tmc]] <-
                  terra::writeRaster(output_rast, filename, ...)
                if ("cumulative" %in% names(results$cost)) {
                  if (replicates > 1) {
                    output_rast <- region$get_rast(
                      results$cost$cumulative$combined[[tmc]][[s]])
                    nonzero_list[[output_cum_cost_key]] <-
                      (nonzero_list[[output_cum_cost_key]] |
                         (sum(results$cost$cumulative$combined[[
                           tmc]][[s]]) > 0))
                  } else {
                    output_rast <- region$get_rast(
                      results$cost$cumulative$combined[[tmc]])
                    nonzero_list[[output_cum_cost_key]] <-
                      (nonzero_list[[output_cum_cost_key]] |
                         sum(results$cost$cumulative$combined[[tmc]]) > 0)
                  }
                  filename <- sprintf(
                    paste0("cumulative_combined_costs_t%0",
                           nchar(as.character(time_steps)), "d%s.tif"),
                    as.integer(tmc), sc)
                  output_list[[output_cum_cost_key]][[tmc]] <-
                    terra::writeRaster(output_rast, filename, ...)
                }
              }

              # Add list of metadata as an attribute
              cost_unit <- attr(results$cost, "unit")
              attr_list <- list(
                category = "combined",
                type = "combined",
                name = "combined",
                cost = TRUE,
                cumulative = FALSE,
                summary = s,
                unit = cost_unit,
                nonzero = nonzero_list[[output_cost_key]]
              )
              attr(output_list[[output_cost_key]], "metadata") <- attr_list
              if ("cumulative" %in% names(results$cost)) {
                attr_list$cumulative <- TRUE
                attr(output_list[[output_cum_cost_key]], "metadata") <-
                  attr_list
              }
            }
          }
        }
      }

      # Return output list as multi-layer rasters
      return(c(spread_rast_list,
               lapply(output_list, function(rast_list) {
                 raster_layers <- terra::rast(rast_list)
                 attr(raster_layers, "metadata") <- attr(rast_list, "metadata")
                 raster_layers
               })))
    }
  }

  # Save collated (patch only) and summary (both) results as CSV files
  self$save_csv  <- function() {

    # Save population spread results
    super$save_csv()

    # Filename parts summaries or single replicate
    s_fname <- list("", mean = "_mean", sd = "_sd")

    # Time step labels
    time_steps_labels <- sprintf(
      paste0("t%0", nchar(as.character(time_steps)), "d"),
      as.integer(0:time_steps))
    collated_labels <- sprintf(
      paste0("t%0", nchar(as.character(time_steps)), "d"),
      c(0, seq(collation_steps, time_steps, by = collation_steps)))

    # Location coordinates and labels
    if (include_collated && region$get_type() == "patch") {
      coords <- region$get_coords(extra_cols = TRUE)
      coords <- coords[, c("lon", "lat",
                           names(which(sapply(coords, is.character))))]
    }

    # Save impacts
    if (length(impacts) > 0) {

      # Save CSV files for each impact
      if (!is.null(names(results$impacts))) {
        impact_i <- names(results$impacts)    # named impacts
      } else {
        impact_i <- 1:length(results$impacts) # indices
      }

      # Results for each impact
      for (i in impact_i) {

        # Include impact name or numeric index
        if (length(impact_i) == 1 && is.numeric(i)) {
          ic <- ""
        } else {
          ic <- paste0("_", i)
        }

        # Results for single or multi-patch only
        if (region$get_type() == "patch") {

          # Impact aspects and their cumulative when present
          aspects <- names(results$impacts[[i]])
          aspects <- aspects[which(!aspects %in% c("total", "cumulative"))]
          if ("cumulative" %in% names(results$impacts[[i]])) {
            cum_aspects <- names(results$impacts[[i]]$cumulative)
            cum_aspects <- cum_aspects[which(cum_aspects != "total")]
            names(cum_aspects) <- paste0("cum_", cum_aspects)
            aspects <- c(aspects, names(cum_aspects))
          } else {
            cum_aspects <- NULL
          }

          # Save impact CSV file(s)
          if (include_collated) { # spatial with coordinates
            for (a in aspects) {
              output_df <- list()
              if (replicates > 1) {
                summaries <- c("mean", "sd")
              } else {
                summaries <- 1
              }
              for (s in summaries) {
                if (replicates > 1) {
                  if (a %in% names(cum_aspects)) {
                    output_df[[s]] <-
                      lapply(results$impacts[[i]]$cumulative[[cum_aspects[a]]],
                             function(a_tm) a_tm[[s]])
                  } else {
                    output_df[[s]] <- lapply(results$impacts[[i]][[a]],
                                             function(a_tm) a_tm[[s]])
                  }
                } else {
                  if (a %in% names(cum_aspects)) {
                    output_df[[s]] <-
                      results$impacts[[i]]$cumulative[[cum_aspects[a]]]
                  } else {
                    output_df[[s]] <- results$impacts[[i]][[a]]
                  }
                }
                names(output_df[[s]]) <- collated_labels
                output_df[[s]] <- cbind(coords, as.data.frame(output_df[[s]]))
                if (a %in% names(cum_aspects)) {
                  filename <- sprintf("cumulative_impacts%s_%s%s.csv", ic,
                                      cum_aspects[a], s_fname[[s]])
                } else {
                  filename <- sprintf("impacts%s_%s%s.csv", ic, a,
                                      s_fname[[s]])
                }
                utils::write.csv(output_df[[s]], filename, row.names = FALSE)
              }
            }

          } else { # spatially implicit

            # Collect and save to single row CSV
            if (replicates > 1) {
              for (a in aspects) {
                if (a %in% names(cum_aspects)) {
                  output_df <- sapply(
                    results$impacts[[i]]$cumulative[[cum_aspects[a]]],
                    function(imp) as.data.frame(imp))
                  filename <- sprintf("cumulative_impacts%s_%s.csv", ic,
                                      cum_aspects[a])
                } else {
                  output_df <- sapply(results$impacts[[i]][[a]],
                                      function(imp) as.data.frame(imp))
                  filename <- sprintf("impacts%s_%s.csv", ic, a)
                }
                colnames(output_df) <- time_steps_labels
                utils::write.csv(output_df, filename, row.names = TRUE)
              }
            } else {
              a_i <- which(names(results$impacts[[i]]) != "cumulative")
              output_df <- t(sapply(results$impacts[[i]][a_i], function(a) a))
              colnames(output_df) <- time_steps_labels
              filename <- sprintf("impacts%s.csv", ic)
              utils::write.csv(output_df, filename, row.names = TRUE)
              if ("cumulative" %in% names(results$impacts[[i]])) {
                output_df <- t(sapply(results$impacts[[i]]$cumulative,
                                      function(a) a))
                colnames(output_df) <- time_steps_labels
                filename <- sprintf("cumulative_impacts%s.csv", ic)
                utils::write.csv(output_df, filename, row.names = TRUE)
              }
            }
          }
        }
      }

      # Impact totals when present
      if (include_collated) {
        if (replicates > 1) {
          for (i in impact_i) {

            # Include impact name or numeric index
            if (length(impact_i) == 1 && is.numeric(i)) {
              ic <- ""
            } else {
              ic <- paste0("_", i)
            }

            # Collect totals and save to CSV
            if (is.list(results$impacts[[i]]$total)) {
              output_df <- sapply(results$impacts[[i]]$total,
                                  function(tot) tot)
              colnames(output_df) <- time_steps_labels
              filename <- sprintf("total_impacts%s.csv", ic)
              utils::write.csv(output_df, filename, row.names = TRUE)
            }

            # Collect cumulative totals and save to CSV when present
            if ("cumulative" %in% names(results$impacts[[i]]) &&
                is.list(results$impacts[[i]]$cumulative$total)) {
              output_df <- sapply(results$impacts[[i]]$cumulative$total,
                                  function(tot) tot)
              colnames(output_df) <- time_steps_labels
              filename <- sprintf("total_cumulative_impacts%s.csv", ic)
              utils::write.csv(output_df, filename, row.names = TRUE)
            }
          }

        } else {

          # Combine impact totals when present and save to CSV
          present <- sapply(results$impacts,
                            function(aspect) is.list(aspect$total))
          if (any(present)) {
            output_df <- t(sapply(results$impacts[present],
                                  function(aspect) aspect$total))
            colnames(output_df) <- time_steps_labels
            if (is.numeric(impact_i)) {
              if (length(impact_i) == 1) {
                rownames(output_df) <- "impact"
              } else {
                rownames(output_df) <- paste("impact", which(present))
              }
            }
            utils::write.csv(output_df, "total_impacts.csv", row.names = TRUE)
          }

          # Combine cumulative impact totals when present and save to CSV
          present <- sapply(results$impacts,
                            function(aspect) is.list(aspect$cumulative$total))
          if (any(present)) {
            output_df <- t(sapply(results$impacts[present],
                                  function(aspect) aspect$cumulative$total))
            colnames(output_df) <- time_steps_labels
            if (is.numeric(impact_i)) {
              if (length(impact_i) == 1) {
                rownames(output_df) <- "impact"
              } else {
                rownames(output_df) <- paste("impact", which(present))
              }
            }
            utils::write.csv(output_df, "total_cumulative_impacts.csv",
                             row.names = TRUE)
          }
        }
      }
    } # impacts

    # Save actions
    if (length(actions) > 0) {

      # Save CSV files for each action
      if (!is.null(names(actions))) {
        action_i <- names(actions)    # named actions
      } else {
        action_i <- 1:length(actions) # indices
      }

      # Results for single or multi-patch only
      if (region$get_type() == "patch") {

        # Results for each action
        for (i in action_i) {

          # Include action name or numeric index
          if (length(action_i) == 1 && is.numeric(i)) {
            ic <- ""
          } else {
            ic <- paste0("_", i)
          }

          # Action key/name. Direct action?
          a <- actions[[i]]$get_label()
          direct_action <-
            (a %in%  c("detected", "control_search_destroy", "removed"))
          a_name <- a_key <- sub("control_", "", a, fixed = TRUE)
          if (actions[[i]]$get_type() == "control") {
            a_key <- paste0(a_key, "_control")
            if (a_key == "search_destroy") {
              a_name <- "found & destroyed"
            } else {
              a_name <- paste(a_name, "control")
            }
          }

          # Action cost and cumulative cost key/name
          if ("cost" %in% names(results$actions[[i]])) {
            if (a_key == "detected") {
              cost_name <- cost_key <- "detection"
            } else if (a_key == "removed") {
              cost_name <- cost_key <- "removal"
            } else {
              cost_key <- a_key
              if (a_key == "search_destroy") {
                cost_name <- "search & destroy control"
              } else {
                cost_name <- a_name
              }
            }
          }

          # Resolve result stages
          result_stages <- stages
          if (is.null(stages) || is.numeric(combine_stages) ||
              !direct_action) {
            result_stages <- 1
            j_fname <- ""
          } else {
            j_fname <- paste0("_stage_", 1:result_stages)
          }

          # Save collated action results with coordinates
          if (include_collated) {

            # Replicate summaries or single replicate
            if (replicates > 1) {
              if (direct_action) {
                summaries <- c("mean", "sd")
              } else {
                summaries <- c("mean")
              }
            } else {
              summaries <- 1
            }

            # Save stages separately when applicable
            for (j in 1:result_stages) {

              # Combine coordinates and collated values & write to CSV files
              for (s in summaries) {
                if (population_model$get_type() == "stage_structured" &&
                    direct_action) {
                  if (replicates > 1) {
                    output_df <- lapply(results$actions[[i]][[a]],
                                        function(a_tm) a_tm[[s]][,j])
                  } else {
                    output_df <- lapply(results$actions[[i]][[a]],
                                        function(a_tm) a_tm[,j])
                  }
                } else {
                  if (replicates > 1) {
                    output_df <- lapply(results$actions[[i]][[a]],
                                        function(a_tm) a_tm[[s]])
                  } else {
                    output_df <- results$actions[[i]][[a]]
                  }
                }
                names(output_df) <- collated_labels
                output_df <- cbind(coords, as.data.frame(output_df))
                filename <- sprintf("actions%s_%s%s%s.csv", ic, a_key,
                                    j_fname[j], s_fname[[s]])
                utils::write.csv(output_df, filename, row.names = FALSE)
              }
            }

            # Cost and cumulative cost
            if ("cost" %in% names(results$actions[[i]])) {

              # Replicate summaries or single replicate
              if (replicates > 1) {
                summaries <- c("mean", "sd")
              } else {
                summaries <- 1
              }

              # Write cost and cumulative cost
              for (s in summaries) {
                if (replicates > 1) {
                  output_df <- lapply(results$actions[[i]]$cost[[a]],
                                      function(a_tm) a_tm[[s]])
                } else {
                  output_df <- results$actions[[i]]$cost[[a]]
                }
                names(output_df) <- collated_labels
                output_df <- cbind(coords, as.data.frame(output_df))
                filename <- sprintf("action_costs%s_%s%s.csv", ic, cost_key,
                                    s_fname[[s]])
                utils::write.csv(output_df, filename, row.names = FALSE)
                if ("cumulative" %in% names(results$actions[[i]]$cost)) {
                  if (replicates > 1) {
                    output_df <-
                      lapply(results$actions[[i]]$cost$cumulative[[a]],
                             function(a_tm) a_tm[[s]])
                  } else {
                    output_df <- results$actions[[i]]$cost$cumulative[[a]]
                  }
                  names(output_df) <- collated_labels
                  output_df <- cbind(coords, as.data.frame(output_df))
                  filename <- sprintf("cumulative_action_costs%s_%s%s.csv", ic,
                                      cost_key, s_fname[[s]])
                  utils::write.csv(output_df, filename, row.names = FALSE)
                }
              }
            }

          } else {

            # Save CSVs without coordinates (spatially implicit)
            if (population_model$get_type() == "stage_structured" &&
                direct_action) {
              if (replicates > 1) {
                for (j in 1:result_stages) {
                  output_df <- sapply(results$actions[[i]][[a]],
                                      function(a_tm) as.data.frame(
                                        lapply(a_tm, function(m) m[,j])))
                  colnames(output_df) <- time_steps_labels
                  filename <- sprintf("actions%s_%s%s.csv", ic, a_key,
                                      j_fname[j])
                  utils::write.csv(output_df, filename, row.names = TRUE)
                }
              } else {
                if (is.numeric(combine_stages)) {
                  output_df <- as.data.frame(results$actions[[i]][[a]])
                  if (length(combine_stages) == 1) {
                    rownames(output_df) <- attr(population_model$get_growth(),
                                                "labels")[combine_stages]
                  } else {
                    rownames(output_df) <- sprintf(
                      "stages %s-%s", min(combine_stages),
                      max(combine_stages))
                  }
                } else {
                  output_df <- sapply(results$actions[[i]][[a]],
                                      function(a_tm) as.data.frame(a_tm))
                }
                colnames(output_df) <- time_steps_labels
                filename <- sprintf("actions%s_%s.csv", ic, a_key)
                utils::write.csv(output_df, filename, row.names = TRUE)
              }
            } else {
              if (replicates > 1) {
                if (direct_action) {
                  output_df <- sapply(results$actions[[i]][[a]],
                                      function(a_tm) as.data.frame(a_tm))
                } else {
                  output_df <- as.data.frame(results$actions[[i]][[a]])
                  rownames(output_df) <- "mean"
                }
              } else {
                output_df <- as.data.frame(results$actions[[i]][[a]])
                rownames(output_df) <- a_name
              }
              colnames(output_df) <- time_steps_labels
              filename <- sprintf("actions%s_%s.csv", ic, a_key)
              utils::write.csv(output_df, filename, row.names = TRUE)
            }

            # Action cost and cumulative cost when present
            if (replicates > 1) {
              output_df <- sapply(results$actions[[i]]$cost[[a]],
                                  function(a_tm) a_tm)
            } else {
              output_df <- as.data.frame(results$actions[[i]]$cost[[a]])
              rownames(output_df) <- paste(cost_name, "cost")
            }
            colnames(output_df) <- time_steps_labels
            filename <- sprintf("action_costs%s_%s.csv", ic, cost_key)
            utils::write.csv(output_df, filename, row.names = TRUE)
            if ("cumulative" %in% names(results$actions[[i]]$cost)) {
              if (replicates > 1) {
                output_df <-
                  sapply(results$actions[[i]]$cost$cumulative[[a]],
                         function(a_tm) a_tm)
              } else {
                output_df <-
                  as.data.frame(results$actions[[i]]$cost$cumulative[[a]])
                rownames(output_df) <- paste("cumulative", cost_name, "cost")
              }
              colnames(output_df) <- time_steps_labels
              filename <- sprintf("cumulative_action_costs%s_%s.csv", ic,
                                  cost_key)
              utils::write.csv(output_df, filename, row.names = TRUE)
            }
          }
        }


        # Combined action costs and/or monetary impacts
        if ("cost" %in% names(results$actions) || "cost" %in% names(results)) {

          # Save collated action costs with coordinates
          if (include_collated) {

            # Replicate summaries or single replicate
            if (replicates > 1) {
              summaries <- c("mean", "sd")
            } else {
              summaries <- 1
            }

            # Combined action costs
            if ("cost" %in% names(results$actions)) {
              for (s in summaries) {
                if (replicates > 1) {
                  output_df <- lapply(results$actions$cost$combined,
                                      function(a_tm) a_tm[[s]])
                } else {
                  output_df <- results$actions$cost$combined
                }
                names(output_df) <- collated_labels
                output_df <- cbind(coords, as.data.frame(output_df))
                filename <- sprintf("action_costs_combined%s.csv",
                                    s_fname[[s]])
                utils::write.csv(output_df, filename, row.names = FALSE)
              }
              if ("cumulative" %in% names(results$actions$cost)) {
                for (s in summaries) {
                  if (replicates > 1) {
                    output_df <-
                      lapply(results$actions$cost$cumulative$combined,
                             function(a_tm) a_tm[[s]])
                  } else {
                    output_df <- results$actions$cost$cumulative$combined
                  }
                  names(output_df) <- collated_labels
                  output_df <- cbind(coords, as.data.frame(output_df))
                  filename <- sprintf("cumulative_action_costs_combined%s.csv",
                                      s_fname[[s]])
                  utils::write.csv(output_df, filename, row.names = FALSE)
                }
              }
            }

            # Combined monetary impacts and action costs
            if ("cost" %in% names(results)) {
              for (s in summaries) {
                if (replicates > 1) {
                  output_df <- lapply(results$cost$combined,
                                      function(a_tm) a_tm[[s]])
                } else {
                  output_df <- results$cost$combined
                }
                names(output_df) <- collated_labels
                output_df <- cbind(coords, as.data.frame(output_df))
                filename <- sprintf("combined_costs%s.csv",
                                    s_fname[[s]])
                utils::write.csv(output_df, filename, row.names = FALSE)
              }
              if ("cumulative" %in% names(results$cost)) {
                for (s in summaries) {
                  if (replicates > 1) {
                    output_df <-
                      lapply(results$cost$cumulative$combined,
                             function(a_tm) a_tm[[s]])
                  } else {
                    output_df <- results$cost$cumulative$combined
                  }
                  names(output_df) <- collated_labels
                  output_df <- cbind(coords, as.data.frame(output_df))
                  filename <- sprintf("cumulative_combined_costs%s.csv",
                                      s_fname[[s]])
                  utils::write.csv(output_df, filename, row.names = FALSE)
                }
              }
            }

          } else {

            # Save CSVs without coordinates (spatially implicit)

            # Combined action costs
            if ("cost" %in% names(results$actions)) {
              if (replicates > 1) {
                output_df <- sapply(results$actions$cost$combined,
                                    function(a_tm) a_tm)
              } else {
                output_df <- as.data.frame(results$actions$cost$combined)
                rownames(output_df) <- "combined actions cost"
              }
              colnames(output_df) <- time_steps_labels
              filename <- "action_costs_combined.csv"
              utils::write.csv(output_df, filename, row.names = TRUE)
              if ("cumulative" %in% names(results$actions$cost)) {
                if (replicates > 1) {
                  output_df <-
                    sapply(results$actions$cost$cumulative$combined,
                           function(a_tm) a_tm)
                } else {
                  output_df <-
                    as.data.frame(results$actions$cost$cumulative$combined)
                  rownames(output_df) <- "cumulative actions cost"
                }
                colnames(output_df) <- time_steps_labels
                filename <- "cumulative_action_costs_combined.csv"
                utils::write.csv(output_df, filename, row.names = TRUE)
              }
            }

            # Combined monetary impacts and action costs
            if ("cost" %in% names(results)) {
              if (replicates > 1) {
                output_df <- sapply(results$cost$combined,
                                    function(a_tm) a_tm)
              } else {
                output_df <- as.data.frame(results$cost$combined)
                rownames(output_df) <- "combined cost"
              }
              colnames(output_df) <- time_steps_labels
              filename <- "combined_costs.csv"
              utils::write.csv(output_df, filename, row.names = TRUE)
              if ("cumulative" %in% names(results$cost)) {
                if (replicates > 1) {
                  output_df <- sapply(results$cost$cumulative$combined,
                                      function(a_tm) a_tm)
                } else {
                  output_df <-
                    as.data.frame(results$cost$cumulative$combined)
                  rownames(output_df) <- "cumulative cost"
                }
                colnames(output_df) <- time_steps_labels
                filename <- "cumulative_combined_costs.csv"
                utils::write.csv(output_df, filename, row.names = TRUE)
              }
            }
          }
        }
      }

      # Save totals CSV
      if (include_collated) {

        # Save totals CSV for each action
        for (i in action_i) {

          # Action key/name. Direct action?
          a <- actions[[i]]$get_label()
          direct_action <-
            (a %in%  c("detected", "control_search_destroy", "removed"))
          a_name <- a_key <- sub("control_", "", a, fixed = TRUE)
          if (actions[[i]]$get_type() == "control") {
            if (a_key == "search_destroy") {
              a_name <- "search & destroy"
            }
            a_key <- paste0(a_key, "_control")
            a_name <- paste(a_name, "control")
          }

          # Action cost and cumulative cost key/name
          if ("cost" %in% names(results$actions[[i]])) {
            if (a_key == "detected") {
              cost_name <- cost_key <- "detection"
            } else if (a_key == "removed") {
              cost_name <- cost_key <- "removal"
            } else {
              cost_key <- a_key
              cost_name <- a_name
            }
          }

          # Include action name or numeric index
          if (length(action_i) == 1 && is.numeric(i)) {
            ic <- ""
          } else {
            ic <- paste0("_", i)
          }

          # Resolve result stages
          result_stages <- stages
          if (is.null(stages) || is.numeric(combine_stages) ||
              !direct_action) {
            result_stages <- 1
            j_fname <- ""
          } else {
            j_fname <- paste0("_stage_", 1:result_stages)
          }

          # Action totals when present
          if (is.list(results$actions[[i]]$total)) {

            # Collect totals at each time step
            if (replicates > 1 && result_stages > 1) {

              # Save stages separately
              for (j in 1:result_stages) {

                # Place summaries in rows
                output_df <- sapply(results$actions[[i]]$total,
                                    function(tot) as.data.frame(
                                      lapply(tot, function(m) m[,j])))
                colnames(output_df) <- time_steps_labels

                # Write to CSV file
                filename <- sprintf("total_actions%s_%s%s.csv", ic, a_key,
                                    j_fname[j])
                utils::write.csv(output_df, filename)
              }

            } else if (replicates > 1 || result_stages > 1) {

              # Place either summaries or stages in rows
              output_df <- sapply(results$actions[[i]]$total,
                                  function(tot) tot)

              colnames(output_df) <- time_steps_labels
              if (replicates == 1 && result_stages > 1) {
                rownames(output_df) <-
                  attr(population_model$get_growth(), "labels")
              }

              # Write to CSV file
              filename <- sprintf("total_actions%s_%s.csv", ic, a_key)
              utils::write.csv(output_df, filename)

            } else {
              output_df <- as.data.frame(results$actions[[i]]$total)
              rownames(output_df) <- a_name
              colnames(output_df) <- time_steps_labels
              filename <- sprintf("total_actions%s_%s.csv", ic, a_key)
              utils::write.csv(output_df, filename, row.names = TRUE)
            }
          }

          # Action cost and cumulative cost totals when present
          if ("cost" %in% names(results$actions[[i]]) &&
              is.list(results$actions[[i]]$cost$total)) {
            if (replicates > 1) {
              output_df <- sapply(results$actions[[i]]$cost$total,
                                  function(tot) tot)
            } else {
              output_df <- as.data.frame(results$actions[[i]]$cost$total)
              rownames(output_df) <- paste(cost_name, "cost")
            }
            colnames(output_df) <- time_steps_labels
            filename <- sprintf("total_action_costs%s_%s.csv", ic, cost_key)
            utils::write.csv(output_df, filename, row.names = TRUE)
            if ("cumulative" %in% names(results$actions[[i]]$cost) &&
                is.list(results$actions[[i]]$cost$cumulative$total)) {
              if (replicates > 1) {
                output_df <- sapply(results$actions[[i]]$cost$cumulative$total,
                                    function(tot) tot)
              } else {
                output_df <-
                  as.data.frame(results$actions[[i]]$cost$cumulative$total)
                rownames(output_df) <- paste("cumulative", cost_name, "cost")
              }
              colnames(output_df) <- time_steps_labels
              filename <- sprintf("total_cumulative_action_costs%s_%s.csv",
                                  ic, cost_key)
              utils::write.csv(output_df, filename, row.names = TRUE)
            }
          }
        }

        # Combined action costs and/or monetary impacts totals
        if ("cost" %in% names(results$actions) || "cost" %in% names(results)) {

          # Combined action cost totals
          if ("cost" %in% names(results$actions) &&
              is.list(results$actions$cost$total)) {
            if (replicates > 1) {
              output_df <- sapply(results$actions$cost$total,
                                  function(tot) tot)
            } else {
              output_df <- as.data.frame(results$actions$cost$total)
              rownames(output_df) <- "combined actions cost"
            }
            colnames(output_df) <- time_steps_labels
            filename <- "total_action_costs_combined.csv"
            utils::write.csv(output_df, filename, row.names = TRUE)
            if ("cumulative" %in% names(results$actions$cost) &&
                is.list(results$actions$cost$cumulative$total)) {
              if (replicates > 1) {
                output_df <- sapply(results$actions$cost$cumulative$total,
                                    function(tot) tot)
              } else {
                output_df <-
                  as.data.frame(results$actions$cost$cumulative$total)
                rownames(output_df) <- "cumulative actions cost"
              }
              colnames(output_df) <- time_steps_labels
              filename <- "total_cumulative_action_costs_combined.csv"
              utils::write.csv(output_df, filename, row.names = TRUE)
            }
          }

          # Combined monetary impacts and action costs totals
          if ("cost" %in% names(results) && is.list(results$cost$total)) {
            if (replicates > 1) {
              output_df <- sapply(results$cost$total,
                                  function(tot) tot)
            } else {
              output_df <- as.data.frame(results$cost$total)
              rownames(output_df) <- "combined cost"
            }
            colnames(output_df) <- time_steps_labels
            filename <- "total_combined_costs.csv"
            utils::write.csv(output_df, filename, row.names = TRUE)
            if ("cumulative" %in% names(results$cost) &&
                is.list(results$cost$cumulative$total)) {
              if (replicates > 1) {
                output_df <- sapply(results$cost$cumulative$total,
                                    function(tot) tot)
              } else {
                output_df <-
                  as.data.frame(results$cost$cumulative$total)
                rownames(output_df) <- "cumulative cost"
              }
              colnames(output_df) <- time_steps_labels
              filename <- "total_cumulative_combined_costs.csv"
              utils::write.csv(output_df, filename, row.names = TRUE)
            }
          }
        }
      }
    } # actions
  }

  # Extended save plots as PNG files
  self$save_plots  <- function(width = 480, height = 480) {

    # Save population spread results
    super$save_plots(width = width, height = height)

    # All plots have time steps on x-axis
    plot_x_label <- paste0("Time steps (", step_units, ")")

    # Plot impacts when present
    if (length(impacts) > 0) {

      # Plots for each impact present
      if (!is.null(names(results$impacts))) {
        impact_i <- names(results$impacts)    # named impacts
      } else {
        impact_i <- 1:length(results$impacts) # indices
      }

      # Impact spatial totals present or spatially implicit
      if (include_collated) {
        is_present <- sapply(results$impacts, function(i) is.list(i$total))
      } else {
        is_present <- sapply(results$impacts, function(i) TRUE)
      }
      if (any(is_present)) {
        for (i in impact_i[is_present]) {

          # Plot impacts at each time step
          if (length(impact_i) == 1 && is.numeric(i)) {
            ic <- c("", "")
          } else {
            ic <- c(paste0("_", i), paste0(" : ", i))
          }
          units <- impacts[[i]]$get_context()$get_impact_measures()
          if (include_collated) {
            values_list <- list(total = sapply(results$impacts[[i]]$total,
                                               function(tot) tot))
          } else {
            if ("combined" %in% names(results$impacts[[i]])) {
              values_list <- list(
                combined = sapply(results$impacts[[i]]$combined,
                                  function(tot) tot))
            } else {
              a <- which(names(results$impacts[[i]]) != "cumulative")
              values_list <- lapply(results$impacts[[i]][a],
                                    function(i_a) sapply(i_a, function(a) a))
            }
          }
          for (a in 1:length(values_list)) {
            values <- values_list[[a]]
            if (length(values_list) > 1) {
              ac <- c(paste0("_", a), paste0(" : ", names(values_list)[a]))
            } else {
              ac <- c("", "")
            }
            if (replicates > 1) { # plot summary mean +/- 2 SD
              values <- list(mean = as.numeric(values["mean",,drop = FALSE]),
                             sd = as.numeric(values["sd",,drop = FALSE]))
              if (include_collated) {
                filename <- sprintf("total_impacts%s%s.png", ic[1], ac[1])
                main_title <- sprintf("Total impacts%s%s (mean +/- 2 SD)",
                                      ic[2], ac[2])
              } else {
                filename <- sprintf("impacts%s%s.png", ic[1], ac[1])
                main_title <- sprintf("Impacts%s%s (mean +/- 2 SD)", ic[2],
                                      ac[2])
              }
              grDevices::png(filename = filename, width = width,
                             height = height)
              graphics::plot(0:time_steps, values$mean, type = "l",
                             main = main_title,
                             xlab = plot_x_label,
                             ylab = sprintf("Impact (%s)", units),
                             ylim = c(0, 1.1*max(values$mean + 2*values$sd)))
              graphics::lines(0:time_steps, values$mean + 2*values$sd,
                              lty = "dashed")
              graphics::lines(0:time_steps,
                              pmax(0, values$mean - 2*values$sd),
                              lty = "dashed")
              invisible(grDevices::dev.off())
            } else {
              if (include_collated) {
                filename <- sprintf("total_impacts%s%s.png", ic[1], ac[1])
                main_title <- sprintf("Total impacts%s%s", ic[2], ac[2])
              } else {
                filename <- sprintf("impacts%s%s.png", ic[1], ac[1])
                main_title <- sprintf("Impacts%s%s", ic[2], ac[2])
              }
              grDevices::png(filename = filename, width = width,
                             height = height)
              graphics::plot(0:time_steps, values, type = "l",
                             main = main_title,
                             xlab = plot_x_label,
                             ylab = sprintf("Impact (%s)", units),
                             ylim = c(0, 1.1*max(values)))
              invisible(grDevices::dev.off())
            }
          }
        }
      }

      # Cumulative impact spatial totals present or spatially implicit
      if (include_collated) {
        is_present <- sapply(results$impacts,
                             function(i) is.list(i$cumulative$total))
      } else {
        is_present <- sapply(results$impacts,
                             function(i) is.list(i$cumulative))
      }
      if (any(is_present)) {
        for (i in impact_i[is_present]) {

          # Plot impacts at each time step
          if (length(impact_i) == 1 && is.numeric(i)) {
            ic <- c("", "")
          } else {
            ic <- c(paste0("_", i), paste0(" : ", i))
          }
          units <- impacts[[i]]$get_context()$get_impact_measures()
          if (include_collated) {
            values_list <-
              list(total = sapply(results$impacts[[i]]$cumulative$total,
                                  function(tot) tot))
          } else {
            if ("combined" %in% names(results$impacts[[i]]$cumulative)) {
              values_list <- list(
                combined = sapply(results$impacts[[i]]$cumulative$combined,
                                  function(tot) tot))
            } else {
              values_list <- lapply(results$impacts[[i]]$cumulative,
                                    function(i_a) sapply(i_a, function(a) a))
            }
          }
          for (a in 1:length(values_list)) {
            values <- values_list[[a]]
            if (length(values_list) > 1) {
              ac <- c(paste0("_", a), paste0(" : ", names(values_list)[a]))
            } else {
              ac <- c("", "")
            }
            if (replicates > 1) { # plot summary mean +/- 2 SD
              values <- list(mean = as.numeric(values["mean",,drop = FALSE]),
                             sd = as.numeric(values["sd",,drop = FALSE]))
              if (include_collated) {
                filename <- sprintf("total_cumulative_impacts%s%s.png",
                                    ic[1], ac[1])
                main_title <-
                  sprintf("Total cumulative impacts%s%s (mean +/- 2 SD)",
                          ic[2], ac[2])
              } else {
                filename <- sprintf("cumulative_impacts%s%s.png", ic[1], ac[1])
                main_title <- sprintf("Cumulative impacts%s%s (mean +/- 2 SD)",
                                      ic[2], ac[2])
              }
              grDevices::png(filename = filename, width = width,
                             height = height)
              graphics::plot(0:time_steps, values$mean, type = "l",
                             main = main_title,
                             xlab = plot_x_label,
                             ylab = sprintf("Cumulative impact (%s)", units),
                             ylim = c(0, 1.1*max(values$mean + 2*values$sd)))
              graphics::lines(0:time_steps, values$mean + 2*values$sd,
                              lty = "dashed")
              graphics::lines(0:time_steps,
                              pmax(0, values$mean - 2*values$sd),
                              lty = "dashed")
              invisible(grDevices::dev.off())
            } else {
              if (include_collated) {
                filename <- sprintf("total_cumulative_impacts%s%s.png",
                                    ic[1], ac[1])
                main_title <- sprintf("Total cumulative impacts%s%s",
                                      ic[2], ac[2])
              } else {
                filename <- sprintf("cumulative_impacts%s%s.png", ic[1], ac[1])
                main_title <- sprintf("Cumulative impacts%s%s", ic[2], ac[2])
              }
              grDevices::png(filename = filename, width = width,
                             height = height)
              graphics::plot(0:time_steps, values, type = "l",
                             main = main_title,
                             xlab = plot_x_label,
                             ylab = sprintf("Cumulative impact (%s)", units),
                             ylim = c(0, 1.1*max(values)))
              invisible(grDevices::dev.off())
            }
          }
        }
      }
    }

    # Plot actions when present
    if (length(actions) > 0) {
      if (!is.null(names(actions))) {
        action_i <- names(actions)    # named actions
      } else {
        action_i <- 1:length(actions) # indices
      }
      for (i in action_i) {

        # Plot action totals/values at each time step
        if (length(action_i) == 1 && is.numeric(i)) {
          ic <- c("", "")
        } else {
          ic <- c(paste0("_", i), paste0(" : ", i))
        }

        # Action label/key/name. Direct action?
        a_lab <- actions[[i]]$get_label()
        direct_action <-
          (a_lab %in%  c("detected", "control_search_destroy", "removed"))
        a_name <- a_key <- sub("control_", "", a_lab, fixed = TRUE)
        if (actions[[i]]$get_type() == "control") {
          a_key <- paste0(a_key, "_control")
          if (a_key == "search_destroy") {
            a_name <- "found & destroyed"
          } else {
            a_name <- paste(a_name, "control")
          }
        }
        if (direct_action) {
          plot_y_label <- sprintf("Number %s", a_name)
        } else {
          plot_y_label <- sprintf("Locations %s applied", a_name)
        }

        # Action cost and cumulative cost key/name
        if ("cost" %in% names(results$actions[[i]])) {
          if (a_key == "detected") {
            cost_name <- cost_key <- "detection"
          } else if (a_key == "removed") {
            cost_name <- cost_key <- "removal"
          } else {
            cost_key <- a_key
            if (a_key == "search_destroy") {
              cost_name <- "search & destroy control"
            } else {
              cost_name <- a_name
            }
          }
        }

        # Resolve the number of (combined) stages used in the results
        result_stages <- stages
        if (is.null(stages) || is.numeric(combine_stages) || !direct_action) {
          result_stages <- 1
        }

        # Stage label for plot headings and files
        stage_label <- ""
        stage_file <- ""
        if (population_model$get_type() == "stage_structured" &&
            direct_action) {
          if (is.numeric(stages) && is.null(combine_stages)) {
            stage_label <- paste0(stage_labels, " ")
            stage_file <- paste0("_stage_", 1:result_stages)
          } else if (is.numeric(combine_stages)) {
            if (length(combine_stages) == 1) {
              stage_label <- paste0(
                attr(population_model$get_growth(),
                     "labels")[combine_stages], " ")
            } else {
              stage_label <- paste0(sprintf(
                "stages %s-%s", min(combine_stages), max(combine_stages)),
                " ")
            }
          }
        }

        # Plot per result stage
        for (s in 1:result_stages) {

          # Plot action totals/values for result stage
          if (include_collated) {
            a <- "total"
          } else {
            a <- a_lab
          }
          if (replicates > 1) {
            if (include_collated || direct_action) {
              values <- sapply(results$actions[[i]][[a]],
                               function(tot) as.data.frame(
                                 lapply(tot, function(m) m[s])))
            } else {
              values <- t(as.matrix(sapply(results$actions[[i]][[a]],
                                           function(tot) tot$mean[s])))
              rownames(values) <- "mean"
            }
          } else {
            values <- sapply(results$actions[[i]][[a]],
                             function(tot) tot[s])
          }
          if (replicates > 1) { # plot summary mean +/- 2 SD
            if (include_collated || direct_action) {
              values <- list(mean = as.numeric(values["mean",,drop = FALSE]),
                             sd = as.numeric(values["sd",,drop = FALSE]))
            } else {
              values <- list(mean = as.numeric(values["mean",,drop = FALSE]))
            }
            if (include_collated) {
              filename <- sprintf("total_actions%s_%s%s.png", ic[1], a_key,
                                  stage_file[s])
              main_title <- sprintf("Total actions%s %s%s (mean +/- 2 SD)",
                                    ic[2], stage_label[s], a_name)
            } else {
              filename <- sprintf("actions%s_%s%s.png", ic[1], a_key,
                                  stage_file[s])
              if (direct_action) {
                main_title <- sprintf("Actions%s %s%s (mean +/- 2 SD)", ic[2],
                                      stage_label[s], a_name)
              } else {
                main_title <- sprintf("Actions%s %s%s (mean)", ic[2],
                                      stage_label[s], a_name)
              }
            }
            if (include_collated || direct_action) {
              ylim <- c(0, 1.1*max(values$mean + 2*values$sd))
            } else {
              ylim <- c(0, 1.1*max(values$mean))
            }
            grDevices::png(filename = filename, width = width, height = height)
            graphics::plot(0:time_steps, values$mean, type = "l",
                           main = main_title,
                           xlab = plot_x_label,
                           ylab = plot_y_label,
                           ylim = ylim)
            if (include_collated || direct_action) {
              graphics::lines(0:time_steps, values$mean + 2*values$sd,
                              lty = "dashed")
              graphics::lines(0:time_steps,
                              pmax(0, values$mean - 2*values$sd),
                              lty = "dashed")
            }
            invisible(grDevices::dev.off())
          } else {
            if (include_collated) {
              filename <- sprintf("total_actions%s_%s%s.png", ic[1], a_key,
                                  stage_file[s])
              main_title <- sprintf("Total actions%s %s%s", ic[2],
                                    stage_label[s], a_key)
            } else {
              filename <- sprintf("actions%s_%s%s.png", ic[1], a_key,
                                  stage_file[s])
              main_title <- sprintf("Actions%s %s%s", ic[2], stage_label[s],
                                    a_key)
            }
            grDevices::png(filename = filename, width = width,
                           height = height)
            graphics::plot(0:time_steps, values, type = "l",
                           main = main_title,
                           xlab = plot_x_label,
                           ylab = plot_y_label,
                           ylim = c(0, 1.1*max(values)))
            invisible(grDevices::dev.off())
          }
        }

        # Plot action cost and cumulative cost
        if ("cost" %in% names(results$actions[[i]])) {

          # Unit label
          unit <- attr(results$actions[[i]]$cost, "unit")
          if (!is.null(unit) && unit != "") {
            unit_lab <- paste0(" (", unit, ")")
          } else {
            unit_lab <- ""
          }

          # Plot action totals/values
          if (include_collated) {
            a <- "total"
          } else {
            a <- a_lab
          }
          if (replicates > 1) {
            values <- sapply(results$actions[[i]]$cost[[a]],
                             function(tot) as.data.frame(tot))
          } else {
            values <- as.numeric(results$actions[[i]]$cost[[a]])
          }
          if (replicates > 1) { # plot summary mean +/- 2 SD
            values <- list(mean = as.numeric(values["mean",,drop = FALSE]),
                           sd = as.numeric(values["sd",,drop = FALSE]))
            if (include_collated) {
              filename <- sprintf("total_action_costs%s_%s.png", ic[1],
                                  cost_key)
              main_title <- sprintf("Total action costs%s %s (mean +/- 2 SD)",
                                    ic[2], cost_name)
            } else {
              filename <- sprintf("action_costs%s_%s.png", ic[1], cost_key)
              main_title <- sprintf("Action costs%s %s (mean +/- 2 SD)",
                                    ic[2], cost_name)
            }
            grDevices::png(filename = filename, width = width, height = height)
            graphics::plot(0:time_steps, values$mean, type = "l",
                           main = main_title,
                           xlab = plot_x_label,
                           ylab = sprintf("Cost%s", unit_lab),
                           ylim = c(0, 1.1*max(values$mean + 2*values$sd)))
            graphics::lines(0:time_steps, values$mean + 2*values$sd,
                            lty = "dashed")
            graphics::lines(0:time_steps,
                            pmax(0, values$mean - 2*values$sd),
                            lty = "dashed")
            invisible(grDevices::dev.off())
          } else {
            if (include_collated) {
              filename <- sprintf("total_action_costs%s_%s.png", ic[1],
                                  cost_key)
              main_title <- sprintf("Total action costs%s %s", ic[2],
                                    cost_name)
            } else {
              filename <- sprintf("action_costs%s_%s.png", ic[1], cost_key)
              main_title <- sprintf("Action costs%s %s", ic[2], cost_name)
            }
            grDevices::png(filename = filename, width = width,
                           height = height)
            graphics::plot(0:time_steps, values, type = "l",
                           main = main_title,
                           xlab = plot_x_label,
                           ylab = sprintf("Cost%s", unit_lab),
                           ylim = c(0, 1.1*max(values)))
            invisible(grDevices::dev.off())
          }
          if ("cumulative" %in% names(results$actions[[i]]$cost)) {
            if (replicates > 1) {
              values <- sapply(results$actions[[i]]$cost$cumulative[[a]],
                               function(tot) as.data.frame(tot))
            } else {
              values <- as.numeric(results$actions[[i]]$cost$cumulative[[a]])
            }
            if (replicates > 1) { # plot summary mean +/- 2 SD
              values <- list(mean = as.numeric(values["mean",,drop = FALSE]),
                             sd = as.numeric(values["sd",,drop = FALSE]))
              if (include_collated) {
                filename <- sprintf("total_cumulative_action_costs%s_%s.png",
                                    ic[1], cost_key)
                main_title <-
                  sprintf("Total cumulative action costs%s %s (mean +/- 2 SD)",
                          ic[2], cost_name)
              } else {
                filename <- sprintf("cumulative_action_costs%s_%s.png",
                                    ic[1], cost_key)
                main_title <-
                  sprintf("Cumulative action costs%s %s (mean +/- 2 SD)",
                          ic[2], cost_name)
              }
              grDevices::png(filename = filename, width = width,
                             height = height)
              graphics::plot(0:time_steps, values$mean, type = "l",
                             main = main_title,
                             xlab = plot_x_label,
                             ylab = sprintf("Cumulative cost%s", unit_lab),
                             ylim = c(0, 1.1*max(values$mean + 2*values$sd)))
              graphics::lines(0:time_steps, values$mean + 2*values$sd,
                              lty = "dashed")
              graphics::lines(0:time_steps,
                              pmax(0, values$mean - 2*values$sd),
                              lty = "dashed")
              invisible(grDevices::dev.off())
            } else {
              if (include_collated) {
                filename <- sprintf("total_cumulative_action_costs%s_%s.png",
                                    ic[1], cost_key)
                main_title <- sprintf("Total cumulative action costs%s %s",
                                      ic[2], cost_name)
              } else {
                filename <- sprintf("cumulative_action_costs%s_%s.png",
                                    ic[1], cost_key)
                main_title <- sprintf("Cumulative action costs%s %s",
                                      ic[2], cost_name)
              }
              grDevices::png(filename = filename, width = width,
                             height = height)
              graphics::plot(0:time_steps, values, type = "l",
                             main = main_title,
                             xlab = plot_x_label,
                             ylab = sprintf("Cumulative cost%s", unit_lab),
                             ylim = c(0, 1.1*max(values)))
              invisible(grDevices::dev.off())
            }
          }
        }
      }

      # Plot combined action costs and/or monetary impacts
      if ("cost" %in% names(results$actions) || "cost" %in% names(results)) {

        # Combined action cost
        if ("cost" %in% names(results$actions)) {

          # Unit label
          unit <- attr(results$actions$cost, "unit")
          if (!is.null(unit) && unit != "") {
            unit_lab <- paste0(" (", unit, ")")
          } else {
            unit_lab <- ""
          }

          # Plot action totals/values
          if (include_collated) {
            a <- "total"
          } else {
            a <- "combined"
          }
          if (replicates > 1) {
            values <- sapply(results$actions$cost[[a]],
                             function(tot) as.data.frame(tot))
          } else {
            values <- as.numeric(results$actions$cost[[a]])
          }
          if (replicates > 1) { # plot summary mean +/- 2 SD
            values <- list(mean = as.numeric(values["mean",,drop = FALSE]),
                           sd = as.numeric(values["sd",,drop = FALSE]))
            if (include_collated) {
              filename <- "total_action_costs_combined.png"
              main_title <- "Total action costs : combined (mean +/- 2 SD)"
            } else {
              filename <- "action_costs_combined.png"
              main_title <- "Action costs : combined (mean +/- 2 SD)"
            }
            grDevices::png(filename = filename, width = width, height = height)
            graphics::plot(0:time_steps, values$mean, type = "l",
                           main = main_title,
                           xlab = plot_x_label,
                           ylab = sprintf("Cost%s", unit_lab),
                           ylim = c(0, 1.1*max(values$mean + 2*values$sd)))
            graphics::lines(0:time_steps, values$mean + 2*values$sd,
                            lty = "dashed")
            graphics::lines(0:time_steps,
                            pmax(0, values$mean - 2*values$sd),
                            lty = "dashed")
            invisible(grDevices::dev.off())
          } else {
            if (include_collated) {
              filename <- "total_action_costs_combined.png"
              main_title <- "Total action costs : combined"
            } else {
              filename <- "action_costs_combined.png"
              main_title <- "Action costs : combined "
            }
            grDevices::png(filename = filename, width = width,
                           height = height)
            graphics::plot(0:time_steps, values, type = "l",
                           main = main_title,
                           xlab = plot_x_label,
                           ylab = sprintf("Cost%s", unit_lab),
                           ylim = c(0, 1.1*max(values)))
            invisible(grDevices::dev.off())
          }
          if ("cumulative" %in% names(results$actions$cost)) {
            if (replicates > 1) {
              values <- sapply(results$actions$cost$cumulative[[a]],
                               function(tot) as.data.frame(tot))
            } else {
              values <- as.numeric(results$actions$cost$cumulative[[a]])
            }
            if (replicates > 1) { # plot summary mean +/- 2 SD
              values <- list(mean = as.numeric(values["mean",,drop = FALSE]),
                             sd = as.numeric(values["sd",,drop = FALSE]))
              if (include_collated) {
                filename <- "total_cumulative_action_costs_combined.png"
                main_title <-
                  "Total cumulative action costs : combined (mean +/- 2 SD)"
              } else {
                filename <- "cumulative_action_costs_combined.png"
                main_title <-
                  "Cumulative action costs : combined (mean +/- 2 SD)"
              }
              grDevices::png(filename = filename, width = width, height = height)
              graphics::plot(0:time_steps, values$mean, type = "l",
                             main = main_title,
                             xlab = plot_x_label,
                             ylab = sprintf("Cumulative cost%s", unit_lab),
                             ylim = c(0, 1.1*max(values$mean + 2*values$sd)))
              graphics::lines(0:time_steps, values$mean + 2*values$sd,
                              lty = "dashed")
              graphics::lines(0:time_steps,
                              pmax(0, values$mean - 2*values$sd),
                              lty = "dashed")
              invisible(grDevices::dev.off())
            } else {
              if (include_collated) {
                filename <- "total_cumulative_action_costs_combined.png"
                main_title <- "Total cumulative action costs : combined"
              } else {
                filename <- "cumulative_action_costs_combined.png"
                main_title <- "Cumulative action costs : combined"
              }
              grDevices::png(filename = filename, width = width,
                             height = height)
              graphics::plot(0:time_steps, values, type = "l",
                             main = main_title,
                             xlab = plot_x_label,
                             ylab = sprintf("Cumulative cost%s", unit_lab),
                             ylim = c(0, 1.1*max(values)))
              invisible(grDevices::dev.off())
            }
          }
        }

        # Combined monetary impacts and action costs
        if ("cost" %in% names(results)) {

          # Unit label
          unit <- attr(results$cost, "unit")
          if (!is.null(unit) && unit != "") {
            unit_lab <- paste0(" (", unit, ")")
          } else {
            unit_lab <- ""
          }

          # Plot totals/values
          if (include_collated) {
            a <- "total"
          } else {
            a <- "combined"
          }
          if (replicates > 1) {
            values <- sapply(results$cost[[a]],
                             function(tot) as.data.frame(tot))
          } else {
            values <- as.numeric(results$cost[[a]])
          }
          if (replicates > 1) { # plot summary mean +/- 2 SD
            values <- list(mean = as.numeric(values["mean",,drop = FALSE]),
                           sd = as.numeric(values["sd",,drop = FALSE]))
            if (include_collated) {
              filename <- "total_combined_costs.png"
              main_title <-
                "Total combined impact and action costs (mean +/- 2 SD)"
            } else {
              filename <- "combined_costs.png"
              main_title <- "Combined impact and action costs (mean +/- 2 SD)"
            }
            grDevices::png(filename = filename, width = width, height = height)
            graphics::plot(0:time_steps, values$mean, type = "l",
                           main = main_title,
                           xlab = plot_x_label,
                           ylab = sprintf("Cost%s", unit_lab),
                           ylim = c(0, 1.1*max(values$mean + 2*values$sd)))
            graphics::lines(0:time_steps, values$mean + 2*values$sd,
                            lty = "dashed")
            graphics::lines(0:time_steps,
                            pmax(0, values$mean - 2*values$sd),
                            lty = "dashed")
            invisible(grDevices::dev.off())
          } else {
            if (include_collated) {
              filename <- "total_combined_costs.png"
              main_title <- "Total combined impact and action costs"
            } else {
              filename <- "combined_costs.png"
              main_title <- "Combined impact and action costs"
            }
            grDevices::png(filename = filename, width = width,
                           height = height)
            graphics::plot(0:time_steps, values, type = "l",
                           main = main_title,
                           xlab = plot_x_label,
                           ylab = sprintf("Cost%s", unit_lab),
                           ylim = c(0, 1.1*max(values)))
            invisible(grDevices::dev.off())
          }
          if ("cumulative" %in% names(results$cost)) {
            if (replicates > 1) {
              values <- sapply(results$cost$cumulative[[a]],
                               function(tot) as.data.frame(tot))
            } else {
              values <- as.numeric(results$cost$cumulative[[a]])
            }
            if (replicates > 1) { # plot summary mean +/- 2 SD
              values <- list(mean = as.numeric(values["mean",,drop = FALSE]),
                             sd = as.numeric(values["sd",,drop = FALSE]))
              if (include_collated) {
                filename <- "total_cumulative_combined_costs.png"
                main_title <-
                  "Total cumulative impact and action costs (mean +/- 2 SD)"
              } else {
                filename <- "cumulative_combined_costs.png"
                main_title <-
                  "Cumulative impact and action costs (mean +/- 2 SD)"
              }
              grDevices::png(filename = filename, width = width,
                             height = height)
              graphics::plot(0:time_steps, values$mean, type = "l",
                             main = main_title,
                             xlab = plot_x_label,
                             ylab = sprintf("Cumulative cost%s", unit_lab),
                             ylim = c(0, 1.1*max(values$mean + 2*values$sd)))
              graphics::lines(0:time_steps, values$mean + 2*values$sd,
                              lty = "dashed")
              graphics::lines(0:time_steps,
                              pmax(0, values$mean - 2*values$sd),
                              lty = "dashed")
              invisible(grDevices::dev.off())
            } else {
              if (include_collated) {
                filename <- "total_cumulative_combined_costs.png"
                main_title <- "Total cumulative impact and action costs"
              } else {
                filename <- "cumulative_combined_costs.png"
                main_title <- "Cumulative impact and action costs"
              }
              grDevices::png(filename = filename, width = width,
                             height = height)
              graphics::plot(0:time_steps, values, type = "l",
                             main = main_title,
                             xlab = plot_x_label,
                             ylab = sprintf("Cumulative cost%s", unit_lab),
                             ylim = c(0, 1.1*max(values)))
              invisible(grDevices::dev.off())
            }
          }
        }
      }
    } # actions
  }

  return(self)
}
