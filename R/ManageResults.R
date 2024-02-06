#' Manage results class builder
#'
#' Builds a class for encapsulating, calculating, and collating incursion
#' management simulation results, including the population at each location at
#' each collation time step, the total population size and area occupied at
#' each time step, as well as spatio-temporal impacts and management action
#' quantities applied (e.g. removals). When simulations are replicated, summary
#' results (means and standard deviations) are produced. Extends the
#' \code{bsspread::Results} package module.
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
#' @return A \code{Results} class object (list) containing functions for
#'   calculating and collating results, as well as accessing lists of results
#'   and the simulation parameters used to produce them:
#'   \describe{
#'     \item{\code{collate(r, tm, n, calc_impacts)}}{Collate the results at
#'       simulation replicate \code{r} and time step \code{tm} using the
#'       current vector or array \code{n} representing the population at each
#'       location, as well as calculated impacts (list).}
#'     \item{\code{finalize()}}{Finalize the results collation (summary
#'       calculations).}
#'     \item{\code{as_list()}}{Return the results as a list (collated, total,
#'       area).}
#'     \item{\code{get_params()}}{Get the simulation parameters used.}
#'     \item{\code{save_rasters(...)}}{Save the collated results as raster TIF
#'       files when the region is grid-based. \code{Terra} raster write options
#'       may be passed to the function.}
#'     \item{\code{save_csv()}}{Save the collated results as comma-separated
#'       values (CSV) files when the region is patch-based. Also saves the
#'       population totals and area occupied to CSV files for both grid and
#'       patch-based region types.}
#'     \item{\code{save_plots()}}{Save plots of the population (staged) totals
#'       and the area occupied as PNG files.}
#'   }
#' @references
#'   Bradhurst, R., Spring, D., Stanaway, M., Milner, J., & Kompas, T. (2021).
#'   A generalised and scalable framework for modelling incursions,
#'   surveillance and control of plant and environmental pests.
#'   \emph{Environmental Modelling & Software}, 139, N.PAG.
#'   \doi{10.1016/j.envsoft.2021.105004}
#'
#'   García Adeva, J. J., Botha, J. H., & Reynolds, M. (2012). A simulation
#'   modelling approach to forecast establishment and spread of Bactrocera
#'   fruit flies. \emph{Ecological Modelling}, 227, 93–108.
#'   \doi{10.1016/j.ecolmodel.2011.11.026}
#' @export
ManageResults <- function(region, population_model,
                          impacts = list(),
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
                get_list = self$get_list, save_csv = self$save_csv,
                save_plots = self$save_plots)

  # Validate impact objects
  if (length(impacts) > 0 && (!is.list(impacts) ||
      !all(sapply(impacts, function(i) inherits(i, "ManageImpacts"))))) {
    stop(paste("Impacts must be a list of 'ManageImpacts' or inherited class",
               "objects."), call. = FALSE)
  }

  # Population stages (NULL or number of stages)
  stages <- population_model$get_stages() # TODO - needed?

  # Initialize additional incursion management result lists
  results <- list()
  zeros <- list(collated = rep(0L, region$get_locations()), total = 0L)
  if (replicates > 1) { # summaries
    zeros$collated <- list(mean = zeros$collated, sd = zeros$collated)
    zeros$total <- list(mean = zeros$total, sd = zeros$total)
  }
  zeros$collated_steps <- list()
  for (tm in as.character(c(0, seq(collation_steps, time_steps,
                                   by = collation_steps)))) {
    zeros$collated_steps[[tm]] <- zeros$collated
  }
  zeros$total_steps <- list()
  for (tm in as.character(0:time_steps)) {
    zeros$total_steps[[tm]] <- zeros$total
  }
  if (length(impacts) > 0) {
    results$impacts <- lapply(impacts, function(impacts_i) {
      impact_aspects <- list()
      for (a in impacts_i$get_context()$get_impact_scope()) {
        impact_aspects[[a]] <- zeros$collated_steps
      }
      if (impacts_i$includes_combined()) {
        impact_aspects$combined <- zeros$collated_steps
      }
      if (impacts_i$get_context()$get_valuation_type() == "monetary") {
        impact_aspects$total <- zeros$total_steps
      }
      impact_aspects
    })
  }
  rm(zeros)

  # Extended collate results
  self$collate <- function(r, tm, n, calc_impacts) {

    # Collate population spread results
    super$collate(r, tm, n)

    # Use character list index (allows initial time = 0 and collated times)
    tmc <- as.character(tm)

    # Collate impacts
    if (length(impacts) > 0) {

      if (replicates > 1) { # summaries

        # Calculates running mean and standard deviation (note: variance*r is
        # stored as SD and transformed at the final replicate and time step)

        # Place calculated impacts in existing results structure
        for (i in 1:length(calc_impacts)) {

          # All calculated impact aspects recorded in collation steps only
          if (tm %% collation_steps == 0) {
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

          # Total combined aspects at every time step
          if ("combined" %in% names(calc_impacts[[i]]) &&
              "total" %in% names(results$impacts[[i]])) {
            previous_mean <- results$impacts[[i]]$total[[tmc]]$mean
            total_impact <- sum(calc_impacts[[i]]$combined)
            results$impacts[[i]]$total[[tmc]]$mean <<-
              previous_mean + (total_impact - previous_mean)/r
            previous_sd <- results$impacts[[i]]$total[[tmc]]$sd
            results$impacts[[i]]$total[[tmc]]$sd <<-
              (previous_sd + ((total_impact - previous_mean)*
                                (total_impact -
                                   results$impacts[[i]]$total[[tmc]]$mean)))
          }
        }

      } else {

        # Place calculated impacts in existing results structure
        for (i in 1:length(calc_impacts)) {

          # All calculated impact aspects recorded in collation steps only
          if (tm %% collation_steps == 0) {
            for (a in names(calc_impacts[[i]])) {
              results$impacts[[i]][[a]][[tmc]] <<- calc_impacts[[i]][[a]]
            }
          }

          # Total combined aspects at every time step
          if ("combined" %in% names(calc_impacts[[i]]) &&
              "total" %in% names(results$impacts[[i]])) {
            results$impacts[[i]]$total[[tmc]] <<-
              sum(calc_impacts[[i]]$combined)
          }
        }
      }

      # TODO collate others
    }
  }

  # Extended finalize the results collation
  self$finalize <- function() {

    # Finalize population spread results
    super$finalize()

    if (replicates > 1) { # summaries

      # Finalize impact results
      if (length(impacts) > 0) {

        # Transform impact standard deviations
        for (i in 1:length(results$impacts)) {
          for (a in names(results$impacts[[i]])) {
            for (tmc in names(results$impacts[[i]][[a]])) {
              results$impacts[[i]][[a]][[tmc]]$sd <<-
                sqrt(results$impacts[[i]][[a]][[tmc]]$sd/(replicates - 1))
            }
          }
        }
      }

      # Finalize additional incursion management results
      # TODO others
    }
  }

  # Get results list
  self$get_list <- function() {
    return(c(super$get_list(), results))
  }

  # Save collated results as raster files
  if (region$get_type() == "grid") {
    self$save_rasters  <- function(...) {

      # Save population spread results
      super$save_rasters()

      # Save additional incursion management results
      # TODO
    }
  }

  # Save collated (patch only) and summary (both) results as CSV files
  self$save_csv  <- function() {

    # Save population spread results
    super$save_csv()

    # Save additional incursion management results
    # TODO
  }

  # Extended save plots as PNG files
  self$save_plots  <- function() {

    # Save population spread results
    super$save_plots()

    # Save additional incursion management results
    # TODO
  }

  return(self)
}
