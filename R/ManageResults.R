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
  if (length(actions) > 0) {
    results$actions <- lapply(actions, function(actions_i) {
      actions_list <- list()
      actions_list[[actions_i$get_label()]] <- zeros$collated_steps
      actions_list$total <- zeros$total_steps
      actions_list
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
    }

    # Collate management actions
    if (length(actions) > 0) {

      if (replicates > 1) { # summaries

        # Calculates running mean and standard deviation (note: variance*r is
        # stored as SD and transformed at the final replicate and time step)

        # Place applied actions in existing results structure
        for (i in 1:length(actions)) {

          # All applied actions recorded in collation steps only
          if (tm %% collation_steps == 0) {
            a <- actions[[i]]$get_label()
            previous_mean <- results$actions[[i]][[a]][[tmc]]$mean

            # results$actions[[i]][[a]][[tmc]]$mean <<-
            #   previous_mean + (calc_impacts[[i]][[a]] - previous_mean)/r
            # previous_sd <- results$actions[[i]][[a]][[tmc]]$sd
            # results$actions[[i]][[a]][[tmc]]$sd <<-
            #   (previous_sd + ((calc_impacts[[i]][[a]] - previous_mean)*
            #                     (calc_impacts[[i]][[a]] -
            #                        results$impacts[[i]][[a]][[tmc]]$mean)))
          }

          # Total applied actions at every time step
          if ("total" %in% names(results$actions[[i]])) {
            previous_mean <- results$actions[[i]]$total[[tmc]]$mean
            # total_impact <- sum(calc_impacts[[i]]$combined)
            # results$impacts[[i]]$total[[tmc]]$mean <<-
            #   previous_mean + (total_impact - previous_mean)/r
            # previous_sd <- results$impacts[[i]]$total[[tmc]]$sd
            # results$impacts[[i]]$total[[tmc]]$sd <<-
            #   (previous_sd + ((total_impact - previous_mean)*
            #                     (total_impact -
            #                        results$impacts[[i]]$total[[tmc]]$mean)))
          }
        }

      } else {

        # Place applied actions in existing results structure
        for (i in 1:length(actions)) {

          # All applied actions recorded in collation steps only
          if (tm %% collation_steps == 0) {
            # for (a in names(calc_impacts[[i]])) {
            #   results$impacts[[i]][[a]][[tmc]] <<- calc_impacts[[i]][[a]]
            # }
          }

          # Total combined aspects at every time step
          if ("total" %in% names(results$actions[[i]])) {
            # results$impacts[[i]]$total[[tmc]] <<-
            #   sum(calc_impacts[[i]]$combined)
          }
        }
      }
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
      super$save_rasters(...)

      # Replicate summaries or single replicate
      if (replicates > 1) {
        summaries <- c("mean", "sd")
      } else {
        summaries <- ""
      }

      # Save impacts
      if (length(impacts) > 0) {

        # Save rasters for each impact aspect at each time step
        if (!is.null(names(results$impacts))) {
          impact_i <- names(results$impacts)    # named impacts
        } else {
          impact_i <- 1:length(results$impacts) # indices
        }
        for (i in impact_i) {
          aspects <- names(results$impacts[[i]])
          for (a in aspects[which(aspects != "total")]) {
            for (tmc in names(results$impacts[[i]][[a]])) {
              for (s in summaries) {

                # Copy impacts into a raster
                if (replicates > 1) {
                  output_rast <-
                    region$get_rast(results$impacts[[i]][[a]][[tmc]][[s]])
                  s <- paste0("_", s)
                } else {
                  output_rast <-
                    region$get_rast(results$impacts[[i]][[a]][[tmc]])
                }

                # Write raster to file
                if (length(impact_i) == 1 && is.numeric(i)) {
                  ic <- ""
                } else {
                  ic <- paste0("_", i)
                }
                filename <- sprintf(
                  paste0("impacts%s_%s_t%0", nchar(as.character(time_steps)),
                         "d%s.tif"),
                  ic, a, as.integer(tmc), s)
                terra::writeRaster(output_rast, filename, ...)
              }
            }
          }
        }
      }

      # Save others
      # TODO
    }
  }

  # Save collated (patch only) and summary (both) results as CSV files
  self$save_csv  <- function() {

    # Save population spread results
    super$save_csv()

    # Replicate summaries or single replicate
    if (replicates > 1) {
      summaries <- c("mean", "sd")
    } else {
      summaries <- ""
    }

    # Location coordinates and labels
    if (region$get_type() == "patch") {
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

      # Collated results for patch only
      if (region$get_type() == "patch") {

        # Combine coordinates and impacts
        output_df <- list()
        for (i in impact_i) {
          output_df[[i]] <- list()
          aspects <- names(results$impacts[[i]])
          for (a in aspects[which(aspects != "total")]) {
            for (tmc in names(results$impacts[[i]][[a]])) {
              if (replicates > 1) {
                if (is.null(output_df[[i]][[tmc]])) {
                  output_df[[i]][[tmc]] <- list()
                }
                for (s in summaries) {
                  if (is.null(output_df[[i]][[tmc]][[s]])) {
                    output_df[[i]][[tmc]][[s]] <- coords
                  }
                  output_df[[i]][[tmc]][[s]][[a]] <-
                    results$impacts[[i]][[a]][[tmc]][[s]]
                }
              } else {
                if (is.null(output_df[[i]][[tmc]])) {
                  output_df[[i]][[tmc]] <- coords
                }
                output_df[[i]][[tmc]][[a]] <-
                  results$impacts[[i]][[a]][[tmc]]
              }
            }
          }
        }

        # Write to CSV files
        for (i in impact_i) {
          for (tmc in names(output_df[[i]])) {
            for (s in summaries) {
              if (length(impact_i) == 1 && is.numeric(i)) {
                ic <- ""
              } else {
                ic <- paste0("_", i)
              }
              if (replicates > 1) {
                s <- paste0("_", s)
              }
              filename <- sprintf(
                paste0("impacts%s_t%0",
                       nchar(as.character(time_steps)), "d%s.csv"),
                ic, as.integer(tmc), s)
              if (replicates > 1) {
                utils::write.csv(output_df[[i]][[tmc]][[s]], filename,
                                 row.names = FALSE)
              } else {
                utils::write.csv(output_df[[i]][[tmc]], filename,
                                 row.names = FALSE)
              }
            }
          }
        }
      }

      # Impact totals when present
      totals_present <- sapply(results$impacts, function(i) is.list(i$total))
      if (any(totals_present)) {
        time_steps_labels <- sprintf(
          paste0("t%0", nchar(as.character(time_steps)), "d"),
          as.integer(0:time_steps))
        for (i in impact_i[totals_present]) {

          # Collect totals at each time step
          if (replicates > 1) {
            totals <- sapply(results$impacts[[i]]$total, function(tot) tot)
          } else {
            totals <- array(sapply(results$impacts[[i]]$total,
                                   function(tot) tot),
                            c(1, time_steps + 1))
          }
          colnames(totals) <- time_steps_labels

          # Write to CSV file
          if (length(impact_i) == 1 && is.numeric(i)) {
            ic <- ""
          } else {
            ic <- paste0("_", i)
          }
          filename <- sprintf("impact_totals%s.csv", ic)
          utils::write.csv(totals, filename,
                           row.names = (length(summaries) > 1))
        }
      }
    }

    # Save others
    # TODO
  }

  # Extended save plots as PNG files
  self$save_plots  <- function() {

    # Save population spread results
    super$save_plots()

    # All plots have time steps on x-axis
    plot_x_label <- paste0("Time steps (", step_units, ")")

    # Plot impact totals when present
    if (length(impacts) > 0) {
      totals_present <- sapply(results$impacts, function(i) is.list(i$total))
      if (any(totals_present)) {

        # Plots for each impact present
        if (!is.null(names(results$impacts))) {
          impact_i <- names(results$impacts)    # named impacts
        } else {
          impact_i <- 1:length(results$impacts) # indices
        }
        for (i in impact_i[totals_present]) {

          # Plot impact totals at each time step
          if (length(impact_i) == 1 && is.numeric(i)) {
            ic <- c("", "")
          } else {
            ic <- c(paste0("_", i), paste0(" : ", i))
          }
          units <- impacts[[i]]$get_context()$get_impact_measures()
          totals <- sapply(results$impacts[[i]]$total, function(tot) tot)
          if (replicates > 1) { # plot summary mean +/- 2 SD
            totals <- list(mean = as.numeric(totals["mean",,drop = FALSE]),
                           sd = as.numeric(totals["sd",,drop = FALSE]))
            grDevices::png(filename = sprintf("impact_totals%s.png", ic[1]))
            graphics::plot(0:time_steps, totals$mean, type = "l",
                           main = sprintf("Total impacts%s (mean +/- 2 SD)",
                                          ic[2]),
                           xlab = plot_x_label,
                           ylab = sprintf("Impact (%s)", units),
                           ylim = c(0, 1.1*max(totals$mean + 2*totals$sd)))
            graphics::lines(0:time_steps, totals$mean + 2*totals$sd,
                            lty = "dashed")
            graphics::lines(0:time_steps,
                            pmax(0, totals$mean - 2*totals$sd),
                            lty = "dashed")
            invisible(grDevices::dev.off())
          } else {
            grDevices::png(filename = sprintf("impact_totals%s.png", ic[1]))
            graphics::plot(0:time_steps, totals, type = "l",
                           main = sprintf("Total impacts%s", ic[2]),
                           xlab = plot_x_label,
                           ylab = sprintf("Impact (%s)", units),
                           ylim = c(0, 1.1*max(totals)))
            invisible(grDevices::dev.off())
          }
        }
      }
    }

    # results_o = results; results = get("results", envir = environment(results_o$get_list))
    # TODO

    # Save others
    # TODO
  }

  return(self)
}
