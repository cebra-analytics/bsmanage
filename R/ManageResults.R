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
#'     \item{\code{collate(r, tm, n, impacts)}}{Collate the results at
#'       simulation replicate \code{r} and time step \code{tm} using the
#'       current vector or array \code{n} representing the population at each
#'       location, as well as calculated impacts.}
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
                                 time_steps = 1,
                                 step_duration = 1,
                                 step_units = "years",
                                 collation_steps = 1,
                                 replicates = 1,
                                 combine_stages = NULL, ...) {

  # Create a bsspread::Results class structure (includes validation)
  super <- bsspread::Results(region, population_model,
                             time_steps = time_steps,
                             step_duration = step_duration,
                             step_units = step_units,
                             collation_steps = collation_steps,
                             replicates = replicates,
                             combine_stages = combine_stages, ...)

  # Population stages (NULL or number of stages)
  stages <- population_model$get_stages()

  # Initialize additional incursion management result lists
  results <- list()
  # TODO

  # Create a class structure
  self <- structure(list(), class = "Results")

  # Extended collate results
  self$collate <- function(r, tm, n, impacts) {

    # Collate population spread results
    super$collate(r, tm, n)

    # Collate additional incursion management results
    # TODO
  }

  # Extended finalize the results collation
  self$finalize <- function() {

    # Finalize population spread results
    super$finalize()

    # Finalize additional incursion management results
    # TODO
  }

  # Get results list
  self$get_list <- function() {
    return(c(super$get_list(), results))
  }

  # Get simulation parameters
  self$get_params  <- function() {
    return(super$get_params(), list())
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
