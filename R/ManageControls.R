#' Manage controls class builder
#'
#' Builds a class for simulating the application of invasive species management
#' controls, such as suppressing growth, fertility, spread, or establishment.
#'
#' @param region A \code{bsspread::Region} or inherited class object
#'   representing the spatial region (template) for the incursion management
#'   simulations.
#' @param population_model A \code{bsspread::Population} or inherited class
#'   object defining the population representation for the management
#'   simulations.
#' @param stages Numeric vector of population stages (indices) to which
#'   management controls are applied. Default is all stages (when set to
#'   \code{NULL}).
#' @param schedule Vector of discrete simulation time steps in which to apply
#'   management controls. Default is all time steps (when set to \code{NULL}).
#' @param ... Additional parameters.
#' @return A \code{ManageControls} class object (list) containing a function
#'   for accessing attributes and applying simulated management controls:
#'   \describe{
#'     \item{\code{get_type()}}{Get the type of management action ("control").}
#'     \item{\code{get_label()}}{Get the management actions label used in
#'       simulation results (e.g. "control_growth").}
#'     \item{\code{get_stages()}}{Get the population stages to which management
#'       controls are applied.}
#'     \item{\code{get_schedule()}}{Get the scheduled simulation time steps in
#'       which management controls are applied.}
#'     \item{\code{apply(n)}}{Apply management controls to a simulated
#'       population vector or matrix \code{n}, potentially with attached
#'       attributes relating to previously applied actions, and return the
#'       resulting population \code{n} along with attached attributes relating
#'       to the newly applied controls.}
#'   }
#' @export
ManageControls <- function(region, population_model,
                           stages = NULL, schedule = NULL, ...) {
  UseMethod("ManageControls")
}

#' @name ManageControls
#' @export
ManageControls.Region <- function(region, population_model,
                                  stages = NULL, schedule = NULL, ...) {

  # Build via base class
  self <- ManageActions(region = region,
                        population_model = population_model,
                        type = "control",
                        stages = stages,
                        schedule = schedule,
                        class = "ManageControls")

  # Get results label
  self$get_label <- function() { # TODO
    return("control_")
  }

  # Control apply method
  self$apply <- function(x) return(x) # TODO

  return(self)
}
