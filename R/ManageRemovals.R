#' Manage removals class builder
#'
#' Builds a class for simulating the application of management removals or
#' eradication of an invasive species.
#'
#' @param region A \code{bsspread::Region} or inherited class object
#'   representing the spatial region (template) for the incursion management
#'   simulations.
#' @param population_model A \code{bsspread::Population} or inherited class
#'   object defining the population representation for the management
#'   simulations.
#' @param removal_pr A single probability value (0-1), or vector of values for
#'   each location specified by the \code{region}, to represent the likelihood
#'   of removal at locations where the invasive species has be detected, as
#'   indicated via a population attribute when present, else removal is applied
#'   at all locations. Default is \code{1}, indicating all (detected)
#'   occurrences are removed.
#' @param radius The radius (m) of the removal. Stochastic removal is applied
#'   to all locations within the specified radius of each location where the
#'   invasive species has been detected. Default is \code{NULL}, indicating
#'   that removal is only applied at detected locations (when specified via a
#'   population attribute).
#' @param apply_stages Numeric vector of population stages (indices) to which
#'   management removals are applied. Default is all stages (when set to
#'   \code{NULL}).
#' @param ... Additional parameters.
#' @return A \code{ManageRemovals} class object (list) containing a function
#'   for accessing attributes and applying simulated management removals:
#'   \describe{
#'     \item{\code{get_type()}}{Get the type of management action ("removal").}
#'     \item{\code{apply(n)}}{Apply management removals to a simulated
#'       population vector or matrix \code{n}, potentially with attached
#'       attributes relating to previously applied actions, and return the
#'       resulting population \code{n} along with attached attributes relating
#'       to the newly applied removals.}
#'   }
#' @export
ManageRemovals <- function(region, population_model,
                           removal_pr = 1,
                           radius = NULL,
                           apply_stages = NULL, ...) {
  UseMethod("ManageRemovals")
}

#' @name ManageRemovals
#' @export
ManageRemovals.Region <- function(region, population_model,
                                  removal_pr = 1,
                                  radius = NULL,
                                  apply_stages = NULL, ...) {

  # Build via base class
  self <- ManageActions(region = region,
                        population_model = population_model,
                        type = "removal",
                        apply_stages = apply_stages,
                        class = "ManageRemovals")

  # Validate removal probability and radius
  if (!is.null(removal_pr) &&
      (!is.numeric(removal_pr) ||
       !length(removal_pr) %in% c(1, region$get_locations()))) {
    stop(paste("Removal probability should be a vector with a value for each",
               "region location."), call. = FALSE)
  }
  if (!is.null(radius) && (!is.numeric(radius) || radius < 0)) {
    stop("The radius (m) parameter must be numeric and >= 0.", call. = FALSE)
  }


  # Removal apply method
  self$apply <- function(x) return(x) # TODO

  return(self)
}
