#' Manage removals class builder
#'
#' Builds a class for simulating the application of management removals or
#' eradication.
#'
#' @param region A \code{bsspread::Region} or inherited class object
#'   representing the spatial region (template) for the incursion management
#'   simulations.
#' @param population_model A \code{bsspread::Population} or inherited class
#'   object defining the population representation for the management
#'   simulations.
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
                           apply_stages = NULL, ...) {
  UseMethod("ManageRemovals")
}

#' @name ManageRemovals
#' @export
ManageRemovals.Region <- function(region, population_model,
                                  apply_stages = NULL, ...) {

  # Build via base class
  self <- ManageActions(region = region,
                        population_model = population_model,
                        type = "removal",
                        apply_stages = apply_stages,
                        class = "ManageRemovals")

  # Removal apply method
  self$apply <- function(x) return(x) # TODO

  return(self)
}
