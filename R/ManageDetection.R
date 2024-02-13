#' Manage detection class builder
#'
#' Builds a class for simulating the application of management detection or
#' surveillance.
#'
#' @param region A \code{bsspread::Region} or inherited class object
#'   representing the spatial region (template) for the incursion management
#'   simulations.
#' @param population_model A \code{bsspread::Population} or inherited class
#'   object defining the population representation for the management
#'   simulations.
#' @param apply_stages Numeric vector of population stages (indices) to which
#'   management detection are applied. Default is all stages (when set to
#'   \code{NULL}).
#' @param ... Additional parameters.
#' @return A \code{ManageDetection} class object (list) containing a function
#'   for accessing attributes and applying simulated management detection:
#'   \describe{
#'     \item{\code{get_type()}}{Get the type of management action
#'       ("detection").}
#'     \item{\code{apply(n)}}{Apply management detection to a simulated
#'       population vector or matrix \code{n}, potentially with attached
#'       attributes relating to previously applied actions, and return the
#'       resulting population \code{n} along with attached attributes relating
#'       to the newly applied detection/surveillance.}
#'   }
#' @export
ManageDetection <- function(region, population_model,
                           apply_stages = NULL, ...) {
  UseMethod("ManageDetection")
}

#' @name ManageDetection
#' @export
ManageDetection.Region <- function(region, population_model,
                                  apply_stages = NULL, ...) {

  # Build via base class
  self <- ManageActions(region = region,
                        population_model = population_model,
                        type = "detection",
                        apply_stages = apply_stages,
                        class = "ManageDetection")

  # Detection/surveillance apply method
  self$apply <- function(x) return(x) # TODO

  return(self)
}
