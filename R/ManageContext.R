#' Manage context class builder
#'
#' Builds a class to represent the context of a biosecurity management resource
#' allocation or scenario analysis, including information about the invasive
#' species being managed, the of impacts considered, and the management
#' resources being utilised, plus the scope of the management specifying
#' whether to find optimal resource allocations across management responses,
#' invasive species, time, and/or space, or alternatively to analyse
#' simulations of management scenarios with different responses, or with and
#' without resources.
#'
#' @param species_names Vector of one or more invasive species (or genus)
#'   names.
#' @param species_types Vector of types of invasive species. One or more of
#'   \code{"pest"}, \code{"weed"}, or \code{"disease"}.
#' @param impact_types Vector of the types of impacts involved. Either
#'   \code{"none"} when impacts are not considered, or one or more of
#'   \code{"ecological"}, \code{"social"}, and/or \code{"economic"}.
#' @param action_types Vector of the types of management actions involved.
#'   Either \code{"none"} when actions are not considered, or one or more of
#'   \code{"surveillance"}, \code{"control"}, and/or \code{"removal"}.
#' @param manage_scope The scope of the resource management. One of
#' \code{"optimal"} resource allocation or management simulation
#' \code{"scenarios"}.
#' @param ... Additional parameters.
#' @return A \code{ManageContext} class object (list) containing functions for
#'   accessing attributes:
#'   \describe{
#'     \item{\code{get_species_names()}}{Get the invasive species names.}
#'     \item{\code{get_species_types()}}{Get the types of invasive species:
#'       "pest", "weed", or "disease".}
#'     \item{\code{get_impact_types()}}{Get the impact types.}
#'     \item{\code{get_action_types()}}{Get the management action types.}
#'     \item{\code{get_manage_scope()}}{Get the management scope.}
#'   }
#' @export
ManageContext <- function(species_names,
                          species_types = c("pest",
                                            "weed",
                                            "disease"),
                          impact_types = c("none",
                                           "ecological",
                                           "social",
                                           "economic"),
                          action_types = c("none",
                                           "surveillance",
                                           "control",
                                           "removal"),
                          manage_scope = c("optimal",
                                           "scenarios"), ...) {
  UseMethod("ManageContext")
}

#' @name ManageContext
#' @export
ManageContext.default <- function(species_names,
                                  species_types = c("pest",
                                                    "weed",
                                                    "disease"),
                                  impact_types = c("none",
                                                   "ecological",
                                                   "social",
                                                   "economic"),
                                  action_types = c("none",
                                                   "surveillance",
                                                   "control",
                                                   "removal"),
                                  manage_scope = c("optimal",
                                                   "scenarios"), ...) {
  # Match arguments to selections
  species_types <- match.arg(species_types, several.ok = TRUE)
  impact_types <- match.arg(impact_types, several.ok = TRUE)
  if ("none" %in% impact_types) {
    impact_types <- "none"
  }
  action_types <- match.arg(action_types, several.ok = TRUE)
  if ("none" %in% action_types) {
    action_types <- "none"
  }
  manage_scope <- match.arg(manage_scope)

  # Create a class structure
  self <- structure(list(), class = "ManageContext")

  # Get the invasive species name
  self$get_species_names <- function() {
    return(species_names)
  }

  # Get the types of invasive species
  self$get_species_types <- function() {
    return(species_types)
  }

  # Get the impact types
  self$get_impact_types <- function() {
    return(impact_types)
  }

  # Get the management action types
  self$get_action_types <- function() {
    return(action_types)
  }

  # Get the management scope
  self$get_manage_scope <- function() {
    return(manage_scope)
  }

  return(self)
}
