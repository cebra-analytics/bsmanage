#' Manage context class builder
#'
#' Builds a class to represent the context of a biosecurity management resource
#' allocation, including information about the invasive pest, weed or disease
#' species or genus being monitored, the type of management resources
#' allocated, the purpose of the management, and the status of the threat.
#'
#' @param species_names Vector of one or more invasive species (or genus)
#'   names.
#' @param species_types Vector of types of invasive species. One or more of
#'   \code{"pest"}, \code{"weed"}, or \code{"disease"}.
#' @param resource_type The type of management resource being allocated. One of
#'   \code{"survey"}, \code{"traps"}, \code{"treatment"}, \code{"removal"},
#'   \code{"mixed"}, or \code{"other"}.
#' @param management_purpose The purpose of the management. One of
#'   \code{"delimitation"}, \code{"containment"}, \code{"eradication"}, or
#'   \code{"other"}.
#' @param threat_status The status of the invasive species threat. One of
#'   \code{"detected"}, \code{"delimited"}, \code{"contained"},
#'   \code{"eradicated"}, or \code{"other"}.
#' @param ... Additional parameters.
#' @return A \code{ManageContext} class object (list) containing functions for
#'   accessing attributes:
#'   \describe{
#'     \item{\code{get_species_names()}}{Get the invasive species names.}
#'     \item{\code{get_species_types()}}{Get the types of invasive species:
#'       "pest", "weed", or "disease".}
#'     \item{\code{get_resource_type()}}{Get the resource type.}
#'     \item{\code{get_management_purpose()}}{Get the management purpose.}
#'     \item{\code{get_threat_status()}}{Get the threat status.}
#'   }
#' @export
ManageContext <- function(species_names,
                          species_types = c("pest",
                                            "weed",
                                            "disease"),
                          resource_type = c("survey",
                                            "traps",
                                            "treatment",
                                            "removal",
                                            "mixed",
                                            "other"),
                          management_purpose = c("delimitation",
                                                 "containment",
                                                 "eradication",
                                                 "other"),
                          threat_status = c("detected",
                                            "delimited",
                                            "contained",
                                            "eradicated",
                                            "other"), ...) {
  UseMethod("ManageContext")
}

#' @name ManageContext
#' @export
ManageContext.default <- function(species_names,
                                  species_types = c("pest",
                                                    "weed",
                                                    "disease"),
                                  resource_type = c("survey",
                                                    "traps",
                                                    "treatment",
                                                    "removal",
                                                    "mixed",
                                                    "other"),
                                  management_purpose = c("delimitation",
                                                         "containment",
                                                         "eradication",
                                                         "other"),
                                  threat_status = c("detected",
                                                    "delimited",
                                                    "contained",
                                                    "eradicated",
                                                    "other"), ...) {
  # Match arguments to selections
  species_types <- match.arg(species_types, several.ok = TRUE)
  resource_type <- match.arg(resource_type)
  management_purpose <- match.arg(management_purpose)
  threat_status <- match.arg(threat_status)

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

  # Get the resource type
  self$get_resource_type <- function() {
    return(resource_type)
  }

  # Get the management purpose
  self$get_management_purpose <- function() {
    return(management_purpose)
  }

  # Get the threat status
  self$get_threat_status <- function() {
    return(threat_status)
  }

  return(self)
}
