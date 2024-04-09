#' Manage impacts class builder
#'
#' Builds a class for calculating incursion impacts during management
#' simulations. Encapsulates \code{bsimpact} impact analysis objects.
#'
#' @param impacts A \code{bsimpact::ImpactAnalysis} or inherited class object
#'   specifying how environment, social, and/or economic impacts are
#'   calculated, classified, and/or combined based on asset values,
#'   classifications, and/or incursion loss rates.
#' @param population_model A \code{Population} or inherited class object
#'   defining the population representation for the management simulations.
#' @param impact_stages Numeric vector of population stages (indices) that
#'   contribute to impacts. Default is all stages (when set to \code{NULL}).
#' @param calc_total Logical indicator for whether or not total impacts may
#'   be (sensibly) calculated by summing across region locations. Default is
#'   \code{NULL}, whereby totals are only calculated when the impact (context)
#'   valuation type is \code{"monetary"}. Set to \code{TRUE} if it makes sense
#'   to sum total \code{"non-monetary"} or \code{"ranking"} impact valuations
#'   across region locations (providing multiple aspects can be combined).
#' @param ... Additional parameters.
#' @return A \code{ManageImpacts} class object (list) containing functions for
#'   getting the impact context and performing impact calculations:
#'   \describe{
#'     \item{\code{get_context()}}{Get \code{bsimpact::Context} object.}
#'     \item{\code{get_calc_total()}}{Get calculate total indicator.}
#'     \item{\code{includes_combined()}}{Logical indicator for when impacts are
#'       combined.}
#'     \item{\code{calculate(n)}}{Perform impact calculations resulting from
#'       incursion population vector or matrix \code{n}, and return a list
#'       of impact values, including combined impacts when applicable.}
#'   }
#' @export
ManageImpacts <- function(impacts, population_model,
                          impact_stages = NULL,
                          calc_total = NULL, ...) {

  # Check the impacts object
  if (!is.null(impacts) && !inherits(impacts, "ImpactAnalysis")) {
    stop("Impacts must be a 'ImpactAnalysis' or inherited class object.",
         call. = FALSE)
  }

  # Check the population model
  if (!is.null(population_model) &&
      !inherits(population_model, "Population")) {
    stop("Population model must be a 'Population' or inherited class object.",
         call. = FALSE)
  }

  # Check the impact stages
  population_type <- population_model$get_type()
  if (population_type == "presence_only" ||
      population_type == "unstructured") {
    impact_stages <- 1
  } else if (population_type == "stage_structured") {
    if (is.null(impact_stages)) {
      impact_stages <- 1:population_model$get_stages()
    } else if (!is.numeric(impact_stages) ||
               !all(impact_stages %in% 1:population_model$get_stages())) {
      stop("Impact stages must be a vector of stage indices consistent ",
           "with the population model.", call. = FALSE)
    }
  }

  # Check and resolve the calculate total indicator
  if (is.null(calc_total)) {
    calc_total <- (impacts$get_context()$get_valuation_type() == "monetary")
  } else if (is.logical(calc_total) && calc_total &&
             length(impacts$get_context()$get_impact_scope()) > 1 &&
             !("combined_impacts" %in% names(impacts))) {
    stop("Cannot combine impacts to calculate total.", call. = FALSE)
  } else if (!is.logical(calc_total)) {
    stop("Calculate total indicator should be logical.", call. = FALSE)
  }

  # Create a class structure
  self <- structure(list(), class = "ManageImpacts")

  # Get context
  self$get_context <- function() {
    return(impacts$get_context())
  }

  # Get calculate total indicator
  self$get_calc_total <- function() {
    return(calc_total)
  }

  # Impacts combined?
  self$includes_combined <- function() {
    return("combined_impacts" %in% names(impacts))
  }

  # Calculate impacts
  self$calculate <- function(n) {

    # Combine impact stage populations
    if (population_model$get_type() == "stage_structured" &&
        is.numeric(impact_stages)) {
      n <- rowSums(n[,impact_stages, drop = FALSE])
    }

    # Set incursion values within impact object
    impacts$get_incursion()$set_values(n)

    # Get incursion impacts
    impact_list <- impacts$incursion_impacts(raw = TRUE)

    # Append combined impacts when present
    if ("combined_impacts" %in% names(impacts)) {
      impact_list$combined <- impacts$combined_impacts(raw = TRUE)
    }

    return(impact_list)
  }

  return(self)
}
