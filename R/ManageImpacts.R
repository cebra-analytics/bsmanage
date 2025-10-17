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
#' @param recovery_delay Number of simulation time steps that incursion impacts
#'   continue to be in effect at previously occupied locations before the
#'   asset value at the locations recover. Only available for spatially
#'   explicit grid or network models. Default \code{NULL} assumes no delay.
#' @param ... Additional parameters.
#' @return A \code{ManageImpacts} class object (list) containing functions for
#'   getting the impact context and performing impact calculations:
#'   \describe{
#'     \item{\code{get_context()}}{Get \code{bsimpact::Context} object.}
#'     \item{\code{get_calc_total()}}{Get calculate total indicator.}
#'     \item{\code{update_recovery_delay(n)}}{Update the recovery delay
#'       attribute attached to the population vector.}
#'     \item{\code{includes_combined()}}{Logical indicator for when impacts are
#'       combined.}
#'     \item{\code{calculate(n, tm)}}{Perform impact calculations resulting
#'       from incursion population vector or matrix \code{n} at time step
#'       \code{tm}, and return a list of impact values, including combined
#'       impacts when applicable.}
#'   }
#' @export
ManageImpacts <- function(impacts, population_model,
                          impact_stages = NULL,
                          calc_total = NULL,
                          recovery_delay = NULL, ...) {

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
  } else if (!is.logical(calc_total)) {
    stop("Calculate total indicator should be logical.", call. = FALSE)
  }

  # Check population capacity is available to calculate density
  if (impacts$get_incursion()$get_type() == "density" &&
      is.null(population_model$get_capacity())) {
    stop("Population capacity is required for density-based impacts.",
         call. = FALSE)
  }

  # Check recovery delay
  if (!is.null(recovery_delay) &&
      (!is.numeric(recovery_delay) || recovery_delay < 0)) {
    stop("Recover delay should a number >= 0.", call. = FALSE)
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

  # Calculate density-based incursion (internal)
  calculate_density_incursion <- function(n) {
    n <- rowSums(as.matrix(n)[,impact_stages, drop = FALSE])
    n_density <- n*0
    idx <- which(population_model$get_capacity() > 0)
    n_density[idx] <- pmin(n[idx]/population_model$get_capacity()[idx], 1)
    return(n_density)
  }

  # Calculate spatially implicit area-based incursion (internal)
  calculate_area_incursion <- function(n) {

    # Get total area occupied
    if (is.numeric(attr(n, "diffusion_radius"))) {
      total_area <- pi*(attr(n, "diffusion_radius"))^2
    } else if (is.numeric(attr(n, "spread_area"))) {
      total_area <- attr(n, "spread_area")
    } else {
      stop(paste("Cannot calculate spatially implicit impacts without area",
                 "occupied."), call. = FALSE)
    }

    # Return total area occupied when population is present, else zero
    return((sum(n[impact_stages]) > 0)*total_area)
  }

  # Update recovery delay
  self$update_recovery_delay <- function(n) {
    if (is.numeric(recovery_delay)) {
      id <- impacts$get_id()
      if (is.list(attr(n, "recovery_delay")) &&
          length(attr(n, "recovery_delay")) >= id &&
          is.numeric(attr(n, "recovery_delay")[[id]])) {

        # Update for presence incursions
        if (impacts$get_incursion()$get_type() == "presence") { # decremented

          # Occurrence and recovery delay locations
          idx <- which(rowSums(as.matrix(n)[,impact_stages, drop = FALSE]) > 0)
          delay_idx <- which(attr(n, "recovery_delay")[[id]] > 0)

          # Decrement delay where population removed or extirpated
          if (any(!delay_idx %in% idx)) {
            decr_idx <- delay_idx[which(!delay_idx %in% idx)]
            attr(n, "recovery_delay")[[id]][decr_idx] <-
              attr(n, "recovery_delay")[[id]][decr_idx] - 1
          }

          # Set recovery for new occurrences
          if (any(!idx %in% delay_idx)) {
            new_idx <- idx[which(!idx %in% delay_idx)]
            attr(n, "recovery_delay")[[id]][new_idx] <- recovery_delay
          }

        } else if (impacts$get_incursion()$get_type() == "density") {

          # Push current density to front of list for first impact only
          if (attr(attr(n, "recovery_delay"), "first") == id) {
            attr(attr(n, "recovery_delay"), "incursions") <-
              c(list(calculate_density_incursion(n)),
                attr(attr(n, "recovery_delay"), "incursions"))
            length(attr(attr(n, "recovery_delay"), "incursions")) <-
              min(length(attr(attr(n, "recovery_delay"), "incursions")),
                  attr(attr(n, "recovery_delay"), "max"))
          }

        } else if (population_model$get_region()$spatially_implicit() &&
                   impacts$get_incursion()$get_type() == "area") {

          # Add current area to front of list for first impact only
          if (attr(attr(n, "recovery_delay"), "first") == id) {
            attr(attr(n, "recovery_delay"), "incursions") <-
              c(calculate_area_incursion(n),
                attr(attr(n, "recovery_delay"), "incursions"))
          }
        }

      } else {

        # Initialise recovery delay
        if (!is.list(attr(n, "recovery_delay"))) {
          attr(n, "recovery_delay") <- list()
        }
        if (impacts$get_incursion()$get_type() == "presence") { # decremented
          attr(n, "recovery_delay")[[id]] <-
            (+(rowSums(as.matrix(n)[,impact_stages, drop = FALSE]) > 0)*
               recovery_delay)
        } else { # constant
          attr(n, "recovery_delay")[[id]] <- recovery_delay
          attr(attr(n, "recovery_delay"), "max") <-
            max(attr(attr(n, "recovery_delay"), "max"), recovery_delay)
          if (is.null(attr(attr(n, "recovery_delay"), "first"))) {
            attr(attr(n, "recovery_delay"), "first") <- id
            if (impacts$get_incursion()$get_type() == "density") {
              attr(attr(n, "recovery_delay"), "incursions") <-
                list(calculate_density_incursion(n))
            } else if (population_model$get_region()$spatially_implicit() &&
                       impacts$get_incursion()$get_type() == "area") {
              attr(attr(n, "recovery_delay"), "incursions") <-
                calculate_area_incursion(n)
            }
          }
        }
      }
    }
    return(n)
  }

  # Calculate impacts
  self$calculate <- function(n, tm) {

    # Check for recovery delay
    recovery_delay <- attr(n, "recovery_delay")

    # Calculate incursion values
    if (impacts$get_incursion()$get_type() == "density") {
      n <- calculate_density_incursion(n)
    } else if (population_model$get_region()$spatially_implicit() &&
               impacts$get_incursion()$get_type() == "area") {
      n <- calculate_area_incursion(n)
    } else if (population_model$get_type() == "stage_structured") {
      n <- rowSums(n[,impact_stages, drop = FALSE])
    }

    # Re-attach recovery delay
    attr(n, "recovery_delay") <- recovery_delay

    # Set incursion values within impact object
    impacts$get_incursion()$set_values(n)

    # Get incursion impacts
    impact_list <- impacts$incursion_impacts(raw = TRUE, time_int = tm)

    # Append combined impacts when present
    if ("combined_impacts" %in% names(impacts)) {
      impact_list$combined <- impacts$combined_impacts(raw = TRUE)
    }

    return(impact_list)
  }

  return(self)
}
