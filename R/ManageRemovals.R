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
#'   of removal at locations where the invasive species has been detected, as
#'   indicated via a population attribute when present, else removal is applied
#'   at all locations. Default is \code{1}, indicating all (detected)
#'   occurrences are removed.
#' @param detected_only A logical indication of whether or not removal is only
#'   applied to detected individuals (e.g. via traps). Default is \code{FALSE},
#'   indicating that removal is applied to all individuals at locations where
#'   the invasive species has been detected (e.g. via treatment).
#' @param radius The radius (m) of the removal. Stochastic removal is applied
#'   to all locations within the specified radius of each location where the
#'   invasive species has been detected. Default is \code{NULL}, indicating
#'   that removal is only applied at detected locations (when specified via a
#'   population attribute).
#' @param stages Numeric vector of population stages (indices) to which
#'   management removals are applied. Default is all stages (when set to
#'   \code{NULL}).
#' @param schedule Vector of discrete simulation time steps in which to apply
#'   management removals Default is all time steps (when set to \code{NULL}).
#' @param ... Additional parameters.
#' @return A \code{ManageRemovals} class object (list) containing a function
#'   for accessing attributes and applying simulated management removals:
#'   \describe{
#'     \item{\code{get_type()}}{Get the type of management action ("removal").}
#'     \item{\code{get_label()}}{Get the management actions label used in
#'       simulation results (i.e. "removed").}
#'     \item{\code{get_stages()}}{Get the population stages to which management
#'       removals are applied.}
#'     \item{\code{get_schedule()}}{Get the scheduled simulation time steps in
#'       which management removals are applied.}
#'     \item{\code{apply(n, tm)}}{Apply management removals to a simulated
#'       population vector or matrix \code{n}, potentially with attached
#'       attributes relating to previously applied actions, providing the time
#'       step \code{tm} is in the \code{schedule}, and return the resulting
#'       population \code{n} along with attached attributes relating to the
#'       newly applied removals.}
#'   }
#' @export
ManageRemovals <- function(region, population_model,
                           removal_pr = 1,
                           detected_only = FALSE,
                           radius = NULL,
                           stages = NULL, schedule = NULL, ...) {
  UseMethod("ManageRemovals")
}

#' @name ManageRemovals
#' @export
ManageRemovals.Region <- function(region, population_model,
                                  removal_pr = 1,
                                  detected_only = FALSE,
                                  radius = NULL,
                                  stages = NULL, schedule = NULL, ...) {

  # Build via base class
  self <- ManageActions(region = region,
                        population_model = population_model,
                        type = "removal",
                        stages = stages,
                        schedule = schedule,
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

  # Notify if radius is provided when detected only
  if (is.numeric(radius) && detected_only) {
    message("Radius is not used when only detected individuals are removed.")
  }

  # Configure region paths via removal radius
  if (is.numeric(radius)) {
    region$configure_paths(max_distance = radius)
  }

  # Get results label
  self$get_label <- function() {
    return("removed")
  }

  # Removal apply method
  self$apply <- function(n, tm) {

    # Scheduled time step?
    if (is.null(schedule) || tm %in% schedule) {

      # Detection-based removal
      if ("detected" %in% names(attributes(n))) {

        # Removal locations
        idx <- which(rowSums(as.matrix(attr(n, "detected"))) > 0)

        # Individuals to which to apply removal
        if (detected_only) {
          n_apply <- attr(n, "detected")
        } else {
          n_apply <- as.numeric(n)
        }

      } else {

        # Removal locations
        if (detected_only) {
          idx <- c() # none detected
        } else {
          idx <- which(rowSums(as.matrix(n)) > 0)
        }

        # Apply to all individuals
        n_apply <- as.numeric(n)
      }

      # Expand removal locations via radius
      if ("detected" %in% names(attributes(n)) && is.numeric(radius) &&
          length(idx) > 0) {
        region$calculate_paths(idx)
        idx <- sort(unique(c(idx, unname(unlist(region$get_paths(idx)$idx)))))
        idx <- idx[which(rowSums(as.matrix(n))[idx] > 0)]
      }

      # Sample and apply removals
      removed <- as.numeric(n)*0
      if (population_model$get_type() == "stage_structured") {
        removed <- array(removed, dim(n))
      }
      if (length(idx) > 0) {
        if (population_model$get_type() == "stage_structured") {
          n_apply <- array(n_apply, dim(n))
          for (i in self$get_stages()) {
            removed[idx,i] <- stats::rbinom(length(idx), size = n_apply[idx,i],
                                            prob = removal_pr[idx])
            n[idx,i] <- n[idx,i] - removed[idx,i]
          }
        } else {
          removed[idx] <- stats::rbinom(length(idx), size = n_apply[idx],
                                        prob = removal_pr[idx])
          if (population_model$get_type() == "presence_only") {
            n[idx] <- n[idx] & !removed[idx]
          } else {
            n[idx] <- n[idx] - removed[idx]
          }
        }
      }

      # Attach removed as an attribute
      if (population_model$get_type() == "presence_only") {
        attr(n, "removed") <- as.logical(removed)
      } else {
        attr(n, "removed") <- removed
      }

    } else {

      # Remove removed attribute
      attr(n, "removed") <- NULL
    }

    return(n)
  }

  return(self)
}
