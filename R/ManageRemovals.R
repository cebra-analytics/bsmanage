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
#' @param remove_always A logical indication of whether or not removal is
#'   always to applied to locations where invasive species are present (even
#'   when they are not detected by explicit surveillance). Default is
#'   \code{FALSE}, indicating that removal is dependent on detection when
#'   surveillance actions are present. Set to \code{TRUE} for locations where
#'   some removal is likely regardless of explicit management efforts (e.g.
#'   via the general public or property owners).
#' @param detected_only A logical indication of whether or not removal is only
#'   applied to detected individuals (e.g. via traps). Default is \code{FALSE},
#'   indicating that removal is applied to all individuals at locations where
#'   the invasive species has been detected (e.g. via treatment).
#' @param removal_cost Numeric vector of distributed removal costs (combined
#'   resource and fixed costs) or a single cost value for each location where
#'   removal is applied. For spatially-implicit area-based regions, costs
#'   should be specified as cost per metres squared. Costs are accumulated for
#'   each application of the removal at each (scheduled) simulation time step.
#'   The cost unit may be added as an attribute
#'   (\code{attr(removal_cost, "unit")}). Default is \code{NULL} when costs
#'   are unavailable.
#' @param radius Optional radius (m) of the removal. Stochastic removal is
#'   applied to all locations within the specified radius of each location
#'   where the invasive species has been detected. Default is \code{NULL},
#'   indicating that removal is only applied at detected locations (when
#'   specified via a population attribute).
#' @param stages Numeric vector of population stages (indices) to which
#'   management removals are applied. Default is all stages (when set to
#'   \code{NULL}).
#' @param schedule Vector of discrete simulation time steps (t = 0, 1, 2, ...)
#'   in which to apply management removals. Default is all time steps (when
#'   set to \code{NULL}).
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
#'     \item{\code{include_cost()}}{Logical indication of a cost parameter
#'       having a value (named as per population attachment).}
#'     \item{\code{get_cost_unit()}}{Get the unit of removal cost.}
#'     \item{\code{clear_attributes(n)}}{Clear attached attributes associated
#'       with this action from a simulated population vector or matrix
#'       \code{n}, and return \code{n} without the attached attributes.}
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
                           remove_always = FALSE,
                           detected_only = FALSE,
                           removal_cost = NULL,
                           radius = NULL,
                           stages = NULL, schedule = NULL, ...) {
  UseMethod("ManageRemovals")
}

#' @name ManageRemovals
#' @export
ManageRemovals.Region <- function(region, population_model,
                                  removal_pr = 1,
                                  remove_always = FALSE,
                                  detected_only = FALSE,
                                  removal_cost = NULL,
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
  if (is.null(removal_pr) ||
      (!is.null(removal_pr) &&
       (!is.numeric(removal_pr) ||
        any(removal_pr < 0) || any(removal_pr > 1) ||
        !length(removal_pr) %in% c(1, region$get_locations())))) {
    stop(paste("Removal probability should be a vector with a value 0-1 for",
               "each region location."), call. = FALSE)
  }
  if (length(removal_pr) == 1) {
    removal_pr <- rep(removal_pr, region$get_locations())
  }
  if (!is.null(radius) && (!is.numeric(radius) || radius < 0)) {
    stop("The radius (m) parameter must be numeric and >= 0.", call. = FALSE)
  }

  # Notify if radius is provided when detected only
  if (is.numeric(radius) && detected_only) {
    message("Radius is not used when only detected individuals are removed.")
  }

  # Check and process removal cost
  if (!is.null(removal_cost)) {
    if (!is.numeric(removal_cost) ||
        !length(removal_cost) %in% c(1, region$get_locations())) {
      stop(paste("The removal cost parameter must be a numeric vector with",
                 "values for each location."), call. = FALSE)
    }
    cost_unit <- attr(removal_cost, "unit")
    if (length(removal_cost) == 1) {
      removal_cost <- removal_cost*(removal_pr > 0)
    }
    attr(removal_cost, "unit") <- cost_unit
  }

  # Get results label
  self$get_label <- function() {
    return("removed")
  }

  # Does cost parameter (named) having a value?
  self$include_cost <- function() {
    include_cost <- is.numeric(removal_cost)
    names(include_cost) <- "removal_cost"
    return(include_cost)
  }

  # Get the unit of removal cost
  self$get_cost_unit <- function() {
    return(attr(removal_cost, "unit"))
  }

  # Clear attached attributes
  self$clear_attributes <- function(n) {
    attr(n, "removed") <- NULL
    attr(n, "removal_cost") <- NULL
    return(n)
  }

  # Removal apply method
  self$apply <- function(n, tm) {

    # Initial zero removals
    removed <- as.numeric(n)*0
    if (population_model$get_type() == "stage_structured") {
      removed <- array(removed, dim(n))
      colnames(removed) <- attr(population_model$get_growth(), "labels")
    }

    # Initially no removal cost locations
    if (!is.null(removal_cost)) {
      cost_apply <- rep(FALSE, region$get_locations())
    }

    # Scheduled time step?
    if (is.null(schedule) || tm %in% schedule) {

      # Detection-based removal
      if (!remove_always && "detected" %in% names(attributes(n))) {

        # Removal locations
        if (population_model$get_type() == "stage_structured") {
          idx <- which(rowSums(attr(n, "detected")[,self$get_stages()]) > 0)
        } else {
          idx <- which(attr(n, "detected") > 0)
        }

        # Individuals to which to apply removal
        if (detected_only) {
          n_apply <- pmin(as.numeric(n), attr(n, "detected"))
        } else {
          n_apply <- as.numeric(n)
        }

      } else {

        # Removal locations
        if (!remove_always && detected_only) {
          idx <- c() # none detected
        } else {
          if (population_model$get_type() == "stage_structured") {
            idx <- which(rowSums(n[,self$get_stages()]) > 0)
          } else {
            idx <- which(n > 0)
          }
        }

        # Apply to all individuals
        n_apply <- as.numeric(n)
      }

      # Expand removal locations via radius
      if ("detected" %in% names(attributes(n)) && is.numeric(radius) &&
          length(idx) > 0 && region$get_type() %in% c("grid", "patch")) {
        idx <- region$get_nearby(idx, radius)
        idx <- idx[which(rowSums(
          as.matrix(n)[,self$get_stages(), drop = FALSE])[idx] > 0)]
      }

      # Sample and apply removals
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

      # Set removal cost locations
      if (!is.null(removal_cost)) {
        if (length(idx) > 0) {
          cost_apply[idx] <- TRUE
        }
      }
    }

    # Attach or update removed as an attribute
    if (population_model$get_type() == "presence_only") {
      if (is.null(attr(n, "removed"))) {
        attr(n, "removed") <- as.logical(removed)
      } else {
        attr(n, "removed") <- attr(n, "removed") | as.logical(removed)
      }
    } else {
      if (is.null(attr(n, "removed"))) {
        attr(n, "removed") <- removed
      } else {
        attr(n, "removed") <- removed + attr(n, "removed")
      }
    }

    # Attach (additional) removal costs as an attribute
    if (!is.null(removal_cost)) {
      if (is.null(attr(n, "removal_cost"))) {
        attr(n, "removal_cost") <- removal_cost*cost_apply
      } else {
        attr(n, "removal_cost") <-
          attr(n, "removal_cost") + removal_cost*cost_apply
      }
    }

    return(n)
  }

  return(self)
}
