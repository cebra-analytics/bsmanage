#' Manage detection class builder
#'
#' Builds a class for simulating the application of detection or surveillance
#' resources applied in the management of an invasive species.
#'
#' @param region A \code{bsspread::Region} or inherited class object
#'   representing the spatial region (template) for the incursion management
#'   simulations.
#' @param population_model A \code{bsspread::Population} or inherited class
#'   object defining the population representation for the management
#'   simulations.
#' @param surveillance A \code{bsdesign::SurveillanceDesign} or inherited class
#'   object representing the distribution of surveillance resources and their
#'   detection sensitivities.
#' @param surv_cost Numeric vector of distributed surveillance costs (combined
#'   resource and fixed costs) or a single cost value for each location where
#'   surveillance is applied. For spatially-implicit area-based regions, costs
#'   should be specified as cost per metres squared. Costs are accumulated for
#'   each application of the surveillance at each (scheduled) simulation time
#'   step. The cost unit may be added as an attribute
#'   (\code{attr(surv_cost, "unit")}), or set within the
#'   \code{bsdesign::Context} object associated with the \code{surveillance}
#'   (\code{bsdesign::SurveillanceDesign}) object. Default is \code{NULL} when
#'   costs are unavailable.
#' @param stages Numeric vector of population stages (indices) to which
#'   management detection are applied. Default is all stages (when set to
#'   \code{NULL}).
#' @param schedule Vector of discrete simulation time steps (t = 0, 1, 2, ...)
#'   in which to apply management detection. Default is all time steps (when
#'   set to \code{NULL}).
#' @param ... Additional parameters.
#' @return A \code{ManageDetection} class object (list) containing a function
#'   for accessing attributes and applying simulated management detection:
#'   \describe{
#'     \item{\code{get_type()}}{Get the type of management action
#'       ("detection").}
#'     \item{\code{get_label()}}{Get the management actions label used in
#'       simulation results (i.e. "detected").}
#'     \item{\code{get_surveillance()}}{Get the surveillance design class
#'       object.}
#'     \item{\code{get_stages()}}{Get the population stages to which management
#'       detection are applied.}
#'     \item{\code{get_schedule()}}{Get the scheduled simulation time steps in
#'       which management detection are applied.}
#'     \item{\code{include_cost()}}{Logical indication of a cost parameter
#'       having a value (named as per population attachment).}
#'     \item{\code{get_cost_unit()}}{Get the unit of surveillance cost.}
#'     \item{\code{clear_attributes(n)}}{Clear attached attributes associated
#'       with this action from a simulated population vector or matrix
#'       \code{n}, and return \code{n} without the attached attributes.}
#'     \item{\code{apply(n, tm)}}{Apply management detection to a simulated
#'       population vector or matrix \code{n}, potentially with attached
#'       attributes relating to previously applied actions, providing the time
#'       step \code{tm} is in the \code{schedule}, and return the resulting
#'       population \code{n} along with attached attributes relating to the
#'       newly applied detection/surveillance.}
#'   }
#' @export
ManageDetection <- function(region,
                            population_model,
                            surveillance,
                            surv_cost = NULL,
                            stages = NULL,
                            schedule = NULL, ...) {
  UseMethod("ManageDetection")
}

#' @name ManageDetection
#' @export
ManageDetection.Region <- function(region,
                                   population_model,
                                   surveillance,
                                   surv_cost = NULL,
                                   stages = NULL,
                                   schedule = NULL, ...) {

  # Build via base class
  self <- ManageActions(region = region,
                        population_model = population_model,
                        type = "detection",
                        stages = stages,
                        schedule = schedule,
                        class = "ManageDetection")

  # Check the surveillance object
  if (!is.null(surveillance) &&
      !inherits(surveillance, "SurveillanceDesign")) {
    stop(paste("Surveillance object must be a 'SurveillanceDesign' or",
               "inherited class object."), call. = FALSE)
  } else if (surveillance$get_divisions()$get_parts() !=
             region$get_locations()) {
    stop("Surveillance object must be compatible with the region object.",
         call. = FALSE)
  }

  # Check and process surveillance cost
  if (!is.null(surv_cost)) {
    if (!is.numeric(surv_cost) ||
        !length(surv_cost) %in% c(1, region$get_locations())) {
      stop(paste("The surveillance cost parameter must be a numeric vector",
                 "with values for each location."), call. = FALSE)
    }
    cost_unit <- attr(surv_cost, "unit")
    if (is.null(cost_unit) || cost_unit == "") {
      cost_unit <- surveillance$get_context()$get_cost_unit()
    }
    if (length(surv_cost) == 1) {
      surv_cost <- surv_cost*(surveillance$get_sensitivity() > 0)
    }
    attr(surv_cost, "unit") <- cost_unit
  }

  # Get results label
  self$get_label <- function() {
    return("detected")
  }

  # Get the surveillance object
  self$get_surveillance <- function() {
    return(surveillance)
  }

  # Does cost parameter (named) having a value?
  self$include_cost <- function() {
    include_cost <- is.numeric(surv_cost)
    names(include_cost) <- "surv_cost"
    return(include_cost)
  }

  # Get the unit of surveillance cost
  self$get_cost_unit <- function() {
    return(attr(surv_cost, "unit"))
  }

  # Clear attached attributes
  self$clear_attributes <- function(n) {
    attr(n, "detected") <- NULL
    attr(n, "undetected") <- NULL
    attr(n, "surv_cost") <- NULL
    return(n)
  }

  # Detection/surveillance apply method
  self$apply <- function(n, tm) {

    # Initial zero detections
    detected <- as.numeric(n)*0
    if (population_model$get_type() == "stage_structured") {
      detected <- array(detected, dim(n))
      colnames(detected) <- attr(population_model$get_growth(), "labels")
    }

    # Only survey undetected population
    if (!is.null(attr(n, "undetected"))) {
      undetected <- attr(n, "undetected")
    } else {
      undetected <- detected
      undetected[] <- n[]
    }

    # Scheduled time step?
    if (is.null(schedule) || tm %in% schedule) {

      # Occupied locations
      idx <- which(rowSums(as.matrix(undetected)) > 0)
      if (length(idx) > 0) {

        # Get detection sensitivity (probability)
        detect_pr <- surveillance$get_sensitivity()[idx]

        # Sample detections
        if (population_model$get_type() == "stage_structured") {
          for (i in self$get_stages()) {
            detected[idx,i] <- stats::rbinom(length(idx),
                                             size = undetected[idx,i],
                                             prob = detect_pr)
          }
          undetected[idx,] <- undetected[idx,] - detected[idx,]
        } else {
          detected[idx] <- stats::rbinom(length(idx),
                                         size = undetected[idx],
                                         prob = detect_pr)
          undetected[idx] <- undetected[idx] - detected[idx]
        }
      }

      # Attach/update surveillance costs as an attribute
      if (!is.null(surv_cost)) {
        if (!is.null(attr(n, "surv_cost"))) {
            attr(n, "surv_cost") <- attr(n, "surv_cost") + surv_cost
        } else {
            attr(n, "surv_cost") <- surv_cost
        }
      }

    } else {

      # Attach/update surveillance costs as an attribute
      if (is.null(surv_cost)) {
        attr(n, "surv_cost") <- surv_cost*0
      }
    }

    # Attach/update detected and undetected as attributes
    if (population_model$get_type() == "presence_only") {
      if (!is.null(attr(n, "detected"))) {
        attr(n, "detected") <- attr(n, "detected") | as.logical(detected)
      } else {
        attr(n, "detected") <- as.logical(detected)
      }
      attr(n, "undetected") <- as.logical(undetected)
    } else {
      if (!is.null(attr(n, "detected"))) {
        attr(n, "detected") <- attr(n, "detected") + detected
      } else {
        attr(n, "detected") <- detected
      }
      attr(n, "undetected") <- undetected
    }

    return(n)
  }

  return(self)
}
