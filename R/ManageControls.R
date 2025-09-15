#' Manage controls class builder
#'
#' Builds a class for simulating the application of invasive species management
#' controls, such as combined surveillance and removal ("search & destroy"),
#' and reducing or suppressing growth, reproduction, spread, or establishment.
#'
#' @param region A \code{bsspread::Region} or inherited class object
#'   representing the spatial region (template) for the incursion management
#'   simulations.
#' @param population_model A \code{bsspread::Population} or inherited class
#'   object defining the population representation for the management
#'   simulations.
#' @param control_type One of \code{"search_destroy"} (default),
#'   \code{"growth"} (including reproduction), \code{"spread"}, or
#'   \code{"establishment"} to indicate the type of control applied.
#' @param control_design A \code{ManageDesign}, \code{ControlDesign}, or
#'   inherited class object representing the distribution of control resources
#'   and their success probabilities or effectiveness. Required for combined
#'   surveillance and removal approaches ("search & destroy", traps, etc.).
#'   Alternatively, optionally utilise to specify the distribution of existing,
#'   known, or scheduled treatment locations for the reduction or suppression
#'   of growth, spread, or establishment.
#' @param control_mult Control multipliers or rates required for control types
#'   \code{"growth"}, \code{"spread"}, or \code{"establishment"}. May be a
#'   single value (0-1), or vector of values for each location specified by
#'   the \code{region}. Control multipliers are applied to reduce (with values
#'   < 1) or suppress (with values = 0) growth, spread, or establishment at
#'   existing, known, or scheduled treatment locations (when specified via
#'   \code{"control_design"}), and at locations (and optionally surroundings)
#'   where the invasive species has been detected (when specified via a
#'   population attribute).
#' @param radius Optional radius (m) of the applied control for types
#'   \code{"growth"}, \code{"spread"}, or \code{"establishment"} only.
#'   Control is applied to all surrounding locations within the specified
#'   radius of each location where the invasive species has been detected.
#'   Default is \code{NULL}, indicating that control is only applied at
#'   existing, known, or scheduled treatment locations, and/or at detected
#'   locations (when specified via a population attribute).
#' @param stages Numeric vector of population stages (indices) to which
#'   search & destroy or growth management controls are applied. Default is all
#'   stages (when set to \code{NULL}).
#' @param apply_to Optional label for growth control to indicate that it should
#'   be only be applied to reproduction or survival rates. If applicable, set
#'   to either \code{"reproduction"} or \code{"survival"}. If not specified,
#'   control is applied to both reproduction and survival. The \code{stages}
#'   and \code{apply_to} parameters may be utilised together, for example, to
#'   control the seasonal survival rates of specified life stages.
#' @param schedule Vector of discrete simulation time steps in which to apply
#'   management controls. Default is all time steps (when set to \code{NULL}).
#' @param ... Additional parameters.
#' @return A \code{ManageControls} class object (list) containing a function
#'   for accessing attributes and applying simulated management controls:
#'   \describe{
#'     \item{\code{get_type()}}{Get the type of management action ("control").}
#'     \item{\code{get_label()}}{Get the management actions label used in
#'       simulation results (e.g. "control_growth").}
#'     \item{\code{get_stages()}}{Get the population stages to which management
#'       controls are applied.}
#'     \item{\code{get_schedule()}}{Get the scheduled simulation time steps in
#'       which management controls are applied.}
#'     \item{\code{apply(n, tm)}}{Apply management controls to a simulated
#'       population vector or matrix \code{n}, potentially with attached
#'       attributes relating to previously applied actions, providing the time
#'       step \code{tm} is in the \code{schedule}, and return the resulting
#'       population \code{n} along with attached attributes relating to the
#'       newly applied controls.}
#'   }
#' @export
ManageControls <- function(region, population_model,
                           control_type = c("search_destroy",
                                            "growth",
                                            "spread",
                                            "establishment"),
                           control_design = NULL,
                           control_mult = NULL,
                           radius = NULL,
                           stages = NULL,
                           apply_to = NULL,
                           schedule = NULL, ...) {
  UseMethod("ManageControls")
}

#' @name ManageControls
#' @export
ManageControls.Region <- function(region, population_model,
                                  control_type = c("search_destroy",
                                                   "growth",
                                                   "spread",
                                                   "establishment"),
                                  control_design = NULL,
                                  control_mult = NULL,
                                  radius = NULL,
                                  stages = NULL,
                                  apply_to = NULL,
                                  schedule = NULL, ...) {

  # Build via base class
  self <- ManageActions(region = region,
                        population_model = population_model,
                        type = "control",
                        stages = stages,
                        schedule = schedule,
                        class = "ManageControls")

  control_type <- match.arg(control_type)

  # Check the control design object
  if (!is.null(control_design) &&
      !inherits(control_design, "ManageDesign")) {
    stop(paste("Control design object must be a 'ControlDesign',",
               "'ManageDesign', or inherited class object."), call. = FALSE)
  } else if (is.null(control_design) && control_type == "search_destroy") {
    stop("Control design object is required for 'search & destroy' control.",
         call. = FALSE)
  } else if (!is.null(control_design) &&
             (control_design$get_divisions()$get_parts() !=
              region$get_locations())) {
    stop("Control design must be compatible with the region object.",
         call. = FALSE)
  }

  # Validate control multiplier, radius, and growth apply to
  if (is.null(control_mult) &&
      control_type %in% c("growth", "spread", "establishment")) {
    stop(paste("Control multiplier is required for growth, spread, and",
               "establishment control."), call. = FALSE)
  } else if (!is.null(control_mult) &&
             (!is.numeric(control_mult) ||
              any(control_mult < 0) || any(control_mult > 1) ||
              !length(control_mult) %in% c(1, region$get_locations()))) {
    stop(paste("Control multiplier should be a vector with a value 0-1",
               "for each region location."), call. = FALSE)
  }
  if (!is.null(radius) && (!is.numeric(radius) || radius < 0)) {
    stop("The radius (m) parameter must be numeric and >= 0.", call. = FALSE)
  }
  if (!is.null(apply_to)) {
    if (!apply_to %in% c("reproduction", "survival")) {
      stop(paste("Growth control 'apply to' attribute should be",
                 "'reproduction' or 'survival'."), call. = FALSE)
    }
  } else {
   apply_to <- "both"
  }

  # Get results label
  self$get_label <- function() {
    return(paste0("control_", control_type))
  }

  # Control apply method
  self$apply <- function(n, tm) {

    # Search and destroy (combined detect and remove)
    if (control_type == "search_destroy") {

      # Initial zero controls
      controlled <- as.numeric(n)*0
      if (population_model$get_type() == "stage_structured") {
        controlled <- array(controlled, dim(n))
        colnames(controlled) <- attr(population_model$get_growth(), "labels")
      }

      # Scheduled time step?
      if (is.null(schedule) || tm %in% schedule) {

        # Occupied locations
        idx <- which(rowSums(
          as.matrix(n)[,self$get_stages(), drop = FALSE]) > 0)
        if (length(idx) > 0) {

          # Get control effectiveness (probability)
          control_pr <- control_design$get_manage_pr()[idx]

          # Sample number controlled and remove when search & destroy
          if (population_model$get_type() == "stage_structured") {
            for (i in self$get_stages()) {
              controlled[idx,i] <- stats::rbinom(length(idx), size = n[idx,i],
                                                 prob = control_pr)
              n[idx,i] <- n[idx,i] - controlled[idx,i]
            }
          } else {
            controlled[idx] <- stats::rbinom(length(idx), size = n[idx],
                                             prob = control_pr)
            if (population_model$get_type() == "presence_only") {
              n[idx] <- n[idx] & !controlled[idx]
            } else {
              n[idx] <- n[idx] - controlled[idx]
            }
          }
        }
      }

      # Attach controlled as an attribute via label
      if (population_model$get_type() == "presence_only") {
        attr(n, self$get_label()) <- as.logical(controlled)
      } else {
        attr(n, self$get_label()) <- controlled
      }
    }

    # Growth, spread, or establishment control
    if (control_type %in% c("growth", "spread", "establishment")) {

      # Initial no application locations
      n_apply <- rep(FALSE, region$get_locations())

      # Scheduled time step?
      if (is.null(schedule) || tm %in% schedule) {

        # Apply control to existing/known/scheduled treatment locations
        if (!is.null(control_design) &&
            !is.null(control_design$get_allocation())) {

          # Occupied locations
          idx <- which(rowSums(
            as.matrix(n)[,self$get_stages(), drop = FALSE]) > 0)
          if (length(idx) > 0) {

            # Get control application locations
            n_apply[idx] <- control_design$get_allocation()[idx] > 0
          }
        }

        # Detection-based control
        if ("detected" %in% names(attributes(n))) {

          # Detected locations
          idx <- which(rowSums(as.matrix(attr(n, "detected"))) > 0)

          # Expand control locations via radius
          if (is.numeric(radius) && length(idx) > 0 &&
              region$get_type() %in% c("grid", "patch")) {
            idx <- region$get_nearby(idx, radius)
            if (control_type %in% c("growth", "spread")) { # occupied only
              idx <- idx[which(rowSums(
                as.matrix(n)[,self$get_stages(), drop = FALSE])[idx] > 0)]
            }
          }

          # Set/update control locations
          if (length(idx) > 0) {
            n_apply[idx] <- TRUE
          }
        }
      }

      # Add control as attributes to process in growth or spread models
      attr(n, self$get_label()) <- n_apply*control_mult + !n_apply
      if (population_model$get_type() == "stage_structured") {
        if (control_type == "growth") {
          attr(attr(n, self$get_label()), "stages") <- self$get_stages()
          attr(attr(n, self$get_label()), "apply_to") <- apply_to
        }
      }
    }

    return(n)
  }

  return(self)
}
