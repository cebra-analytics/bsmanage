#' Manage controls class builder
#'
#' Builds a class for simulating the application of invasive species management
#' controls, such as combined surveillance and removal ("search & destroy"),
#' suppressing growth, fertility, spread, or establishment.
#'
#' @param region A \code{bsspread::Region} or inherited class object
#'   representing the spatial region (template) for the incursion management
#'   simulations.
#' @param population_model A \code{bsspread::Population} or inherited class
#'   object defining the population representation for the management
#'   simulations.
#' @param control_design A \code{ManageDesign}, \code{ControlDesign}, or
#'   inherited class object representing the distribution of control resources
#'   and their success probabilities or effectiveness.
#' @param control_type One of \code{"search_destroy"} (default),
#'   \code{"growth"} (including fertility), \code{"spread"}, or
#'   \code{"establishment"} to indicate the type of control applied.
#' @param stages Numeric vector of population stages (indices) to which
#'   management controls are applied. Default is all stages (when set to
#'   \code{NULL}).
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
ManageControls <- function(region, population_model, control_design,
                           control_type = c("search_destroy",
                                            "growth",
                                            "spread",
                                            "establishment"),
                           stages = NULL,
                           schedule = NULL, ...) {
  UseMethod("ManageControls")
}

#' @name ManageControls
#' @export
ManageControls.Region <- function(region, population_model, control_design,
                                  control_type = c("search_destroy",
                                                   "growth",
                                                   "spread",
                                                   "establishment"),
                                  stages = NULL,
                                  schedule = NULL, ...) {

  # Build via base class
  self <- ManageActions(region = region,
                        population_model = population_model,
                        type = "control",
                        stages = stages,
                        schedule = schedule,
                        class = "ManageControls")

  # Check the control design object
  if (!is.null(control_design) &&
      !inherits(control_design, "ManageDesign")) {
    stop(paste("Control design object must be a 'ControlDesign',",
               "'ManageDesign', or inherited class object."), call. = FALSE)
  } else if (control_design$get_divisions()$get_parts() !=
             region$get_locations()) {
    stop("Control design must be compatible with the region object.",
         call. = FALSE)
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
      }

      # Scheduled time step?
      if (is.null(schedule) || tm %in% schedule) {

        # Occupied locations
        idx <- which(rowSums(as.matrix(n)) > 0)
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

    } else { # growth, spread, or establishment
      attr(n, self$get_label()) <- control_design$get_manage_pr()
    }

    return(n)
  }

  return(self)
}
