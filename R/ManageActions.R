#' Manage actions class builder
#'
#' Builds a generic class for simulating the application of management actions,
#' such as detection, control, and removal.
#'
#' @param region A \code{bsspread::Region} or inherited class object
#'   representing the spatial region (template) for the incursion management
#'   simulations.
#' @param population_model A \code{bsspread::Population} or inherited class
#'   object defining the population representation for the management
#'   simulations.
#' @param type One of \code{"detection"} (default), \code{"control"},
#'   or \code{"removal"} to indicate the type of management actions applied.
#' @param apply_stages Numeric vector of population stages (indices) to which
#'   management actions are applied. Default is all stages (when set to
#'   \code{NULL}).
#' @param ... Additional parameters.
#' @return A \code{ManageActions} class object (list) containing a function for
#'   applying simulated management actions:
#'   \describe{
#'     \item{\code{get_type()}}{Get the management actions type.}
#'     \item{\code{apply(n)}}{Apply management actions to a simulated
#'       population vector or matrix \code{n}, potentially with attached
#'       attributes relating to previously applied actions, and return the
#'       resulting population \code{n} along with attached attributes relating
#'       to the newly applied actions.}
#'   }
#' @export
ManageActions <- function(region, population_model,
                          type = c("detection", "control", "removal"),
                          apply_stages = NULL,
                          class = character(), ...) {
  UseMethod("ManageActions")
}

#' @name ManageActions
#' @export
ManageActions.Region <- function(region, population_model,
                                 type = c("detection", "control", "removal"),
                                 apply_stages = NULL,
                                 class = character(), ...) {

  # Check the population model
  if (!is.null(population_model) &&
      !inherits(population_model, "Population")) {
    stop("Population model must be a 'Population' or inherited class object.",
         call. = FALSE)
  } else if (nrow(as.matrix(population_model$make(0))) !=
             region$get_locations()) {
    stop("Population model must be compatible with the region object.",
         call. = FALSE)
  }

  # Check the applied stages
  population_type <- population_model$get_type()
  if (population_type == "presence_only" ||
      population_type == "unstructured") {
    apply_stages <- 1
  } else if (population_type == "stage_structured") {
    if (is.null(apply_stages)) {
      apply_stages <- 1:population_model$get_stages()
    } else if (!is.numeric(apply_stages) ||
               !all(apply_stages %in% 1:population_model$get_stages())) {
      stop("Apply stages must be a vector of stage indices consistent ",
           "with the population model.", call. = FALSE)
    }
  }

  type <- match.arg(type)

  # Create a class structure
  self <- structure(list(), class = c(class, "ManageActions"))

  # Get type
  self$get_type <- function() {
    return(type)
  }

  # Generic apply method (overridden in inherited classes)
  self$apply <- function(x) return(x) # no change

  return(self)
}