#' Manage design base class builder
#'
#' Builds a base class to represent management design functionality for the
#' effective allocation of management resources across one or more divisions
#' (management responses, strategies, invasion pathways, invasive species,
#' spatial locations, time, etc.) via methods that utilize management costs,
#' savings, benefits, and/or overall management effectiveness or likely success
#' rate.
#'
#' @param context A \code{ManageContext} or inherited class object representing
#'   the context of a biosecurity management resource allocation design.
#' @param divisions A \code{bsdesign::Divisions} or inherited class object
#'   representing one or more divisions (management responses, strategies,
#'   invasion pathways, invasive species, spatial locations, time, etc.) for
#'   the management design.
#' @param establish_pr A vector of (relative) probability values to represent
#'   the likelihood of pest establishment at each division part (management
#'   responses, strategies, invasion pathways, invasive species, spatial
#'   locations, time, etc.) specified by \code{divisions}. Default is
#'   \code{NULL}. Values are assumed to be relative when their maximum is
#'   greater than 1, or an attribute \code{relative = TRUE} is attached to the
#'   parameter.
#' @param optimal The strategy used for finding an effective management
#'   resource allocation. One of (maximum) \code{"saving"} (or cost-dependent
#'   benefit), (maximum) \code{"benefit"} (independent of management resource
#'   costs), or (maximum) \code{"effectiveness"}, or \code{"none"} for
#'   representing existing management resource allocation designs only.
#' @param alloc_unit The descriptive unit to describe allocated management
#'   resource quantities. One of \code{"units"}, \code{"hours"},
#'   \code{"traps"}, \code{"treatments"}, \code{"removals"}, or user specified.
#' @param cost_unit The descriptive unit to describe allocated management
#'   resource costs, cost-based benefit savings, and/or fixed costs when
#'   applicable. One of \code{"$"}, \code{"hours"}, or user specified.
#' @param benefit A vector of values quantifying the benefit (or cost-based
#'   saving) associated with allocated management resources at each division
#'   part (management responses, strategies, invasion pathways, invasive
#'   species, spatial locations, time, etc.) specified by \code{divisions}.
#'   Default is \code{NULL}. When the benefit refers to cost-based savings
#'   (i.e. \code{optimal} is \code{"saving"}), then the units should be
#'   consistent with the \code{cost_unit} parameter.
#' @param alloc_cost A vector of cost per unit of allocated management
#'   resources at each division part (management responses, strategies,
#'   invasion pathways, invasive species, spatial locations, time, etc.)
#'   specified by \code{divisions}. Default is \code{NULL}. Units should be
#'   consistent with the \code{cost_unit} parameter.
#' @param fixed_cost A vector of fixed costs, such as travel costs or time, at
#'   each division part (management responses, strategies, invasion pathways,
#'   invasive species, spatial locations, time, etc.) specified by
#'   \code{divisions}. Default is \code{NULL}. Units should be consistent with
#'   \code{alloc_cost} when specified. Otherwise the units should be consistent
#'   with the \code{alloc_unit} parameter.
#' @param budget The cost budget or constraint for the resource allocation in
#'   the management design. Default is \code{NULL}. Units should be consistent
#'   with \code{alloc_cost} when specified. Otherwise the units should be
#'   consistent with the \code{alloc_unit} parameter.
#' @param overall_pr The desired (minimum) overall system-wide likely
#'   management effectiveness or success rate of the management design
#'   (e.g. 0.8) when \code{optimal} is \code{"effectiveness"}. Default is
#'   \code{NULL}.
#' @param min_alloc A vector of minimum permissible allocated management
#'   resource quantities at each division part (management responses,
#'   strategies, invasion pathways, invasive species, spatial locations, time,
#'   etc.) specified by \code{divisions}. Used to avoid impractically low
#'   allocation quantities. Default is \code{NULL}.
#' @param discrete_alloc A logical to indicate that the allocated management
#'   resource quantities at each division part (management responses,
#'   strategies, invasion pathways, invasive species, spatial locations, time,
#'   etc.) specified by \code{divisions} should be discrete integers. Used to
#'   allocate discrete management resource units, such as traps or removals.
#'   Default is \code{FALSE} for continuous resources quantities, such as
#'   control hours.
#' @param exist_alloc A vector of existing management resource quantities at
#'   each division part (management responses, strategies, invasion pathways,
#'   invasive species, spatial locations, time, etc.) specified by
#'   \code{divisions}. Should only be used to represent existing management
#'   resource allocation designs when \code{optimal = "none"}. Default is
#'   \code{NULL}.
#' @param exist_manage_pr A vector, or list of vectors, of existing management
#'   effectiveness or success probability values for existing management
#'   resources at each division part (management responses, strategies,
#'   invasion pathways, invasive species, spatial locations, time, etc.)
#'   specified by \code{divisions}. Multiple existing management success
#'   probabilities may be specified in a list. Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{ManageDesign} class object (list) containing functions for
#'   for allocating resources, and calculating (unit and overall) likely
#'   management effectiveness or success rates:
#'   \describe{
#'     \item{\code{get_context()}}{Get context object.}
#'     \item{\code{get_divisions()}}{Get divisions object.}
#'     \item{\code{get_allocation()}}{Get allocated resources via specified
#'       strategy, utilizing savings, benefits, budget constraints, and/or
#'       desired overall management success probability level.}
#'     \item{\code{get_manage_pr()}}{Get the division part management
#'       effectiveness or success probabilities of the allocated management
#'       design combined with any existing effectiveness or success
#'       probabilities specified via \code{exist_manage_pr}.}
#'     \item{\code{get_overall_pr()}}{Get the overall system-wide likely
#'       management effectiveness or success rate of the management design.}
#'     \item{\code{save_design(...)}}{Save the management design as a
#'       collection of raster TIF and/or comma-separated value (CSV) files,
#'       appropriate for the \code{divisions} type, including the management
#'       resource \code{allocation}, the likely management effectiveness or
#'       success rates (\code{manage_pr}), and a \code{summary} (CSV) of the
#'       total allocation, total costs (when applicable), and the overall
#'       system-wide effectiveness or likely management success rate.
#'       \code{Terra} raster write options may be passed to the function for
#'       saving grid-based designs.} ####
#'   }
#' @references
#'   Cannon, R. M. (2009). Inspecting and monitoring on a restricted budget -
#'   where best to look? \emph{Preventive Veterinary Medicine}, 92(1–2),
#'   163-174. \doi{10.1016/j.prevetmed.2009.06.009}
#' @include ManageContext.R
#' @export
ManageDesign <- function(context,
                         divisions,
                         establish_pr = NULL,
                         optimal = c("saving", "benefit", "effectiveness",
                                     "none"),
                         alloc_unit = c("units", "hours", "traps",
                                        "treatments", "removals", "user"),
                         cost_unit = c("$", "hours", "user"),
                         benefit = NULL,
                         alloc_cost = NULL,
                         fixed_cost = NULL,
                         budget = NULL,
                         overall_pr = NULL,
                         min_alloc = NULL,
                         discrete_alloc = FALSE,
                         exist_alloc = NULL,
                         exist_manage_pr = NULL,
                         class = character(), ...) {
  UseMethod("ManageDesign")
}

#' @name ManageDesign
#' @export
ManageDesign.ManageContext <- function(context,
                                       divisions,
                                       establish_pr = NULL,
                                       optimal = c("saving", "benefit",
                                                   "effectiveness", "none"),
                                       alloc_unit = c("units", "hours",
                                                      "traps", "treatments",
                                                      "removals", "user"),
                                       cost_unit = c("$", "hours", "user"),
                                       benefit = NULL,
                                       alloc_cost = NULL,
                                       fixed_cost = NULL,
                                       budget = NULL,
                                       overall_pr = NULL,
                                       min_alloc = NULL,
                                       discrete_alloc = FALSE,
                                       exist_alloc = NULL,
                                       exist_manage_pr = NULL,
                                       class = character(), ...) {

  # Check divisions
  if (!inherits(divisions, "Divisions")) {
    stop(paste("Divisions parameter must be a 'Divisions' or inherited class",
               "object."), call. = FALSE)
  }

  # Number of division parts
  parts <- divisions$get_parts()

  # Check establish_pr
  if (!is.null(establish_pr) &&
      (!is.numeric(establish_pr) || any(establish_pr < 0) ||
       !length(establish_pr) %in% c(1, parts))) {
    stop(paste("The establishment probability must be a numeric vector with",
               "values >= 0 for each division part."), call. = FALSE)
  }

  # Resolve if establish_pr is relative
  if (!is.null(establish_pr) &&
      ((!is.null(attr(establish_pr, "relative")) &&
        as.logical(attr(establish_pr, "relative"))) ||
       max(establish_pr) > 1)) {
    relative_establish_pr <- TRUE
  } else {
    relative_establish_pr <- FALSE
  }

  # Match optimal arguments
  optimal <- match.arg(optimal)

  # Check benefit, and overall_pr
  if (!is.null(benefit) &&
      (!is.numeric(benefit) || !length(benefit) %in% c(1, parts))) {
    stop(paste("The benefit parameter must be a numeric vector with values",
               "for each division part."), call. = FALSE)
  }
  if (!is.null(overall_pr) &&
      (!is.numeric(overall_pr) || overall_pr < 0 || overall_pr > 1)) {
    stop(paste("The overall probability/effectiveness parameter must be",
               "numeric, >= 0 and <= 1."), call. = FALSE)
  }

  # Ensure relevant parameters are present for optimal strategy
  if (optimal == "saving" && is.null(benefit)) {
    stop("The benefit parameter must be specified for optimal saving.",
         call. = FALSE)
  } else if (optimal == "benefit" && is.null(benefit)) {
    stop("The benefit parameter must be specified for optimal benefit.",
         call. = FALSE)
  } else if (optimal == "benefit" &&
             (is.null(budget) && is.null(overall_pr))) {
    stop(paste("Either the budget or overall probability/effectiveness",
               "parameter must be specified for optimal benefit."),
         call. = FALSE)
  } else if (optimal == "effectiveness" &&
             (is.null(budget) && is.null(overall_pr))) {
    stop(paste("Either the budget or overall probability/effectiveness",
               "parameter must be specified for optimal effectiveness."),
         call. = FALSE)
  }

  # Check alloc_cost, fixed_cost, budget, min_alloc, exist_alloc, &
  # exist_manage_pr
  if (!is.null(alloc_cost) &&
      (!is.numeric(alloc_cost) || !length(alloc_cost) %in% c(1, parts))) {
    stop(paste("The allocation cost parameter must be a numeric vector with",
               "values for each division part."), call. = FALSE)
  }
  if (!is.null(fixed_cost) &&
      (!is.numeric(fixed_cost) || !length(fixed_cost) %in% c(1, parts))) {
    stop(paste("The fixed cost parameter must be a numeric vector with values",
               "for each division part."), call. = FALSE)
  }
  if (!is.null(budget) && (!is.numeric(budget) || budget <= 0)) {
    stop("The budget parameter must be numeric and > 0.", call. = FALSE)
  }
  if (!is.null(min_alloc) &&
      (!is.numeric(min_alloc) || !length(min_alloc) %in% c(1, parts))) {
    stop(paste("The minimum allocation parameter must be a numeric vector",
               "with values for each division part."), call. = FALSE)
  }
  if (!is.logical(discrete_alloc)) {
    stop("The discrete allocation indicator parameter must be logical.",
         call. = FALSE)
  }
  if (!is.null(exist_alloc) && optimal != "none") {
    stop(paste("The existing allocation parameter should only be specified",
               "when the optimal parameter is 'none'."), call. = FALSE)
  }
  if (!is.null(exist_alloc) &&
      (!is.numeric(exist_alloc) || !length(exist_alloc) == parts)) {
    stop(paste("The existing allocation parameter must be a numeric vector",
               "with values for each division part."), call. = FALSE)
  }
  if (!is.null(exist_manage_pr) &&
      (!(is.numeric(exist_manage_pr) || is.list(exist_manage_pr)) ||
       (is.numeric(exist_manage_pr) && !length(exist_manage_pr) %in% c(1, parts)) ||
       (is.list(exist_manage_pr) &&
        !all(sapply(exist_manage_pr, length) %in% c(1, parts))))) {
    stop(paste("The existing management probability/effectiveness parameter",
               "must be a numeric vector, or list of numeric vectors, with",
               "values for each division part."), call. = FALSE)
  }

  # Create a class structure
  self <- structure(list(), class = c(class, "ManageDesign"))

  # Get context object
  self$get_context <- function() {
    return(context)
  }

  # Get divisions object
  self$get_divisions <- function() {
    return(divisions)
  }

  # Get the allocated surveillance resource quantities of the design
  self$get_allocation <- function() {
    # overridden in inherited classes
  }

  # Get the management probabilities or effectiveness for each division part
  # of the design
  manage_pr <- NULL
  self$get_manage_pr <- function() {

    # Combine (union) multiple management probabilities
    if (is.list(exist_manage_pr)) {
      manage_pr <<- 1
      for (i in 1:length(exist_manage_pr)) {
        manage_pr <<- manage_pr*(1 - exist_manage_pr[[i]])
      }
      manage_pr <<- 1 - manage_pr
    } else {
      manage_pr <<- exist_manage_pr
    }

    # Ensure values for each division part
    if (length(manage_pr) == 1) {
      manage_pr <<- rep(manage_pr, parts)
    }

    return(manage_pr)
  }

  # Get the overall management probability or effectiveness of the design
  self$get_overall_pr <- function() {
    # overridden in inherited classes
  }

  # Save the management design as a collection of appropriate files
  self$save_design <- function(...) {
    # overridden in inherited classes
  }

  return(self)
}
