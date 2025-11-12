#' Manage design base class builder
#'
#' Builds a base class to represent management design functionality for the
#' effective allocation of management resources across one or more divisions
#' (management responses, strategies, invasion pathways, invasive species,
#' spatial locations, time, etc.) via methods that utilize management costs,
#' savings, benefits, and/or overall management probability of success (or
#' effectiveness).
#'
#' @param context A \code{ManageContext} or inherited class object representing
#'   the context of a biosecurity management resource allocation design.
#' @param divisions A \code{bsdesign::Divisions} or inherited class object
#'   representing one or more divisions (management responses, strategies,
#'   invasion pathways, invasive species, spatial locations, time, etc.) for
#'   the management design.
#' @param dim_type The type of dimension that the management resources are
#'   allocated across. One of \code{"spatial"} locations (default),
#'   \code{"temporal"}, \code{"spatiotemporal"}, invasive \code{"species"},
#'   invasion \code{"pathways"}, \code{"strategies"}, management
#'   \code{"responses"}, or other user defined. The dimensions should be
#'   appropriately configured via the \code{divisions} object.
#' @param establish_pr A vector of (relative) probability values to represent
#'   the likelihood of pest establishment at each division part specified by
#'   \code{divisions}. Default is \code{NULL}. Values are assumed to be
#'   relative when their maximum is greater than 1, or an attribute
#'   \code{relative = TRUE} is attached to the parameter.
#' @param optimal The strategy used for finding an effective management
#'   resource allocation. One of (maximum) \code{"saving"} (or cost-dependent
#'   benefit), (maximum) \code{"benefit"} (independent of management resource
#'   costs), (maximum) number of management \code{"successes"}, or (maximum)
#'   overall system-wide \code{"effectiveness"} (probability of success), or
#'   \code{"none"} for representing existing management resource allocation
#'   designs only. Maximum \code{"effectiveness"} can only be used when actual
#'   (not relative) establishment probability (\code{establish_pr}) values are
#'   provided.
#' @param alloc_unit The descriptive unit to describe allocated management
#'   resource quantities. One of \code{"units"}, \code{"hours"},
#'   \code{"traps"}, \code{"treatments"}, \code{"removals"}, or user specified.
#' @param cost_unit The descriptive unit to describe allocated management
#'   resource costs, cost-based benefit savings, and/or other costs when
#'   applicable. One of \code{"$"}, \code{"hours"}, or user specified.
#' @param benefit A vector of values quantifying the benefit (or cost-based
#'   saving) associated with allocated management resources at each division
#'   part specified by \code{divisions}. Default is \code{NULL}. When the
#'   benefit refers to cost-based savings (i.e. \code{optimal} is
#'   \code{"saving"}), then the units should be consistent with the
#'   \code{cost_unit} parameter.
#' @param alloc_cost A vector of cost per unit of allocated management
#'   resources at each division part specified by \code{divisions}. Default is
#'   \code{NULL}. Units should be consistent with the \code{cost_unit}
#'   parameter.
#' @param budget The cost budget or constraint for the resource allocation in
#'   the management design. Default is \code{NULL}. Units should be consistent
#'   with \code{alloc_cost} when specified. Otherwise the units should be
#'   consistent with the \code{alloc_unit} parameter.
#' @param average_pr The desired (minimum) weighted average probability of
#'   success (or effectiveness) of the management design (e.g. 0.95). The
#'   weighted average is calculated using (relative) establishment probability
#'   (\code{establish_pr}) values. Default is \code{NULL}.
#' @param overall_pr The desired (minimum) overall system-wide probability of
#'   success (or effectiveness) of the management design (e.g. 0.95). Can only
#'   be used when actual (not relative) establishment probability
#'   (\code{establish_pr}) values are provided. Default is \code{NULL}.
#' @param exist_alloc A vector of existing management resource quantities at
#'   each division part specified by \code{divisions}. Should only be used to
#'   represent existing management resource allocation designs when
#'   \code{optimal = "none"}. Default is \code{NULL}.
#' @param exist_manage_pr A vector, or list of vectors, of probability of
#'   success (or effectiveness) values for existing management resources at
#'   each division part specified by \code{divisions}. Multiple existing
#'   management success probabilities may be specified in a list. Default is
#'   \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{ManageDesign} class object (list) containing functions for
#'   allocating resources, and calculating (unit and overall) likely management
#'   success probabilities (or effectiveness):
#'   \describe{
#'     \item{\code{get_context()}}{Get context object.}
#'     \item{\code{get_divisions()}}{Get divisions object.}
#'     \item{\code{get_dim_type()}}{Get dimension type.}
#'     \item{\code{get_cost_unit()}}{Get cost unit.}
#'     \item{\code{get_allocation()}}{Get allocated resources via specified
#'       strategy, utilizing savings, benefits, budget constraints, and/or
#'       desired overall management success probability level.}
#'     \item{\code{get_manage_pr()}}{Get the probability of success (or
#'       effectiveness) values at each division part for the allocated
#'       management design, combined with any existing success probabilities or
#'       effectiveness specified via \code{exist_manage_pr}.}
#'     \item{\code{get_average_pr()}}{Get the weighted average probability
#'       of success (or effectiveness) of the management design. Weighted via
#'       \code{establish_pr} values.}
#'     \item{\code{get_overall_pr()}}{Get the overall system-wide probability
#'       of success (or effectiveness) of the management design. Only available
#'       when actual (not relative) \code{establish_pr} values are provided.}
#'     \item{\code{save_design(...)}}{Save the management design as a
#'       collection of raster TIF and/or comma-separated value (CSV) files,
#'       appropriate for the \code{divisions} type, including the management
#'       resource \code{allocation}, the probability of management success (or
#'       effectiveness) values (\code{manage_pr}), \code{manage_cost} (combined
#'       allocation and fixed costs), and a \code{summary} (CSV) of the total
#'       allocation, costs (when applicable), the weighted average probability
#'       of success (\code{average_pr}), and (when available) the overall
#'       system-wide probability of success (or effectiveness) of the
#'       management design (\code{overall_pr}). \code{Terra} raster write
#'       options may be passed to the function for saving grid-based designs.}
#'   }
#' @references
#'   Cannon, R. M. (2009). Inspecting and monitoring on a restricted budget -
#'   where best to look? \emph{Preventive Veterinary Medicine}, 92(1–2),
#'   163-174. \doi{10.1016/j.prevetmed.2009.06.009}
#'
#'   Hauser, C. E., Runge, M. C., Cooch, E. G., Johnson, F. A., & Harvey,
#'   I. W. F. (2007). Optimal control of Atlantic population Canada geese.
#'   \emph{Ecological Modelling}, 201(1), 27–36.
#'   \doi{10.1016/j.ecolmodel.2006.07.019}
#'
#'   Hauser, C. E., & McCarthy, M. A. (2009). Streamlining 'search and
#'   destroy': cost-effective surveillance for invasive species management.
#'   \emph{Ecology Letters}, 12(7), 683–692.
#'   \doi{10.1111/j.1461-0248.2009.01323.x}
#'
#'   Giljohann, K. M., Hauser, C. E., Williams, N. S. G., & Moore, J. L.
#'   (2011). Optimizing invasive species control across space: willow invasion
#'   management in the Australian Alps. \emph{Journal of Applied Ecology},
#'   48(5), 1286–1294. \doi{10.1111/j.1365-2664.2011.02016.x}
#'
#'   McCarthy, M. A., Thompson, C. J., Hauser, C., Burgman, M. A., Possingham,
#'   H. P., Moir, M. L., Tiensin, T., & Gilbert, M. (2010). Resource allocation
#'   for efficient environmental management. \emph{Ecology Letters}, 13(10),
#'   1280–1289. \doi{10.1111/j.1461-0248.2010.01522.x}
#'
#'   Moore, J. L., Rout, T. M., Hauser, C. E., Moro, D., Jones, M., Wilcox,
#'   C., & Possingham, H. P. (2010). Protecting islands from pest invasion:
#'   optimal allocation of biosecurity resources between quarantine and
#'   surveillance. \emph{Biological Conservation}, 143(5), 1068–1078.
#'   \doi{10.1016/j.biocon.2010.01.019}
#'
#'   Moore, A. L., McCarthy, M. A., & Lecomte, N. (2016). Optimizing ecological
#'   survey effort over space and time.
#'   \emph{Methods in Ecology and Evolution}, 7(8), 891–899.
#'   \doi{10.1111/2041-210X.12564}
#'
#'   Rout, T. M., Moore, J. L., Possingham, H. P., & McCarthy, M. A. (2011).
#'   Allocating biosecurity resources between preventing, detecting, and
#'   eradicating island invasions. \emph{Ecological Economics}, 71, 54–62.
#'   \doi{10.1016/j.ecolecon.2011.09.009}
#'
#'   Rout, T. M., Moore, J. L., & McCarthy, M. A. (2014). Prevent, search or
#'   destroy? A partially observable model for invasive species management.
#'   \emph{Journal of Applied Ecology}, 51(3), 804–813.
#'   \doi{10.1111/1365-2664.12234}
#' @include ManageContext.R
#' @export
ManageDesign <- function(context,
                         divisions,
                         dim_type = c("spatial",
                                      "temporal",
                                      "spatiotemporal",
                                      "species",
                                      "pathways",
                                      "strategies",
                                      "responses",
                                      "user"),
                         establish_pr = NULL,
                         optimal = c("saving",
                                     "benefit",
                                     "successes",
                                     "effectiveness",
                                     "none"),
                         alloc_unit = c("units",
                                        "hours",
                                        "traps",
                                        "treatments",
                                        "removals",
                                        "user"),
                         cost_unit = c("$",
                                       "hours",
                                       "user"),
                         benefit = NULL,
                         alloc_cost = NULL,
                         budget = NULL,
                         average_pr = NULL,
                         overall_pr = NULL,
                         exist_alloc = NULL,
                         exist_manage_pr = NULL,
                         class = character(), ...) {
  UseMethod("ManageDesign")
}

#' @name ManageDesign
#' @export
ManageDesign.ManageContext <- function(context,
                                       divisions,
                                       dim_type = c("spatial",
                                                    "temporal",
                                                    "spatiotemporal",
                                                    "species",
                                                    "pathways",
                                                    "strategies",
                                                    "responses",
                                                    "user"),
                                       establish_pr = NULL,
                                       optimal = c("saving",
                                                   "benefit",
                                                   "successes",
                                                   "effectiveness",
                                                   "none"),
                                       alloc_unit = c("units",
                                                      "hours",
                                                      "traps",
                                                      "treatments",
                                                      "removals",
                                                      "user"),
                                       cost_unit = c("$",
                                                     "hours",
                                                     "user"),
                                       benefit = NULL,
                                       alloc_cost = NULL,
                                       budget = NULL,
                                       average_pr = NULL,
                                       overall_pr = NULL,
                                       exist_alloc = NULL,
                                       exist_manage_pr = NULL,
                                       class = character(), ...) {

  # Check divisions
  if (!inherits(divisions, "Divisions")) {
    stop(paste("Divisions parameter must be a 'bsdesign::Divisions' or",
               "inherited class object."), call. = FALSE)
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

  # Match dim_type, optimal, alloc_unit, & cost_unit arguments
  if (!is.character(dim_type) || length(dim_type) > 1) {
    dim_type <- match.arg(dim_type)
  }
  optimal <- match.arg(optimal)
  if (!is.character(alloc_unit) || length(alloc_unit) > 1) {
    alloc_unit <- match.arg(alloc_unit)
  }
  if (!is.character(cost_unit) || length(cost_unit) > 1) {
    cost_unit <- match.arg(cost_unit)
  }

  # Check benefit, average_pr, and overall_pr
  if (!is.null(benefit) &&
      (!is.numeric(benefit) || !length(benefit) %in% c(1, parts))) {
    stop(paste("The benefit parameter must be a numeric vector with values",
               "for each division part."), call. = FALSE)
  }
  if (!is.null(average_pr) &&
      (!is.numeric(average_pr) || average_pr < 0 || average_pr > 1)) {
    stop(paste("The (weighted) average probability/effectiveness parameter",
               "must be numeric, >= 0 and <= 1."), call. = FALSE)
  }
  if (!is.null(overall_pr) && relative_establish_pr) {
    stop(paste("The overall probability/effectiveness parameter can only be",
               "used when actual (not relative) establishment probability",
               "values are provided."), call. = FALSE)
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
  } else if (optimal == "benefit" && !relative_establish_pr &&
             (is.null(budget) && is.null(average_pr) && is.null(overall_pr))) {
    stop(paste("Either the budget, or the average or overall probability",
               "parameter must be specified for optimal benefit."),
         call. = FALSE)
  } else if (optimal == "benefit" && relative_establish_pr &&
             (is.null(budget) && is.null(average_pr))) {
    stop(paste("Either the budget or the average probability parameter must",
               "be specified for optimal benefit."),
         call. = FALSE)
  } else if (optimal == "successes" && !relative_establish_pr &&
             (is.null(budget) && is.null(overall_pr))) {
    stop(paste("Either the budget, or the average or overall probability",
               "parameter must be specified for optimal management successes."),
         call. = FALSE)
  } else if (optimal == "successes" && relative_establish_pr &&
             (is.null(budget) && is.null(overall_pr))) {
    stop(paste("Either the budget or the average probability parameter must",
               "be specified for optimal management successes."),
         call. = FALSE)
  } else if (optimal == "effectiveness" &&
             (is.null(budget) && is.null(overall_pr))) {
    stop(paste("Either the budget, or the average or overall probability",
               "parameter must be specified for optimal effectiveness."),
         call. = FALSE)
  }

  # Check alloc_cost, budget, exist_alloc, & exist_manage_pr
  if (!is.null(alloc_cost) &&
      (!is.numeric(alloc_cost) || !length(alloc_cost) %in% c(1, parts))) {
    stop(paste("The allocation cost parameter must be a numeric vector with",
               "values for each division part."), call. = FALSE)
  }
  if (!is.null(budget) && (!is.numeric(budget) || budget <= 0)) {
    stop("The budget parameter must be numeric and > 0.", call. = FALSE)
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

  # Get dimension type
  self$get_dim_type <- function() {
    return(dim_type)
  }

  # Get cost unit
  self$get_cost_unit <- function() {
    return(cost_unit)
  }

  # Get the allocated surveillance resource quantities of the design
  self$get_allocation <- function() {
    return(exist_alloc)
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

  # Get the weighted average management probability of success
  self$get_average_pr <- function() {
    # overridden in inherited classes
  }

  # Get the overall management probability or effectiveness of the design
  if (!relative_establish_pr) {
    self$get_overall_pr <- function() {
      # overridden in inherited classes
    }
  }

  # Save the management design as a collection of appropriate files
  self$save_design <- function(...) {
    # overridden in inherited classes
  }

  return(self)
}
