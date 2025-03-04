#' Control design class builder
#'
#' Builds a class to represent management control functionality for the
#' effective allocation of management control resources across one or more
#' divisions (invasion pathways, invasive species, spatial locations, etc.) via
#' Lagrange-based methods for optimizing objective functions specified with
#' management costs, savings, benefits, and/or overall probability of
#' management success (or effectiveness).
#'
#' @param context A \code{ManageContext} or inherited class object representing
#'   the context of a biosecurity management resource allocation design.
#' @param divisions A \code{bsdesign::Divisions} or inherited class object
#'   representing one or more divisions (invasion pathways, invasive species,
#'   spatial locations, etc.) for the management design.
#' @param dim_type The type of dimension that the management resources are
#'   allocated across. One of \code{"spatial"} locations (default), invasive
#'   \code{"species"}, invasion \code{"pathways"}, or other user defined. The
#'   dimensions should be appropriately configured via the \code{divisions}
#'   object.
#' @param establish_pr A vector of (relative) probability values to represent
#'   the likelihood of pest establishment at each division part specified by
#'   \code{divisions}. Values are assumed to be relative when their maximum is
#'   greater than 1, or an attribute \code{relative = TRUE} is attached to the
#'   parameter.
#' @param lambda A vector of efficacy or control rates for each division part
#'   specified by \code{divisions}, such that the probability of successful
#'   control of an incursion when present at a part can be expressed via
#'  \code{pr(control|presence) = 1 - exp(-lambda*allocation)}, for a given
#'   continuous allocation of management control resources. Required unless
#'   \code{unit_pr} is provided for discrete allocation.
#' @param unit_pr A vector of unit effectiveness for each division part
#'   specified by \code{divisions}, such that the probability of successful
#'   control of an incursion when present at a part can be expressed via
#'   \code{pr(control|presence) = 1 - (1 - unit_pr)^allocation}, for a given
#'   discrete allocation of management control resources. Required unless
#'   \code{lambda} is provided for continuous allocation.
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
#' @param fixed_cost A vector of fixed costs, such as travel costs or time, at
#'   each division part specified by \code{divisions}. Default is \code{NULL}.
#'   Units should be consistent with \code{alloc_cost} when specified.
#'   Otherwise the units should be consistent with the \code{alloc_unit}
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
#' @param discrete_alloc A logical to indicate that the allocated management
#'   resource quantities at each division part specified by \code{divisions}
#'   should be discrete integers. Used to allocate discrete management resource
#'   units, such as traps or removals. Default is \code{FALSE} for continuous
#'   resources quantities, such as control hours.
#' @param exist_alloc A vector of existing management resource quantities at
#'   each division part specified by \code{divisions}. Should only be used to
#'   represent existing management resource allocation designs when
#'   \code{optimal = "none"}. Default is \code{NULL}.
#' @param exist_manage_pr A vector, or list of vectors, of probability of
#'   success (or effectiveness) values for existing management resources at
#'   each division part specified by \code{divisions}. Multiple existing
#'   management success probabilities may be specified in a list. Default is
#'   \code{NULL}.
#' @param previous_control A vector of control proportion values previously
#'   applied to each division part specified by \code{divisions}, along with an
#'   optional attached vector attribute \code{repeats} to indicate that the
#'   control was applied repeatedly. These values modify the establishment
#'   likelihood values utilised in the allocation design such that:
#'   \code{mod_establish_pr = establish_pr*(1 - previous_control)^repeats}.
#'   Default is \code{NULL}.
#' @param ... Additional parameters.
#' @return A \code{ControlDesign} class object (list) containing functions for
#'   allocating resources, and calculating (unit and overall) likely management
#'   success probabilities (or effectiveness):
#'   \describe{
#'     \item{\code{get_context()}}{Get context object.}
#'     \item{\code{get_divisions()}}{Get divisions object.}
#'     \item{\code{get_dim_type()}}{Get dimension type.}
#'     \item{\code{get_mod_establish_pr()}}{Get the modified establishment
#'       likelihood (only when previous control is specified).}
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
#'       effectiveness) values (\code{manage_pr}), and a \code{summary} (CSV)
#'       of the total allocation, costs (when applicable), the weighted average
#'       probability of success (\code{average_pr}), and (when available) the
#'       overall system-wide probability of success (or effectiveness) of the
#'       management design (\code{overall_pr}). \code{Terra} raster write
#'       options may be passed to the function for saving grid-based designs.}
#'   }
#' @references
#'   Cannon, R. M. (2009). Inspecting and monitoring on a restricted budget -
#'   where best to look? \emph{Preventive Veterinary Medicine}, 92(1–2),
#'   163-174. \doi{10.1016/j.prevetmed.2009.06.009}
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
#'   Moore, A. L., McCarthy, M. A., & Lecomte, N. (2016). Optimizing ecological
#'   survey effort over space and time.
#'   \emph{Methods in Ecology and Evolution}, 7(8), 891–899.
#'   \doi{10.1111/2041-210X.12564}
#' @include ManageDesign.R
#' @include LagrangeMgmtDesign.R
#' @export
ControlDesign <- function(context,
                          divisions,
                          dim_type = c("spatial",
                                       "species",
                                       "pathways",
                                       "user"),
                          establish_pr,
                          lambda = NULL,
                          unit_pr = NULL,
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
                          fixed_cost = NULL,
                          budget = NULL,
                          average_pr = NULL,
                          overall_pr = NULL,
                          min_alloc = NULL,
                          discrete_alloc = FALSE,
                          exist_alloc = NULL,
                          exist_manage_pr = NULL,
                          previous_control = NULL,
                          class = character(), ...) {
  UseMethod("ControlDesign")
}

#' @name ControlDesign
#' @export
ControlDesign.ManageContext <- function(context,
                                        divisions,
                                        dim_type = c("spatial",
                                                     "species",
                                                     "pathways",
                                                     "user"),
                                        establish_pr,
                                        lambda = NULL,
                                        unit_pr = NULL,
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
                                        fixed_cost = NULL,
                                        budget = NULL,
                                        average_pr = NULL,
                                        overall_pr = NULL,
                                        min_alloc = NULL,
                                        discrete_alloc = FALSE,
                                        exist_alloc = NULL,
                                        exist_manage_pr = NULL,
                                        previous_control = NULL,
                                        class = character(), ...) {

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

  # Build via base class (for checks)
  self <- ManageDesign(context = context,
                       divisions = divisions,
                       dim_type = dim_type,
                       establish_pr = establish_pr,
                       optimal = optimal,
                       alloc_unit = alloc_unit,
                       cost_unit = cost_unit,
                       benefit = benefit,
                       alloc_cost = alloc_cost,
                       budget = budget,
                       average_pr = average_pr,
                       overall_pr = overall_pr,
                       exist_alloc = exist_alloc,
                       exist_manage_pr = exist_manage_pr,
                       class = "ControlDesign", ...)
  super <- list(get_manage_pr = self$get_manage_pr)

  # Number of division parts
  parts <- divisions$get_parts()

  # Resolve if establish_pr is relative
  if ((!is.null(attr(establish_pr, "relative")) &&
       as.logical(attr(establish_pr, "relative"))) || max(establish_pr) > 1) {
    relative_establish_pr <- TRUE
  } else {
    relative_establish_pr <- FALSE
  }

  # Check lambda (continuous) and unit_pr (discrete)
  if (is.null(lambda) && is.null(unit_pr)) {
    stop(paste("Either the lambda (continuous) or unit_pr (discrete)",
               "parameter must be provided."), call. = FALSE)
  } else if (!is.null(lambda)) {

    # Check lambda
    if (!is.numeric(lambda) || any(lambda < 0) ||
        !length(lambda) %in% c(1, parts)) {
      stop(paste("The lambda parameter must be numeric, >= 0, and match the",
                 "number of division parts."), call. = FALSE)
    }

  } else if (!is.null(unit_pr)) {

    # Check unit_pr
    if (!is.numeric(unit_pr) || any(unit_pr < 0) ||
        !length(unit_pr) %in% c(1, parts)) {
      stop(paste("The unit_pr parameter must be numeric, >= 0, and match the",
                 "number of division parts."), call. = FALSE)
    }

    # Convert to lambda
    lambda <- -1*log(1 - unit_pr)
  }

  # Lambda value for each part
  if (length(lambda) == 1) {
    lambda <- rep(lambda, parts)
  }

  # Check fixed_cost, min_alloc, & discrete_alloc
  if (!is.null(fixed_cost) &&
      (!is.numeric(fixed_cost) || !length(fixed_cost) %in% c(1, parts))) {
    stop(paste("The fixed cost parameter must be a numeric vector with values",
               "for each division part."), call. = FALSE)
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

  # Check and apply previous_control
  if (!is.null(previous_control) &&
      (!is.numeric(previous_control) || any(previous_control < 0) ||
       !length(previous_control) %in% c(1, parts))) {
    stop(paste("The previous control must be a numeric vector with",
               "values >= 0 for each division part."), call. = FALSE)
  }
  if (!is.null(previous_control)) {
    repeats <- 1
    if (!is.null(attr(previous_control, "repeats"))) {
      repeats <- attr(previous_control, "repeats")
    }
    attributes(previous_control) <- NULL
    establish_pr <- establish_pr*(1 - previous_control)^repeats

    # Get the modified establishment likelihood
    self$get_mod_establish_pr <- function() {
      return(establish_pr)
    }
  }

  # Resolve alloc_cost, fixed_cost, min_alloc, and exist_manage_pr
  if (length(alloc_cost) == 1) {
    alloc_cost <- rep(alloc_cost, parts)
  } else if (is.null(alloc_cost)) {
    alloc_cost <- rep(1, parts)
  }
  if (length(fixed_cost) == 1) {
    fixed_cost <- rep(fixed_cost, parts)
  } else if (is.null(fixed_cost)) {
    fixed_cost <- rep(0, parts)
  }
  if (!is.null(min_alloc)) {
    if (length(min_alloc) == 1) {
      min_alloc <- rep(min_alloc, parts)
    }
    if (discrete_alloc) {
      min_alloc <- pmax(ceiling(min_alloc), 1)
    }
  } else {
    if (discrete_alloc) {
      min_alloc <- rep(1, parts)
    } else {
      min_alloc <- rep(0, parts)
    }
  }
  if (is.null(exist_manage_pr)) {
    exist_manage_pr <- rep(0, parts)
  } else {
    exist_manage_pr <- super$get_manage_pr() # combine via base class
  }

  # Uniform benefit when optimal saving or benefit not specified
  if (!optimal %in% c("saving", "benefit")) {
    benefit <- 1
  }

  ## Lagrange optimization of allocated cost per division part x_alloc
  ## given the management resource quantity allocation qty_alloc
  ## where qty_alloc = (x_alloc - fixed_cost)/alloc_cost

  # Lagrange optimization parameters/functions
  f_obj <- NULL # Objective function
  f_deriv <- NULL # Derivative of objective function
  f_pos <- NULL # Pseudo-inverse of derivative given marginal benefit alpha
  alpha_unconstr <- NULL # Unconstrained marginal benefit alpha
  alpha_min <- NULL # Minimum marginal benefit alpha
  f_unit_eff <- NULL # Unit effectiveness calculation function
  f_inv_unit_eff <- NULL # Inverse of unit effectiveness calculation function
  search_alpha <- NULL # Search alpha for optimal objective
  set_lagrange_params <- function() {

    # Objective function
    f_obj <<- function(x_alloc) {

      # Quantity allocation (units)
      n_alloc <- (x_alloc >= fixed_cost)*(x_alloc - fixed_cost)/alloc_cost

      if (optimal == "effectiveness" && !relative_establish_pr) {

        # maximum effectiveness
        return(-1*log(1 - (establish_pr*
                             ((1 - exist_manage_pr)*exp(-1*lambda*n_alloc)))))
      } else {

        # maximum saving/benefit (benefit = 1 for successes)
        incl_x <- (optimal == "saving")
        return(
          benefit*establish_pr*(1 - exist_manage_pr)*exp(-1*lambda*n_alloc) +
            (n_alloc > 0)*x_alloc*incl_x)
      }
    }

    # Derivative of objective function
    f_deriv <<- function(x_alloc) {

      # Quantity allocation (units)
      n_alloc <- (x_alloc >= fixed_cost)*(x_alloc - fixed_cost)/alloc_cost

      if (optimal == "effectiveness" && !relative_establish_pr) {

        # handle establish_pr of 1 via substituting for close to 1
        if (any(establish_pr == 1)) {
          establish_pr[which(establish_pr == 1)] <- 1 - 1e-16
        }

        # maximum effectiveness
        return((-1*establish_pr*(1 - exist_manage_pr)*
                  lambda/alloc_cost*exp(-1*lambda*n_alloc))/
                 (1 - (establish_pr*(1 - exist_manage_pr)*
                         exp(-1*lambda*n_alloc))))
      } else {

        # maximum saving/benefit (benefit = 1 for successes)
        incl_x <- (optimal == "saving")
        return((n_alloc > 0)*incl_x -
                 (benefit*establish_pr*(1 - exist_manage_pr)*
                    lambda/alloc_cost*exp(-1*lambda*n_alloc)))
      }
    }

    # Pseudo-inverse of derivative given marginal benefit alpha
    f_pos <<- function(alpha) {
      values <- lambda/alloc_cost*benefit*establish_pr*(1 - exist_manage_pr)
      idx <- which(values > 0)
      if (optimal == "effectiveness" && !relative_establish_pr) {

        # maximum effectiveness
        idx <- idx[which(1 - lambda[idx]/alloc_cost[idx]/alpha > 0)]
        values[-idx] <- 0
        values[idx] <- pmax(
          (alloc_cost[idx]/lambda[idx]*
             (log(-1*lambda[idx]/alloc_cost[idx]/alpha + 1) -
                log(1/establish_pr[idx]) +
                log(1 - exist_manage_pr[idx]))), 0)
        idx <- which(values > 0)
        values[idx] <- (pmax(min_alloc[idx]*alloc_cost[idx], values[idx]) +
                          fixed_cost[idx])

      } else {

        # maximum saving/benefit (benefit = 1 for successes)
        incl_x <- (optimal == "saving")
        idx <- idx[which((alpha - 1*incl_x) >= -1*values[idx])]
        values[-idx] <- 0
        values[idx] <-
          (pmax(min_alloc[idx]*alloc_cost[idx],
                (-1*alloc_cost[idx]/lambda[idx]*
                   log(-1*(alpha - 1*incl_x)/values[idx]))) +
             fixed_cost[idx])

        # limit to zero cost allocation via f_obj(0)
        if (optimal == "saving") {
          values <-
            (values < benefit*establish_pr*(1 - exist_manage_pr))*values
        }
      }

      return(values)
    }

    # Unconstrained marginal benefit alpha
    alpha_unconstr <<- (optimal == "saving") - 1

    # Minimum marginal benefit alpha
    alpha_min <<- min(f_deriv(fixed_cost))

    # Function for calculating unit effectiveness
    f_unit_eff <<- function(x_alloc) {
      return(1 - ((1 - exist_manage_pr)*
                    exp((-1*lambda*(x_alloc - (x_alloc > 0)*fixed_cost)/
                           alloc_cost))))
    }

    # Function for calculating inverse of unit effectiveness
    f_inv_unit_eff <<- function(unit_eff) {
      x_alloc <- -1*alloc_cost/lambda*log((1 - unit_eff)/(1 - exist_manage_pr))
      return(x_alloc + (x_alloc > 0)*fixed_cost)
    }

    # Search alpha for optimal objective (even when no constraints)
    search_alpha <<- any(fixed_cost > 0 | min_alloc > 0)
  }
  set_lagrange_params()

  # Function for calculating management probabilities or effectiveness
  calculate_manage_pr <- function(n_alloc) {
    return(1 - (1 - exist_manage_pr)*exp(-1*lambda*n_alloc))
  }

  # Function for calculating weighted average management probability
  calculate_average_pr <- function(manage_pr) {
    return(sum(establish_pr*manage_pr)/sum(establish_pr))
  }

  # Function for calculating overall management probability or effectiveness
  calculate_overall_pr <- function(manage_pr) {
    return(1 - ((1 - prod(1 - establish_pr*(1 - manage_pr)))/
                  (1 - prod(1 - establish_pr))))
  }

  # Get the allocated management resource values of the design
  qty_alloc <- NULL
  self$get_allocation <- function() {
    if (optimal != "none" && is.null(qty_alloc)) {

      if (discrete_alloc) {

        # Make a copy of altered parameters
        fixed_cost_orig <- fixed_cost
        budget_orig <- budget
        min_alloc_orig <- min_alloc
        exist_manage_pr_orig <- exist_manage_pr

        # Initial allocation
        qty_alloc <<- rep(0, parts)
      }

      # Iterative addition of discrete allocations or single continuous
      add_allocation <- TRUE
      while (add_allocation) {

        # Calculate minimum cost allocation
        if (any(min_alloc > 0)) {
          min_x_alloc <- min_alloc*alloc_cost + (min_alloc > 0)*fixed_cost
        } else {
          min_x_alloc <- min_alloc
        }

        # Get cost allocation x_alloc via Lagrange management design
        lagrangeMgmtDesign <- LagrangeMgmtDesign(context,
                                                 divisions,
                                                 establish_pr,
                                                 f_obj,
                                                 f_deriv,
                                                 f_pos,
                                                 alpha_unconstr,
                                                 alpha_min,
                                                 f_unit_eff,
                                                 f_inv_unit_eff,
                                                 budget = budget,
                                                 average_pr = average_pr,
                                                 overall_pr = overall_pr,
                                                 min_alloc = min_x_alloc,
                                                 search_alpha = search_alpha)
        x_alloc <- lagrangeMgmtDesign$get_cost_allocation()

        # Optimal resource allocation
        if (discrete_alloc) {

          # Add discrete allocation
          n_alloc <- floor((x_alloc >= fixed_cost)*
                             (x_alloc - fixed_cost)/alloc_cost)
          # n_alloc <- (n_alloc*alloc_cost > fixed_cost)*n_alloc
          qty_alloc <<- qty_alloc + n_alloc

          # Alter parameters and indicate further allocation required
          fixed_cost[which(qty_alloc > 0)] <<- 0
          exist_manage_pr <<- calculate_manage_pr(n_alloc)
          add_allocation <- (sum(x_alloc) > 0)
          if (is.numeric(budget)) {
            total_x_alloc <- sum(qty_alloc*alloc_cost +
                                   (qty_alloc > 0)*fixed_cost_orig)
            add_allocation <- (total_x_alloc < budget && add_allocation)
            budget <<- budget - total_x_alloc
          }
          if (is.numeric(average_pr)) {
            add_allocation <-
              (calculate_average_pr(exist_manage_pr) < average_pr &&
                 add_allocation)
          }
          if (!relative_establish_pr && is.numeric(overall_pr)) {
            add_allocation <-
              (calculate_overall_pr(exist_manage_pr) < overall_pr &&
                 add_allocation)
          }
          min_alloc[which(qty_alloc > 0)] <<- 1

          # Reset Lagrange parameters
          set_lagrange_params()

        } else {

          # Continuous allocation
          qty_alloc <<- ((x_alloc >= fixed_cost)*
                           (x_alloc - fixed_cost)/alloc_cost)
          add_allocation <- FALSE
        }
      }

      # Return altered parameters to their original values
      if (discrete_alloc) {
        fixed_cost <<- fixed_cost_orig
        budget <<- budget_orig
        min_alloc <<- min_alloc_orig
        exist_manage_pr <<- exist_manage_pr_orig
      }
    }

    return(qty_alloc)
  }

  # Get the management probabilities or effectiveness for each division part
  # of the design
  manage_pr <- NULL
  self$get_manage_pr <- function() {
    if (is.null(manage_pr)) {
      if (optimal != "none" && !is.null(qty_alloc)) {
        manage_pr <<- calculate_manage_pr(qty_alloc)
      } else if (optimal == "none" && !is.null(exist_alloc)) {
        manage_pr <<- calculate_manage_pr(exist_alloc)
      } else if (optimal == "none") {
        manage_pr <<- super$get_manage_pr()
      }
    }
    return(manage_pr)
  }

  # Get the weighted average management probability of success
  self$get_average_pr <- function() {
    average_eff <- NULL
    manage_pr <- self$get_manage_pr()
    if (!is.null(manage_pr)) {
      if (parts == 1) {
        average_eff <- manage_pr
      } else if (!is.null(establish_pr)) {
        average_eff <- calculate_average_pr(manage_pr)
      }
    }
    return(average_eff)
  }

  # Get the overall management probability or effectiveness of the design
  if (!relative_establish_pr) {
    self$get_overall_pr <- function() {
      system_eff <- NULL
      manage_pr <- self$get_manage_pr()
      if (!is.null(manage_pr)) {
        if (parts == 1) {
          system_eff <- manage_pr
        } else if (!is.null(establish_pr)) {
          system_eff <- calculate_overall_pr(manage_pr)
        }
      }
      return(system_eff)
    }
  }

  # Save the management design as a collection of appropriate files
  self$save_design <- function(...) {

    # Save allocation and management probability/effectiveness
    if (divisions$get_type() == "grid") {
      terra::writeRaster(divisions$get_rast(self$get_allocation()),
                         "allocation.tif", ...)
      terra::writeRaster(divisions$get_rast(self$get_manage_pr()),
                         "manage_pr.tif", ...)
    } else if (divisions$get_type() == "patch") {
      write.csv(cbind(divisions$get_coords(extra_cols = TRUE),
                      allocation = self$get_allocation(),
                      manage_pr = self$get_manage_pr()),
                file = "design.csv", row.names = FALSE)
    } else if (divisions$get_type() == "other") {
      write.csv(cbind(divisions$get_data(),
                      allocation = self$get_allocation(),
                      manage_pr = self$get_manage_pr()),
                file = "design.csv", row.names = FALSE)
    }

    # Save summary
    summary_data <- data.frame(total_allocation = sum(self$get_allocation()))
    if (!all(alloc_cost == 1)) {
      summary_data$allocation_cost <- sum(self$get_allocation()*alloc_cost)
    }
    if (!all(fixed_cost == 0)) {
      summary_data$fixed_cost <- sum((self$get_allocation() > 0)*fixed_cost)
    }
    if (optimal == "saving") {
      summary_data$total_saving <- sum(establish_pr*benefit*
                                         self$get_manage_pr())
    }
    summary_data$average_pr <- self$get_average_pr()
    if (relative_establish_pr) {
      summary_data$overall_pr <- self$get_overall_pr()
    }
    write.csv(summary_data, file = "summary.csv", row.names = FALSE)

    return(summary_data)
  }

  return(self)
}
