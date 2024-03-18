#' Manage control design class builder
#'
#' Builds a class to represent management control functionality for the
#' effective allocation of management control resources across one or more
#' divisions (management responses, strategies, invasion pathways, invasive
#' species, spatial locations, time, etc.) via Lagrange-based methods for
#' optimizing objective functions specified with management costs, savings,
#' benefits, and/or overall management effectiveness or likely success rate.
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
#'   locations, time, etc.) specified by \code{divisions}. Values are assumed
#'   to be relative when their maximum is greater than 1, or an attribute
#'   \code{relative = TRUE} is attached to the parameter.
#' @param lambda A vector of efficacy or control rates for each spatial
#'   location specified by \code{divisions}, such that the probability of
#'   successful control of an incursion when present at a part can be expressed
#'   via \code{pr(control|presence) = 1 - exp(-lambda*allocation)}, for a given
#'   allocation of management control resources.
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
#' @return A \code{ManageControlDesign} class object (list) containing
#'   functions for for allocating resources, and calculating (unit and overall)
#'   likely management effectiveness or success rates:
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
#'       saving grid-based designs.}
#'   }
#' @references
#'   Cannon, R. M. (2009). Inspecting and monitoring on a restricted budget -
#'   where best to look? \emph{Preventive Veterinary Medicine}, 92(1–2),
#'   163-174. \doi{10.1016/j.prevetmed.2009.06.009}
#' @include ManageDesign.R
#' @export
ManageControlDesign <- function(context,
                                divisions,
                                establish_pr,
                                lambda,
                                optimal = c("saving", "benefit",
                                            "effectiveness", "none"),
                                alloc_unit = c("units", "hours", "traps",
                                               "treatments", "removals",
                                               "user"),
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
  UseMethod("ManageControlDesign")
}

#' @name ManageControlDesign
#' @export
ManageControlDesign.ManageContext <- function(context,
                                              divisions,
                                              establish_pr,
                                              lambda,
                                              optimal = c("saving",
                                                          "benefit",
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
                                              overall_pr = NULL,
                                              min_alloc = NULL,
                                              discrete_alloc = FALSE,
                                              exist_alloc = NULL,
                                              exist_manage_pr = NULL,
                                              class = character(), ...) {

  # Build via base class (for checks and overall probability/effectiveness)
  self <- ManageDesign(context = context,
                       divisions = divisions,
                       establish_pr = establish_pr,
                       lambda = lambda,
                       optimal = optimal,
                       alloc_unit = alloc_unit,
                       cost_unit = cost_unit,
                       benefit = benefit,
                       alloc_cost = alloc_cost,
                       fixed_cost = fixed_cost,
                       budget = budget,
                       overall_pr = overall_pr,
                       min_alloc = min_alloc,
                       discrete_alloc = discrete_alloc,
                       exist_alloc = exist_alloc,
                       exist_manage_pr = exist_manage_pr,
                       class = "ManageControlDesign", ...)
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

  # Check lambda
  if (!is.numeric(lambda) || any(lambda < 0) ||
      !length(lambda) %in% c(1, parts)) {
    stop(paste("The lambda parameter must be numeric, >= 0, and match the",
               "number of division parts."), call. = FALSE)
  } else if (length(lambda) == 1) {
    lambda <- rep(lambda, parts)
  }

  # Match optimal arguments
  optimal <- match.arg(optimal)

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

  # Check and resolve empty optimal strategy parameters
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
  f_unit_manage_pr <- NULL # Unit mgmt pr./effectiveness calculation function
  f_inv_unit_manage_pr <- NULL # Inverse of unit mgmt pr. calculation function
  search_alpha <- NULL # Search alpha for optimal objective
  set_lagrange_params <- function() {

    # Objective function
    f_obj <<- function(x_alloc) {

      # Quantity allocation (units)
      n_alloc <- (x_alloc >= fixed_cost)*(x_alloc - fixed_cost)/alloc_cost

      if (optimal == "effectiveness" && !relative_establish_pr) {

        # maximum effectiveness
        return(log(1 - (establish_pr*
                          (1 - ((1 - exist_manage_pr)*
                                  exp(-1*lambda*n_alloc))))))
      } else {

        # maximum benefit (benefit = 1 for effectiveness)
        incl_x <- (optimal %in% c("saving"))
        return((benefit*establish_pr*(1 - exist_manage_pr)*
                  exp(-1*lambda*n_alloc)) + (n_alloc > 0)*x_alloc*incl_x)
      }
    }

    # Derivative of objective function
    f_deriv <<- function(x_alloc) {

      # Quantity allocation (units)
      n_alloc <- (x_alloc >= fixed_cost)*(x_alloc - fixed_cost)/alloc_cost

      if (optimal == "effectiveness" && !relative_establish_pr) {

        # maximum effectiveness
        return(-1*establish_pr*(1 - exist_manage_pr)*
                 lambda/alloc_cost*exp(-1*lambda*n_alloc)/
                 (1 - (establish_pr*
                         (1 - ((1 - exist_manage_pr)*
                                 exp(-1*lambda*n_alloc))))))
      } else {

        # maximum benefit (benefit = 1 for effectiveness)
        incl_x <- (optimal %in% c("saving"))
        return((n_alloc > 0)*incl_x -
                 (benefit*establish_pr*(1 - exist_manage_pr)*
                    lambda/alloc_cost*exp(-1*lambda*n_alloc)))
      }
    }

    # Pseudo-inverse of derivative given marginal benefit alpha
    f_pos <<- function(alpha) {
      values <- lambda/alloc_cost*benefit*establish_pr*(1 - exist_manage_pr)
      idx <- which(values > 0)
      values[-idx] <- 0
      if (optimal == "effectiveness" && !relative_establish_pr) {

        # maximum effectiveness
        values[idx] <- pmax(
          ((alpha > -1*lambda[idx]/alloc_cost[idx])*
             (alloc_cost[idx]/lambda[idx]*
                (log(-1*lambda[idx]/alloc_cost[idx]/alpha - 1) -
                   log(1/establish_pr[idx] - 1) +
                   log(1 - exist_manage_pr[idx])))), 0)
        idx <- which(values > 0)
        values[idx] <- (pmax(min_alloc[idx]*alloc_cost[idx], values[idx]) +
                          fixed_cost[idx])

      } else {

        # maximum benefit (benefit = 1 for effectiveness)
        incl_x <- (optimal %in% c("saving"))
        values[idx] <-
          (((alpha - 1*incl_x) >= -1*values[idx])*
             (pmax(min_alloc[idx]*alloc_cost[idx],
                   (-1*alloc_cost[idx]/lambda[idx]*
                      log(-1*(alpha - 1*incl_x)/values[idx]))) +
                fixed_cost[idx]))

        # limit to zero cost allocation via f_obj(0)
        if (optimal %in% c("saving")) {
          values <-
            (values < benefit*establish_pr*(1 - exist_manage_pr))*values
        }
      }

      return(values)
    }

    # Unconstrained marginal benefit alpha
    alpha_unconstr <<- (optimal %in% c("saving")) - 1

    # Minimum marginal benefit alpha
    alpha_min <<- min(f_deriv(fixed_cost))

    # Function for calculating unit management probability/effectiveness
    f_unit_manage_pr <<- function(x_alloc) {
      return(1 - ((1 - exist_manage_pr)*
                    exp((-1*lambda*(x_alloc - (x_alloc > 0)*fixed_cost)/
                           alloc_cost))))
    }

    # Function for calculating inverse of unit management probability
    f_inv_unit_manage_pr <<- function(unit_manage_pr) {
      x_alloc <- (-1*alloc_cost/lambda*
                    log((1 - unit_manage_pr)/(1 - exist_manage_pr)))
      return(x_alloc + (x_alloc > 0)*fixed_cost)
    }

    # Search alpha for optimal objective (even when no constraints)
    search_alpha <<- any(fixed_cost > 0 | min_alloc > 0)
  }
  set_lagrange_params()

  # Function for calculating management probability/effectiveness
  calculate_manage_pr <- function(n_alloc) {
    return(1 - (1 - exist_manage_pr)*exp(-1*lambda*n_alloc))
  }

  # Function for calculating overall management probability
  calculate_overall_pr <- function(manage_pr) {
    if (relative_establish_pr) {
      return(sum(establish_pr*manage_pr)/sum(establish_pr))
    } else {
      return((1 - prod(1 - establish_pr*manage_pr))/
               (1 - prod(1 - establish_pr)))
    }
  }

  # Get the allocated surveillance resource quantities of the design
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

        # Get cost allocation x_alloc via Lagrange surveillance design
        lagrangeSurvDesign <- bsdesign::LagrangeSurvDesign(
          bsdesign::Context(""),
          divisions,
          establish_pr,
          f_obj,
          f_deriv,
          f_pos,
          alpha_unconstr,
          alpha_min,
          f_unit_sens = f_unit_manage_pr,
          f_inv_unit_sens = f_inv_unit_manage_pr,
          budget = budget,
          confidence = overall_pr,
          min_alloc = min_alloc,
          search_alpha = search_alpha)
        x_alloc <- lagrangeSurvDesign$get_cost_allocation()

        # Optimal resource allocation
        if (discrete_alloc) {

          # Add discrete allocation
          n_alloc <- floor((x_alloc >= fixed_cost)*
                             (x_alloc - fixed_cost)/alloc_cost)
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
          if (is.numeric(overall_pr)) {
            add_allocation <-
              (calculate_manage_pr(exist_manage_pr) < overall_pr &&
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

  # Get the overall management probability or effectiveness of the design
  self$get_overall_pr <- function() {
    overall_pr <- NULL
    manage_pr <- self$get_manage_pr()
    if (!is.null(manage_pr)) {
      if (parts == 1) {
        overall_pr <- manage_pr
      } else if (!is.null(establish_pr)) {
        overall_pr <- calculate_overall_pr(manage_pr)
      }
    }
    return(overall_pr)
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
    summary_data$overall_pr <- self$get_overall_pr()
    write.csv(summary_data, file = "summary.csv", row.names = FALSE)

    return(summary_data)
  }

  return(self)
}
