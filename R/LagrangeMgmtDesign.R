#' Lagrange management design class builder
#'
#' Builds a generic class to represent management design functionality for
#' the effective allocation of management resources across one or more
#' divisions (parts, locations, categories, etc.) via Lagrange-based methods
#' for optimizing objective functions specified with management costs,
#' savings, benefits, and/or overall probability of management success (or
#' effectiveness).
#'
#' @param context A \code{ManageContext} or inherited class object representing
#'   the context of a biosecurity management resource allocation design.
#' @param divisions A \code{bsdesign::Divisions} or inherited class object
#'   representing one or more divisions (management responses, strategies,
#'   invasion pathways, invasive species, spatial locations, time, etc.) for
#'   the management design.
#' @param establish_pr A vector of (relative) probability values to represent
#'   the likelihood of pest establishment at each division part (location,
#'   category, etc.) specified by \code{divisions}. Values are assumed to be
#'   relative when their maximum is greater than 1, or an attribute
#'   \code{relative = TRUE} is attached to the parameter.
#' @param f_obj The objective function for calculating values to optimize
#'   (minimize) in the form \code{function(x_alloc)}, where \code{x_alloc}
#'   represents a candidate resource allocation at each division part
#'   (location, category, etc.) specified by \code{divisions}.
#' @param f_deriv The derivative of the objective function \code{f_obj} in the
#'   form \code{function(x_alloc)}, where \code{x_alloc} represents a candidate
#'   resource allocation at each division part (location, category, etc.)
#'   specified by \code{divisions}.
#' @param f_pos The pseudo-inverse of the derivative function \code{f_deriv} in
#'   the form \code{function(alpha)}, where \code{alpha} represents the
#'   marginal benefit value for a candidate resource allocation \code{x_alloc},
#'   which is returned by the function for each division part (location,
#'   category, etc.) specified by \code{divisions}.
#' @param alpha_unconstr The marginal benefit value to utilize when the search
#'   for the optimal resource allocation is not constrained via a \code{budget}
#'   or a desired overall effectiveness \code{overall_pr}.
#' @param alpha_min The minimum marginal benefit value to utilize when
#'   searching for the optimal resource allocation.
#' @param f_unit_eff A function for calculating the unit (division part)
#'   effectiveness, or probability of successful management of an incursion
#'   when present. The function should be in the form \code{function(x_alloc)},
#'   where \code{x_alloc} represents a candidate resource allocation at each
#'   division part (location, category, etc.) specified by \code{divisions}.
#' @param f_inv_unit_eff A function for calculating the inverse of the unit
#'   effectiveness, or candidate resource allocation. The function should be in
#'   the form \code{function(unit_eff)}, where \code{unit_eff} represents the
#'   unit effectiveness at each division part (location, category, etc.)
#'   specified by \code{divisions}.
#' @param budget The cost budget or constraint for the resource allocation in
#'   the management design. Default is \code{NULL}.
#' @param average_pr The desired (minimum) weighted average probability of
#'   success (or effectiveness) of the management design (e.g. 0.95). The
#'   weighted average is calculated using (relative) establishment probability
#'   (\code{establish_pr}) values. Default is \code{NULL}.
#' @param overall_pr The desired (minimum) overall system-wide probability of
#'   success (or effectiveness) of the management design (e.g. 0.95). Can only
#'   be used when actual (not relative) establishment probability
#'   (\code{establish_pr}) values are provided. Default is \code{NULL}.
#' @param min_alloc A vector of minimum permissible allocated management
#'   resource quantities at each division part (location, category, etc.)
#'   specified by \code{divisions}. Used to avoid impractically low allocation
#'   quantities. Default is \code{NULL}.
#' @param search_alpha A logical indicator to search for the optimal resource
#'   allocation, even when it is not constrained (via \code{budget} or
#'   \code{overall_pr}), such as when fixed costs are present. Default is
#'   \code{FALSE} indicates that search only occurs when constrained.
#' @param ... Additional parameters.
#' @return A \code{LagrangeMgmtDesign} class object (list) containing functions
#'   for allocating resource costs:
#'   \describe{
#'     \item{\code{get_cost_allocation()}}{Get allocated management resources
#'       (costs) via Lagrange-based method, utilizing savings, benefits, budget
#'       constraints, and/or desired system-wide effectiveness level.}
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
#' @include ManageContext.R
#' @export
LagrangeMgmtDesign <- function(context,
                               divisions,
                               establish_pr,
                               f_obj,
                               f_deriv,
                               f_pos,
                               alpha_unconstr,
                               alpha_min,
                               f_unit_eff,
                               f_inv_unit_eff,
                               budget = NULL,
                               average_pr = NULL,
                               overall_pr = NULL,
                               min_alloc = NULL,
                               search_alpha = FALSE, ...) {
  UseMethod("LagrangeMgmtDesign")
}

#' @name LagrangeMgmtDesign
#' @export
LagrangeMgmtDesign.ManageContext <- function(context,
                                             divisions,
                                             establish_pr,
                                             f_obj,
                                             f_deriv,
                                             f_pos,
                                             alpha_unconstr,
                                             alpha_min,
                                             f_unit_eff,
                                             f_inv_unit_eff,
                                             budget = NULL,
                                             average_pr = NULL,
                                             overall_pr = NULL,
                                             min_alloc = NULL,
                                             search_alpha = FALSE, ...) {

  # Check divisions
  if (!inherits(divisions, "Divisions")) {
    stop(paste("Divisions parameter must be a 'bsdesign::Divisions' or",
               "inherited class object."), call. = FALSE)
  }

  # Number of division parts
  parts <- divisions$get_parts()

  # Check establish_pr, alpha_unconstr, and alpha_min
  if (!is.numeric(establish_pr) || any(establish_pr < 0) ||
      length(establish_pr) != parts) {
    stop(paste("The establishment probability must be numeric,  >= 0, and",
               "match the number of division parts."), call. = FALSE)
  }
  if ((!is.null(attr(establish_pr, "relative")) &&
       as.logical(attr(establish_pr, "relative"))) || max(establish_pr) > 1) {
    relative_establish_pr <- TRUE
  } else {
    relative_establish_pr <- FALSE
  }
  if (!is.numeric(alpha_unconstr)) {
    stop("The unconstrained marginal benefit alpha value must be numeric.",
         call. = FALSE)
  }
  if (!is.numeric(alpha_min)) {
    stop("The minimum marginal benefit alpha value must be numeric.",
         call. = FALSE)
  }

  # Check f_obj, f_deriv, f_pos, f_unit_eff, and f_inv_unit_eff
  if (!is.function(f_obj) || length(formalArgs(f_obj)) != 1) {
    stop("The objective function should have form function(x_alloc).",
         call. = FALSE)
  }
  if (!is.function(f_deriv) || length(formalArgs(f_deriv)) != 1) {
    stop("The derivative function should have form function(x_alloc).",
         call. = FALSE)
  }
  if (!is.function(f_pos) || length(formalArgs(f_pos)) != 1) {
    stop(paste("The pseudo-inverse-derivative function should have form",
               "function(alpha)."), call. = FALSE)
  }
  if (!is.function(f_unit_eff) || length(formalArgs(f_unit_eff)) != 1) {
    stop("The unit effectiveness function should have form function(x_alloc).",
         call. = FALSE)
  }
  if (!is.function(f_inv_unit_eff) ||
      length(formalArgs(f_inv_unit_eff)) != 1) {
    stop(paste("The inverse unit effectiveness function should have form",
               "function(unit_effectiveness)."), call. = FALSE)
  }

  # Check budget, average_pr, overall_pr, min_alloc, and
  # search_alpha (indicator)
  if (!is.null(budget) && (!is.numeric(budget) || budget <= 0)) {
    stop("The budget parameter must be numeric and > 0.", call. = FALSE)
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
  if (!is.null(min_alloc) &&
      (!is.numeric(min_alloc) || !length(min_alloc) %in% c(1, parts))) {
    stop(paste("The minimum allocation parameter must be a numeric vector",
               "with values for each division part."), call. = FALSE)
  }
  if (!is.logical(search_alpha)) {
    stop("The search alpha indicator parameter must be logical.",
         call. = FALSE)
  }

  # Resolve min_alloc
  if (!is.null(min_alloc)) { # LATER -> discrete ####
    if (length(min_alloc) == 1) {
      min_alloc <- rep(min_alloc, parts)
    }
  } else {
    min_alloc <- rep(0, parts)
  }

  ## Lagrange optimization of allocated cost per division part x_alloc
  ## given the management resource quantity allocation qty_alloc
  ## where qty_alloc = (x_alloc - fixed_cost)/alloc_cost

  # Optimal cost allocation for alpha value within budget or overall
  # effectiveness level
  allocate <- function(alpha) {

    # Generate full allocation
    x_alloc <- f_pos(alpha)

    # Optimal within budget or target average or overall effectiveness
    if (is.numeric(budget) || is.numeric(average_pr)  ||
        is.numeric(overall_pr)) {

      # Order by f(f+(a))/f+(a)
      rank_values <- abs(f_obj(x_alloc)/x_alloc)
      rank_values[which(!is.finite(rank_values))] <- 0
      idx <- order(rank_values, decreasing = TRUE)

      # Determine allocation within budget
      nonzero <- which(x_alloc[idx] > 0)

      # Optimal within budget
      if (is.numeric(budget)) {
        cum_cost <- cumsum(x_alloc[idx][nonzero])
        over_budget <- which(cum_cost > budget)
        if (length(over_budget)) {
          x_alloc[idx][nonzero][over_budget[1]] <-
            (x_alloc[idx][nonzero][over_budget[1]] -
               max(cum_cost[over_budget[1]] - budget,
                   min_alloc[idx][nonzero][over_budget[1]]))
          x_alloc[idx][nonzero][over_budget[-1]] <- 0
        }
      }

      # Optimal up to average effectiveness level
      if (is.numeric(average_pr)) {

        # Unit effectiveness
        exist_eff <- f_unit_eff(0)
        new_eff <- f_unit_eff(x_alloc)

        # Calculate cumulative average effectiveness
        if (length(nonzero)) {
          cum_eff <- ((sum(establish_pr*exist_eff) +
                         cumsum((establish_pr*
                                   (new_eff - exist_eff))[idx][nonzero]))/
                        sum(establish_pr))
        } else {
          cum_eff <- 0
        }

        # Select allocation up to average effectiveness level
        over_eff <- which(cum_eff > average_pr)
        idx_w <- idx[nonzero][-over_eff] # within average_pr
        i_th <- idx[nonzero][over_eff[1]] # at threshold
        idx_o <- c(idx[nonzero][over_eff[-1]], idx[-nonzero]) # over or zero
        if (length(over_eff)) {
          th_eff <-
            (average_pr*sum(establish_pr) -
               (sum((establish_pr*new_eff)[idx_w]) +
                  sum((establish_pr*exist_eff)[idx_o])))/
            establish_pr[i_th]
          x_alloc[i_th] <- max(f_inv_unit_eff(th_eff)[i_th], min_alloc[i_th])
          x_alloc[idx_o] <- 0
        }

        # Add average effectiveness as an attribute
        attr(x_alloc, "average_pr") <-
          (sum(establish_pr*f_unit_eff(x_alloc))/sum(establish_pr))
      }

      # Optimal up to overall effectiveness level
      if (is.numeric(overall_pr)) {

        # Unit effectiveness
        exist_eff <- f_unit_eff(0)
        new_eff <- f_unit_eff(x_alloc)

        # Calculate overall effectiveness
        if (length(nonzero)) {
          cum_eff <-
            1 - ((1 - (prod(1 - establish_pr*(1 - exist_eff))*
                         cumprod(
                           ((1 - establish_pr*(1 - new_eff))/
                              (1 - establish_pr*(1 - exist_eff))
                           )[idx][nonzero])))/
                   (1 - prod(1 - establish_pr)))
        } else {
          cum_eff <- 0
        }

        # Select allocation up to overall effectiveness level
        over_eff <- which(cum_eff > overall_pr)
        idx_w <- idx[nonzero][-over_eff] # within overall_pr
        i_th <- idx[nonzero][over_eff[1]] # at threshold
        idx_o <- c(idx[nonzero][over_eff[-1]], idx[-nonzero]) # over or zero
        if (length(over_eff)) {
          th_eff <-
            1 - ((1 - ((1 - (1 - overall_pr)*(1 - prod(1 - establish_pr)))/
                         (prod((1 - establish_pr*(1 - new_eff))[idx_w])*
                            prod((1 - establish_pr*(1 - exist_eff))[idx_o])))
                  )/establish_pr[i_th])
          x_alloc[i_th] <- max(f_inv_unit_eff(th_eff)[i_th], min_alloc[i_th])
          x_alloc[idx_o] <- 0
        }

        # Add overall effectiveness as an attribute
        attr(x_alloc, "overall_pr") <-
          1 - ((1 - prod(1 - establish_pr*(1 - f_unit_eff(x_alloc))))/
                 (1 - prod(1 - establish_pr)))
      }
    }

    # Add total as an attribute
    attr(x_alloc, "total") <- sum(x_alloc)

    return(x_alloc)
  }

  # Create a class structure
  self <- structure(list(), class = "LagrangeMgmtDesign")

  # Get the allocated management resource costs for the design
  self$get_cost_allocation <- function() {

    # No constraint
    best_alpha <- alpha_unconstr

    # Search for minimum objective via marginal benefit (alpha) values
    if (is.numeric(budget) || is.numeric(average_pr) ||
        is.numeric(overall_pr) || search_alpha) {
      interval <- (0:100)/100*alpha_min
      alpha_range <- range(interval)[2] - range(interval)[1]
      precision <- 8 # for alpha
      while (alpha_range > abs(best_alpha*10^(-1*precision))) {

        # Get allocation for each alpha in interval
        alloc <- as.data.frame(t(sapply(interval[-1], function(a) {
          alloc <- allocate(a)
          c(obj = sum(f_obj(alloc)), total = attr(alloc, "total"),
            average_pr = attr(alloc, "average_pr"),
            overall_pr = attr(alloc, "overall_pr"))})))

        # Choose alpha corresponding to the last repeated minimum objective
        if (is.numeric(average_pr) && max(alloc$average_pr) >= average_pr ||
            is.numeric(overall_pr) && max(alloc$overall_pr) >= overall_pr) {
          idx <- c()
          if (is.numeric(average_pr) && max(alloc$average_pr) >= average_pr) {
            idx <- c(idx, which(alloc$average_pr >= average_pr))
          }
          if (is.numeric(overall_pr) && max(alloc$overall_pr) >= overall_pr) {
            idx <- sort(unique(c(idx, which(alloc$overall_pr >= overall_pr))))
          }
          idx <- idx[which(alloc$total[idx] <= min(alloc$total[idx]))]
          if (min(alloc$obj) < 0) {
            i <- max(idx[which(alloc$obj[idx] <=
                                 min(alloc$obj[idx])*(1 - 10^(-1*precision)))])
          } else {
            i <- max(idx[which(alloc$obj[idx] <=
                                 min(alloc$obj[idx])*(1 + 10^(-1*precision)))])
          }
        } else {
          i <- max(which(alloc$obj <= min(alloc$obj)))
        }
        best_alpha <- interval[i + 1]

        # Update interval and range for next iteration
        if (i < length(interval) - 1) {
          interval <- (0:100)/100*(interval[i + 2] - interval[i]) + interval[i]
        } else {
          interval <- (0:100)/100*(interval[i + 1] - interval[i]) + interval[i]
        }
        alpha_range <- range(interval)[2] - range(interval)[1]
      }
    }

    return(as.numeric(allocate(best_alpha)))
  }

  return(self)
}
