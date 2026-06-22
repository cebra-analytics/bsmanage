#' Simulation state for ManageSimulator runs
#'
#' @noRd
manage_sim_env <- function(region,
                           time_steps,
                           step_duration,
                           step_units,
                           collation_steps,
                           replicates,
                           result_stages,
                           initializer,
                           population_model,
                           dispersal_models,
                           impacts,
                           actions,
                           user_function) {
  sim_env <- new.env(parent = emptyenv())
  sim_env$region <- region
  sim_env$time_steps <- time_steps
  sim_env$step_duration <- step_duration
  sim_env$step_units <- step_units
  sim_env$collation_steps <- collation_steps
  sim_env$replicates <- replicates
  sim_env$result_stages <- result_stages
  sim_env$initializer <- initializer
  sim_env$population_model <- population_model
  sim_env$dispersal_models <- dispersal_models
  sim_env$impacts <- impacts
  sim_env$actions <- actions
  sim_env$user_function <- user_function
  sim_env$continued_incursions <- NULL
  sim_env$results <- NULL
  sim_env
}

#' Initialise ManageResults for a simulation run
#'
#' @noRd
manage_setup_results <- function(sim_env) {
  # Continued incursions function
  sim_env$continued_incursions <- sim_env$initializer$continued_incursions()

  # Results setup
  sim_env$results <- ManageResults(
    sim_env$region,
    sim_env$population_model,
    impacts = sim_env$impacts,
    actions = sim_env$actions,
    time_steps = sim_env$time_steps,
    step_duration = sim_env$step_duration,
    step_units = sim_env$step_units,
    collation_steps = sim_env$collation_steps,
    replicates = sim_env$replicates,
    combine_stages = sim_env$result_stages
  )
  invisible(sim_env)
}

#' Run one simulation replicate and collate into sim_env$results
#'
#' @noRd
run_one_replicate <- function(r, sim_env) {
  region <- sim_env$region
  time_steps <- sim_env$time_steps
  initializer <- sim_env$initializer
  population_model <- sim_env$population_model
  dispersal_models <- sim_env$dispersal_models
  impacts <- sim_env$impacts
  actions <- sim_env$actions
  user_function <- sim_env$user_function
  continued_incursions <- sim_env$continued_incursions
  results <- sim_env$results

  # Initialize population array
  n <- initializer$initialize()

  # Set diffusion attributes when spatially implicit (single patch)
  if (region$spatially_implicit()) {

    # Diffusion model
    if (any(sapply(dispersal_models,
                   function(dm) inherits(dm, "Diffusion")))) {
      idx <- which(sapply(dispersal_models,
                          function(dm) inherits(dm, "Diffusion")))[1]
      attr(n, "initial_n") <- n
      attr(n, "diffusion_rate") <-
        dispersal_models[[idx]]$get_diffusion_rate()
      attr(n, "diffusion_radius") <- 0
    }

    # Area spread model
    if (any(sapply(dispersal_models,
                   function(dm) inherits(dm, "AreaSpread")))) {
      capacity <- population_model$get_capacity()
      capacity_area <- attr(capacity, "area")
      if (population_model$get_type() == "stage_structured") {
        stages <- population_model$get_capacity_stages()
        attr(n, "spread_area") <-
          sum(n[, stages]) / as.numeric(capacity) * capacity_area
      } else { # unstructured
        attr(n, "spread_area") <- n / as.numeric(capacity) * capacity_area
      }
    }
  }

  # Calculate impacts
  if (length(impacts)) {
    calc_impacts <- lapply(impacts, function(impacts_i) {
      n <<- impacts_i$calculate(n, 0)
      attr(n, "impacts")
    })
    attr(n, "impacts") <- NULL

    # Apply any dynamically linked impacts to capacity
    population_model$set_capacity_mult(n)

  } else {
    calc_impacts <- NULL
  }

  # Apply actions
  if (length(actions)) {
    for (i in seq_along(actions)) {
      n <- actions[[i]]$apply(n, 0)
    }
  }

  # Initial results (t = 0)
  results$collate(r, 0, n, calc_impacts)

  # Time steps
  for (tm in seq_len(time_steps)) {

    # Population growth
    n <- population_model$grow(n, tm)

    # Dispersal for each spread vector
    if (length(dispersal_models)) {

      # Pack into list of original, remaining and relocated populations
      n <- dispersal_models[[1]]$pack(n)

      # Perform dispersal for each spread vector
      for (i in seq_along(dispersal_models)) {
        n <- dispersal_models[[i]]$disperse(n, tm)
      }

      # Unpack population array from separated list
      n <- dispersal_models[[1]]$unpack(n)
    }

    # Calculate impacts
    if (length(impacts)) {
      calc_impacts <- lapply(impacts, function(impacts_i) {
        n <<- impacts_i$calculate(n, tm)
        attr(n, "impacts")
      })
      attr(n, "impacts") <- NULL

      # Apply any dynamically linked impacts to capacity
      population_model$set_capacity_mult(n)

    }

    # Apply actions
    if (length(actions)) {

      # Clear attributes
      for (i in seq_along(actions)) {
        n <- actions[[i]]$clear_attributes(n)
      }

      # Apply sequentially
      for (i in seq_along(actions)) {
        n <- actions[[i]]$apply(n, tm)
      }
    }

    # User-defined function
    if (is.function(user_function)) {
      n_attr <- attributes(n) # get attributes
      if (length(formals(user_function)) == 3) {
        n <- user_function(n, r, tm)
      } else { # previously just n
        n <- user_function(n)
      }
      if (length(n_attr)) {
        for (i in seq_along(n_attr)) { # restore attributes
          if (!names(n_attr[i]) %in% names(attributes(n))) {
            attr(n, names(n_attr[i])) <- n_attr[[i]]
          }
        }
      }
    }

    # Collate results
    results$collate(r, tm, n, calc_impacts)

    # Continued incursions
    if (is.function(continued_incursions)) {
      n <- continued_incursions(tm, n)
    }

  } # time steps

  invisible(NULL)
}
