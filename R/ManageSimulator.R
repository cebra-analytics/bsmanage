#' Manage simulator class builder
#'
#' Builds a class to configure and run replicate discrete-time incursion
#' management simulations over a given spatial region using (sub)models for
#' population growth, dispersal, impacts, detection, and management. Extends
#' the \code{bsspread::Simulator} package module.
#'
#' @param region A \code{raster::RasterLayer}, \code{terra::SpatRaster}, or
#'   \code{bsspread::Region} or inherited class object representing the spatial
#'   region (template) for the incursion management simulations.
#' @param time_steps The number of discrete time steps to simulate. Default is
#'   1.
#' @param step_duration The duration of the simulation time steps in units
#'   specified by \code{step_units}. Default is 1.
#' @param step_units The units for the simulation step duration
#'   (\code{step_duration}) as a character string. Default is "years".
#' @param collation_steps The interval in time steps for collating results.
#'   Default is 1, that is, results are collated at every time step.
#' @param replicates The number of replicate or repeated simulations to be run.
#'   Default is 1. Note that replicate simulations results are collated as
#'   summary statistics across simulations.
#' @param result_stages Optionally combine (sum) specified stages (a vector of
#'   stage indices) of stage-based population results. The default
#'   (\code{NULL}) maintains results for each stage.
#' @param parallel_cores Number of cores available for parallel processing.
#'   The default NULL implies no parallel processing.
#' @param initializer A \code{bsspread::Initializer} or inherited class object
#'   for generating the initial invasive species population distribution or
#'   incursion locations, as well as optionally generating subsequent
#'   incursions during the incursion management simulations.
#' @param population_model A \code{bsspread::Population} or inherited class
#'   object defining the population representation and growth functionality for
#'   the incursion management simulations.
#' @param dispersal_models A list of \code{bsspread::Dispersal} or inherited
#'   class objects defining the dispersal functionality for the different
#'   spread vectors to be simulated.
#' @param impacts A list of \code{ManageImpacts} class objects specifying how
#'   to calculate various impacts of the simulated population at each time
#'   step. Each impact object encapsulates a \code{bsimpact::ImpactAnalysis} or
#'   inherited class object.
#' @param user_function An optional user-defined \code{function(n)} that is
#'   applied to the population vector or matrix \code{n} (returning a
#'   transformed \code{n}) prior to collating the results at each simulation
#'   time step.
#' @param ... Additional parameters.
#' @return A \code{ManageSimulator} class object (list) containing functions for
#'   setting objects (in the function environment) and running the simulations:
#'   \describe{
#'     \item{\code{set_initializer(object)}}{Set the initializer object.}
#'     \item{\code{set_population_model(model)}}{Set the population model
#'       object.}
#'     \item{\code{set_dispersal_models(models)}}{Set the list of dispersal
#'       model objects.}
#'     \item{\code{run()}}{Run the simulations and return the results.}
#'   }
#' @references
#'   Bradhurst, R., Spring, D., Stanaway, M., Milner, J., & Kompas, T. (2021).
#'   A generalised and scalable framework for modelling incursions,
#'   surveillance and control of plant and environmental pests.
#'   \emph{Environmental Modelling & Software}, 139, N.PAG.
#'   \doi{10.1016/j.envsoft.2021.105004}
#'
#'   García Adeva, J. J., Botha, J. H., & Reynolds, M. (2012). A simulation
#'   modelling approach to forecast establishment and spread of Bactrocera
#'   fruit flies. \emph{Ecological Modelling}, 227, 93–108.
#'   \doi{10.1016/j.ecolmodel.2011.11.026}
#' @include ManageResults.R
#' @export
ManageSimulator <- function(region,
                            time_steps = 1,
                            step_duration = 1,
                            step_units = "years",
                            collation_steps = 1,
                            replicates = 1,
                            result_stages = NULL,
                            parallel_cores = NULL,
                            initializer = NULL,
                            population_model = NULL,
                            dispersal_models = list(),
                            impacts = list(),
                            user_function = NULL, ...) {
  UseMethod("ManageSimulator")
}

#' @name ManageSimulator
#' @export
ManageSimulator.Raster <- function(region, ...) {
  # Call Region class version
  ManageSimulator(bsspread::Region(region), ...)
}

#' @name ManageSimulator
#' @export
ManageSimulator.SpatRaster <- function(region, ...) {
  # Call Region class version
  ManageSimulator(bsspread::Region(region), ...)
}

#' @name ManageSimulator
#' @export
ManageSimulator.Region <- function(region,
                                   time_steps = 1,
                                   step_duration = 1,
                                   step_units = "years",
                                   collation_steps = 1,
                                   replicates = 1,
                                   result_stages = NULL,
                                   parallel_cores = NULL,
                                   initializer = NULL,
                                   population_model = NULL,
                                   dispersal_models = list(),
                                   impacts = list(),
                                   user_function = NULL, ...) {

  # Create a bsspread::Simulator class structure (includes validation)
  self <- bsspread::Simulator(region,
                              time_steps = time_steps,
                              step_duration = step_duration,
                              step_units = step_units,
                              collation_steps = collation_steps,
                              replicates = replicates,
                              result_stages = result_stages,
                              parallel_cores = parallel_cores,
                              initializer = initializer,
                              population_model = population_model,
                              dispersal_models = dispersal_models,
                              user_function = user_function, ...)

  # Check impact objects
  if (length(impacts) &&
      !all(unlist(lapply(impacts, inherits, "ManageImpacts")))) {
    stop("Impacts must be a list of 'ManageImpacts' objects.", call. = FALSE)
  }

  # Extend (override) run simulator function
  results <- NULL # DEBUG ####
  self$run <- function() {

    # Should at least have an initializer and a population model
    object_absence <- c(is.null(initializer), is.null(population_model))
    if (any(object_absence)) {
      stop(sprintf("The simulator requires the %s to be set.",
                   paste(c("initializer", "population model")[object_absence],
                         collapse = " and ")),
           call. = FALSE)
    }

    # Continued incursions function
    continued_incursions <- initializer$continued_incursions()

    # Results setup
    results <<- ManageResults(region, population_model, # DEBUG ####
                              impacts = impacts,
                              time_steps = time_steps,
                              step_duration = step_duration,
                              step_units = step_units,
                              collation_steps = collation_steps,
                              replicates = replicates,
                              combine_stages = result_stages)

    # Replicates
    for (r in 1:replicates) {

      # Initialize population array
      n <- initializer$initialize()

      # Set diffusion attributes when spatially implicit (single patch)
      if (region$spatially_implicit()) {
        attr(n, "initial_n") <- n
        attr(n, "diffusion_rate") <-
          unlist(lapply(dispersal_models, function(dm) {
            if (inherits(dm, "Diffusion")) {
              dm$get_diffusion_rate()
            }
          }))[1] # assume one only
        if (is.numeric(attr(n, "diffusion_rate"))) {
          attr(n, "diffusion_radius") <- 0
        }
      }

      # Calculate impacts
      if (length(impacts)) {
        calc_impacts <- list()
        for (i in 1:length(impacts)) {
          calc_impacts[[i]] <- impacts[[i]]$calculate(n)
        }
      }

      # Initial results (t = 0)
      results$collate(r, 0, n, calc_impacts)

      # Time steps
      for (tm in 1:time_steps) {

        # Set time step attribute when spatially implicit (single patch)
        if (region$spatially_implicit()) {
          attr(n, "tm") <- tm
        }

        # Population growth
        n <- population_model$grow(n)

        # Dispersal for each spread vector
        if (length(dispersal_models)) {

          # Pack into list of original, remaining and relocated populations
          n <- dispersal_models[[1]]$pack(n)

          # Perform dispersal for each spread vector
          for (i in 1:length(dispersal_models)) {
            n <- dispersal_models[[i]]$disperse(n)
          }

          # Unpack population array from separated list
          n <- dispersal_models[[1]]$unpack(n)
        }

        # Calculate impacts
        if (length(impacts)) {
          calc_impacts <- lapply(impacts, function(impacts_i) {
            impacts_i$calculate(n)
          })
        }

        # User-defined function
        if (is.function(user_function)) {
          n <- user_function(n)
        }

        # Collate results
        results$collate(r, tm, n, calc_impacts)

        # Continued incursions
        if (is.function(continued_incursions)) {
          n <- continued_incursions(tm, n)
        }

      } # time steps

    } # replicates

    # Finalize results
    results$finalize()

    # Return results
    return(results)
  }

  return(self)
}