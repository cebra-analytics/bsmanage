context("ManageSimulator parallel replicates")

make_spread_simulator <- function(replicates = 4L, parallel_cores = NULL) {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  region <- bsspread::Region(template)
  initial_n <- rep(0, region$get_locations())
  initial_n[5922] <- 10
  population_model <- bsspread::UnstructPopulation(region, growth = 1.5)
  initializer <- bsspread::Initializer(
    initial_n,
    region = region,
    population_model = population_model
  )
  dispersal <- bsspread::Dispersal(
    region,
    population_model,
    proportion = 1,
    max_distance = 1000
  )
  ManageSimulator(
    region,
    time_steps = 3,
    replicates = replicates,
    parallel_cores = parallel_cores,
    initializer = initializer,
    population_model = population_model,
    dispersal_models = list(dispersal)
  )
}

test_that("parallel FORK replicates match serial with per-replicate seeding", {
  skip_if_not(.Platform$OS.type == "unix", "FORK cluster requires Unix")
  simulator <- make_spread_simulator(replicates = 4L)
  expect_silent(
    res_serial <- simulator$run(
      random_seed = 100L,
      per_replicate_seed = TRUE
    )
  )
  simulator <- make_spread_simulator(replicates = 4L)
  res_parallel <- suppressMessages(simulator$run(
    parallel_replicates = TRUE,
    replicate_workers = 2L,
    cluster_type = "FORK",
    random_seed = 100L,
    per_replicate_seed = TRUE
  ))
  expect_equal(
    res_serial$get_list(),
    res_parallel$get_list()
  )
})

test_that("PSOCK parallel replicates require worker_init", {
  skip_if_not(.Platform$OS.type == "unix", "PSOCK test requires Unix cluster")
  simulator <- make_spread_simulator(replicates = 2L)
  expect_error(
    simulator$run(
      parallel_replicates = TRUE,
      replicate_workers = 2L,
      cluster_type = "PSOCK"
    ),
    "worker_init"
  )
})

test_that("replicate_workers defaults to min(parallel_cores, replicates)", {
  skip_if_not(.Platform$OS.type == "unix", "FORK cluster requires Unix")
  simulator <- make_spread_simulator(replicates = 6L, parallel_cores = 4L)
  res <- suppressMessages(simulator$run(
    parallel_replicates = TRUE,
    replicate_workers = 4L,
    cluster_type = "FORK",
    random_seed = 1L
  ))
  expect_is(res, "ManageResults")
})

test_that("timestep_callback fires once per time step in serial runs", {
  simulator <- make_spread_simulator(replicates = 1L)
  calls <- 0L
  timestep_cb <- function(tm, r, t0, t1, t2, t3, t4, t5,
                          n, gc_time_prev, collations = NULL) {
    calls <<- calls + 1L
    gc_time_prev
  }
  suppressMessages(simulator$run(
    timestep_callback = timestep_cb
  ))
  expect_equal(calls, 3L)
})

test_that("parallel FORK sim_env has no results before workers fork", {
  skip_if_not(.Platform$OS.type == "unix", "FORK cluster requires Unix")
  simulator <- make_spread_simulator(replicates = 2L)
  merge_cb <- function(phase, sim_env, ...) {
    if (identical(phase, "before_pool")) {
      expect_null(sim_env$results)
    }
    invisible(NULL)
  }
  suppressMessages(simulator$run(
    parallel_replicates = TRUE,
    replicate_workers = 2L,
    cluster_type = "FORK",
    parallel_merge_callback = merge_cb
  ))
})

test_that("parallel_merge_callback fires for each merged replicate", {
  skip_if_not(.Platform$OS.type == "unix", "FORK cluster requires Unix")
  simulator <- make_spread_simulator(replicates = 3L)
  phases <- character()
  merge_cb <- function(phase, sim_env, reps_merged, reps_total,
                       rep_outputs = NULL, out = NULL, ...) {
    phases <<- c(phases, phase)
    invisible(NULL)
  }
  suppressMessages(simulator$run(
    parallel_replicates = TRUE,
    replicate_workers = 2L,
    cluster_type = "FORK",
    parallel_merge_callback = merge_cb
  ))
  expect_true("before_pool" %in% phases)
  expect_true("after_merge" %in% phases)
  expect_equal(sum(grepl("^received r=", phases)), 3L)
  expect_equal(sum(grepl("^merged r=", phases)), 3L)
})

test_that("parallel runs attach parallel_stats to results", {
  skip_if_not(.Platform$OS.type == "unix", "FORK cluster requires Unix")
  simulator <- make_spread_simulator(replicates = 2L)
  res <- suppressMessages(simulator$run(
    parallel_replicates = TRUE,
    replicate_workers = 2L,
    cluster_type = "FORK"
  ))
  stats <- attr(res, "parallel_stats", exact = TRUE)
  expect_is(stats, "list")
  expect_equal(stats$reps, 2L)
  expect_true(is.numeric(stats$wall_s))
})
