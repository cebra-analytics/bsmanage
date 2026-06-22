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
