#' Shared worker state for parallel replicate execution
#'
#' Populated on the parent before \code{makeCluster(FORK)} so forked workers
#' inherit valid terra external pointers. Socket workers repopulate via
#' \code{worker_init}.
#'
#' @noRd
manage_parallel_worker_state <- new.env(parent = emptyenv())

#' Force region and dispersal models to serial inner parallelism
#'
#' @noRd
force_serial_inner_parallel <- function(sim_env) {
  sim_env$region$set_cores(1)
  if (length(sim_env$dispersal_models)) {
    for (i in seq_along(sim_env$dispersal_models)) {
      sim_env$dispersal_models[[i]]$set_cores(1)
    }
  }
  invisible(NULL)
}

#' Resolve persistent cluster type from auto/FORK/PSOCK
#'
#' @noRd
parallel_resolve_cluster_type <- function(cluster_type = c("auto", "FORK", "PSOCK")) {
  cluster_type <- match.arg(cluster_type)
  if (cluster_type != "auto") {
    return(cluster_type)
  }
  if (.Platform$OS.type == "unix") {
    "FORK"
  } else {
    "PSOCK"
  }
}

#' Label for persistent parallel backend
#'
#' @noRd
parallel_persistent_backend_label <- function(cluster_type) {
  paste0(cluster_type, " persistent")
}

#' Merge deferred replicate collations into parent results
#'
#' @noRd
merge_replicate_collations <- function(out, sim_env) {
  for (col in out$collations) {
    sim_env$results$collate(col$r, col$tm, col$n, col$calc_impacts)
  }
  invisible(NULL)
}

#' Worker entry point for parallel replicate execution
#'
#' Reads simulation state from \code{manage_parallel_worker_state} so only
#' \code{r} is passed through \code{sendCall} (terra-safe on FORK workers).
#'
#' @noRd
manage_parallel_worker <- function(r) {
  sim_env <- manage_parallel_worker_state$sim_env
  random_seed <- manage_parallel_worker_state$random_seed
  per_replicate_seed <- manage_parallel_worker_state$per_replicate_seed
  if (per_replicate_seed && !is.null(random_seed)) {
    set.seed(random_seed + as.integer(r) - 1L)
  }
  force_serial_inner_parallel(sim_env)
  run_one_replicate(r, sim_env, defer_collate = TRUE)
}

#' Initialise a persistent parallel cluster for replicate execution
#'
#' @noRd
parallel_init_cluster <- function(n_workers,
                                  cluster_type,
                                  worker_init = NULL,
                                  psock_exports = NULL,
                                  psock_export_envir = .GlobalEnv) {
  cl <- parallel::makeCluster(n_workers, type = cluster_type, outfile = "")

  export_vars <- c(
    "manage_parallel_worker_state",
    "manage_parallel_worker",
    "run_one_replicate",
    "force_serial_inner_parallel"
  )
  parallel::clusterExport(cl, export_vars, envir = environment())

  if (cluster_type == "PSOCK") {
    if (!is.function(worker_init)) {
      parallel::stopCluster(cl)
      stop(
        paste(
          "PSOCK parallel replicates require worker_init(sim_env);",
          "terra-backed objects cannot be serialised to socket workers."
        ),
        call. = FALSE
      )
    }
    psock_vars <- unique(c("worker_init", psock_exports))
    parallel::clusterExport(cl, psock_vars, envir = psock_export_envir)
    parallel::clusterEvalQ(cl, {
      worker_init(manage_parallel_worker_state$sim_env)
      force_serial_inner_parallel(manage_parallel_worker_state$sim_env)
    })
  } else {
    parallel::clusterEvalQ(cl, {
      force_serial_inner_parallel(manage_parallel_worker_state$sim_env)
    })
  }

  attr(cl, "cluster_type") <- cluster_type
  cl
}

#' Run replicates on a persistent worker pool
#'
#' @noRd
run_parallel_replicates <- function(sim_env,
                                    n_workers,
                                    cluster_type = c("auto", "FORK", "PSOCK"),
                                    random_seed = NULL,
                                    per_replicate_seed = TRUE,
                                    worker_init = NULL,
                                    psock_exports = NULL,
                                    psock_export_envir = .GlobalEnv) {
  cluster_type <- parallel_resolve_cluster_type(cluster_type)
  n_workers <- min(as.integer(n_workers), sim_env$replicates)
  replicate_seq <- seq_len(sim_env$replicates)
  reps_total <- length(replicate_seq)

  if (!reps_total) {
    return(list(
      wall_s = 0,
      reps = 0L,
      cores = n_workers,
      backend = parallel_persistent_backend_label(cluster_type)
    ))
  }

  force_serial_inner_parallel(sim_env)

  manage_parallel_worker_state$sim_env <- sim_env
  manage_parallel_worker_state$random_seed <- random_seed
  manage_parallel_worker_state$per_replicate_seed <- per_replicate_seed

  cl <- parallel_init_cluster(
    n_workers,
    cluster_type = cluster_type,
    worker_init = worker_init,
    psock_exports = psock_exports,
    psock_export_envir = psock_export_envir
  )
  on.exit({
    parallel::stopCluster(cl)
    manage_parallel_worker_state$sim_env <- NULL
    manage_parallel_worker_state$random_seed <- NULL
    manage_parallel_worker_state$per_replicate_seed <- NULL
  }, add = TRUE)

  pending_reps <- as.list(replicate_seq)
  active_rep <- rep(NA_integer_, length(cl))

  for (i in seq_along(cl)) {
    if (!length(pending_reps)) {
      break
    }
    r <- pending_reps[[1L]]
    pending_reps <- pending_reps[-1L]
    active_rep[i] <- r
    parallel:::sendCall(cl[[i]], manage_parallel_worker, list(r = r))
  }

  rep_wall_start <- Sys.time()
  while (any(!is.na(active_rep))) {
    res <- parallel:::recvOneResult(cl)
    worker_i <- res$node
    if (inherits(res$value, "try-error")) {
      stop(sprintf(
        "Parallel replicates: replicate %d FAILED on worker %d:\n%s",
        active_rep[worker_i],
        worker_i,
        as.character(res$value)
      ), call. = FALSE)
    }
    merge_replicate_collations(res$value, sim_env)
    active_rep[worker_i] <- NA_integer_

    if (length(pending_reps)) {
      r <- pending_reps[[1L]]
      pending_reps <- pending_reps[-1L]
      active_rep[worker_i] <- r
      parallel:::sendCall(cl[[worker_i]], manage_parallel_worker, list(r = r))
    }
  }

  list(
    wall_s = as.numeric(Sys.time() - rep_wall_start, units = "secs"),
    reps = reps_total,
    cores = n_workers,
    backend = parallel_persistent_backend_label(cluster_type)
  )
}
