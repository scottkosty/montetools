dgp_params_ <- c(1, 2, 3)

nvec_ <- c(5, 9)

dgp_ <- function(n, dgp_param) {
  # use dgp_param just to trigger error if dgp_param is missing.
  if (identical(dgp_param, 3.11111)) {
    rnorm(n)
  } else {
    rnorm(n, sd = 1.01)
  }
}


statistic_multi_stochastic_mean <- function(dataf, dgp_param_shared) {
  bootstrap_stat1 <- mean(rnorm(10))
  stat1 <- mean(dataf) + bootstrap_stat1
  ret_m <- rbind(
    mean_bs = stat1
  )
  return(ret_m)
}


statistic_multi_stochastic_median <- function(dataf, dgp_param_shared) {
  bootstrap_stat1 <- median(rnorm(10))
  stat1 <- median(dataf) + bootstrap_stat1
  ret_m <- rbind(
    median_bs = stat1
  )
  return(ret_m)
}


statistic_multi_stochastic_l1 <- list(statistic_multi_stochastic_mean, statistic_multi_stochastic_median)
statistic_multi_stochastic_l2 <- list(statistic_multi_stochastic_median, statistic_multi_stochastic_mean)


gen_statistic_multi_stochastic <- function(reset_seed) {
  statistic_fn <- function(dataf, dgp_param_shared) {
    starting_seed <- rng_snapshot()

    stat1 <- statistic_multi_stochastic_mean(dataf = dataf, dgp_param_shared = dgp_param_shared)
  
    if (reset_seed) {
      restore_rng(starting_seed)
    }

    stat2 <- statistic_multi_stochastic_median(dataf = dataf, dgp_param_shared = dgp_param_shared)
  
    ret_m <- rbind(
      mean_bs = stat1,
      median_bs = stat2
    )
    return(ret_m)
  }
  return(statistic_fn)
}
statistic_multi_stochastic_noreset <- gen_statistic_multi_stochastic(reset_seed = FALSE)
statistic_multi_stochastic_reset <- gen_statistic_multi_stochastic(reset_seed = TRUE)

diagnostic_ <- function(stats_m, true_poi) {
  # just using true_poi to trigger error if pulled from missing attribue.
  stopifnot(!is.null(true_poi))
  if (is.matrix(stats_m)) {
    stopifnot(ncol(stats_m) == 1)
    # if we do not do this, then the column name in the returned data.frame
    # is not "mydiag" And the tests comparing output of diags fails.
    ret <- data.frame(mydiag = stats_m[, 1] + true_poi)
  } else {
    ret <- data.frame(mydiag = stats_m + true_poi)
  }
  return(ret)
}


nsims1 <- 3
nsims2 <- 5


mc_args_l <- list(
  dgp_params = dgp_params_,
  nvec = nvec_,
  nsims = nsims1 + nsims2,
  dgp = dgp_,
  verbose = 0
)


mc_args_local <- mc_args_l

set.seed(1)
mc_args_local[["statistics"]] <- statistic_multi_stochastic_noreset
mc_noreset <- do.call(mc_run, mc_args_local)
mc_args_local[["statistics"]] <- statistic_multi_stochastic_reset
mc_reset <- do.call(mc_run, mc_args_local)

mc_args_local[["statistics"]] <- statistic_multi_stochastic_l1
mc2 <- do.call(mc_run, mc_args_local)
mc_args_local[["statistics"]] <- statistic_multi_stochastic_l2
mc3 <- do.call(mc_run, mc_args_local)

# this would be true if the stats did not use "bootstrapping". But since they do,
# and the seed isn't reset, it is false.
expect_false(all.equal(mc_noreset, mc_reset, sensitive_to_stat_order = FALSE, verbose = 0))
#
expect_true(all.equal(mc_reset, mc2, sensitive_to_stat_order = FALSE))
expect_true(all.equal(mc2, mc3, sensitive_to_stat_order = FALSE))

mc_args_local[["statistics"]] <- statistic_multi_stochastic_mean
mc_mean <- do.call(mc_run, mc_args_local)
#
mc_args_local[["statistics"]] <- statistic_multi_stochastic_median
mc_median <- do.call(mc_run, mc_args_local)
#
mc_merged <- mc_merge(mc_mean, mc_median, verbose = 0)
# TODO: document that the following tests passing is not obvious. I think it only works because we reset
#       the seed (to core_seed or something like that?). Otherwise, it would not
#       work, because the seed is changed after dgp().
expect_true(all.equal(mc3, mc_merged, sensitive_to_stat_order = FALSE))
nsims_vec <- get_nsims_vec(mc_merged)
expect_identical(get_nsims_vec(mc_merged), get_nsims_vec(mc3))
expect_error(mc_reproduce(mc_merged, verbose = 0), NA)
