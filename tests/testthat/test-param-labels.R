dgp_params_unlabeled <- list(
  # use case: imagine they are a vector of beta coefficients
  1:100,
  200:240
)

dgp_params_labeled <- list(
  mydgpplab1 = 1:100,
  mydgpplab2 = 200:240
)

nvec_ <- c(5, 9)
nsims_1 <- 1

dgp_ <- function(n, dgp_param) {
  # use dgp_param just to trigger error if dgp_param is missing.
  if (identical(dgp_param, 3.11111)) {
    rnorm(n)
  } else {
    rnorm(n, sd = 1.01)
  }
}

statistic_1 <- function(dataf) {
  stat <- matrix(mean(dataf))
  rownames(stat) <- "mystatname"
  return(stat)
}

diagnostic_ <- function(stats_m, true_poi) {
  ret <- data.frame(mydiag = stats_m - mean(true_poi))
  return(ret)
}

dgpp_to_poi_unlabeled <- function(dgp_param) {
  return(dgp_param)
}

# This will give the same label to all DGP params
# and thus should trigger an error.
dgpp_to_poi_labeled_dups <- function(dgp_param) {
  ret_l <- list(dgp_param, "label_from_dgpp_to_poi")
  return(ret_l)
}

dgpp_to_poi_labeled_unique <- function(dgp_param) {
  ret_l <- list(dgp_param, min(dgp_param))
  return(ret_l)
}

mc_args_l <- list(
  dgp_params = dgp_params_unlabeled,
  nvec = nvec_,
  nsims = nsims_1,
  dgp = dgp_,
  statistics = statistic_1,
  diagnostics = diagnostic_,
  dgpp_to_poi = dgpp_to_poi_unlabeled,
  verbose = 0
)


mc_args_l1 <- mc_args_l
expect_error(do.call(mc_run, mc_args_l1), NA)

mc_args_l2 <- mc_args_l
mc_args_l2[["dgpp_to_poi"]] <- dgpp_to_poi_labeled_dups
# error because if user supplies labels, they should not
# be duplicates.
expect_error(do.call(mc_run, mc_args_l2))

mc_args_l3 <- mc_args_l
mc_args_l3[["dgpp_to_poi"]] <- dgpp_to_poi_labeled_unique
expect_error(do.call(mc_run, mc_args_l3), NA)

mc_args_l4 <- mc_args_l
mc_args_l4[["dgp_params"]] <- dgp_params_labeled
expect_error(do.call(mc_run, mc_args_l4), NA)
