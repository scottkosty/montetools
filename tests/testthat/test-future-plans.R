dgp_params_ <- c(4)

nvec_ <- c(1)
nsims_1 <- 1

dgp_ <- function(n, dgp_param) {
  # use dgp_param just to trigger error if dgp_param is missing.
  if (identical(dgp_param, 3.11111)) {
    rnorm(n)
  } else {
    rnorm(n, sd = 1.01)
  }
}


statistic_4 <- function(dataf) {
  stat <- mean(dataf)
  return(c("mystatname" = stat))
}

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

args_ <- list(
  dgp_params = dgp_params_,
  nvec = nvec_,
  nsims = nsims_1,
  dgp = dgp_,
  statistics = statistic_4,
  verbose = 0
)


test_that("Check that default is sequential.", {
    mc <- do.call(mc_run, args_)
    # check that default is sequential.
    plans_ <- get_plans(mc)
    expect_true(all(plans_ == "sequential"))
})


if (run_unix_only_tests) {
  test_that("Our interactions with plan() are as expected.", {
  
    plan("multicore")
    mc_multicore <- do.call(mc_run, args_)
  
    plans_ <- get_plans(mc_multicore)
    expect_true(all(plans_ == "multicore"))
    # set back.
    plan("sequential")
  
    plan("multicore")
    args_[["parallel"]] <- FALSE
    mc2 <- do.call(mc_run, args_)
    plans_ <- get_plans(mc2)
    expect_true(all(plans_ == "sequential"))
    # test that on.exit() sets it back.
    expect_equal(current_plan(), "multicore")

    plan("sequential")
    args_[["parallel"]] <- TRUE
    mc3 <- do.call(mc_run, args_)
    plans_ <- get_plans(mc3)
    expect_true(all(plans_ == "multicore"))
    # test that on.exit() sets it back.
    expect_equal(current_plan(), "sequential")

    # multisession does not work well with devtools load_all (which I guess devtools::test() uses?):
    #  https://github.com/HenrikBengtsson/future/issues/206
    #plan("multisession")
    #mc_multisession <- do.call(mc_run, args_)
    #plan("sequential")
    #expect_equal(current_plan(), "sequential")
  })
}
