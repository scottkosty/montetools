# todo: import MC object from some .Rds file somewhere? rather
# than regenerating it here...


dgp_params_ <- c(1, 2, 3)
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


statistic <- function(dataf) {
  stat <- matrix(mean(dataf))
  rownames(stat) <- "mystatname"
  return(stat)
}


# only diff between diagnostic_df and diagnostic_m is the
# return statement
diagnostic_df <- function(stats_m, true_poi) {
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
  ret_m <- stats_m + true_poi
  colnames(ret_m) <- "mydiag"
  # return(ret_m)
  return(ret)
}


diagnostic_m <- function(stats_m, true_poi) {
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
  ret_m <- stats_m + true_poi
  colnames(ret_m) <- "mydiag"
  return(ret_m)
  #return(ret)
}


# only diff is we do not add a column name.
diagnostic_m_nonames <- function(stats_m, true_poi) {
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
  ret_m <- stats_m + true_poi
  #colnames(ret_m) <- "mydiag"
  return(ret_m)
  #return(ret)
}


mc <- mc_run(
         dgp_params = dgp_params_,
         nvec = nvec_,
         nsims = 2,
         dgp = dgp_,
         statistics = statistic,
         verbose = 0
)


test_that("A diagnostic can return a matrix with column names.", {
  diags1 <- mc_diags(mc, diagnostics = diagnostic_df)
  diags2 <- mc_diags(mc, diagnostics = diagnostic_m)
  expect_equal(diags1, diags2)
  # TODO  for some reason, matching the message (e.g., putting "no names")
  #       doesn't work. try again sometime. Try to make a minimal example
  #       that will help me solve the problem here or report upstream to
  #       testthat.
  # expect_warning(mc_diags(mc, diagnostics = diagnostic_m_nonames))
})


statistic_ci <- function(dataf) {
  mean_ <- mean(dataf)
  stat <- matrix(c(mean_ - 1, mean_, mean_ + 1), ncol = 3)
  rownames(stat) <- "mystatname"
  return(stat)
}


mc_ci <- mc_run(
         dgp_params = dgp_params_,
         nvec = nvec_,
         nsims = 2,
         dgp = dgp_,
         statistics = statistic_ci,
         verbose = 0
)


test_that("CI diag works as expected.", {
  expect_error(mc_diags(mc_ci, diagnostics = gen_diags_ci()), NA)
})


statistic_ci_always_error <- function(dataf) {
  stop("always give an error")
  mean_ <- mean(dataf)
  stat <- matrix(c(mean_ - 1, mean_, mean_ + 1), ncol = 3)
  rownames(stat) <- "mystatname"
  return(stat)
}


set.seed(3)
mc_ci_always_error <- mc_run(
         dgp_params = dgp_params_,
         nvec = nvec_,
         nsims = 2,
         dgp = dgp_,
         statistics = ret_na_if_error(statistic_ci_always_error, verbose = 0),
         verbose = 0
)


test_that("CI diag works if stat returns always has errors.", {
  expect_error(mc_diags(mc_ci_always_error, diagnostics = gen_diags_ci()), NA)
})


statistic_ci_sometimes_error <- function(dataf) {
  mean_ <- mean(dataf)
  if (mean_ < 0) {
    # pretend that negative mean is a problem.
    stop("negative mean.")
  }
  stat <- matrix(c(mean_ - 1, mean_, mean_ + 1), ncol = 3)
  rownames(stat) <- "mystatname"
  return(stat)
}


# TODO: use 'do.call' to reduce duplication.
set.seed(3)
mc_ci_sometimes_error <- mc_run(
         dgp_params = dgp_params_,
         nvec = nvec_,
         nsims = 2,
         dgp = dgp_,
         statistics = ret_na_if_error(statistic_ci_sometimes_error, verbose = 0),
         verbose = 0
)


test_that("CI diag works if stat has some errors but not all.", {
  expect_error(mcdiags_ci_sometimes_error <- mc_diags(mc_ci_sometimes_error, diagnostics = gen_diags_ci()), NA)

  nrows_ <- simplify2array(mcdiags_results_apply(mcdiags_ci_sometimes_error, FUN = function(x) nrow(x[["mean"]])))
  # Before the correct_rownames_of_NAs() fix, there would be rows with NA, so
  # it would appear that there were 2 statistics, when really there is just 1.
  expect_identical(nrows_, rep(1L, times = length(nvec_) * length(dgp_params_)))
})


statistic_ci_sometimes_warning <- function(dataf) {
  mean_ <- mean(dataf)
  if (mean_ < 0) {
    # pretend that negative mean might be a problem.
    warning("negative mean.")
  }
  stat <- matrix(c(mean_ - 1, mean_, mean_ + 1), ncol = 3)
  rownames(stat) <- "mystatname"
  return(stat)
}


# The following tests are not really about MC diags but we have the necessary
# setup here.
# TODO: use do.call() to reduce duplication.
mc_ci_sometimes_warning_l <- list(
         dgp_params = dgp_params_,
         nvec = nvec_,
         nsims = 2,
         dgp = dgp_,
         statistics = ret_na_if_error(statistic_ci_sometimes_warning, verbose = 0),
         verbose = 0
)


test_that("Handling of warnings respects options('warn').", {
  original_warn <- getOption("warn")

  options(warn = 0)
  set.seed(3)
  suppressWarnings(expect_warning(mc_warn0 <- do.call(mc_run, mc_ci_sometimes_warning_l)))
  options(warn = 1)
  set.seed(3)
  suppressWarnings(expect_warning(mc_warn1 <- do.call(mc_run, mc_ci_sometimes_warning_l)))
  options(warn = 2)
  set.seed(3)
  # the warning is caught and turned into an NA by montetools.
  expect_warning(mc_warn2 <- do.call(mc_run, mc_ci_sometimes_warning_l), NA)
  
  expect_true(all.equal(mc_warn0, mc_warn1))
  expect_false(all.equal(mc_warn1, mc_warn2, verbose = 0))

  options(warn = original_warn)
})


statistic_ci_sometimes_warning_2x3 <- function(dataf) {
  mean_ <- mean(dataf)
  median_ <- median(dataf)
  if (mean_ < 0) {
    # pretend that negative mean might be a problem.
    warning("negative mean.")
  }
  if (mean_ < 0) {
    # pretend that positive median might be a problem.
    warning("positive median.")
  }
  stat_mean <- matrix(c(mean_ - 1, mean_, mean_ + 1), ncol = 3)
  stat_median <- matrix(c(median_ - 1, median_, median_ + 1), ncol = 3)
  stat <- rbind(stat_mean, stat_median)
  rownames(stat) <- c("mean_ci", "median_ci")
  return(stat)
}


mc_ci_sometimes_warning_2x3_l <- mc_ci_sometimes_warning_l
mc_ci_sometimes_warning_2x3_l[["statistics"]] <- ret_na_if_error(statistic_ci_sometimes_warning_2x3, verbose = 0)


test_that("Handling of warnings works well with 2x3 stat.", {
  original_warn <- getOption("warn")

  mc_ci_sometimes_warning_2x3_l[["nsims"]] <- 3

  options(warn = 2)
  set.seed(3)
  expect_warning(mc_warn2_2x3 <- do.call(mc_run, mc_ci_sometimes_warning_2x3_l), NA)

  options(warn = original_warn)
})


statistic_ci_always_error_for_small_n_2x3 <- function(dataf) {
  if (length(dataf) == 5) {
    stop("Sample size too small.")
  }

  mean_ <- mean(dataf)
  median_ <- median(dataf)

  stat_mean <- matrix(c(mean_ - 1, mean_, mean_ + 1), ncol = 3)
  stat_median <- matrix(c(median_ - 1, median_, median_ + 1), ncol = 3)
  stat <- rbind(stat_mean, stat_median)
  rownames(stat) <- c("mean_ci", "median_ci")
  return(stat)
}


test_that("Handling of errors works well when all errors in small n.", {
  mc_ci_always_error_for_small_n_2x3_l <- mc_ci_sometimes_warning_l
  mc_ci_always_error_for_small_n_2x3_l[["statistics"]] <- ret_na_if_error(statistic_ci_always_error_for_small_n_2x3, verbose = 0)
  expect_error(mc38 <- do.call(mc_run, mc_ci_always_error_for_small_n_2x3_l), NA)
  expect_error(mc38_diags <- mc_diags(mc38, diagnostics = gen_diags_ci()), NA)
  #
  # This did not use to work because *all* sims are NA in some chunks. It now
  # works because of correct_NA_chunks_ACROSS_chunks().
  expect_error(mc_table(mc38_diags), NA)
})


test_that("Errors are given if duplicate diagnostic column names", {
  mc_ci2 <- mc_run(
           dgp_params = dgp_params_,
           nvec = nvec_,
           nsims = 2,
           dgp = dgp_,
           statistics = statistic_ci,
           diagnostics = c(gen_diags_ci(), gen_diags_ci()),
           # if verbose is greater than 0, then error would be given,
           # because diags would be calculated (and checked).
           verbose = 0
  )
  expect_error(mcdiags <- mc_diags(mc_ci2), "has duplicate")
})
