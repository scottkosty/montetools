dgp_params_ <- c(1, 2, 3)
nvec_ <- c(5, 9)
nsims_1 <- 1

statistic <- function(dataf) {
  stat <- matrix(mean(dataf))
  rownames(stat) <- "mystatname"
  return(stat)
}

dgp <- function(n, dgp_param) {
  rnorm(n, sd = 1.01)
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
  return(ret)
}


# Note that testthat tests can be run in parallel. From what I understand from
# ?tempfile() that it is thread-safe. Using tempfile() additionally helps in
# the case that the test dir is not writeable.
#
# An alternative to tempfile() would be to use the process ID, or more simply
# the name of the test file (currently "test-backup-mechanism.R").
bufile_ <- tempfile(fileext = ".Rds")
#
# does not stop by default; only stop if args matched
gen_hooks_with_stop <- function(pp = c(), nn = c()) {
  hook_stop <- function(mc_part_done, mc_args_next, user_args, pn_pair_next, ppp = pp, nnn = nn) {
    if (is.null(mc_args_next)) {
      # TODO: test that if we return now... the emergency file is the same as the final one!
      return(NULL)
    }
    if (mc_args_next$ptup[["dgpp"]] %in% ppp && mc_args_next$n %in% nnn) {
      stop("failing on purpose")
    }
    return(NULL)
  }

  hooks_ <- c(hooks_pnchunk_default, gen_hook_save_partial(filename = bufile_), hook_stop)
  return(hooks_)
}


test_that("backup mechanism basics work.", {

  mc_args <- list(
           dgp_params = dgp_params_,
           nvec = nvec_,
           nsims = 2,
           dgp = dgp,
           statistics = statistic,
           hooks_pnchunk = gen_hooks_with_stop(),
           verbose = 0
  )
  set.seed(72)
  # check that the backup file does not already exist (e.g., from
  # a previous test run). We use a tempfile so this should be false.
  expect_false(file.exists(bufile_))
  # check no fail for baseline, and also store results.
  expect_error(mc_all_succeed <- do.call(mc_run, mc_args), NA)
  expect_false(file.exists(bufile_))

  set.seed(72)
  # now we run our real test
  # todo: if p = 2, n = 9 we get a failure with mc_reproduce(mc_BU) below, and also if p = 3, n = 9.
  # The reason for this is that currently we require a *balanced* MC object for many functions, so
  # at some point we look for an attribute in an MC pair that does not exist. We should eventually
  # allow for unbalanced MCs.
  mc_args[["hooks_pnchunk"]] <- gen_hooks_with_stop(pp = 2, nn = 5)
  expect_false(file.exists(bufile_))
  expect_error(do.call(mc_run, mc_args), "failing on purpose")
  expect_true(file.exists(bufile_))
  mc_BU <- readRDS(bufile_)
  pn_pairs_ <- get_pn_pairs(mc_BU)
  # make_expectation(get_pn_pairs(mc_BU))
  expect_equal(get_pn_pairs(mc_BU), list(structure(c(p = "6717f2823d3202449301145073ab8719",
n = "5"), class = "pnpair")))
  expect_error(validate_montecarlo(mc_BU), NA)

  # \label{useseed}
  rnorm(1)
  mc_args[["partial_results"]] <- bufile_
  # This should not fail *if recovery works as planned* because the pn-pair that
  # will fail already has results in ther partial results.
  mc_args[["hooks_pnchunk"]] <- gen_hooks_with_stop(pp = 1, nn = 5)
  expect_error(mc_from_recovery <- do.call(mc_run, mc_args), NA)
  # should not exist now that it was recovered
  expect_false(file.exists(bufile_))

  # set wrong seed should not matter
  set.seed(3)
  expect_error(mc_reproduce(mc_BU, verbose = 0), NA)
  #
  # Setting wrong seed and manually assigning should matter.
  # use different object so don't have to restore original seed
  mc_from_recovery_temp <- mc_from_recovery
  set.seed(3)
  attr(mc_from_recovery_temp[[1]][[1]], "rnginfo_before")[["seed"]] <- .Random.seed
  expect_error(mc_reproduce(mc_from_recovery_temp, verbose = 0))


  # Without the feature of resetting rnginfo, because of \ref{useseed} these tests
  # would fail.
  # \label{test_seed_recovery}
  expect_true(all.equal(mc_all_succeed, mc_from_recovery, verbose = 0))
  expect_true(identical(get_all_mcpn_attr(mc_from_recovery, "seed_first"),
                   get_all_mcpn_attr(mc_all_succeed, "seed_first")))
  expect_true(identical(get_all_mcpn_attr(mc_from_recovery, "seed_last"),
                   get_all_mcpn_attr(mc_all_succeed, "seed_last")))
})
