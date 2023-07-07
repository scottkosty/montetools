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

statistic_1 <- function(dataf) {
  stat <- matrix(mean(dataf))
  rownames(stat) <- "mystatname"
  return(stat)
}

statistic_2 <- function(dataf) {
  stat <- matrix(mean(dataf))
  colnames(stat) <- "mystatname"
  return(stat)
}

statistic_3 <- function(dataf) {
  stat <- data.frame(mystatname = mean(dataf))
  return(stat)
}

statistic_4 <- function(dataf) {
  stat <- mean(dataf)
  return(c("mystatname" = stat))
}

# the names are expected to be different (so all.equal() fails)
statistic_5 <- function(dataf) {
  stat <- mean(dataf)
  return(stat)
}


statistic_l <- list(statistic_1, statistic_2, statistic_3, statistic_4, statistic_5)


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


dgpp_to_poi_1 <- function(dgp_param, maxn) {
  return(dgp_param)
}
#
dgpp_to_poi_2 <- function(dgp_param) {
  return(dgp_param)
}
#
dgpp_to_poi_3 <- NULL

test_that("Checking that mc_run() gives correct result.", {
  # we already check different 'dgp_params' arguments in the test test-obj_to_list.R.

  dgpp_to_poi_l <- list(dgpp_to_poi_1, dgpp_to_poi_2, dgpp_to_poi_3)

  for (i in seq_along(dgpp_to_poi_l)) {
    set.seed(1)
    mc <- mc_run(
             dgp_params = dgp_params_,
             nvec = nvec_,
             nsims = nsims_1,
             dgp = dgp_,
             statistics = statistic_1,
             dgpp_to_poi = dgpp_to_poi_l[[i]],
             verbose = 0
    )
    if (i != 1) {
      expect_true(isTRUE(all.equal(mc, mc_prev, verbose = 0)))
    }
    mc_prev <- mc
  }
  # only need to validate the last one if they are all identical.
  validate_montecarlo(mc)

  set.seed(1)
  nsims_2 <- c(1, 1)
  mc2 <- mc_run(
           dgp_params = dgp_params_,
           nvec = nvec_,
           nsims = nsims_2,
           dgp = dgp_,
           statistics = statistic_1,
           dgpp_to_poi = dgpp_to_poi_1,
           verbose = 0
  )
  expect_true(isTRUE(all.equal(mc, mc2, verbose = 0)))

  set.seed(1)
  # should be different but no error
  nsims_3 <- c(1, 2)
  mc3 <- mc_run(
           dgp_params = dgp_params_,
           nvec = nvec_,
           nsims = nsims_3,
           dgp = dgp_,
           statistics = statistic_1,
           dgpp_to_poi = dgpp_to_poi_1,
           verbose = 0
  )
  expect_false(isTRUE(all.equal(mc, mc3, verbose = 0)))

  if (run_hc_tests) {
    # todo: consider using "expect_equal_to_reference" when output is complex?
    # when output is simple, use "make_expectation"
    statms <- get_all_mc_stats_m(mc)
    pois <- sapply(get_param_tuples(mc), `[[`, "true_poi")
    # For updating hardcopy tests, pipe to 'xsel -b' with following command:
    # system2("xsel", args = "-b", input = capture.output(make_expectation(statms)))
    statms_expected <- 
expect_equal(statms, list(structure(1.45616309749746, .Dim = c(1L, 
1L), .Dimnames = list("mystatname", NULL)), structure(0.483329027649171, .Dim = c(1L, 
1L), .Dimnames = list("mystatname", NULL)), structure(-0.737293488121383, .Dim = c(1L, 
1L), .Dimnames = list("mystatname", NULL)), structure(-0.109743729681682, .Dim = c(1L, 
1L), .Dimnames = list("mystatname", NULL)), structure(0.244190613600415, .Dim = c(1L, 
1L), .Dimnames = list("mystatname", NULL)), structure(-0.0430723221391681, .Dim = c(1L, 
1L), .Dimnames = list("mystatname", NULL))))
    pois_expected <- c(`6717f2823d3202449301145073ab8719` = 1, db8e490a925a60e62212cefc7674ca02 = 2, 
  e5b57f323c7b3719bbaaf9f96b260d39 = 3)
    expect_equal(statms, statms_expected)
    expect_equal(pois, pois_expected)
  }

  # should give error because length of nsims_4 is too long
  nsims_4 <- c(1, 2, 3)
  expect_error(
    mc4 <- mc_run(
           dgp_params = dgp_params_,
           nvec = nvec_,
           nsims = nsims_4,
           dgp = dgp_,
           statistics = statistic_1,
           dgpp_to_poi = dgpp_to_poi_1,
           verbose = 0
    ))

  for (i in seq_along(statistic_l)) {
    set.seed(1)
# DBG (very helpful for debugging)
#    if (i == 4) debug(get_statnames_from_stat)
    mc <- mc_run(
             dgp_params = dgp_params_,
             nvec = nvec_,
             nsims = nsims_1,
             dgp = dgp_,
             statistics = statistic_l[[i]],
             verbose = 0
    )
# This doesn't work because the columns of the stats are different.
# todo: is this worth standardizing?
#    if (i != 1) {
#      message("i is: ", i)
#      expect_true(isTRUE(all.equal(mc, mc_prev, verbose = 0)))
#    }
#    mc_prev <- mc

    diags_ <- mc_diags(mc = mc, diagnostics = diagnostic_)
    if (i != 1) {
#      message("i is: ", i)
      if (i == 5) {
        # the names are expected to be different.
        expect_false(isTRUE(all.equal(diags_, diags_prev, verbose = 0)))
      } else {
        expect_true(isTRUE(all.equal(diags_, diags_prev, verbose = 0)))
      }
    }
    diags_prev <- diags_
  }

  # do one test where we print indiv responses because this forces extra computation so need to check it works.
  # need to suppress both messages and cat() output I think. But an error will still pass through.
  # I only had 'verbose = 1' in mind for this, but might as well set 'verbose' very high so other
  # code paths are tested.
  expect_error(capture.output(suppressMessages(
           mc <- mc_run(
           dgp_params = dgp_params_,
           nvec = nvec_,
           nsims = nsims_1,
           dgp = dgp_,
           statistics = statistic_1,
           diagnostics = diagnostic_,
           # can't set to Inf because future's debug info is not captured for some reason.
           verbose = 1e7
  ))), NA)

  if (run_hc_tests) {
    # For updating hardcopy tests, pipe to 'xsel -b' with following command:
    # system2("xsel", args = "-b", input = capture.output(make_expectation(mc_table(diags_, format = "latex"))))
expect_equal(mc_table(diags_, format = "latex"), c("\\begin{tabular}{lll}", "  \\hline", 
"\\hline", "\\multicolumn{1}{l}{} & \\multicolumn{2}{c}{$n$} \\\\", 
"param. & 5 & 9 \\\\ ", "  \\hline", "1 & 2.456 & 1.483 \\\\ ", 
"  2 & 1.263 & 1.890 \\\\ ", "  3 & 3.244 & 2.957 \\\\ ", "\\hline", 
"nsims  & 1 & 1 \\\\", "   \\hline", "\\hline", "\\end{tabular}"
))
  }
})


test_that("The differences in arguments of mc_run() and do_all_mc() are as expected.", {
  args_public <- formalArgs(mc_run)
  args_private <- formalArgs(do_mc_run)
  args_only_private <- c("init_seeds", "allow_non_local")
  expect_true(all(args_public %in% args_private))
  expect_true(all(!(args_only_private %in% args_public)))
  expect_true(all(args_private %in% c(args_public, args_only_private)))
})


test_that("mc_run() works well for when nsims is 0.", {
  # TODO: use do.call so don't have to keep repeating.
  mc0 <- mc_run(
           dgp_params = dgp_params_,
           nvec = nvec_,
           nsims = 0,
           dgp = dgp_,
           statistics = statistic_1,
           verbose = 0
  )
  expect_equal(get_nsims_vec(mc0), c(0, 0))
  mc01 <- mc_run(
           dgp_params = dgp_params_,
           nvec = nvec_,
           nsims = c(0, 1),
           dgp = dgp_,
           statistics = statistic_1,
           verbose = 0
  )
  expect_equal(get_nsims_vec(mc01), c(0, 1))
  mc10 <- mc_run(
           dgp_params = dgp_params_,
           nvec = nvec_,
           nsims = c(1, 0),
           dgp = dgp_,
           statistics = statistic_1,
           verbose = 0
  )
  expect_equal(get_nsims_vec(mc10), c(1, 0))
})
