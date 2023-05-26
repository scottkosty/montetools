dgp_params_ <- c(1, 2, 3)

nvec_ <- c(5, 9)

dgp_ <- function(dgp_param, n) {
  rnorm(n)
}

statistic_ <- function(dataf) {
  mean(dataf)
}

diagnostic_ <- function(stats_m, true_poi) {
  # colMeans(stats_m)
  # use "true_poi" just to trigger error if it doesn't exist.
  # This caused an error before because we didn't adecuately handle dgp_tuples when merging.
  # This only triggered when there were different params that were combined. (currently the case for the 3rd test case).
  # This return doesn't actually make sense.
  ret <- data.frame(mydiag = stats_m + true_poi)
  return(ret)
}

cases_l <- list(
  list(
    first = list(dgp_params = c(1, 2, 3),
         nvec = c(5, 9),
         dgp = dgp_,
         statistics = statistic_,
         nsims = c(1, 2),
         verbose = 0
    ),
    second = list(dgp_params = c(1, 2, 3),
         nvec = c(5, 9),
         dgp = dgp_,
         statistics = statistic_,
         nsims = c(3, 1),
         verbose = 0
    ),
    # created with dput(mc1_combined) (and manually checked)
    expected_pn_pairs = list(
      structure(c(p = "6717f2823d3202449301145073ab8719", n = "5"), class = "pnpair"),
      structure(c(p = "6717f2823d3202449301145073ab8719", n = "9"), class = "pnpair"),
      structure(c(p = "db8e490a925a60e62212cefc7674ca02", n = "5"), class = "pnpair"),
      structure(c(p = "db8e490a925a60e62212cefc7674ca02", n = "9"), class = "pnpair"),
      structure(c(p = "e5b57f323c7b3719bbaaf9f96b260d39", n = "5"), class = "pnpair"),
      structure(c(p = "e5b57f323c7b3719bbaaf9f96b260d39", n = "9"), class = "pnpair")
    ),
    expected_nsims = c(4, 3, 4, 3, 4, 3)
  ),

  list(
    first = list(dgp_params = c(1, 2, 3),
         nvec = c(5, 9),
         dgp = dgp_,
         statistics = statistic_,
         nsims = c(1, 2),
         verbose = 0
    ),
    # one overlap (3).
    second = list(dgp_params = c(3, 4, 5),
         nvec = c(5, 9),
         dgp = dgp_,
         statistics = statistic_,
         nsims = c(3, 1),
         verbose = 0
    ),
    # created with dput(mc1_combined) (and manually checked)
    expected_pn_pairs =
list(structure(c(p = "5e338704a8e069ebd8b38ca71991cf94", n = "5"
), class = "pnpair"), structure(c(p = "5e338704a8e069ebd8b38ca71991cf94", 
n = "9"), class = "pnpair"), structure(c(p = "6717f2823d3202449301145073ab8719", 
n = "5"), class = "pnpair"), structure(c(p = "6717f2823d3202449301145073ab8719", 
n = "9"), class = "pnpair"), structure(c(p = "db8e490a925a60e62212cefc7674ca02", 
n = "5"), class = "pnpair"), structure(c(p = "db8e490a925a60e62212cefc7674ca02", 
n = "9"), class = "pnpair"), structure(c(p = "dbc09cba9fe2583fb01d63c70e1555a8", 
n = "5"), class = "pnpair"), structure(c(p = "dbc09cba9fe2583fb01d63c70e1555a8", 
n = "9"), class = "pnpair"), structure(c(p = "e5b57f323c7b3719bbaaf9f96b260d39", 
n = "5"), class = "pnpair"), structure(c(p = "e5b57f323c7b3719bbaaf9f96b260d39", 
n = "9"), class = "pnpair"))
    ,
    expected_nsims = c(3, 1, 1, 2, 1, 2, 3, 1, 4, 3)
  ),

  # testing nvec and params diff
  list(
    first = list(dgp_params = c(1, 2, 3),
         nvec = c(2, 3),
         dgp = dgp_,
         statistics = statistic_,
         nsims = c(1, 1),
         verbose = 0
    ),
    # one overlap (3).
    second = list(dgp_params = c(3, 4, 5),
         nvec = c(1, 3, 4),
         dgp = dgp_,
         statistics = statistic_,
         nsims = c(1, 1, 1),
         verbose = 0
    ),
    # created with dput(mc1_combined) (and manually checked)
    expected_pn_pairs =
list(structure(c(p = "5e338704a8e069ebd8b38ca71991cf94", n = "1"
), class = "pnpair"), structure(c(p = "5e338704a8e069ebd8b38ca71991cf94", 
n = "3"), class = "pnpair"), structure(c(p = "5e338704a8e069ebd8b38ca71991cf94", 
n = "4"), class = "pnpair"), structure(c(p = "6717f2823d3202449301145073ab8719", 
n = "2"), class = "pnpair"), structure(c(p = "6717f2823d3202449301145073ab8719", 
n = "3"), class = "pnpair"), structure(c(p = "db8e490a925a60e62212cefc7674ca02", 
n = "2"), class = "pnpair"), structure(c(p = "db8e490a925a60e62212cefc7674ca02", 
n = "3"), class = "pnpair"), structure(c(p = "dbc09cba9fe2583fb01d63c70e1555a8", 
n = "1"), class = "pnpair"), structure(c(p = "dbc09cba9fe2583fb01d63c70e1555a8", 
n = "3"), class = "pnpair"), structure(c(p = "dbc09cba9fe2583fb01d63c70e1555a8", 
n = "4"), class = "pnpair"), structure(c(p = "e5b57f323c7b3719bbaaf9f96b260d39", 
n = "1"), class = "pnpair"), structure(c(p = "e5b57f323c7b3719bbaaf9f96b260d39", 
n = "2"), class = "pnpair"), structure(c(p = "e5b57f323c7b3719bbaaf9f96b260d39", 
n = "3"), class = "pnpair"), structure(c(p = "e5b57f323c7b3719bbaaf9f96b260d39", 
n = "4"), class = "pnpair"))
,
    # the one "2" is because there is overlap for p = 3, n = 3.
    expected_nsims = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1)
  )
)

test_that("Checking that mc_merge() gives correct result.", {
  for (i in seq_along(cases_l)) {
    # DBG
    # message("i is: ", i)
    case <- cases_l[[i]]

    set.seed(1)
    mc1a <- do.call(mc_run, case[["first"]])
    set.seed(1)
    mc1b <- do.call(mc_run, case[["second"]])
    mc1_combined_ab <- mc_merge(mc1a, mc1b, verbose = 0)
    # TODO: we will test that order doesn't matter for the diagnostics result.
    mc1_combined_ba <- mc_merge(mc1b, mc1a, verbose = 0)
    pairs_ <- get_pn_pairs(mc1_combined_ab, sortby = "phash")
    expect_identical(pairs_, case[["expected_pn_pairs"]])


    diags_combine_ab <- mc_diags(mc = mc1_combined_ab, diagnostics = diagnostic_)
    diags_combine_ba <- mc_diags(mc = mc1_combined_ba, diagnostics = diagnostic_)

    # These should not be equal because the stats_m's are ordered differently.
    expect_false(isTRUE(all.equal(mc1_combined_ab, mc1_combined_ba, verbose = 0)))
    # But the diagnostics should be equal.
    expect_true(isTRUE(all.equal(diags_combine_ab, diags_combine_ba, verbose = 0)))
  
    nsims_ab <- sapply(X = pairs_, FUN = get_nsims, mcx = mc1_combined_ab)
    nsims_ba <- sapply(X = pairs_, FUN = get_nsims, mcx = mc1_combined_ba)
    expect_identical(nsims_ab, nsims_ba)
    expect_identical(nsims_ab, case[["expected_nsims"]])
  }
})
