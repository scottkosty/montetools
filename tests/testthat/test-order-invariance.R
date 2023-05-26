dgp_params_ <- c(1, 2, 3)

nvec_ <- c(5, 9)
nsims_1 <- 3

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

mc_args_l <- list(
         dgp_params = dgp_params_,
         nvec = nvec_,
         nsims = nsims_1,
         dgp = dgp_,
         statistics = statistic_4,
         oloop_is_p = FALSE,
         verbose = 0
)


test_that("mc_run() gives same stat_m regardless of order.", {
  mc_args_local_si_123_l <- mc_args_l
  mc_args_local_si_l <- mc_args_l
  mc_args_local_si_l[["dgp_params"]] <- c(3, 2, 1)

  for (si in seed_inits_l) {
    mc_args_local_si_l[["seed_initializer"]] <- si
    mc_args_local_si_123_l[["seed_initializer"]] <- si

    # need to construct the point of reference dependent on "si" as well. i.e.,
    # cannot use global 'mc123' because it would be constructed with whatever the
    # default seed_initializer is.
    set.seed(7)
    mc123_local <- do.call(mc_run, mc_args_local_si_123_l)
    set.seed(7)
    mc321 <- do.call(mc_run, mc_args_local_si_l)
    if (si == "seed_init_noop") {
      expect_this <- expect_false
    } else {
      expect_this <- expect_true
    }
    expect_this(isTRUE(all.equal(mc123_local, mc321, sensitive_to_pn_order = FALSE, verbose = 0)))
  }
})


test_that("We can do individually, cbind, and reproduce.", {
  mc_args_local_si_l <- mc_args_l
  mc_args_local_si_123_l <- mc_args_l

  for (si in seed_inits_l) {
    mc_args_local_si_l[["seed_initializer"]] <- si
    mc_args_local_si_123_l[["seed_initializer"]] <- si

    set.seed(7)
    # "_si" to emphasize depends on "si"
    mc123_si <- do.call(mc_run, mc_args_local_si_123_l)

    set.seed(7)
    mc_args_l1 <- mc_args_local_si_l
    mc_args_l1[["dgp_params"]] <- c(2)
    mc2 <- do.call(mc_run, mc_args_l1)

    set.seed(7)
    mc_args_l2 <- mc_args_local_si_l
    mc_args_l2[["dgp_params"]] <- c(1)
    mc1 <- do.call(mc_run, mc_args_l2)

    set.seed(7)
    mc_args_l3 <- mc_args_local_si_l
    mc_args_l3[["dgp_params"]] <- c(3)
    mc3 <- do.call(mc_run, mc_args_l3)

    if (si %in% c("seed_init_add_ints", "seed_init_pnhash")) {
      expect_this <- expect_false
    } else if (si == "seed_init_noop") {
      expect_this <- expect_true
    }
    expect_this(isTRUE(all.equal(get_all_mc_stats_m(mc1), get_all_mc_stats_m(mc2), verbose = 0)))
    expect_this(isTRUE(all.equal(get_all_mc_stats_m(mc2), get_all_mc_stats_m(mc3), verbose = 0)))
    expect_this(isTRUE(all.equal(get_all_mc_stats_m(mc3), get_all_mc_stats_m(mc1), verbose = 0)))

    mc213_cbinded <- cbind(mc2, mc1, mc3, verbose = 0)
    mc213_cbinded2 <- cbind(cbind(mc2, mc1, verbose = 0), mc3, verbose = 0)
    expect_true(isTRUE(all.equal(mc213_cbinded, mc213_cbinded2, verbose = 0)))
    mc213_cbinded3 <- cbind(mc3, mc2, mc1, verbose = 0)
    #
    #
    # if we don't sort, then "mc213_cbinded3" will have different order and all.equal() returns FALSE.
    expect_false(isTRUE(all.equal(mc213_cbinded, mc213_cbinded3, verbose = 0)))
    #
    phashes_sorted <- get_phashes(mc213_cbinded, sortby = "phash")
    # make sure not, e.g., subset of the other MC
    stopifnot(identical(length(phashes_sorted), length(mc213_cbinded3)))
    #
    mc213_cbinded3_sorted <- mc213_cbinded3[phashes_sorted]
    mc213_cbinded_sorted <- mc213_cbinded[phashes_sorted]
    expect_true(isTRUE(all.equal(mc213_cbinded_sorted, mc213_cbinded3_sorted, verbose = 0)))

    if (si == "seed_init_noop") {
      expect_this <- expect_false
    } else {
      expect_this <- expect_true
    }
    expect_this(isTRUE(all.equal(mc123_si, mc213_cbinded, sensitive_to_pn_order = FALSE, verbose = 0)))


    set.seed(7)
    mc_args_l4 <- mc_args_local_si_l
    mc_args_l4[["oloop_is_p"]] <- TRUE
    mc123_oloop_is_p <- do.call(mc_run, mc_args_l4)
    if (si == "seed_init_noop") {
      expect_this <- expect_false
    } else {
      expect_this <- expect_true
    }
    expect_this(isTRUE(all.equal(mc123_si, mc123_oloop_is_p, verbose = 0)))
  }
})


test_that("Results are or are not invariant to user seed depending on policy.", {
  mc_args_local_si_l <- mc_args_l
  for (si in seed_inits_l) {
    mc_args_local_si_l[["seed_initializer"]] <- si
    set.seed(7)
    mc123_seed7 <- do.call(mc_run, mc_args_local_si_l)
    set.seed(8)
    mc123_seed8 <- do.call(mc_run, mc_args_local_si_l)
    if (si %in% c("seed_init_add_ints", "seed_init_noop")) {
      expect_this <- expect_false
    } else if (si == "seed_init_pnhash") {
      expect_this <- expect_true
    } else {
      stop("seed_initializer value not handled: ", si)
    }
    expect_this(isTRUE(all.equal(mc123_seed7, mc123_seed8, verbose = 0)))


    set.seed(1)
    mc_args_l2 <- mc_args_local_si_l
    mc_args_l2[["dgp_params"]] <- c(2)
    mc2 <- do.call(mc_run, mc_args_l2)

    set.seed(2)
    mc_args_l13 <- mc_args_local_si_l
    mc_args_l13[["dgp_params"]] <- c(1, 3)
    mc13 <- do.call(mc_run, mc_args_l13)

    # now cbind results from different seeds.
    # montetools stores seed info by pn so can still reproduce.
    # But result will not be the same as a fresh run with one seed.
    mc213_local <- cbind(mc2, mc13, verbose = 0)
    if (si %in% c("seed_init_add_ints", "seed_init_noop")) {
      expect_this <- expect_false
    } else if (si == "seed_init_pnhash") {
      expect_this <- expect_true
    }
    expect_this(isTRUE(all.equal(mc213_local, mc123_seed7, sensitive_to_pn_order = FALSE, verbose = 0)))
    expect_error(mc_reproduce(mc213_local, verbose = 0), NA)
  }
})


test_that("Results are reproducible with different user RNG.", {
  mc_args_local_si_123_l <- mc_args_l

  for (si in seed_inits_l) {
    mc_args_local_si_123_l[["seed_initializer"]] <- si

    RNGkind("Mersenne-Twister")
    set.seed(7)
    seed_7 <- .Random.seed
    mc123_local <- do.call(mc_run, mc_args_local_si_123_l)
    # todo: loop through several kinds of RNGs?
    RNGkind("L'Ecuyer-CMRG")
    set.seed(7)
    seed_7_ecuyer <- .Random.seed
    expect_false(identical(seed_7, seed_7_ecuyer))
    user_rngkind <- RNGkind()
    mc123_b <- do.call(mc_run, mc_args_local_si_123_l)
    if (si %in% c("seed_init_noop", "seed_init_add_ints")) {
      expect_this <- expect_false
    } else {
      expect_this <- expect_true
    }
    expect_this(isTRUE(all.equal(mc123_local, mc123_b, sensitive_to_pn_order = FALSE, verbose = 0)))

    # test that the original RNGkind was restored (after being potentially changed inside mc_run(), as
    # with seed_policy "seed_init_pnhash").
    user_rngkind_after <- RNGkind()
    expect_identical(user_rngkind, user_rngkind_after)

    # All are reproducible.
    expect_error(mc_reproduce(mc123_b, verbose = 0), NA)
  }
})
