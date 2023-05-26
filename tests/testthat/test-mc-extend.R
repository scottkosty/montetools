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


glob1 <- function(x) {
  return(mean(x))
}


statistic_4 <- function(dataf) {
  stat <- glob1(dataf)
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


nsims1 <- 3
nsims2 <- 5


mc_args_l <- list(
  dgp_params = dgp_params_,
  nvec = nvec_,
  nsims = nsims1 + nsims2,
  dgp = dgp_,
  statistics = statistic_4,
  verbose = 0
)


glob1_bu <- glob1
for (si in seed_inits_l) {
  glob1 <- glob1_bu

  mc_args_local_l <- mc_args_l
  mc_args_local_l[["seed_initializer"]] <- si


  set.seed(1)
  mc_all_in_one <- do.call(mc_run, mc_args_local_l)
  
  
  set.seed(1)
  mc_args_local_l[["nsims"]] <- nsims1
  mc1 <- do.call(mc_run, mc_args_local_l)

  # test that globals are restored
  rm(glob1)

  
  test_that("mc_extend() basics.", {
    # This first one should fail because restore_globals is FALSE.
    #
    # The suppressMessages() is so the system.time() text of "Timing
    # stopped..." does not show up (which only showed when testthat
    # parallel was false. That's not needed for most tests in here
    # because most errors don't occur during the actual simulations.
    suppressMessages(
      expect_error(mc_extend(mc1, nsims_additional = nsims2, restore_globals = FALSE, verbose = 0),
                   "could not find function")
    )
    expect_error(mc_extended <- mc_extend(mc1, nsims_additional = nsims2, restore_globals = TRUE, verbose = 0), NA)
  
    # If there are any duplicates, then random streams are likely overlapping.
    all_pairs <- get_pn_pairs(mc_extended)
    stats_ms_l <- lapply(X = all_pairs, FUN = function(pn) mc_stats_m(mc_extended, pn))
    vec <- do.call(rbind, stats_ms_l)[, 1]
  # todo: bad randomization causes this test to fail.
    expect_identical(anyDuplicated(vec), 0L)
  
    # These do not need to be equal (i.e., mc_extend() would still be useful), but in
    # our design they are. TODO: document that this is a non-obvious feature.
    expect_true(all.equal(mc_all_in_one, mc_extended, verbose = 0))
  
    # seed should not affect result of mc_extend() at all.
    set.seed(21)
    mc_extended2 <- mc_extend(mc1, nsims_additional = nsims2, verbose = 0)
    expect_true(all.equal(mc_extended, mc_extended2, verbose = 0))
  
    # todo: but seed *should* affect mc_run(). Test that!
    #       update: ^^^ not for all seed_initializers.
  
    expect_error(mc_reproduce(mc_extended, verbose = 0), NA)
  
    mc_extended_x2 <- mc_extend(mc_extended, nsims_additional = 1, verbose = 0)
    # this is essentially a check that after extending once, the *new*
    # "seed_last" is updated.
    expect_error(mc_reproduce(mc_extended_x2, verbose = 0), NA)
  })
  
  test_that("mc_extend() works with various kinds of nsims vecs", {
    mc_extended3 <- mc_extend(mc1, nsims_additional = c(2, 3), verbose = 0)
    nsims_vec3 <- get_nsims_vec(mc_extended3)
    expect_true(all.equal(nsims_vec3, nsims1 + c(2, 3)))
  
    mc_extended4 <- mc_extend(mc1, nsims_additional = c(1, 1), verbose = 0)
    nsims_vec4 <- get_nsims_vec(mc_extended4)
    expect_true(all.equal(nsims_vec4, nsims1 + c(1, 1)))
  
    # todo: we need to do other tests with nsims = 0.
    mc_extended5 <- mc_extend(mc1, nsims_additional = c(0, 1), verbose = 0)
    nsims_vec5 <- get_nsims_vec(mc_extended5)
    expect_true(all.equal(nsims_vec5, nsims1 + c(0, 1)))
  
    # silly but might as well allow
    mc_extended6 <- mc_extend(mc_extended5, nsims_additional = c(0, 0), verbose = 0)
    nsims_vec6 <- get_nsims_vec(mc_extended6)
    expect_true(all.equal(nsims_vec5, nsims_vec6))
    expect_error(mc_reproduce(mc_extended6, verbose = 0), NA)
  
    if (run_hc_tests) {
      # hardcoded test
      # For updating hardcopy tests, pipe to 'xsel -b' with following command:
      # system2("xsel", args = "-b", input = capture.output(make_expectation(table_)))
      statms <- get_all_mc_stats_m(mc_extended5)
      pois <- sapply(get_param_tuples(mc_extended5), `[[`, "true_poi")
      expect_equal(pois, c(`6717f2823d3202449301145073ab8719` = 1, 
      db8e490a925a60e62212cefc7674ca02 = 2, e5b57f323c7b3719bbaaf9f96b260d39 = 3))
      if (si == "seed_init_add_ints") {
if (hc_broken_in_parallel) {
expect_equal(statms, list(structure(c(-0.02707473210864, 0.654421887225158, 
-1.00624619889998), .Dim = c(3L, 1L), .Dimnames = list(c("mystatname", 
"mystatname", "mystatname"), "mystatname")), structure(c(0.271283821856054, 
0.190572273477819, -0.0905261323766084, -0.161390351677297), .Dim = c(4L, 
1L), .Dimnames = list(c("mystatname", "mystatname", "mystatname", 
"mystatname"), "mystatname")), structure(c(0.352841833256796, 
-0.392984299135568, -0.110816261632126), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), "mystatname")), 
    structure(c(-0.433849782274499, 0.482217642950643, -0.155234979778213, 
    0.861341830665476), .Dim = c(4L, 1L), .Dimnames = list(c("mystatname", 
    "mystatname", "mystatname", "mystatname"), "mystatname")), 
    structure(c(0.0411089701443692, 0.186461540742763, 0.588961200046429
    ), .Dim = c(3L, 1L), .Dimnames = list(c("mystatname", "mystatname", 
    "mystatname"), "mystatname")), structure(c(-0.236407074910613, 
    -0.339116229038998, -0.190322064377141, 0.00290603765922987
    ), .Dim = c(4L, 1L), .Dimnames = list(c("mystatname", "mystatname", 
    "mystatname", "mystatname"), "mystatname"))))
}
      } else if (si == "seed_init_pnhash") {
#system2("xsel", args = "-b", input = capture.output(make_expectation(statms)))
expect_equal(statms, list(structure(c(1.45616309749746, -0.14502192186287, 
-0.376072181002796), .Dim = c(3L, 1L), .Dimnames = list(c("mystatname", 
"mystatname", "mystatname"), NULL)), structure(c(0.483329027649171, 
0.120111644119869, 0.635994427417812, -0.0444050049434794), .Dim = c(4L, 
1L), .Dimnames = list(c("mystatname", "mystatname", "mystatname", 
"mystatname"), NULL)), structure(c(-0.737293488121383, 0.213491052014004, 
0.13812096085893), .Dim = c(3L, 1L), .Dimnames = list(c("mystatname", 
"mystatname", "mystatname"), NULL)), structure(c(-0.109743729681682, 
-0.0677283038068912, 0.164418579139007, 0.587479948992767), .Dim = c(4L, 
1L), .Dimnames = list(c("mystatname", "mystatname", "mystatname", 
"mystatname"), NULL)), structure(c(0.244190613600415, -0.33216548283803, 
0.234547696503498), .Dim = c(3L, 1L), .Dimnames = list(c("mystatname", 
"mystatname", "mystatname"), NULL)), structure(c(-0.0430723221391681, 
-0.0974689179950976, -0.264614288485159, 0.432393076379125), .Dim = c(4L, 
1L), .Dimnames = list(c("mystatname", "mystatname", "mystatname", 
"mystatname"), NULL))))
      } else if (si == "seed_init_noop") {
if (hc_broken_in_parallel) {
expect_equal(statms, list(structure(c(0.0668337813512683, -0.172794731497622, 
1.03559211435925), .Dim = c(3L, 1L), .Dimnames = list(c("mystatname", 
"mystatname", "mystatname"), "mystatname")), structure(c(0.126037606290266, 
0.411373825024283, 0.305041588829751, -0.368033386199094), .Dim = c(4L, 
1L), .Dimnames = list(c("mystatname", "mystatname", "mystatname", 
"mystatname"), "mystatname")), structure(c(-0.149292241483288, 
0.318763635465126, 1.02506615819138), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), "mystatname")), 
    structure(c(0.0821819503378338, 0.346075754481257, 0.288534447777724, 
    -0.200173804876059), .Dim = c(4L, 1L), .Dimnames = list(c("mystatname", 
    "mystatname", "mystatname", "mystatname"), "mystatname")), 
    structure(c(0.439328181968177, 0.430244135565748, 0.61870245421345
    ), .Dim = c(3L, 1L), .Dimnames = list(c("mystatname", "mystatname", 
    "mystatname"), "mystatname")), structure(c(-0.311828376575145, 
    0.179442847596921, 0.149478604287574, -0.133863747078471), .Dim = c(4L, 
    1L), .Dimnames = list(c("mystatname", "mystatname", "mystatname", 
    "mystatname"), "mystatname"))))
}
      }
    }
  })
}
