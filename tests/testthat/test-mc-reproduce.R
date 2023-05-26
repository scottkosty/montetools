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
  # just using true_poi to trigger error if pulled from missing attribute.
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
  verbose = 0
)


for (si in seed_inits_l) {

  set.seed(7)
  mc_args_local_l <- mc_args_l
  mc_args_local_l[["seed_initializer"]] <- si
  mc <- do.call(mc_run, mc_args_local_l)
  
  
  test_that("mc_run() gives same stat_m regardless of 'future' plan.", {
    # check that default is sequential.
    plans_ <- get_plans(mc)
    expect_true(all(plans_ == "sequential"))
  
    if (run_unix_only_tests) {
      plan("multicore")
      set.seed(7)
      mc_args_local2_l <- mc_args_local_l
      mc_args_local2_l[["parallel"]] <- NA
      mc_multicore <- do.call(mc_run, mc_args_local2_l)
      expect_true(all.equal(mc, mc_multicore, verbose = 0))
      plans_ <- get_plans(mc_multicore)
      expect_true(all(plans_ == "multicore"))
      # restore plan to sequential
      plan("sequential")
    }
  })

#seed_inits_l <- c("seed_init_add_ints", "seed_init_pnhash", "seed_init_noop")

  if (run_hc_tests) {
    test_that("hardcode test passes", {
      statms <- get_all_mc_stats_m(mc)
      pois <- sapply(get_param_tuples(mc), `[[`, "true_poi")
      expect_equal(pois, c(`6717f2823d3202449301145073ab8719` = 1, 
      db8e490a925a60e62212cefc7674ca02 = 2, e5b57f323c7b3719bbaaf9f96b260d39 = 3
      ))
      if (si == "seed_init_add_ints") {
      # For updating hardcopy tests, pipe to 'xsel -b' with following command:
      # system2("xsel", args = "-b", input = capture.output(make_expectation(statms)))
expect_equal(statms, list(structure(c(-0.0863962601255231, 0.344685426184259, 
0.208146164071232), .Dim = c(3L, 1L), .Dimnames = list(c("mystatname", 
"mystatname", "mystatname"), NULL)), structure(c(-0.125445086053366, 
0.0902149557180181, 0.433031770192872), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL)), structure(c(-0.0289844690168496, 
-0.0386965489786193, -0.295312778773189), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL)), structure(c(-0.310061577063305, 
-0.0690371596912068, 0.230600247123265), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL)), structure(c(0.695250677407886, 
0.308787248760125, 0.310426172680773), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL)), structure(c(0.533012860179053, 
0.101864406087286, 0.426547737495814), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL))))
      } else if (si == "seed_init_pnhash") {
        # For updating hardcopy tests, pipe to 'xsel -b' with following command:
        # system2("xsel", args = "-b", input = capture.output(make_expectation(statms)))
expect_equal(statms, list(structure(c(1.45616309749746, -0.14502192186287, 
-0.376072181002796), .Dim = c(3L, 1L), .Dimnames = list(c("mystatname", 
"mystatname", "mystatname"), NULL)), structure(c(0.483329027649171, 
0.120111644119869, 0.635994427417812), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL)), structure(c(-0.737293488121383, 
0.213491052014004, 0.13812096085893), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL)), structure(c(-0.109743729681682, 
-0.0677283038068912, 0.164418579139007), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL)), structure(c(0.244190613600415, 
-0.33216548283803, 0.234547696503498), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL)), structure(c(-0.0430723221391681, 
-0.0974689179950976, -0.264614288485159), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL))))
      } else if (si == "seed_init_noop") {
        # For updating hardcopy tests, pipe to 'xsel -b' with following command:
        # system2("xsel", args = "-b", input = capture.output(make_expectation(statms)))
expect_equal(statms, list(structure(c(0.741115747025206, -0.514467370324641, 
-0.646603702272565), .Dim = c(3L, 1L), .Dimnames = list(c("mystatname", 
"mystatname", "mystatname"), NULL)), structure(c(-0.0942399955582295, 
-0.438192130956469, -0.249030388527424), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL)), structure(c(0.36028034233341, 
-1.04339184282994, -0.260755661395917), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL)), structure(c(0.281531782264542, 
0.196819241975756, 0.0973152689359221), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL)), structure(c(-0.303887801279731, 
-0.0768759720758417, 0.563010994429381), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL)), structure(c(0.0582027679864314, 
0.460306342196536, 0.456895016559891), .Dim = c(3L, 1L), .Dimnames = list(
    c("mystatname", "mystatname", "mystatname"), NULL))))
      }
    })
  }
  
  
  test_that("mc_reproduce() indeed reproduces (i.e., gives no error).", {
  
    set.seed(1)
    validate_montecarlo(mc)
    expect_error(mc_reproduce(mc, verbose = 0), NA)
  })
  
  
  test_that("mc_reproduce() handles extra arguments correctly.", {
    # error because we don't allow to overwrite dgp.
    expect_error(mc_reproduce(mc, verbose = 0, dgp = 3))
    # error because unused argument by mc_run().
    expect_error(mc_reproduce(mc, verbose = 0, xyz = 3))
  })
  
  
  test_that("mc_reproduce() can reproduce only the first n sims correctly.", {
    expect_error(mc_reproduce(mc, nsims = 1, verbose = 0), NA)
    expect_error(mc_reproduce(mc, nsims = 2, verbose = 0), NA)
    expect_error(mc_reproduce(mc, nsims = 3, verbose = 0), NA)
    # Gives error because these nsims are greater than the original.
    expect_error(mc_reproduce(mc, nsims = 4, verbose = 0), "should be less than or equal")
  })
  
  
  test_that("mc_reproduce() restores pre-rngstate correctly.", {
    # This chunk passes because of the on_parent_exit() code in 'hook_set_seed_factory'.
    #
    # Check that we preserve seed, or at least that we don't leave it in deterministic state
    mc_reproduce(mc, nsims = 1, verbose = 0)
    after1 <- rnorm(1)
    mc_reproduce(mc, nsims = 1, verbose = 0)
    after2 <- rnorm(1)
    expect_true(!identical(after1, after2))
    #
    set.seed(1)
    after3 <- rnorm(1)
    set.seed(1)
    mc_reproduce(mc, nsims = 1, verbose = 0)
    after4 <- rnorm(1)
    expect_true(identical(after3, after4))
  })
  
  # I'm not sure how the wrong seed could actually be stored, but this essentially tests
  # that if a result cannot be reproduced because of a different seed then an error
  # is given.
  test_that("mc_reproduce() fails when recorded pn-chunk seed is not the one that was used.", {
    mc2 <- mc
    set.seed(2)
    # just set a seed that we know is clearly wrong.
    attr(mc2[[1]][[1]], "rnginfo_before")[["seed"]] <- .Random.seed
    expect_error(mc_reproduce(mc2, verbose = 0))
  })
  
  
  dgpp_to_poi_ <- function(dgp_param, maxn) {
    rnorm(1)/maxn
  }
  
  mc_args_local3_l <- mc_args_local_l
  mc_args_local3_l[["dgpp_to_poi"]] <- dgpp_to_poi_
  mcpoi <- do.call(mc_run, mc_args_local3_l)
  
  
  # TODO: document, that this is partly why we must calculate the POIs even
  #       if we don't use them in mc_run() (e.g., diagnostic is NULL)... it's part of
  #       reproducibility!
  # This test passes because of hook_set_seed_factory_dgpp().
  test_that("Can reproduce with stochastic dgpp_to_poi().", {
    expect_error(mc_reproduce(mcpoi, nsims = 1, verbose = 0), NA)
  })
  
  
  test_that("mc_reproduce() fails when recorded dgp_to_poi seed is not the one that was used.", {
    mcpoi2 <- mcpoi
    set.seed(1)
    some_seed <- .Random.seed
    expect_error(mc_reproduce(mcpoi2, verbose = 0), NA)
    # might as well check that mc_reproduce() did not alter the seed.
    expect_true(identical(.Random.seed, some_seed))
    # manually assign an incorrect seed
    attr(mcpoi2[[1]], "param_tuple")[["rngstate_before"]]$seed <- some_seed
    expect_error(mc_reproduce(mcpoi2, verbose = 0))
  })
  
  
  statistic_constant <- function(dataf) {
    return("mystatname" = 5)
  }
  
  
  statistic_2x1_one_is_constant <- function(dataf) {
    stat_mean <- mean(dataf)
    stat_constant <- 1
    ret_m <- matrix(c(stat_mean, stat_constant), nrow = 2)
    rownames(ret_m) <- c("mean", "constant")
    return(ret_m)
  }
  
  
  test_that("mc_reproduce() gives warning if constant stats_m matrices.", {
    set.seed(10)
    mc1 <- mc_run(
             dgp_params = dgp_params_,
             nvec = nvec_,
             nsims = 4,
             dgp = dgp_,
             statistics = statistic_constant,
             verbose = 0
    )
    expect_warning(mc_reproduce(mc1, verbose = 0, check_nonconstant = TRUE))
    expect_warning(mc_reproduce(mc1, verbose = 0, check_nonconstant = FALSE), NA)
  
    set.seed(10)
    mc2 <- mc_run(
             dgp_params = dgp_params_,
             nvec = nvec_,
             nsims = 4,
             dgp = dgp_,
             statistics = statistic_2x1_one_is_constant,
             verbose = 0
    )
    expect_warning(mc_reproduce(mc2, verbose = 0, check_nonconstant = TRUE))
    expect_warning(mc_reproduce(mc2, verbose = 0, check_nonconstant = FALSE), NA)
  })
}
