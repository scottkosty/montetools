# this file is automatically sourced by other test files.

# run hardcode tests.
# (helpful to turn these off while first dealing with other issues).
#
# hardcoded tests are helpful for discovering bugs (e.g., non-reproducibility in some dimension) due to
# changes in packages or different platforms.
# HC = "hard coded"
run_hc_tests <- TRUE

is_windows <- Sys.info()['sysname'] == "Windows"
# useful to set to FALSE even on Linux sometimes for where we want to compare
# output to Windows machines (e.g., to make sure number of tests is as expected
# the same).
run_unix_only_tests <- !is_windows


# todo: set RNGkind() here?

# These tests fail when testthat tests are run in parallel (by setting the flag in DESCRIPTION).
# I'm not sure why.
# It seems that the "seed_initializer_pnhash" tests do not fail.
# I probably have a bug in my code somewhere.
# Perhaps the parallelization code of testthat changes something about the
# seed so that the hardcopy tests fail.
# It seems that mc_reproduce() always succeeds though, so it is not as concerning.
# TODO: even though not critical, would be good to know why and fix.
#       Make a minimal example.
# To reproduce: set this to TRUE. All tests pass when not in parallel, but
# a few tests fail the parallel flag is set.
hc_broken_in_parallel <- FALSE


seed_inits_l <- c("seed_init_add_ints", "seed_init_pnhash", "seed_init_noop")
# cuts testing time about in half:
# seed_inits_l <- c("seed_init_pnhash")

# We put in cheap asserts by default.
# This option is for checks that are not computationally cheap so not
# good to enable by default, but good for testing and debugging.
options(montetools_asserts = TRUE)

# ?future::future.options
# "Be strict; catch all RNG mistakes"
# todo: what is an example of what this option would catch?
options(future.rng.onMisuse = "error")

# run the tests with the following uncommented from
# time to time.
# I will run these before every release.
# All tests (including hard coded tests) pass as of:
# 7a1a57a5 (2023-01-18)
# d69f741d (2023-05-16)
# options(montetools_parallel_override = "future_for_with_hook")
