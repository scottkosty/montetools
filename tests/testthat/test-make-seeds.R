# Thee tests are important because future.apply::make_rng_seeds()
# depends on the unexported future.apply:::make_rng_seeds, so it
# would be reasonable for that function's interface/behavior to
# change.
#
# Alternative: walk nextRNGStream() manually.
# Alternative: use something like the following, which is a
#              tweaked version of an example in ?future_lapply::future_lapply.
# future_lapply(1:14, FUN = function(x) fun(),
#                future.seed = TRUE)

# TODO: try also plan(multicore) (if linux...)

# TODO: also an output test! just hardcode the list of seeds (get from dput()).

# This is the feature we essentially rely on to be able to extend, and still
# reproduce.

set.seed(1)
n1 <- 10
n2 <- 14
plan('sequential')
seeds_short <- future_lapply(seq.int(n1), FUN = function(x) .Random.seed,
                       future.seed = TRUE)
seeds_part2 <- make_seeds(n2 - n1, begin_after = seeds_short[[n1]])
seeds_combined <- c(seeds_short, seeds_part2)

set.seed(1)
seeds_long <- future_lapply(seq.int(n2), FUN = function(x) .Random.seed,
                         future.seed = TRUE)

test_that("Shorter sequence is a subset of longer sequence.", {
  expect_true(all.equal(seeds_short, seeds_long[1:n1]))
  set.seed(1)
})

test_that("We can reproduce longer sequence from last element of shorter.", {
  expect_true(all.equal(seeds_part2, seeds_long[(n1 + 1):n2]))
  expect_identical(seeds_combined, seeds_long)
})



if (run_unix_only_tests) {
  test_that("Seeds do not depend on plan.", {
    set.seed(1)
    plan('multicore')
    seeds_short_multicore <- future_lapply(seq.int(n1), FUN = function(x) .Random.seed,
                           future.seed = TRUE)
    expect_identical(seeds_short, seeds_short_multicore)
    plan('sequential')
  })
}


test_that("different ways to make seeds are equivalent.", {
  set.seed(1)
  seeds_short2 <- make_seeds(n1)
  expect_identical(seeds_short, seeds_short2)
  next_ <- parallel::nextRNGStream(seeds_short[[n1]])
  stopifnot(identical(next_, seeds_long[[n1 + 1]]))
})


test_that("pre-generating seeds gives equivalent output of future_lapply().", {
  set.seed(1)
  res1 <- future_lapply(seq.int(n1), FUN = function(x) rnorm(2),
                             future.seed = seeds_short)
  set.seed(1)
  res2 <- future_lapply(seq.int(n1), FUN = function(x) rnorm(2),
                             future.seed = TRUE)
  expect_identical(res1, res2)
})
