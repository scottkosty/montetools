test_that("seed initializer basics", {
  set.seed(7)
  seed7 <- .Random.seed
  phash1 <- "blah"
  phash2 <- "abcxyz"
  phash2_rev <- "zyxcba"
  phash2_sum_equiv <- "aadxyz"

  # 'n' changes, 'p' constant
  seedn5p1 <- seed_init_add_ints(base_seed = seed7, n = 5, phash = phash1)
  expect_false(identical(seed7, seedn5p1))
  seedn6p1 <- seed_init_add_ints(base_seed = seed7, n = 6, phash = phash1)
  expect_false(identical(seedn5p1, seedn6p1))

  # 'p' changes
  seedn5p2 <- seed_init_add_ints(base_seed = seed7, n = 5, phash = phash2)
  expect_false(identical(seedn5p1, seedn5p2))
  seedn6p2 <- seed_init_add_ints(base_seed = seed7, n = 6, phash = phash2)
  expect_false(identical(seedn6p1, seedn6p2))


  seedn5p1_2 <- seed_init_add_ints(base_seed = seed7, n = 5, phash = phash1)
  expect_true(identical(seedn5p1, seedn5p1_2))

  # if seed changes, then result changes
  set.seed(8)
  seed8 <- .Random.seed
  seedn5p1_seed8 <- seed_init_add_ints(base_seed = seed8, n = 5, phash = phash1)
  expect_false(identical(seedn5p1, seedn5p1_seed8))
  seedn5p2_seed8 <- seed_init_add_ints(base_seed = seed8, n = 5, phash = phash2)
  expect_false(identical(seedn5p2, seedn5p2_seed8))

  seedn6p1 <- seed_init_add_ints(base_seed = seed7, n = 6, phash = phash1)
  expect_false(identical(seedn5p1, seedn6p1))

  seedn5p2rev_seed8 <- seed_init_add_ints(base_seed = seed8, n = 5, phash = phash2_rev)
  expect_false(identical(seedn5p2rev_seed8, seedn5p2_seed8))
  seedn5p2sum_equiv_seed8 <- seed_init_add_ints(base_seed = seed8, n = 5, phash = phash2_sum_equiv)
  expect_false(identical(seedn5p2sum_equiv_seed8, seedn5p2_seed8))

  # seed is preserved. I don't know if it's strictly necessary to preserve the seed,
  # but I think it makes sense.
  set.seed(50)
  seed_before <- .Random.seed
  seedn5p1_seed8_x2 <- seed_init_add_ints(base_seed = seed8, n = 5, phash = phash1)
  # while we're at it, might as well do this auxiliary test:
  expect_true(identical(seedn5p1_seed8, seedn5p1_seed8_x2))
  seed_after <- .Random.seed
  # main test of this chunk:
  expect_true(identical(seed_before, seed_after))
})
