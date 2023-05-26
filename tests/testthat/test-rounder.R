test_that("rounder basics", {
  rounder_2digits <- gen_rounder_fixed(digits = 2)
  rounder_3digits <- gen_rounder_fixed(digits = 3)

  vec1 <- c(basic = 0.416666666666667, perc = 0.0833333333333333)
  expected <- c(basic = "0.417", perc = "0.083")
  result <- rounder_3digits(vec1)
  expect_equal(result, expected)

  result2 <- rounder_2digits(vec1)
  expected2 <- c(basic = "0.42", perc = "0.08")
  expect_equal(result2, expected2)
})
