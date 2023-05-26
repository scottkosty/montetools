# emulating emergency file results
partial_l <- list(
  `100` = list(a = "a", b = "b", c = "c"),
  `1000` = list(a = "a2")
)

test_that("outside_in works well for partial results", {

  ret <- outside_in(partial_l)
  expected_b_names <- c("100")
  expected_c_names <- c("100")

  expect_equal(names(ret), c("a", "b", "c"))
  expect_equal(names(ret[["a"]]), c("100", "1000"))
  expect_equal(names(ret[["b"]]), c("100"))
  expect_equal(names(ret[["c"]]), c("100"))
})
