x1 <- data.frame(lower = c(1, 2, 3, 4), est = c(1, 2, 3, 4), upper = c(1, 2, 3, 4))
eres1 <- c(1, 2, 3)
x2 <- data.frame(lower = c(1, 4, 3, 4), est = c(1, 2, 3, 4), upper = c(1, 2, 3, 4))
eres2 <- c(2, 3, 1)
x3 <- data.frame(lower = c(5, 4, 3, 9), est = c(1, 2, 3, 4), upper = c(0, 2, 3, 4))
eres3 <- c(3, 2, 1)

# should give an error if check is TRUE:
x4 <- data.frame(lower = c(15, 4, 3, -9), est = c(1, 2, 3, 4), upper = c(0, 2, 3, 4))

# keywords: confidence bands, confidence intervals.
test_that("Inferring column ordering for cbands/CIs", {

  res1 <- infer_col_order(x1)
  res2 <- infer_col_order(x2)
  res3 <- infer_col_order(x3)

  expect_equal(res1, eres1)
  expect_equal(res2, eres2)
  expect_equal(res3, eres3)

  expect_error(infer_col_order(x4, check = TRUE))
  expect_error(infer_col_order(x4, check = FALSE), NA)
})
