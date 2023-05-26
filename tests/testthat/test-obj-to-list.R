# obj_to_list() is the main building block for dgp_params_to_list().

test_that("obj_to_list gives correct result", {

  # I don't think the names of the list are that important.
  eresA <- list("1" = 1, "2" = 2, "3" = 3)
  vec1 <- c(1, 2, 3)
  res1 <- obj_to_list(vec1)
  res2 <- obj_to_list(as.list(vec1))
  res3 <- obj_to_list(cbind(vec1))

  expect_identical(res1, eresA)
  # we do not assign the names in obj_to_list() because the list can contain
  # functions or anything, so let's just be consistent and not attempt any magic.
  # Here we assign the names after so that expect_identical() passes.
  names(res2) <- c("1", "2", "3")
  expect_identical(res2, eresA)
  expect_identical(res3, eresA)

  eresB <- list("1,2,3" = c(1, 2, 3))
  res4 <- obj_to_list(rbind(vec1))
  expect_identical(res4, eresB)
  # different because passes one-row data frames to dgp() instead of vectors
  # (so that elements can have different classes)
  eresC <- list(
                "1" = data.frame(vec1 = 1),
                "2" = data.frame(vec1 = 2),
                "3" = data.frame(vec1 = 3)
           )
  eresC <- lapply(eresC, function(x) {
                     attr(x, "row.names") <- as.integer(x$vec1)
                     #rownames(x) <- x$vec1
                     return(x)
           })
  res5 <- obj_to_list(data.frame(vec1))
  expect_identical(res5, eresC)

#  expect_equal(res2, eres2)
#  expect_equal(res3, eres3)
#
#  expect_error(infer_col_order(x4, check = TRUE))
#  expect_error(infer_col_order(x4, check = FALSE), NA)
})
