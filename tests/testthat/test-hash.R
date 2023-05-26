test_that("hash of a function is the same before and after using it", {
  fun1 <- function(x) sqrt(x)
  hash1 <- calc_hash(fun1)
  fun1(7)
  hash2 <- calc_hash(fun1)
  expect_identical(hash1, hash2)

  list_with_fun <- list(f1 = function(x) sqrt(x))
  hash3 <- calc_hash(list_with_fun)
  list_with_fun[[1]](7)
  hash4 <- calc_hash(list_with_fun)
  expect_identical(hash3, hash4)

  fun2 <- function(x) sqrt(x^2)
  expect_false(identical(hash1, calc_hash(fun2)))
})


test_that("A hash is not changed by using or compiling a function", {
  # This also tests a function in an attribute.
  obj_with_func_attr <- 1:5
  attr(obj_with_func_attr, "this_is_a_func") <- function(x) 2 + 5*x^3
  list_with_func_attr <- list(list(list(ofa = obj_with_func_attr, "text"), "another element"), 2, 5, "hi")
  hash_before_using <- calc_hash(list_with_func_attr)
  func_ret <- attr(list_with_func_attr[[1]][[1]][["ofa"]], "this_is_a_func")(2)
  expect_equal(func_ret, 42)
  hash_after_using <- calc_hash(list_with_func_attr)
  expect_identical(hash_before_using, hash_after_using)
  # do an *explicit* byte compile.
  attr(list_with_func_attr[[1]][[1]][["ofa"]], "this_is_a_func") <- compiler::cmpfun(attr(list_with_func_attr[[1]][[1]][["ofa"]], "this_is_a_func"))
  hash_after_using_and_compiling <- calc_hash(list_with_func_attr)
  expect_identical(hash_before_using, hash_after_using_and_compiling)
})


test_that("(inverted) hash of equivalent behaviors of functions are the same", {
  # It is good if these tests start to fail. We want the same hashes in these cases.
  # todo: It might be possible to compare the byte compilations of the functions in some way.
  fun1 <- function(x) sqrt(x)
  fun3 <- function(x) {
    sqrt(x)
  }
  expect_false(identical(calc_hash(fun1), calc_hash(fun3)))

  fun4 <- function(x) {
    # this comment causes different hashes.
    # The output of body() is the same, but the
    # (non-printed) contents are not. Maybe because of different environments.
    sqrt(x)
  }
  expect_false(identical(calc_hash(fun3), calc_hash(fun4)))
})


dgp_params_ <- list(
  list(function(x) x^2),
  list(function(x) x^5)
)

nvec_ <- c(5, 9)

dgp_ <- function(n, dgp_param) {
  rnorm(1)
  dgp_param[[1]](1)
}


dgpp_to_poi_ <- function(dgpp) {
  dgpp[[1]](3)
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

mc <- mc_run(
         dgp_params = dgp_params_,
         nvec = c(3, 4),
         nsims = 2,
         dgp = dgp_,
         statistics = statistic_4,
         dgpp_to_poi = dgpp_to_poi_,
         verbose = 0
)

# now that the function has been called inside dpg_params, there
# might be byte code and thus the hash might have changed if
# we did not work around this.
#
# It was already run from mc_run, but just to make *sure* we call the
# functions explicitly here:
dgp_params_[[1]][[1]](4)
dgp_params_[[2]][[1]](4)
mc2 <- mc_run(
         dgp_params = dgp_params_,
         nvec = c(3, 4),
         nsims = 2,
         dgp = dgp_,
         statistics = statistic_4,
         dgpp_to_poi = dgpp_to_poi_,
         verbose = 0
)


test_that("hash of a dgp_param with a function is equal to hardcoded snapshot", {
  expect_identical(names(mc), c("85282d2bfda73b27c319a35f4f3e9fb8", "8210274e4f420a6cf873c7eabf21ee15"))
  expect_identical(names(mc), names(mc2))
})
