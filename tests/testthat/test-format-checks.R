dgp_params_ <- c(1, 2, 3)

nvec_ <- c(5, 9)
nsims_1 <- 1

dgp_ <- function(n, dgp_param) {
  rnorm(n)
}

statistic_ <- function(dataf) {
  mean(dataf)
}

test_that("format checking works", {

  mc <- mc_run(
           dgp_params = dgp_params_,
           nvec = nvec_,
           nsims = nsims_1,
           dgp = dgp_,
           statistics = statistic_,
           verbose = 0
  )

  expect_error(check_format(mc), NA)
  attr(mc, "format") <- get_ver_format() + 1
  expect_error(check_format(mc))
  # currently don't port data structures over yet.
  attr(mc, "format") <- get_ver_format() - 1
  # currently don't port data structures over yet.
  expect_error(check_format(mc))
  attr(mc, "format") <- get_ver_format()
  expect_error(check_format(mc), NA)
})
