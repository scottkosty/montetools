# These tests check some various situations all the way through from
# mc_run() to mc_diags() to mc_table().
# We include hardcoded tests of mc_table() so we know if something changes.

dgp_params_ <- c(1, 2, 3)
nvec_ <- c(5, 9)
nsims_1 <- 1

dgp_ <- function(n, dgp_param) {
  # use dgp_param just to trigger error if dgp_param is missing.
  if (identical(dgp_param, 3.11111)) {
    rnorm(n)
  } else {
    rnorm(n, sd = 1.01)
  }
}


statistic_2dim <- function(dataf) {
  vec <- c(mean = mean(dataf), median = median(dataf))
  ret_m <- matrix(vec, nrow = 2, ncol = 1)
  return(ret_m)
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



for (si in seed_inits_l) {
  if (run_hc_tests) {
    # IT = Integration Test
    # HC = hard-coded
    test_that("HC IT of 2-dim stat passes.", {
      set.seed(10)
      mc <- mc_run(
               dgp_params = dgp_params_,
               nvec = nvec_,
               nsims = 2,
               dgp = dgp_,
               statistics = statistic_2dim,
               seed_initializer = si,
               verbose = 0
      )
      diags_ <- mc_diags(mc, diagnostics = diagnostic_)
      expect_error(table_ <- mc_table(diags_, format = "tex"), NA)
      # For updating hardcopy tests, pipe to 'xsel -b' with following command:
      # system2("xsel", args = "-b", input = capture.output(make_expectation(table_)))
      if (si == "seed_init_add_ints") {
if (hc_broken_in_parallel) {
        expect_equal(table_, "\\begin{tabular}{llll}\n  \\hline\n\\hline\n\\multicolumn{2}{l}{} & \\multicolumn{2}{c}{$n$} \\\\\nparam. & stat. & 5 & 9 \\\\ \n  \\hline\n1 & stat1 & 0.440 & 0.572 \\\\ \n\\vspace{0.2cm}   & stat2 & 0.576 & 0.614 \\\\ \n  2 & stat1 & 2.430 & 1.851 \\\\ \n\\vspace{0.2cm}   & stat2 & 2.747 & 1.909 \\\\ \n  3 & stat1 & 2.877 & 2.888 \\\\ \n\\vspace{0.4cm}   & stat2 & 3.014 & 3.003 \\\\ \n   \\hline\n\\vspace{0.2cm}\\hline\n\\end{tabular}\n\\vspace{0.2cm}NA\nNA\n\\vspace{0.4cm}NA\nNA\n\\vspace{0.2cm}NA\nNA\n\\vspace{0.2cm}NA")
}
      } else if (si == "seed_init_pnhash") {
      # system2("xsel", args = "-b", input = capture.output(make_expectation(table_)))
expect_equal(table_, c("\\begin{tabular}{llll}", "  \\hline", 
"\\hline", "\\multicolumn{2}{l}{} & \\multicolumn{2}{c}{$n$} \\\\", 
"param. & stat. & 5 & 9 \\\\ ", "  \\hline", "1 & stat1 & 1.656 & 1.302 \\\\ ", 
"   & stat2 & 1.615 & 1.303 \\\\ ", "  2 & stat1 & 1.738 & 1.911 \\\\ ", 
"   & stat2 & 1.830 & 1.753 \\\\ ", "  3 & stat1 & 2.956 & 2.930 \\\\ ", 
"   & stat2 & 3.114 & 2.791 \\\\ ", "\\hline", "nsims &  & 2 & 2 \\\\", 
"   \\hline", "\\hline", "\\end{tabular}"))
      } else if (si == "seed_init_noop") {
if (hc_broken_in_parallel) {
        expect_equal(table_, "\\begin{tabular}{llll}\n  \\hline\n\\hline\n\\multicolumn{2}{l}{} & \\multicolumn{2}{c}{$n$} \\\\\nparam. & stat. & 5 & 9 \\\\ \n  \\hline\n1 & stat1 & 0.933 & 0.750 \\\\ \n\\vspace{0.2cm}   & stat2 & 0.607 & 0.761 \\\\ \n  2 & stat1 & 1.750 & 1.719 \\\\ \n\\vspace{0.2cm}   & stat2 & 1.952 & 1.761 \\\\ \n  3 & stat1 & 2.861 & 2.700 \\\\ \n\\vspace{0.4cm}   & stat2 & 2.607 & 2.761 \\\\ \n   \\hline\n\\vspace{0.2cm}\\hline\n\\end{tabular}\n\\vspace{0.2cm}NA\nNA\n\\vspace{0.4cm}NA\nNA\n\\vspace{0.2cm}NA\nNA\n\\vspace{0.2cm}NA")
}
      }
    })
  }
}
