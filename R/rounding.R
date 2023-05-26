# TODO: should make a lot of tests when editing this file

# tests:
#x1 <- c(0.5, 1)
# "0.5" "1.0"

# useful for advanced rounding:
# https://stackoverflow.com/questions/22094444/rounding-a-vector-so-that-all-resulting-elements-are-different
# https://stackoverflow.com/a/12135122/1376404

nmo <- function(vec, digits = 3) {
  # converts to character:
  fmt <- paste0("%.", digits, "f")
  xxx <- sprintf(fmt, vec)
  names(xxx) <- names(vec)
  #xxx <- prettyNum(vec)
  #xxx <- format(vec)

  # nice property in some sense is it keeps type as numeric
  # but then strange things happen (to see, use the following code
  # and run the tests)
  # xxx <- signif(vec, digits = 3)

  return(xxx)
}


round_m_to_fixed <- function(m, digits) {
  #ret <- format(block, digits = 5)
  ret <- apply(X = m, MARGIN = 1, FUN = nmo, digits = digits)
  if (ncol(m) == 1) {
    # R used to simplify to a vector.
    # (https://stackoverflow.com/a/67089346/1376404)
    # as of 2021-03-06, R-devel provides a simplify argument for apply().
    # switch to that in a few years if pmpinf requires a higher R version anyway?
    ret <- matrix(ret, nrow = 1)
  }
  return(t(ret))
}


#' @export
#' @title Generate a fixed-digit rounder
#' @param digits The number of digits to round to.
gen_rounder_fixed <- function(digits) {
rounder <- function(block_m) {
  # allow arg to be a vector. Useful for generalization and also for
  # unit tests.
  if (is.vector(block_m)) {
    ret <- nmo(block_m, digits = digits)
  } else {
    ret <- round_m_to_fixed(m = block_m, digits = digits)
  }

  return(ret)
}
return(rounder)
}


# For compare_across_dgpps = TRUE we would need to pass scarfs for all dgpps!
#
#' @export
#' @title A rounder that chooses digits based on comparing entries in various ways
#' @description This is a work in progress. Do not use yet.
#' @param compare_across_nobs Compare across number of observations.
#' @param compare_across_stats Compare across number of stats.
#' @param compare_across_dgpps Compare across DGPPs (not yet implemented).
gen_rounder_comparison <- function(compare_across_nobs = TRUE, compare_across_stats = TRUE, compare_across_dgpps = TRUE) {
rounder <- function(block_m) {

  min_order_preserving_digit <- function(vec) {
    # todo: first identify ties, then remove them? hmm not sure.
    rank_ <- rank(vec)
    digits_candidate <- 0
    # 10 is arbitrary, but need to stop somewhere, no?
    # todo: a bisect algorithm would be more efficient.
    while (digits_candidate < 10) {
      rank_candidate <- rank(round(vec, digits = digits_candidate))
      if (identical(rank_, rank_candidate)) {
        break
      }
      digits_candidate <- digits_candidate + 1
    }
    return(digits_candidate)
  }

  # TODO: for row, have option to only compare to left and right, not full row.
  digits_rows <- apply(X = block_m, MARGIN = 1, FUN = min_order_preserving_digit)
  # for col, makes more sense to preserve rank across all, not just adjascent.
  digits_cols <- apply(X = block_m, MARGIN = 2, FUN = min_order_preserving_digit)

  # this is where need to take into account prefs
  all_cells_same_digit <- TRUE
  if (all_cells_same_digit) {
    digit_ <- max(c(digits_rows, digits_cols))
  }
  ret <- round_m_to_fixed(m = block_m, digits = digit_)

  return(ret)
}
return(rounder)
}
