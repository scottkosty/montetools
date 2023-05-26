###########################################################
# TODO: process these notes next time I look at this:
#give warning along lines of "your CI is likely too large to capture sdtol of...". Perhaps change to... or increase nsims?
#
#
#color red if the "new is smaller" and "blue" if larger than the "before".
#-- this way can easily see...
#
#Also allow comparing row-by-row. e.g., have statname-before, statname-after
#-- allow user to choose different "comparison methods". compmeth
#
#default the multiple comparison to FALSE? just to be conservative? hmmm
###########################################################

# mc_compare is not ready yet to be exported.
#
# TODO: change "before" to target, and "after" to "current" ?
#
#' TODO: I think this function assumes that nsims of before are very high?
#'        e.g., do we reuire its values to be close to its plim?
#' To understand the default comparison mode, note that if before = after,
#' this function could still give warnings if 'sdtol' and nsims are both relatively
#' small because we look at whether the bounds of the *confidence interval* are
#' within sdtol of the mean of the other.
#' @param sdtol We consider P(|E(X_i) - E(Y_i)| <= SD(X_i)), or something like that, where we ignore the randomness of SD(X_i) for now. TODO: could take upper bound of CI for SD(X_i).
#' @param before The "before" MC.
#' @param after The "after" MC.
#' @param diagnostics The diagnostics for comparison.
#' @param aggregators The aggregators (e.g., mean) of diagnostics results.
#' @param sdtol The tolerance, in units of SDs.
#' @importFrom stats var qnorm pnorm
mc_compare <- function(before, after, diagnostics, aggregators, sdtol = 1/5) {
  # TODO: should we compare e.g., statistic *function* and dgp() among the MC's?
  #       maybe don't give error, but still report what is different?


  if (missing(aggregators)) {
    aggregators <- "mean"
  }
  # note that we should really check whether the *function* is the mean. hmmmmm
  if (aggregators != "mean") {
    # todo: implement conservative checks that don't assume anything about distribution.
    #       e.g., for max
    stop("We currently don't support checking statistical significance of non-mean aggregator.")
  }

  aggregators_ <- list(mean = mean, variance = var)

  pairs_before <- get_pn_pairs(before)
  pairs_after <- get_pn_pairs(after)
  pairs_common <- intersect(pairs_before, pairs_after)

  not_comparing <- !(pairs_after %in% pairs_common)
  if (any(not_comparing)) {
    pairs_str <- paste(pairs_after[not_comparing], collapse = '\n')
    warning("We do not compare the following pairs of 'after' because these are not in 'before': ", "\n", pairs_str)
    # todo: make this output nicer.
  }
  if (all(not_comparing)) {
    stop("There is no common p-n pair to compare. That is, the 'before' and 'after' do not contain simulations for even one same parameter and same number of observations.")
  }
  # I think no need to give warning about pairs in before but not in after,
  # because this should be more expected by the user.
  before <- mc_pnpairs_subset(before, pn_pairs = pairs_common)
  after <- mc_pnpairs_subset(after, pn_pairs = pairs_common)

  statnames_before <- get_statnames(before)
  statnames_after <- get_statnames(after)
  statnames_common <- intersect(statnames_before, statnames_after)
  notcommon_idx <- !(statnames_after %in% statnames_common)
  if (any(notcommon_idx)) {
    warning("We do not compare the following stats of 'after' because these are not in 'before': ", "\n", statnames_after[notcommon_idx])
  }
  if (all(notcommon_idx)) {
    stop("No common statistics with the same name to compare.")
  }
  # alternative to removing the non-common statistics:
  # could instead just use statnames_common to subset the operation
  before <- mc_stats_subset(mc = before, keep_these = statnames_common)
  after <- mc_stats_subset(mc = after, keep_these = statnames_common)


  diags_before <- mc_diags(before, diagnostics = diagnostics, aggregators = aggregators_)
  diags_after <- mc_diags(after, diagnostics = diagnostics, aggregators = aggregators_)

  for (pn in pairs_common) {
    p <- pn[["p"]]
    n <- pn[["n"]]

    means_diff <- diags_before[[p]][[n]][[aggregators]] - diags_after[[p]][[n]][[aggregators]]
    var_before <- diags_before[[p]][[n]][["variance"]]
    var_after <- diags_after[[p]][[n]][["variance"]]

    nsims_before <- get_nsims(before, pn_pair = pn)
    nsims_after <- get_nsims(after, pn_pair = pn)

    var_of_diff <- (1/nsims_before)*var_before + (1/nsims_after)*var_after

    zdiffs <- means_diff/sqrt(var_of_diff)

    # which conf is best default? be conservative.
    conf <- 0.95
    alpha <- 1 - conf
    # todo: maybe use student-t dist?
    critse <- qnorm(1 - alpha/2) * sqrt(var_of_diff)
    ci_lower_of_diff <- means_diff - critse
    ci_upper_of_diff <- means_diff + critse

    # We use the following to give red flag if difference is statistically greater than sdtol * SD (?)
    # use standard deviation of before, because we expect that to have a high nsims
    # and we trust (?) it the most?
    # todo: Eventually, need to take into account the variance of var_before.
    pmax_ <- pmax(abs(ci_lower_of_diff),
         # no need to take abs of ci_upper_of_diff because:
         # ci_upper_of_diff >= abs(ci_lower_of_diff) <=> abs(ci_upper_of_diff) > abs(ci_lower_of_diff)
         ci_upper_of_diff)
    # not really "diff"
    # could fail because diff is too big or not enough precision.
    diff_in_sdunits <- pmax_ / sqrt(var_before)
    ci_check_fails <- diff_in_sdunits > sdtol
    # sdtol (purposefully) does not go to 0 as nsims increases.
    # MC CI length should go to 0.

    pvals <- 2*pnorm(-abs(zdiffs))

    # diff?
    alpha2 <- 1 - alpha
    check_these <- pvals < alpha2

    # relying on SDs does assume something about dist. .e.g, no outliers and normal?
    # Maybe use interquartile range?

    # TODO: where check_these is TRUE, report CI to user

    # TODO: remember all estimators could have high correlation.

    # TODO: This is not right, but the goal is to give one overall p-value.
    # na.rm happens when zdiff is 0 (i.e., same numbers, e.g., both before and after are 1)
    one_overall_pval <- min(pvals, na.rm = TRUE)

    message("For p = ", p, ", n = ", n, ":")
    message("distances *in SD units* from 'before' means to furthest CI bound of 'after': ")
    print(diff_in_sdunits)
    message("")
    message("distances in original units from 'before' means to furthest CI bound of 'after': ")
    print(pmax_)
    message("")
    message("distances from 'before' means to 'after' means (i.e., nothing about statistical significance): ")
    print(means_diff)
    message("")


    if (any(is.na(ci_check_fails))) {
      # make sure the problem indeed comes from 0 variance
      stopifnot(identical(is.na(c(ci_check_fails)), c(var_before) == 0 | c(var_after) == 0))
      #
      # even if means are equal to each other, if 0 variance,
      # can't make a statement about statistical significance.
      # TODO: unless: try just assuming one is 0, there are bounds...
      #       can put bounds on variance.
      warning("Not comparing some since variance is 0.")
    }
    if (any(as.vector(ci_check_fails[!is.na(ci_check_fails)]))) {
      warning("For p = ", p, ", n = ", n, ": ", "CI is not inside +/- ", sdtol, " SD's of 'before'.")
      message("")
    }
  }

  # TODO: this is just a placeholder until we figure out correct procedures above.
  dummy_return <- 0.5

  return(dummy_return)

  # note that we need to estimate variance.
}
