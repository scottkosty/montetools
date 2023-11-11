# ci = confidence interval
# ht = hypothesis testing

# describe what an aggregator is.
# each aggregator must take a vector and return a scalar.
run_aggregators <- function(l, aggregators) {
  rb <- do.call(rbind, l)

  ret_l <- lapply(X = aggregators, FUN = function(agg) {
                 a_ <- apply(X = rb, MARGIN = 2, FUN = agg)
                 return(a_)
                 })

  return(ret_l)
}

# allow user to pass which aggregator as argument?
#
# microop: There is surely a more efficient way to calculate both
# mean and variance (also max, min, etc.) of a vector at once. Is it worth the complication?
#
# typically takes the mean, but could take quantiles or max.
# for example, if "computation time" is a diagnostic, it's common to take the minimum (reference Python timeit).
#
# note that do_mc_pair() calls this function directly.
#
# the old way (see my archived version of 'diags_aggregator_old' in archived old version) was for the inefficient list storage. However it had the benefit of (maybe?) working with 'aux' list.
# TODO: Need to provide an alternative way to use the aux list, etc. First have a use case before adding that complexity.
diags_aggregator <- function(diags_stacked_df, aggregators, statnames) {
  # todo: could do this more efficiently... could have one function that does all transformations, rather than breaking it up into aggregators.
  #       how much would that buy?
  #       I'm sure there's a "language of transformations" I could use that would make it easy to pass arguments.
  # todo: return a matrix? hmm not sure.

  # TODO: add a test where this would a problem if we removed this condition.
  #       now we do not remove NAs, so I don't think this is triggered if all NAs.
  #       How should we update it?
  if (nrow(diags_stacked_df) == 0) {
    stop("We do not (yet?) handle a diags_stacked_df with nrow of 0.")
  }

  nstats <- length(statnames)
  nsims <- nrow(diags_stacked_df) / nstats
  if (!(nsims %% 1 == 0)) {
    internal_error("inferred 'nsims' is not an integer.")
  }


  ret_l <- list()
  for (sf in aggregators) {
    # TODO: make a utility function to do this. Rely on someone else's function? e.g., `by`?
    aggregated_by_stat_l <- list()
    for (s in seq.int(nstats)) {
      subset_idx <- rep(FALSE, times = nstats)
      subset_idx[[s]] <- TRUE
      diags_this_stat_df <- diags_stacked_df[subset_idx, , drop = FALSE]
      by_diag <- sapply(diags_this_stat_df, FUN = sf)
      aggregated_by_stat_l <- c(aggregated_by_stat_l, list(by_diag))
    }
    flee <- do.call(rbind, aggregated_by_stat_l)
    rownames(flee) <- statnames
    ret_l <- c(ret_l, list(flee))
  }
  names(ret_l) <- names(aggregators)

  # This is needed for "display_nsims" argument to mc_table() because otherwise
  # mc_table() just sees the (aggregated) diagnostics.
  attr(ret_l, "nsims") <- nsims

  return(ret_l)
}

#' @name mc_diagnostics
#'
#' @title Diagnostic functions for evaluating Monte Carlo results
#'
#' @details An MC diagnostic, or 'diag' for short, is a function whose purpose
#' is to evaluate and summarize the results of MC simulations. A diag can be passed to the
#' 'diagnostics' argument of `mc_run` to display results after each pn-chunk
#' finishes, or can be passed to `mc_diags()` to calculate diagnostics on a
#' completed MC object.
#'
#' A diagnostic function must accept two arguments. The first argument, 'stats_m', is the matrix
#' of stacked (across simulations) statistics. The second argument, 'true_poi', is the true POI value, i.e., the value of the POI associated with the data generating process parameter (DGPP) that the DGP used to produce the simulated data set (often the true POI *is* the DGPP). You may also pass a list of such functions.
#'
#' The diagnostic function should return a data.frame or matrix with column names the names of the diagnostics. No column name in the returned object should be empty.
#'
#' Some common diagnostics are functions of only the simulated statistic values (e.g., average/median of rejection rates for MC hypothesis tests, or confidence band length for MC estimation), and some also input the dgp param (e.g., squared error for MC estimation).
#' @seealso See gen_diags_ci() to create diagnostic functions for estimators with confidence intervals.
NULL

# TODO: link "POI" to documentation ?mc_poi and the help should also cover the term "dgpp".

# TODO: does it make sense to document the arguments for diags_ci ? User will just use them as arguments themselves.

#' @name diags_ci
#' @rdname diags_ci
#'
#' @title Diagnostics for statistics of the form c(lower, est, upper).
#'
#' @param lower The vector of lower bounds of the confidence band estimates.
#' @param est The vector of point estimates.
#' @param upper The vector of upper bounds of the confidence band estimates.
#' @param true_poi The vector of parameter values.
#'
#' @description Often used as arguments to [gen_diags_ci()], these diagnostics calculate the following: lower/upper/combined coverage.
#' @seealso See gen_diags_ci() to create diagnostic functions for estimators with confidence intervals. See diags_est for estimator diagnostics.
NULL

#' @name diags_est
#' @rdname diags_est
#'
#' @title Diagnostics for statistics that are (typically point) estimators.
#'
#' @param est The vector of point estimates.
#' @param true_poi The vector of parameter values.
#'
#' @description These diagnostics can be used to calculate the (empirical) MSE. They can be combined with CI diags (e.g., see gen_diags_ci()).
#' @seealso See diags_ci for info on CI diagnostics. See gen_diags_ci() to create diagnostic functions for estimators with confidence intervals. See est_diag_to_ci_diag() for how to use a estimator diag as a CI diag.
NULL


#' @rdname diags_est
#' @export
# TODO: document that these should be *vectorized*.
# TODO: data.frame because one col (covers) is logical. TODO: document this requirement (and why)!
#       could force "covers" to return 0/1 instead...
#       but might as well. Could imagine returning other types. e.g., integer type. or higher precision in some cases.
#       any examples where character type could be interesting? hmmm
#       If no good examples, enforce numeric type for now?
diag_est_bias <- function(est, true_poi) {
  ret <- data.frame(bias = est - true_poi)
  return(ret)
}


#' @export
#' @rdname diags_est
diag_est_l1 <- function(est, true_poi) {
  ret <- data.frame(l1 = rowSums(abs(est - true_poi)))
  return(ret)
}


#' @export
#' @rdname diags_est
diag_est_l2 <- function(est, true_poi) {
  ret <- data.frame(l2 = sqrt(rowSums((est - true_poi)^2)))
  return(ret)
}


#' @export
# Note that this diagnostic is not the same as L2 in the case
# where est is one-dimensional. Rather it is the same as L1.
#' @rdname diags_est
diag_est_sq_error <- function(est, true_poi) {
  # TODO: give error if est is not one column?
  ret <- data.frame(squared_error = (est - true_poi)^2)
  return(ret)
}
sq_error_labels <- list(
  # The name of the following list should correspond with the name in the data.frame.
  # One reason we need that is because we can't rely on order because we don't know
  # how many columns diagnostics will return.
  squared_error = list(
    # indiv means non-aggregated
    # TODO: think of better name. nonaggregated
    indiv = list(
      # TODO: could call this "ascii", but maybe unicode and accents are fine?
      #       In that case call it "unicode"?
      plain = "sq. error",
      latex = "$(\\hat{\\beta} - \\beta)^2$"
    ),
    mean  = list(
      plain = "MSE",
      latex = "MSE"
    )
  )
)
attr(diag_est_sq_error, "labels") <- sq_error_labels


#' @export
#' @rdname diags_est
diag_est_abs_error <- function(est, true_poi) {
  # TODO: give error if est is not one column?
  ret <- data.frame(abs_error = abs(est - true_poi))
  return(ret)
}
abs_error_labels <- list(
  abs_error = list(
    indiv = list(
      plain = "abs. error",
      latex = "$|\\hat{\\beta} - \\beta|$"
    ),
    mean  = list(
      plain = "MAE",
      latex = "MAE"
    )
  )
)
attr(diag_est_abs_error, "labels") <- abs_error_labels


#' @export
#' @rdname diags_ci
diag_ci_length <- function(lower, est, upper, true_poi) {
  ret <- data.frame(length = upper - lower)
  return(ret)
}

#' @export
#' @rdname diags_ci
diag_ci_covers <- function(lower, est, upper, true_poi) {
  ret <- data.frame(covers = lower <= true_poi & true_poi <= upper)
  return(ret)
}

#' @export
#' @rdname diags_ci
diag_ci_covers_l <- function(lower, est, upper, true_poi) {
  ret <- data.frame(covers_l = lower <= true_poi)
  # otherwise, LaTeX error.
  names(ret) <- "covers.l"
  return(ret)
}

#' @export
#' @rdname diags_ci
diag_ci_covers_u <- function(lower, est, upper, true_poi) {
  ret <- data.frame(covers_u = true_poi <= upper)
  names(ret) <- "covers.u"
  return(ret)
}


#' @export
#' @title Diagnostic for proportion NA
#' @description Diagnostic for proportion NA. Useful in conjunction with statistics wrapped in ret_na_if_error.
#' @param stats_m The stats_m of an MC pn-pair.
#' @param true_poi The parameter of interest.
diag_prop_NA <- function(stats_m, true_poi) {
  # copied from somewhere else.
  # any() or all()?
  row_has_NA <- apply(X = stats_m, MARGIN = 1, FUN = function(row_) any(is.na(row_)))
  row_has_NA_df <- data.frame(has_NA = row_has_NA)
  return(row_has_NA_df)
}


est_diag_to_ci_diag <- function(est_diag) {
  ret_fn <- function(lower, est, upper, true_poi) {
    # might as well remove them to make clear we ignore them.
    rm(lower, upper)
    ret <- est_diag(est = est, true_poi = true_poi)
    return(ret)
  }

  labels_ <- attr(est_diag, "labels")
  if (!is.null(labels_)) {
    attr(ret_fn, "labels") <- labels_
    # might as well tidy up
    attr(est_diag, "labels") <- NULL
  }

  return(ret_fn)
}


# have gen_diags_ci and diags_ht? (for hypoth

# diags need to know true parameter for coverage and for squared error.
# col_order: a vector of length 3 with (ordered) elements the column names or indexes corresponding to the lower
#         confidence band/point, the column name/index of the estimate, and the column name/index of the upper confidence band/point. If missing, the elements will be inferred from examining the ordering of the contents of one_sim_res.

# TODO: change name of arg from 'diags_l', since they are not technically "MC diags", are they?
#       update: why note? seems fine.

#' @title Generates MC diagnostics for estimators with confidence intervals
#'
#' @param diags_ci_l A list of individual CI diagnostics. Each function in this list should accept arguments "lower", "est", "upper", and "true_poi". See ?diags_ci
#' @param diags_est_l A list of individual estimator diagnostics. Each function in this list should accept arguments "est" and "true_poi". See ?diags_est
#'
#' @details Returns diagnostics that can be used in the 'diagnostics' argument of mc_run() or mc_diags().
#' @export
gen_diags_ci <- function(diags_ci_l = list(diag_ci_length, diag_ci_covers), diags_est_l = list(diag_est_bias, diag_est_sq_error)) {

  diags_l <- c(diags_ci_l, lapply(diags_est_l, FUN = est_diag_to_ci_diag))

  # TODO: why have 'diags_l' as an arg? remove it? Ah, maybe to avoid error about globals?
  ret_fn <- function(one_sim_res, true_poi, col_order,
                     diags = diags_l) {
    # EXPIRE this check? it was a quick way to catch some problems..
    stopifnot(!is.null(true_poi))

    if (!is.matrix(one_sim_res)) {
      stop("We expected one_sim_res to be a matrix. This is an internal problem. Please report with an example to reproduce.")
    }
    if (!ncol(one_sim_res) == 3) {
      # If there is just one col and all are NAs, then we assume all sims had
      # errors and just proceed with a CI matrix of NAs.
      if (ncol(one_sim_res) == 1 && all(is.na(one_sim_res[, 1]))) {
        # create a "NA" matrix with dimensions consistent with a "real" CI sim res.
        one_sim_res <- cbind(one_sim_res, one_sim_res, one_sim_res)
        col_order <- c(1, 2, 3)
      } else {
        stop("There should be *three* columns in each simulation result (i.e., the return from statistic()): a column with lower band/point, the estimate, and the upper band/point.")
      }
    }

    # TODO: also skip this if the columns of sim_res are called "upper", "lower" and "est" ?
    if (!all(names(one_sim_res) %in% c("lower", "est", "upper"))) {
      # TODO: don't allow col_order to be missing. Need to fix this at a higher level somehow and just pass arg?
      stop("can use col_order, but it's expensive. maybe give a warning?")
    }

    if (missing(col_order)) {
      verbose <- 1
      if (verbose >= 2) {
        message("Going to infer which columns are the 'lower', 'est', and 'upper'. We assume an ordering exists. You may use 'check = TRUE' arg of infer_col_order() to check that assumption holds for your data.")
      }
      # microop: this is run once for every sim. We could just run it on one sim and assume same
      #          ordering across sims.
      # This is actually pretty cheap as long as we use check = FALSE.
      col_order <- infer_col_order(one_sim_res, check = FALSE)
    }
    lower <- one_sim_res[, col_order[[1]], drop = TRUE]
    est <- one_sim_res[, col_order[[2]], drop = TRUE]
    upper <- one_sim_res[, col_order[[3]], drop = TRUE]

    # since used drop = TRUE above, we lose the rowname when nstats is 1, so restore
    names(lower) <- names(est) <- names(upper) <- rownames(one_sim_res)
  
    by_diag_l <- lapply(X = diags, function(f) {
                  f(lower = lower, est = est, upper = upper, true_poi = true_poi)
               })
    ret <- do.call(cbind, by_diag_l)

    stopifnot(identical(nrow(ret), nrow(one_sim_res)))
    # TODO: even if one_sim_res is a matrix, ret is a data.frame (I think because we want columns with logical values.)
    # Instead, change logical column to 0/1 and just make diag a matrix?
    # The problem is that we want to preserve the row names.
    # maybe use a different attribute? just call the attribute "statname" ?
    #
    # This gives a warning since row names of a data.frame should be unique.
    # rownames(ret) <- rownames(one_sim_res)
    #attributes(ret)$row.names <- rownames(one_sim_res)
    return(ret)
  }
  labels_ <- combine_labels_from_diags(diags_l)
  attr(ret_fn, "labels") <- labels_
  return(ret_fn)
}

#' @title Diagnostic for rejection statistics
#' @export
#' @param stats_m the matrix of statistic values.
#' @param true_poi the true POI.
diag_ht_rr <- function(stats_m, true_poi) {
  # We don't actually need to do any statistical manipulation of stats_m.
  # The diagnostics aggregator will take care of aggregating the
  # individual diagnostics. So we just do some sanity checks and return a data.frame.

  if (is.matrix(stats_m)) {
    values_are_logical <- all(apply(stats_m, MARGIN = 2, FUN = is.logical))
    if (!values_are_logical) {
      values_are_01 <- all(stats_m %in% c(0, 1))
    }
  } else if (is.data.frame(stats_m)) {
    # I forget if it can currently be a data.frame. We check just in case.
    values_are_logical <- all(sapply(stats_m, is.logical))
    if (!values_are_logical) {
      values_are_01 <- all(unlist(stats_m) %in% c(0, 1))
    }
  } else {
    stop("Error: unexpected stats_m format.")
  }
  if (!values_are_logical && !values_are_01) {
    stop("diag_ht_rr expects either (a) a statistic with entries of TRUE (reject) or FALSE (fail to reject) entries; or (b) a statistic with entries of 0 (fail to reject) or 1 (reject).")
  }

  stopifnot(ncol(stats_m) == 1)
  ret_df <- data.frame(reject = stats_m[, 1])

  return(ret_df)
}


#' @title A generator of diagnostics of p-value statistics
#'
#' @param p The level corresponding to the hypothesis test.
#' @export
diag_ht_pval2rr_gen <- function(p) {
  diag_ <- function(stats_m, true_poi) {
    reject_m <- stats_m <= p
    # alternative: instead of above, could do:
    # diag_ht_rr(stats_m = stats_m <= p)
    stopifnot(ncol(stats_m) == 1)
    ret_df <- data.frame(reject = reject_m[, 1])
    return(ret_df)
  }
  return(diag_)
}


# abstract interface so can change underlying container.
# run a function on every param/nobs combination
# parallel assumes "mclapply()" for now (this is not exposed to user anyway).
# ... are passed to FUN.
#' @importFrom parallel mclapply
param_nobs_apply <- function(X, FUN, parallel = FALSE, ...) {
  # We copy X to X_outer because we will have inner lapplys (which have X as arg). It's
  # not necessary but makes it a bit more readable.
  X_outer <- X
  rm(X)

  all_pairs <- get_pn_pairs(X_outer)
  f_ <- function(l) {
    p_ <- l[['p']]
    n_ <- l[['n']]
    # TODO: This is a hack (although seems somewhat robust). Should names of param_tuples be the p_hashes?
    p_idx <- which(names(X_outer) == p_)
    # TODO: why pass n? what does the function do with it? maybe needs it for *storing* result, not for computing ?
    a_ <- FUN(X_outer[[p_]][[n_]], n = n_, ptup = get_attr(X_outer[[p_]], "param_tuple"), ...)
    return(a_)
  }
  if (parallel) {
    Xret_flat <- mclapply(X = all_pairs, FUN = f_)
  } else {
    Xret_flat <- lapply(X = all_pairs, FUN = f_)
  }
  Xret <- list_to_mc(l = Xret_flat, all_pairs = all_pairs)

  attributes(Xret) <- attributes(X_outer)
  # also copy over p-specific attributes, like "param_tuple"
  # and pn-specific (currently these don't exist but good in case we add in the future).
  for (p_idx in seq_along(X_outer)) {
    attributes(Xret[[p_idx]]) <- attributes(X_outer[[p_idx]])
    for (n_idx in seq_along(X_outer[[p_idx]])) {
      # TODO: create a helper function for this... I do it in other places...
      #       and make corresponding tests
      attributes(Xret[[p_idx]][[n_idx]]) <- c(attributes(Xret[[p_idx]][[n_idx]]), attributes(X_outer[[p_idx]][[n_idx]])[names(attributes(X_outer[[p_idx]][[n_idx]])) != "names"])
    }
  }

  # This was the old way of doing it. The above looks more complicated, but it
  # allows for parallelization. Even without parallelization (e.g., lapply()),
  # the above might be a bit faster.
  #X_old <- X_outer
  #for (dgpp_idx in seq_along(X_outer)) {
  #  for (n_idx in seq_along(X_outer[[dgpp_idx]])) {
  #    # I don't pass nvec anymore (since no longer store as an attribute). If I bring this old code back, I'll need to
  #    # get nvec differently, or change the code. should be able to get nvec from the names of the MC list!
  #    X_old[[dgpp_idx]][[n_idx]] <- FUN(X_outer[[dgpp_idx]][[n_idx]], n = nvec[[n_idx]], ptup = param_tuples[[dgpp_idx]])
  #  }
  #}
  #stopifnot(identical(Xret, X_old))

  return(Xret)
}


# *partial* work on a potential alternative to param_nobs_apply(). This might be faster
# for the case when nparams is a factor of ncpus.
#
# The idea is to run at the param level... don't run separately by nobs!
# we can take advantage of the fact that it is the same param for all nobs.
# This is good justification for why we store "p" on outside list, by the way.
#param_apply <- function(X, FUN, param_tuples, nvec, parallel = FALSE) {
#  # first merge all into one single list
#  Xret <- X
#  fun_ <- function(p) {
#    all_nobs_within_p <- lapply(X = X[[p]], FUN = function(x) x[["sims_l"]][["stats_m"]])
#    within_p_ret <- do.call(rbind, all_nobs_within_p)
#    wrapper <- list(sims_l = list(stats_m = within_p_ret))
#    a_ <- FUN(wrapper, n = 111, ptup = param_tuples[[which(p == names(X))]])
#    return(a_)
#    # todo: now need to break it back up and store each nobs.
#  }
#  if (parallel) {
#    testing <- mclapply(X = names(X), FUN = fun_)
#  } else {
#    testing <- lapply(X = names(X), FUN = fun_)
#  }
#  # todo: now need to store 'testing' back in format of Xret.
#  return(testing)
#}


diagnostics_combine <- function(diagnostics_l) {
  combined <- function(stats_m, true_poi) {
    all_diag_rets <- lapply(X = diagnostics_l, function(one_diag) {
                              one_ret <- one_diag(stats_m, true_poi)
                              # have to check diag here.
                              # In one case, one of two diags returned
                              # a scalar, and after cbind below, the scalar
                              # was repeated (and thus went undetected).
                              check_diag_ret(stats_m = stats_m, diag_ret = one_ret)
                              return(one_ret)
                     })
    combined_diag_df <- do.call(cbind, all_diag_rets)
    return(combined_diag_df)
  }

  # now we aggregate the labels.
  labels_ <- list()
  for (i in seq_along(diagnostics_l)) {
    labels_i <- attr(diagnostics_l[[i]], "labels")
    # Not necessary, but might as well clear the indiv attributes
    # since diagnostics_l will stay in memory and we've processed them.
    attr(diagnostics_l[[i]], "labels") <- NULL
    # This works whether labels_i is NULL or not.
    labels_ <- c(labels_, labels_i)
  }

  if (length(labels_) == 0) {
    # this means no individual "labels" attribute exists
    labels_ <- list(NULL)
  }

  attr(combined, "labels") <- labels_

  return(combined)
}


process_diag_label <- function(diagnostic) {
  labels_ <- attr(diagnostic, "labels")
  if (is.null(labels_)) {
    # apply a default
    attr(diagnostic, "labels") <- list(NULL)
  }
  return(diagnostic)
}


# Performs some checks but the main purpose of this function is that we accept a *list* of diagnostics
# It combines a list of diagnostics into one diagnostic (and handles the labels attribute appropriately)
# This function inputs either a function or list of functions; and outputs a function.
# This way, internally, we can always assume a function.
process_diagnostic_arg <- function(diagnostics) {
  if (is.function(diagnostics)) {
    diagnostics <- process_diag_label(diagnostics)
    return(diagnostics)
  }

  error_msg <- "Argument 'diagnostics' must be a function or a list of functions."
  if (!is.list(diagnostics)) {
    stop(error_msg)
  } else {
    is_function <- sapply(X = diagnostics, FUN = is.function)
    if (any(!is_function)) {
      stop(error_msg)
    }
  }

  combined_diag <- diagnostics_combine(diagnostics)


  return(combined_diag)
}


mean_na_rm <- function(x) mean(x, na.rm = TRUE)


mc_has_na <- function(mc) {
  stats_m_l <- get_all_mc_stats_m(mc)

  stats_m_has_NA <- function(one_stats_m) {
    row_has_NA <- apply(X = one_stats_m, MARGIN = 1, FUN = function(row_) any(is.na(row_)))
    return(any(row_has_NA))
  }

  any_NA_by_stats_m <- sapply(stats_m_l, FUN = stats_m_has_NA)
  return(any(any_NA_by_stats_m))
}


check_diag_ret <- function(stats_m, diag_ret) {
  if (!is.data.frame(diag_ret) && !is.matrix(diag_ret)) {
    # todo: what about a named vector? can we share some of the code we use to convert statistic ret for here?
    warning("Each diagnostic function should return a data frame or matrix.")
  }

  if (!identical(nrow(diag_ret), nrow(stats_m))) {
    stop("It is expected that each diagnostic returns a data.frame or matrix with the same number of rows as the stats matrix (the input to diagnostics). See the argument 'aggregators' (default of mean) for how the individual diagnostic rows are aggregated into summaries.")
  }
}


check_diags_df <- function(diags_df) {
  empty_names_idx <- names(diags_df) == ""
  if (any(empty_names_idx)) {
    warning("After the return from the 'diagnostic' function was converted to a data frame, there are columns with empty names. It's recommended for each diagnostic to return a data frame with non-empty names for columns.")
    # we use some generic names just to proceed as best we can.
    names(diags_df)[empty_names_idx] <- paste0("diagnostic", which(empty_names_idx))
  }

  cnames <- colnames(diags_df)
  if (anyDuplicated(cnames)) {
    stop("The return from the diagnostics has duplicate(s) in the column names: ",
         paste(cnames, collapse = ", "))
  }
}


# TODO: rename. "mc" should be "pnch" for "pn-chunk".
# TODO: also, this function should have "diags" in the name, right?
process_one_mc <- function(sims_one_mc_l, n, ptup, diagnostics, aggregators) {
  #    the_stats_l <- lapply(sims_one_mc_l[["mc"]], `[[`, "stat")
  # TODO: we're currently ignoring sims_one_mc_l$sims_l$aux
  stats_m <- sims_one_mc_l[["stats_m"]]

  # We don't give the first arg a name on purpose. that way robust to
  # misspellings. If had to give a name, would give it "stacked_stats_m".
  diag_ret <- diagnostics(stats_m, true_poi = ptup[["true_poi"]])
  check_diag_ret(stats_m = stats_m, diag_ret = diag_ret)
  diags_df <- data.frame(diag_ret,
                         # otherwise if input is a vector, the name of the
                         # column will be the object name of the vector in
                         # the arg to data.frame ('diag_ret' in this case).
                         # This can produce a data.frame with empty names,
                         # but we check for this and give warning below.
                         fix.empty.names = FALSE,
                         # this allows a user's statistic() to return column names
                         # with LaTeX in them, like '$M_{CB}(X_{m_{1}})$'.
                         #
                         # todo: look into which functions rely on the names
                         # of a data.frame being strictly checked.
                         #
                         # todo: make tests that use LaTeX in names. i.e., the
                         # tests should fail if this is changed to TRUE.
                         # @sk: see the simulations in frcbstats paper
                         check.names = FALSE
  )
  check_diags_df(diags_df = diags_df)

  statnames_ <- unique(rownames(stats_m))
  aggregated <- diags_aggregator(diags_stacked_df = diags_df, aggregators = aggregators, statnames = statnames_)

  return(aggregated)

# EXPIRE {this was old, before I moved to using a data.frame for sim results}
#    # could potentially parallelize over this. If so,
#    # turn off parallelization in param_nobs_apply.
##    diags_foreach_l <- mclapply(X = sims_one_mc_l[["sims_l"]], FUN = diagnostics, true_poi = ptup[["true_poi"]])
#    diags_foreach_l <- lapply(X = sims_one_mc_l[["sims_l"]], FUN = diagnostics, true_poi = ptup[["true_poi"]])
#    oneret <- diags_aggregator(diags_foreach_l, aggregators = aggregators)
#    return(oneret)
}


# TODO: link "pn-pair" to documentation.

# on the other hand.... it makes sense that diags requires the parameter and this makes it clear
#     if diags does not require it in the function and estimator does not, it's not as clear that estimator doesn't have access to it.
#
# if diagnostics is NULL, will get diagnostics from sims_l (if mc_run specified the diags)
# TODO: link "pn-pair" to central documentation on "mc_pnpair"

#' @title Run diagnostics on result of Monte Carlo simulations
#'
#' @eval param_mc()
#' @param diagnostics See '?mc_diagnostics' for information. Each diagnostics in the list will be run for each statistic for every pn-pair.
#' @param aggregators a list of functions to aggregate the simulation results. Typically this is left at the default of using the mean or changed to the median by specifying list(median = median).
#'        By default (if aggregators is NULL), aggregators will report 'diag_prop_NA'.
#' @eval param_verbose()
#'
#' @description Applies diagnostic and aggregates the results.
#'
#' @export
mc_diags <- function(mc, diagnostics = NULL, aggregators = NULL, verbose = 1) {
  # for internal variable name purposes:
  # diagnostics_arg might be a function or a list of functions, but the
  # internal diagnostics is always one function.
  diagnostics_arg <- diagnostics
  rm(diagnostics)

  if (is.null(diagnostics_arg)) {
    if (is.null(attr(mc, "diagnostics"))) {
      stop("The argument 'diagnostics' must be specified in the call to mc_diags or in the call to mc_run that produced the mc you pass to mc_diags. See examples section.")
    } else {
      diagnostics <- attr(mc, "diagnostics")
    }
  } else {
    diagnostics <- process_diagnostic_arg(diagnostics_arg)
  }

  mc <- resolve_user_mc_arg(mc, verbose = verbose)


  if (is.null(aggregators)) {
    if (mc_has_na(mc)) {
      aggregators <- list(mean = mean_na_rm)
      # infinite recursion:
      # diagnostics <- diagnostics_combine(list(diagnostics, diag_prop_NA))
      diag_user <- diagnostics
      diagnostics <- diagnostics_combine(list(diag_user, diag_prop_NA))
    } else {
      aggregators <- list(mean = mean)
    }
  }


  # todo: we used to give an argument "dgpp_to_poi" to mc_diags() but I guess
  # now we just require it to be specified in mc_run() ? Clarify this. I can
  # imagine a use case where user wants to use a different dgpp_to_poi, e.g.,
  # to improve approximation after simulations are run. But for this use case,
  # butter just to overwrite dgpp_to_poi *inside* MC object and rerun it. Make
  # a convenience function for that?
  dgpp_to_poi <- get_attr(mc, "dgpp_to_poi")


  # We can run this in parallel because we can parallelize across nobs and params because should be mostly balanced.
  # ^^ well, it depends on nsims. If nsims is not balanced, this might not be ideal parameterization.
  # todo: shouldn't hardcode parallel.
  # This makes a small absolute difference but considerable relative difference. Would need to test with very large (e.g., 100k) nsims to see if it is worth documenting and exposing to user.
  parallel_ <- FALSE
  # param_nobs_apply copies attributes from mc to the return.
  myret <- param_nobs_apply(X = mc, FUN = process_one_mc, parallel = parallel_, diagnostics = diagnostics, aggregators = aggregators)
  # At this point, myret is not a "montecarlo" object, or a "mcdiags" object (since it doesn't have
  # the 'labels' attribute). We unclass it, otherwise print.montecarlo() will be called when
  # debugging, which produces strange results.
  myret <- unclass(myret)
  myret <- transfer_labels_from_diag_to_diag_result(myret, diagnostics = diagnostics, aggregators = aggregators)
  class(myret) <- "mcdiags"
  return(myret)
}


#' @export
all.equal.mcdiags <- function(target, current, tolerance = sqrt(.Machine$double.eps), verbose = 1, ...) {
  validate_mcdiags(target)
  validate_mcdiags(current)

  pairs_ <- get_pn_pairs(target,
                         # ordering should not affect outcome. (todo: document this)
                         sortby = "phash")
  if (!identical(pairs_, get_pn_pairs(current, sortby = "phash"))) {
    if (verbose >= 1) {
      message("pn pairs are not the same.")
    }
    return(FALSE)
  }

  is_all_eq <- TRUE
  for (pn in pairs_) {
    if (!isTRUE(all.equal(
                          get_pn_diags_result(target, pn),
                          get_pn_diags_result(current, pn),
                          tolerance = tolerance))) {
      if (verbose >= 1) {
        message("The stats_m of pair '", pn, "' is not equal.")
      }
      is_all_eq <- FALSE
    }
  }

  return(is_all_eq)
}


mcdiags_results_apply <- function(mcdiags, FUN) {
  validate_mcdiags(mcdiags)

  FUN_outer <- FUN
  lapply(X = get_pn_pairs(mcdiags), FUN = function(pn) {
    return(FUN_outer(get_pn_diags_result(mcdiags, pn)))
  })
}


#' @export
print.mcdiags <- function(x, format = "plain", ...) {
  # for efficiency, comment out since this check is inside
  # the call to apply_diag_labels_to_colnames() just below
  # validate_mcdiags(x)

  print_mcdiags_table(x)

  # EXPIRE: the below was before we implemented printing of
  #         plain text tables.
  #
  # TODO: is this still relevant? If so, document an example
  #       where it makes a difference.
  #x <- apply_diag_labels_to_colnames(x, format = format)
  #
  #for (pn in get_pn_pairs(x)) {
  #  cat(pn_to_label_str(mcx = x, pn_pair = pn), "\n")
  #  pn_res <- get_pn_diags_result(x, pn)
  #  attr(pn_res, "nsims") <- NULL
  #  print(pn_res)
  #  cat("\n")
  #}
}


combine_labels_from_diags <- function(diags_l) {
  l_ <- lapply(diags_l, FUN = function(diag) attr(diag, "labels"))
  # TODO: remove the "NULLs" or keep them?
}


transfer_labels_from_diag_to_diag_result <- function(diags_ret, diagnostics, aggregators) {
  labels_diag_all_aggs <- attr(diagnostics, "labels")

  # todo: mc_diags() aggregates, so we could preserve only those labels.
  #       but what about "indiv" diag? I think we could remove that also.
  #       (this todo is the reason we accept 'aggregators' argument but don't
  #       use it yet).
  attr(diags_ret, "labels") <- labels_diag_all_aggs
  return(diags_ret)
}


apply_diag_labels <- function(orig_names, diag_labels, format, aggregators) {
  if (length(aggregators) > 1) {
    stop("Currently we don't handle more than one aggregator.")
  }
  remove_these <- sapply(diag_labels, is.null)
  label_candidates_l <- diag_labels[!remove_these]
  label_candidates <- sapply(label_candidates_l, names)

  new_names <- orig_names
  for (i in seq_along(orig_names)) {
    orig_name <- orig_names[[i]]
    labels_this_diag <- label_candidates_l[[orig_name]]
    if (is.null(labels_this_diag)) {
      # The diagnostic just didn't have a label attribute.
    } else {
      labels_this_diag_agg <- labels_this_diag[[aggregators]]
      ideal_if_exists <- labels_this_diag_agg[[format]]
      if (!is.null(ideal_if_exists)) {
        new_names[[i]] <- ideal_if_exists
      }
    }
  }
  return(new_names)
}


validate_mcdiags <- function(mcdiags) {
  stopifnot(is.list(mcdiags))
  stopifnot(is.mcdiags(mcdiags))

  mc_attributes_required
  # "diagnostic" is an optional attribute. If given to mc_run(), it will be passed as
  # an attribute. But if only given directly to mc_diags(), currently mc_diags() has
  # no use for appending the attribute so it is not present.
  attributes_required <- c(mc_attributes_required(), "labels")
  attributes_mcdiags <- names(attributes(mcdiags))
  if (!all(attributes_required %in% attributes_mcdiags)) {
    stop("Not all required attributes present.")
  }

  return(NULL)
}


apply_diag_labels_to_colnames <- function(mcdiags, format) {
  validate_mcdiags(mcdiags)

  diag_labels <- get_attr(mcdiags, "labels")

  f <- function(x) {
    # TODO: assumes just one aggregator
    x_colnames <- colnames(x[[1]])
    aggregators <- names(x)
    if (length(aggregators) > 1) {
      stop("we only handle one aggregator for now")
    }
    cn_subbed <- apply_diag_labels(orig_names = x_colnames, diag_labels = diag_labels, aggregators = aggregators, format = format)
    colnames(x[[1]]) <- cn_subbed
    return(x)
  }


  # we don't use mcx_mcx_apply() because that does not preserve attributes.
  pairs_ <- get_pn_pairs(mcdiags)
  for (pair in pairs_) {
    p_ <- pair[["p"]]
    n_ <- pair[["n"]]
    # this does seem to preserve attributes
    mcdiags[[p_]][[n_]] <- f(mcdiags[[p_]][[n_]])
  }
  return(mcdiags)
}


# this is just to centralize a function used in print.montecarlo()
# and in print.mcdiags()
print_mcdiags_table <- function(x, print_param_col = NA) {
  # instead of having print_param_col arg here, we could put it in
  # mc_table(), but mc_table() already has a lot of arguments.
  if (!is.na(print_param_col) && print_param_col == "always") {
    print(mc_table(x, colname_poi = "param."), row.names = FALSE)
  } else {
    print(mc_table(x), row.names = FALSE)
  }
}
