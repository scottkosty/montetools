param_mc <- function() {
  # todo: for param_mc: put that it can be a file name of a saved .Rds MC (?)
  # first, need to audit that resolve_user_mc_arg() is indeed used.
  c(
    "@param mc An MC object (e.g., created from mc_run)."
  )
}


param_diags_or_mc <- function() {
  # todo: document if it can be a filename. e.g., as used in mc_table().
  c(
    "@param diags_or_mc The return from \\code{\\link{mc_diags}}. If instead you pass an MC object, the diagnostics will be run automatically if you specified the argument 'diagnostics' to mc_run()."
  )
}


# diags_or_mc is for when the MC object will be coerced to a diags object.
# mcx below is for when no coercion is done.
param_mcx <- function() {
  c(
    "@param mcx A \"montecarlo\" object (e.g., created from mc_run), or an \"mcdiags\" object (e.g., created from mc_diags)."
  )
}


param_restore_globals <- function() {
  c(
    "@param restore_globals logical for whether to restore the globals that were captured (if the argument 'store_globals' was TRUE) by mc_run()."
  )
}


param_verbose <- function() {
  c(
    "@param verbose An integer (default of 1) to control the level feedback. Set to 0 to turn off messages that are not warnings or errors. At value 1, basic progress information is printed; at values 2 and higher, increasingly more messages are printed that are useful when debugging an unexpected result."
  )
}


get_phash <- function(pn_pair) {
  pn_pair[["p"]]
}


get_n_as_int <- function(pn_pair) {
  as.integer(pn_pair[["n"]])
}


get_time_finished <- function(mc, pn_pair) {
  p_ <- pn_pair[["p"]]
  n_ <- pn_pair[["n"]]
  get_attr(mc[[p_]][[n_]], "sys_stats")[["time_finished"]]
}


# We do not assume all combinations are present (might be partial results), we don't use e.g., expand.grid().
# assumes nobs_is_outer is FALSE to keep it simple. We could create a helper nobs_is_outer() that returns
# TRUE or FALSE, but for now we just keep it simple.
#
#' Get list of pn pairs from a Monte Carlo object
#
#' @export
#' @eval param_mc()
#' @param sortby The method to sort by, one of "none", "phash", "nobs", "time_finished".
get_pn_pairs <- function(mc, sortby = "none") {

  if (!(sortby %in% c("none", "phash", "nobs", "time_finished"))) {
    stop("Incorrect 'sortby' argument.")
  }

  ret <- list()
  for (p in names(mc)) {
    for (n in names(mc[[p]])) {
      pair_ <- structure(c(p = p, n = n), class = "pnpair")
      ret <- c(ret, list(pair_))
    }
  }

  if (sortby != "none") {
    phash_vec <- sapply(ret, get_phash)
    nobs_vec <- sapply(ret, get_n_as_int)
    if (sortby == "phash") {
      order_ <- order(phash_vec, nobs_vec)
    } else if (sortby == "nobs") {
      order_ <- order(nobs_vec, phash_vec)
    } else if (sortby == "time_finished") {
      time_finished_vec <- sapply(ret, FUN = function(pair_) get_time_finished(mc = mc, pn_pair = pair_))
      order_ <- order(as.numeric(time_finished_vec))
    }

    ret <- ret[order_]
  }

  return(ret)
}


# useful in hooks
#' @export
#' @title Get the most recently run pn-pair of an MC
#' @description Get the most recently run pn-pair of an MC. This function is useful in MC hooks.
#' @eval param_mc()
get_pn_pair_most_recent <- function(mc) {
  pn_pairs <- get_pn_pairs(mc, sortby = "time_finished")
  pn_most_recent <- pn_pairs[[length(pn_pairs)]]
  return(pn_most_recent)
}


#' @export
#' @title Nicely format a pn-pair
#' @description Nicely format a pn-pair
#' @param pair The pn-pair
pn_to_str <- function(pair) {
  paste0("(", pair[["p"]], ", ", pair[["n"]], ")")
}


# This function is called by pn_to_label_str(), which is in turn called by both
# print.montecarlo() and print.mcdiags(). It is also called directly by
# hook_progress_message(), which prefixes "processing" and appends "...".
#
# todo: can "p_label" either be "POI label" or "DGPP label"?
#' @importFrom glue glue
format_pn_label_str <- function(p_label, n, nsims = NA) {
  npart <- paste0("n = ", n)
  if (is.na(p_label)) {
    # e.g., only one parameter
    ret <- npart
  } else {

    # Alternative: could exclude wrapping in quotes. User could always choose to
    #              wrap in quotes by embedding nested quotes in the label string.
    #
    # we wrap it in quotes (i.e., \""), because it makes it especially
    # easier to parse when the label itself is a comma, which is how
    # the fallback mechanism converts vector-parameters.
    # Alternative, could wrap in '<' and '>', or '<<' and '>>'.
    #
    # EXPIRE (used to be "POI <poi_label>", then I considered "parameter <poi_label>", but
    # now I'm thinking just "<poi_label>"
    # msg_poi_label_part <- paste0("parameter \"", poi_label, "\", ")
    #
    # At some point, tried just omitting a prefix. i.e., just "{p_label}", but
    # didn't feel right.
    #
    #
    p_label_part <- glue("param = \"{p_label}\"")
    ret <- paste0(p_label_part, ", ", npart)
  }

  if (!is.na(nsims)) {
    ret <- glue(ret, ", nsims = {nsims}")
  }
  return(ret)
}


pn_to_label_str <- function(mcx, pn_pair) {
  param_tups <- get_param_tuples(mcx)
  p_ <- pn_pair[["p"]]
  nsims <- get_nsims(mcx = mcx, pn_pair = pn_pair)
  p_label <- param_tups[[p_]][["label"]]
  format_pn_label_str(p_label = p_label, n = pn_pair[["n"]], nsims = nsims)
}


#' @export
print.pnpair <- function(x, ...) {
  # (https://stackoverflow.com/questions/36699272/why-is-message-a-better-choice-than-print-in-r-for-writing-a-package)
  cat(pn_to_str(x), "\n")
}


# This is mainly so that we can use it in "message()"
#' @export
as.character.pnpair <- function(x, ...) {
  pn_to_str(x)
}


#' Get the stats matrix of a 'montecarlo' object.
#'
#' @export
#' @eval param_mc()
#' @param pn_pair The pn-pair of the MC object to get the stats matrix of.
mc_stats_m <- function(mc, pn_pair) {
  mc[[pn_pair[["p"]]]][[pn_pair[["n"]]]][["stats_m"]]
}


# I'm not sure we need to export this. Let's keep stats_m as "read-only" in the interface
# (the user can easily work around this but at least we signal we're not sure why user wants
# to write directly to the stats). If a user wants to write to stats_m, probably there is a
# helper function that should be written. e.g., like the subset function we have in manipulations.R
`mc_stats_m<-` <- function(mc, pn_pair, value) {
  mc[[pn_pair[["p"]]]][[pn_pair[["n"]]]][["stats_m"]] <- value
  mc
}


get_all_mc_stats_m <- function(mc) {
  pnpairs <- get_pn_pairs(mc)
  lapply(pnpairs, FUN = function(pn) mc_stats_m(mc, pn_pair = pn))
}


get_stacked_stats_m <- function(mc) {
  pnpairs <- get_pn_pairs(mc)
  ptups <- get_param_tuples(mc)

  # We now loop through and make (1) a col with "n", (2) a col with "dgpp",
  # and (3) a col with "poi".
  # todo: this code could be made more elegant
  first <- TRUE
  for (pn in pnpairs) {
    stats_m_one <- mc_stats_m(mc, pn_pair = pn)
    # will reassign these row names as a *column* in the data.frame. Can't have dup row names.
    rownames_vec <- rownames(stats_m_one)
    rownames(stats_m_one) <- NULL
    stats_m_one_df <- as.data.frame(stats_m_one)
    if (is.null(colnames(stats_m_one))) {
      if (ncol(stats_m_one_df) > 1) {
        stop("need to fix this for higher-dimensional stats")
      } else {
        # TODO: this string "value" is relied on in mc_plot_density().
        #       Make more robust, e.g., store "value" in a global var?
        #       Similar for "dgpp_label" and "poi" creation below.
        names(stats_m_one_df) <- "value"
      }
    }
    stats_m_one_df$statname <- rownames_vec
    stats_m_one_df$n <- pn[["n"]]

    ptup <- ptups[[pn[["p"]]]]
    stats_m_one_df$dgpp_label <- ptup$label
    stats_m_one_df$poi <- ptup$true_poi

    if (first) {
      first <- FALSE
      ret <- stats_m_one_df
    } else {
      ret <- rbind(ret, stats_m_one_df)
    }
  }
  return(ret)
}


mc_mc_apply_to_all_stats_m <- function(mc, FUN) {
  pnpairs <- get_pn_pairs(mc)
  for (pn in pnpairs) {
    mc_stats_m(mc, pn_pair = pn) <- FUN(mc_stats_m(mc, pn_pair = pn))
  }
  return(mc)
}


#' @export
#' @title Get diagnostics result of a pn-pair
#' @description Get diagnostics result of a pn-pair
#' @eval param_mc()
#' @param pn_pair The pn_pair of the MC.
get_pn_diags_result <- function(mc, pn_pair) {
  res <- mc[[pn_pair[["p"]]]][[pn_pair[["n"]]]]
  # TODO: if we don't strip the non-names attributes then need to modify all.equal.mcdiags to ignore these. TODO: yes, this is probably better.
  attr(res, "sys_stats") <- NULL
  attr(res, "rnginfo_before") <- NULL
  attr(res, "rnginfo_after") <- NULL
  attr(res, "seed_first") <- NULL
  attr(res, "seed_last") <- NULL

  return(res)
}


# todo: make tests? yes, if I use outside of comparison.R
mc_pnpairs_subset <- function(mc, pn_pairs) {
  # faster to NULL out the other ones,
  # or add the others?
  ret <- mc
  for (p in names(ret)) {
    for (n in names(ret[[p]])) {
      if (!list(c(p = p, n = n)) %in% pn_pairs) {
        ret[[p]][[n]] <- NULL
      }
    }
  }
  # now wipe out all outer elments that are empty
  for (p in names(ret)) {
    if (length(ret[[p]]) == 0) {
      ret[[p]] <- NULL
    }
  }
  return(ret)
}


#' @export
#' @title Get number of simulations of a pn-pair
#' @description Get number of simulations of a pn-pair for an MC object or mcdiags object.
#' @eval param_mcx()
#' @param pn_pair The pn_pair of the MC or mcdiags.
get_nsims <- function(mcx, pn_pair) {

  if (is.mcdiags(mcx)) {
    nsims <- get_attr(mcx[[pn_pair[["p"]]]][[pn_pair[["n"]]]], "nsims")
    return(nsims)
  }

  nrow_stats_m <- nrow(mc_stats_m(mcx, pn_pair))
  if (nrow_stats_m == 0) {
    # this handles the case when nsims = 0.
    # We return here because otherwise nstats is 0
    # and 0/0 is Inf.
    return(0)
  }

  nstats <- get_nstats(mcx)
  nsims <- nrow_stats_m/nstats
  stopifnot(nsims > 0)
  return(nsims)
}


#' @export
#' @title Get number of simulations for an MC
#' @description Get number of simulations for each pn pair of an MC
#' @eval param_mcx()
get_nsims_vec <- function(mcx) {
  # TODO: maybe create a class "mcx". i.e., "montecarlo" AND
  #       "mcdiags" would also be "mcx" objects.
  # todo: keep these loops separate or combine
  # and reduce duplication?
  if (is.mcdiags(mcx)) {
    nsims_vec <- c()
    for (p_idx in seq_along(mcx)) {
      for (n_idx in seq_along(mcx[[p_idx]])) {
        nsims_ <- attr(mcx[[p_idx]][[n_idx]], "nsims")
        nsims_vec <- c(nsims_vec, nsims_)
      }
    }
    return(nsims_vec)
  }

  p_ <- names(mcx)[[1]]
  nvec <- names(mcx[[p_]])
  ret <- c()
  for (n_ in nvec) {
    nsims_ <- get_nsims(mcx = mcx, pn_pair = c(p = p_, n = n_))
    ret <- c(ret, nsims_)
  }
  return(ret)
}


get_nstats <- function(mc) {
  return(length(get_statnames(mc)))
}


# Is there a method that we should override for this? e.g., names()? hmm maybe not.
#
# example: if nsims is 0, then we return NULL.
get_statnames <- function(mc) {
  # todo: we assume that all pairs have the same stat names!
  # ideally we would relax this assumption.
  # Maybe don't want to run a computationally complicated stat
  # for high nobs.
  # Maybe create an arg that defaults to "assume_balanced = TRUE",
  # so can use efficient methods, but allows to be overriden?
  pn_pairs <- get_pn_pairs(mc)
  rownames_ <- rownames(mc_stats_m(mc, pn_pair = pn_pairs[[1]]))
  first_dup_idx <- anyDuplicated(rownames_)
  if (first_dup_idx == 0) {
    # anyDuplicated(7):       0
    # anyDuplicated(c(7, 4)): 0
    # anyDuplicated(NULL):    0
    # if nisms is 0, we also end up here and ret becomes NULL, which
    # seems reasonable.
    ret <- rownames_
  } else {
    ret <- rownames_[1:(first_dup_idx - 1)]
  }
  # old alternative that is less efficient:
  # This was inefficient using unique on a long vector.
  # instead, above we just use anyDuplicated() to loop through the names and stop when we get
  # to a duplicate.
  #ret_old <- unique(rownames(mc_stats_m(mc, pn_pair = pn_pairs[[1]])))
  #stopifnot(identical(ret, ret_old))

  # This is for the case when nsims = 0 for some pn pairs but not others.
  if (is.null(ret)) {
    # todo: we already handled i = 1 above
    for (i in seq_along(pn_pairs)) {
      ret_alt <- unique(rownames(mc_stats_m(mc, pn_pair = pn_pairs[[i]])))
      if (!is.null(ret_alt)) {
        return(ret_alt)
      }
    }
  }

  if (any(ret == "")) {
    # I had a note in correct_rownames_of_NAs() that such statnames would cause problems,
    # although I'm not sure if this is still true.
    warning("statnames should not be empty")
  }

  return(ret)
}


#' This function can be used to convert from different formats to montetools format.
#' @export
#' @param l The list.
#' @param all_pairs All of the pairs to create.
list_to_mc <- function(l, all_pairs) {
  stopifnot(length(l) == length(all_pairs))

  ret <- create_empty_mc(all_pairs = all_pairs)

  for (i in seq_along(l)) {
    p_ <- all_pairs[[i]][["p"]]
    n_ <- all_pairs[[i]][["n"]]
    ret[[p_]][[n_]] <- l[[i]]
  }
  return(ret)
}


# This function has "dummy contents". i.e., it's expected they will be replaced by
# whichever function is calling it. It can be viewed as a constructor that initializes
# the contents of the double-list elements with garbage.
#   TODO can I just loop through and replace with NULL?
create_empty_mc <- function(all_pairs) {
  # this utility function just creates the elements of the outer list, so that
  # places can use double-indices to replace things with true contents.

  p_vec <- unique(sapply(X = all_pairs, `[[`, 'p'))
  # the *contents* of these list elements will be replaced later in this function.
  ret_outer <- as.list(p_vec)
  names(ret_outer) <- p_vec

  n_vec <- unique(sapply(X = all_pairs, `[[`, 'n'))


  # todo: should this be default way to store?
  all_pairs_m <- do.call(rbind, all_pairs)


  for (p in p_vec) {
    n_for_this_p <- all_pairs_m[all_pairs_m[, "p"] == p, "n"]
    ret_inner <- as.list(n_for_this_p)
    names(ret_inner) <- n_for_this_p
    ret_outer[[p]] <- ret_inner
  }

  return(ret_outer)
}


check_format <- function(mc) {
  object_format <- get_attr(mc, "format")
  version_format <- get_ver_format()
  if (object_format < version_format) {
    stop("This 'montecarlo' object was created by an older version of montetools. Please either run the sims again to create a new 'montecarlo' object or contact the author of montetools to help you update the format of the old object to the new package version. I'm sorry for this inconvience. As this package matures in the future, such changes in format will not cause these types of problems.")
  } else if (object_format > version_format) {
    stop("This 'montecarlo' object was created by a newer version of montetools. Please upgrade to the newest version in order to use this object. I'm sorry for this inconvience. As this package matures in the future, such changes in format will not cause these types of problems.")
  }
}


# this is the format from this present package's code.
get_ver_format <- function() {
  1
}


get_statnames_from_stat <- function(one_stat_ret) {
  # if not null, should just use these row names!
  stopifnot(is.null(rownames(one_stat_ret)))

  cnames <- colnames(one_stat_ret)
  if (length(cnames) == 1 && nrow(one_stat_ret) == 1) {
    return(cnames)
  }

  if (is.atomic(one_stat_ret) && length(one_stat_ret) == 1 && !is.null(names(one_stat_ret))) {
    return(names(one_stat_ret))
  }

  # just generate our own row names, "stat1", "stat2"...
  generic_names <- paste0("stat", seq.int(nrow(one_stat_ret)))
  return(generic_names)
}


get_param_tuple <- function(mcp) {
  get_attr(mcp, "param_tuple")
}


get_param_tuple <- function(mc, phash) {
  # todo: probably there is something more efficient than going through get_param_tuples().
  ptups <- get_param_tuples(mc)
  ptup <- ptups[[phash]]
  return(ptup)
}


get_phashes <- function(mc, sortby) {
  if (!(sortby %in% c("none", "phash"))) {
    stop("For get_phashes(), it doesn't make sense to sort by anything other than 'none', or 'phash'.")
  }

  # microop: could make a more efficient implementation, but this nicely relies
  # on get_pn_pairs(). Could centralize the code that's inside there, but
  # probably not worth it.
  pn_pairs <- get_pn_pairs(mc, sortby = sortby)
  phashes <- sapply(pn_pairs, `[[`, 1)
  phashes_unique <- unique(phashes)
  return(phashes_unique)
}


get_param_tuples <- function(mc, sortby = "none") {

  ptups <- lapply(mc, FUN = get_attr, "param_tuple")
  phashes_sorted <- get_phashes(mc, sortby = sortby)
  stopifnot(identical(length(phashes_sorted), length(ptups)))
  ptups_sorted <- ptups[phashes_sorted]
  return(ptups_sorted)
}


# should be used for exported functions. Allows user to specify
# MC as an object or a string of a file name of a .Rds file.
resolve_user_mc_arg <- function(mc, verbose) {
  if (is.character(mc)) {
    if (!file.exists(mc)) {
      stop("'mc' argument is a string, but file not found.")
    }
    if (verbose >= 2) message("Reading in .Rds file ", mc, "...")
    ret <- readRDS(mc)
    if (verbose >= 2) message("Done.")
  } else {
    validate_montecarlo(mc)
    ret <- mc
  }
  return(ret)
}


get_dgp_params <- function(mc) {
  ptups <- lapply(mc, FUN = get_attr, "param_tuple")
  dgpps <- lapply(ptups, FUN = `[[`, "dgpp")
  return(dgpps)
}


# assumes a "balanced MC"
get_nvec <- function(mc) {
  names(mc[[1]])
}


get_dgpp_to_poi <- function(mc) {
  get_attr(mc, "dgpp_to_poi", error_if_null = FALSE)
}


get_statistic <- function(mc) {
  get_attr(mc, "statistic")
}


get_stat_knowledge <- function(mc) {
  # TODO: instead of having stat_knowledge NULL, can we make it an empty vector?
  # if so, remove error_if_null = FALSE.
  get_attr(mc, "stat_knowledge", error_if_null = FALSE)
}


get_dgp <- function(mc) {
  get_attr(mc, "dgp")
}


# 'diagnostics' is not strictly a necessary MC arg for running mc_run(), but it is considered
# a 'core' arg because it is necessary for reproducing the final MC table.
#' @export
#' @title Get core MC arguments.
#' @description Get core MC arguments, e.g., as pass to mc_run().
#' @eval param_mc()
get_core_mc_args <- function(mc) {
  ret_l <- list(
    dgp_params = get_dgp_params(mc),
    # should get_nvec return integer or character? not sure
    nvec = as.integer(get_nvec(mc)),
    dgp = get_dgp(mc),
    dgpp_to_poi = get_dgpp_to_poi(mc),
    statistics = get_statistic(mc),
    nsims = get_nsims_vec(mc),
    stat_knowledge = get_stat_knowledge(mc)
  )
  diagnostics <- attr(mc, "diagnostics")
  if (!is.null(diagnostics)) {
    ret_l <- c(ret_l, list(diagnostics = diagnostics))
  }
  return(ret_l)
}


#' Extract only the first 'n' sims of each pn-pair
#'
#' Useful to see if can reproduce just the first, e.g., 1, sim of each pn-pair.
#'
#' @eval param_mc()
#' @param n The number of simulations to limit the MC to.
#' @export
mc_first_n_sims <- function(mc, n) {
  mcbu <- mc
  nstats <- get_nstats(mc)
  # we will limit to this number of rows in stat_m.
  nrows_subset <- nstats * n
  for (pn in get_pn_pairs(mc)) {
    # drop = FALSE to preserve rownames (statnames) and type.
    mc_stats_m(mc, pn) <- mc_stats_m(mc, pn)[1:nrows_subset, , drop = FALSE]
  }
  return(mc)
}


get_rnginfo_before <- function(mc, pn_pair) {
  p_ <- pn_pair[["p"]]
  n_ <- pn_pair[["n"]]
  get_attr(mc[[p_]][[n_]], "rnginfo_before")
}


get_rnginfo_after <- function(mc, pn_pair) {
  p_ <- pn_pair[["p"]]
  n_ <- pn_pair[["n"]]
  get_attr(mc[[p_]][[n_]], "rnginfo_after")
}


get_dgpp_rnginfo_before <- function(mc, dgpp_next) {
  ptups <- get_param_tuples(mc)
  for (p in ptups) {
    if (isTRUE(all.equal(p[["dgpp"]], dgpp_next))) {
      return(p[["rngstate_before"]])
    }
  }
  stop("'dgpp_next' match not found in mc's dgp_params. Please report this potential bug with an example to reproduce the error.")
}


# mostly used internally for testing.
get_plans <- function(mc) {
  sapply(get_pn_pairs(mc), FUN = function(pn) {
    p_ <- pn[["p"]]
    n_ <- pn[["n"]]
    attr(mc[[p_]][[n_]], "sys_stats")[["future_plan"]]
  })
}


# apply a function to MC or mcdiags and return object of same class.
# microop: could allow parallelization.
#
# Note that this does *not* preserve attributes, or even class.
mcx_mcx_apply <- function(mc, FUN) {
  pairs_ <- get_pn_pairs(mc)
  retmc <- create_empty_mc(all_pairs = pairs_)
  for (pair in pairs_) {
    p_ <- pair[["p"]]
    n_ <- pair[["n"]]
    retmc[[p_]][[n_]] <- FUN(mc[[p_]][[n_]])
  }
  return(retmc)
}


mcx_l_apply_stats_m <- function(mc, FUN) {
  pairs_ <- get_pn_pairs(mc)
  ret_l <- list()
  for (pair in pairs_) {
    p_ <- pair[["p"]]
    n_ <- pair[["n"]]
    fun_ret <- FUN(mc_stats_m(mc, pn_pair = pair))
    ret_l <- c(ret_l, list(fun_ret))
    # TODO: return a double-list or a flattned list? hmmm
    names(ret_l)[[length(ret_l)]] <- paste0(n_, "_", p_)
  }
  return(ret_l)
}


get_before_seeds_from_mc <- function(mc) {
  get_all_mcpn_attr(mc, a = "seed_last")
}


get_all_mcpn_attr <- function(mc, a) {
  fun_ <- function(mcpn) {
    get_attr(mcpn, a)
  }
  mcx_mcx_apply(mc, FUN = fun_)
}


get_nparams <- function(mc) {
  length(mc)
}


get_attach_name <- function() {
  return("montetools-globals-restored")
}


do_attach_globals <- function(mc) {
  globals_l <- get_attr(mc, "globals")

  # micro optimization (no need to attach and set up sys.exit() if empty globals_l)
  if (length(globals_l) == 0 || is.null(globals_l[[1]])) {
    return(FALSE)
  }

  # todo: in strikes_ml, this gives error for "infer_col_order".
  #       need to improve detection first.
  #for (glob in names(globals_l)) {
  #  if (exists(glob)) {
  #    stop("Following global already exists: ", glob)
  #  }
  #}

  attach_name <- get_attach_name()
  # todo: ask for an alternative to using attach()? Can I do the following using
  #       with() instead?
  attach(globals_l, pos = 2, name = attach_name)
  # the following use of on.exit() is documented in ?attach and is good practice.
  on_parent_exit({
    globals_are_attached <- "montetools-globals-restored" %in% search()
    if (globals_are_attached) {
      # The reason for conditioning on globals_are_attached is that I prefer
      # to detach as soon as possible so we have a separate detach() command
      # just after the mc_run()). So this hook should only be called if there
      # was an error during the mc_run() call below.
      detach(name = attach_name, character.only = TRUE)
    }
  }, add = TRUE)
  return(TRUE)
}


do_detach_globals <- function() {
  attach_name <- get_attach_name()
  detach(name = attach_name, character.only = TRUE)
}


correct_NA_chunks_ACROSS_chunks <- function(mc) {
  rownames_l <- mcx_l_apply_stats_m(mc, FUN = function(stats_m) rownames(stats_m))
  stats_m_lens <- sapply(rownames_l, FUN = length)
  max_idx <- which.max(stats_m_lens)
  # TODO: get rid of max_ and just rely on max_idx

  # microop quick return
  max_ <- stats_m_lens[[max_idx]]
  if (all(stats_m_lens == max_)) {
    return(mc)
  }

  stats_m_expand_NAs <- function(stats_m) {
    # TODO: this assumes that all pn-chunks have same number of stats.
    #       need to redesign this when we allow for different pn-chunks
    #       do have different stats.
    if (ncol(stats_m) == 1 && all(is.na(stats_m[, 1]))) {
      # TODO: ncol 1 (as currently below) or copy ncol from a "good" run also?
      NA_correct_correct_length_stats_m <- matrix(NA, nrow = max_, ncol = 1)
      rownames(NA_correct_correct_length_stats_m) <- rownames_l[[max_idx]]
      return(NA_correct_correct_length_stats_m)
    }
    return(stats_m)
  }

  mc_corrected <- mc_mc_apply_to_all_stats_m(mc = mc, FUN = stats_m_expand_NAs)

  return(mc_corrected)
}


globals_were_stored <- function(mc) {
  return(!identical(get_attr(mc, "globals"), list(NULL)))
}


resolve_diags_or_mc_arg <- function(diags_or_mc, verbose = 1) {
  # todo: share this code with resolve_user_mc_arg() ?
  if (is.character(diags_or_mc)) {
    if (!file.exists(diags_or_mc)) {
      stop("'diags_or_mc' argument is a string, but file not found.")
    }
    if (verbose >= 2) message("Reading in .Rds file ", diags_or_mc, "...")
    diags_or_mc <- readRDS(diags_or_mc)
    if (verbose >= 2) message("Done.")
  }

  if (inherits(diags_or_mc, "mcdiags")) {
    diags_result <- diags_or_mc
    # nothing to do
  } else if (inherits(diags_or_mc, "montecarlo")) {
    if (is.null(attr(diags_or_mc, "diagnostics"))) {
      stop("You must give the argument 'diagnostics' to mc_run(), or use mc_diags() on the return of mc_run().")
    } else {
      diags_result <- mc_diags(diags_or_mc)
    }
  } else {
    # This assumes that the user argument is called "diags_result".
    # Could alternatively look at the system call to figure it out,
    # but might as well use the same argument name.
    # Could use a global name and share it with param_diags_or_mc().
    stop("'diags_or_mc' must be the return from mc_diags(), or alternatively the return from mc_run() if you specified the 'diagnostics' argument.")
  }
}


# todo: in theory could try to use the base "labels" S3 function. That way, users would just have to supply a method if their
# param is a special class.
gen_label <- function(poi_ret, dgpp_label, i, verbose) {
  if (!is.null(dgpp_label)) {
    return(dgpp_label)
  }

  # 5 is arbitrary, but vectors that are too long
  # would be difficult to read. If the user prefers
  # a long label, they can set it manually.
  if (is.numeric(poi_ret) && length(poi_ret) <= 5) {
    return(paste(poi_ret, collapse = ", "))
  }

  # We used to give an error like the following:
  #   warning("dgpp_to_poi() is expected to return a list of length 2. What to use for label? convert true_poi to char and use that. But what if true_poi is e.g., a function?")
  # but:
  # (1) we might reach this point if user does not supply dgpp_to_poi function. i.e., the default identity function.
  # (2) also, what if dgp_params only has length 1? We don't really need a label...
  # (3) finally, we don't want to concern the user too much about the label. Things
  #     will still work without it. It is more of an "intermediate" feature, so let them
  #     get a handle on the basics first.
  #
  if (verbose >= 1) {
    # this way, the message is only shown once, and only if length(dgp_params) >= 2.
    # if there is only one dgp param, then doesn't make sense to worry about a label.
    # We still set one here, but it's not important.
    if (i == 2) {
      message("user did not supply param labels, so generating default ones: param1, param2, ...")
    }
  }
  return(paste0("param", i))
}


# This is called after dgp_params are ensured to be a list (by obj_to_list)
# It is very simple, but the abstraction makes it easier to read, and it's
# possible it can get more complicated at some point.
get_dgpp_labels <- function(dgp_params_list) {
  return(names(dgp_params_list))
}


# 2 purposes:
# (1) be able to accept several kinds of objects (vectors, data.frames, lists)
# (2) convert these formats to a single internal format: a list with names. e.g.,
#     row names of data.frames should be used as the indiv param names.
dgp_params_to_list <- function(obj) {
  ret <- obj_to_list(obj)

  # The names need to be unique. For one, when we look at emergency file and check
  # if a parameter has already been processed, we compare the names. So I think
  # it makes sense to use the hash of the dgp_param element since that is the definition
  # of a parameter.
  # I thought about using the POI label, but that can get tricky. For example, POI label
  # could depend on the simulation. Also, possible multiple DGP have same POI!
  # TODO: hmmm but eventually will want to subset based on DGP parameter, etc., so maybe
  # require user to choose (unique?) names?
  # Note that the hash only depends on the *contents* of ret, not the names.
  # get the labels from the names before overriding.
  dgpp_labels <- get_dgpp_labels(obj)
  dgpp_hashes <- sapply(X = ret, FUN = calc_hash)
  names(ret) <- dgpp_hashes
  if (!is.null(dgpp_labels)) {
    if (!all(dgpp_labels == dgpp_hashes)) {
      # TODO: this condition is not elegant or intuivitive. If we remove it,
      #       tests that call mc_reproduce() start failing because we call
      #       this function on dgp_params that were already processed so
      #       the hashes are in the names, so we then copy those names to
      #       the attribute, but they're not *really* dgpp_label"s.
      # TODO: instead, should always store the label. Even if
      #       dgpp_labels is NULL. Just store a label like "" or <EMPTY>".
      #       THEN, can check whether that label exists. That would be better,
      #       and more readable, than the current condition.
      #       THEN add a todo to explore whether we are duplicating too much with calling
      #       this again from mc_reproduce(). In theory we should only call it once.
      for (i in seq_along(ret)) {
        attr(ret[[i]], "dgpp_label") <- dgpp_labels[[i]]
      }
    }
  }
  return(ret)
}


#' Reports whether x is a "montecarlo" object
#' @param x An object to test
#' @export
is.montecarlo <- function(x) inherits(x, "montecarlo")


#' Reports whether x is an "mcdiags" object
#' @param x An object to test
#' @export
is.mcdiags <- function(x) inherits(x, "mcdiags")


create_pseudo_mc_from_sims_l <- function(sims_l, pn_pair, attrs) {
  sims_l_formatted <- format_sims_list(sims_l)
  attr(sims_l_formatted, "sys_stats") <- gen_sys_stats(
                                                       # could give this a fake value also, if necessary.
                                                       systemtime = NULL,
                                                       time_finished = Sys.time())
  pmc <- list_to_mc(l = list(sims_l_formatted), all_pairs = list(pn_pair))
  # todo: maybe make a new class, "pseudo-montecarlo" that derives from "montecarlo"?
  #       i.e., signal that it is fake and allow testing that it is fake?
  class(pmc) <- "montecarlo"
  pmc <- apply_attrs_to_obj(obj = pmc, attrs = attrs)
  return(pmc)
}
