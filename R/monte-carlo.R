process_statistics_arg <- function(statistics_arg) {
  # TODO: even if one statistic, put in a list? yes, I guess so.
  #       START WITH THAT!
  # NOOOO make a new stats...
  if (is.function(statistics_arg)) {
    return(statistics_arg)
  } else if (is.list(statistics_arg)) {
    statistic_call_others <- function(dataf, dgp_param_shared) {
      starting_seed <- rng_snapshot()

      agg_ret <- NULL
      # todo: could potentially call the stats in parallel. Not sure of a use case
      #       where having this level of parallelism would make a big difference.
      for (s in seq_along(statistics_arg)) {
        # todo: Reset seed at end or beginning?
        # microop: don't need to restore when s == 1. But then can't run statistics
        #          in parallel.
        restore_rng(starting_seed)
        # don't use the named arg 'dataf = dataf' so that statistic can be list(mean, median), if
        # dgp_param_shared is NULL.
        # There's a test, so if you want, change "dataf" to "dataf = dataf" and a test will fail.
        one_stat_ret <- statistics_arg[[s]](dataf, dgp_param_shared = dgp_param_shared)
        no_names_from_ret <- is.null(names(one_stat_ret)) &&
          # for a matrix, "names" only returns colnames so we check those as well.
          (is.matrix(one_stat_ret) && any(is.null(rownames(one_stat_ret))))
        # convert a (potential) vector to a matrix
        #
        if (!is.matrix(one_stat_ret)) {
          # if we call rbind() on a matrix, it strips the row names (because row names are taken
          # from the 'x' in rbind(x = blah) which is not set.
          one_stat_ret <- rbind(one_stat_ret)
        }
        if (no_names_from_ret) {
          # then we will take the names from the list of statistics (i.e., the names of the arg).
          #
          # todo: what if nrow(one_stat_ret) > 1? does this give the rows different
          #       row names?
          rownames(one_stat_ret) <- names(statistics_arg)[[s]]
        }
        agg_ret <- rbind(agg_ret, one_stat_ret)
      }
      return(agg_ret)
    }
    return(statistic_call_others)
  } else {
    stop("'statistics' arg must be a list of statistics, or a function if one statistic.")
  }
}


run_pnchunk_hooks <- function(hooks, hook_args) {
  hook_rets_l <- list()
  for (i in seq_along(hooks)) {
    accepts_nsims_dispatched_arg <- "nsims_dispatched" %in% formalArgs(hooks[[i]])
    hook_args_i <- hook_args
    if (accepts_nsims_dispatched_arg) {
      hook_args_i <- c(hook_args_i, list(nsims_dispatched = NULL))
    }
    hook_ret <- do.call(hooks[[i]], hook_args_i)
    hook_rets_l <- c(hook_rets_l, list(hook_ret))
  }
  return(hook_rets_l)
}


format_sims_list <- function(list_of_sims) {
  # rename
  sims_l <- list_of_sims
  one_nsims <- length(list_of_sims)

  # This is where we convert to a more efficient format.
  # todo: could we have done this faster if we use e.g., sapply above instead of lapply?
  stats_l <- lapply(sims_l, `[[`, "stat")
  stats_stacked <- do.call(rbind, stats_l)

  first_non_NA_idx <- match(FALSE, is.na(stats_l))
  if (is.na(first_non_NA_idx)) {
    # If all are NA, then just go forward with the first.
    # todo: this variable name is deceiving now though.
    #       Maybe change to "representative_ret" ?
    first_non_NA_idx <- 1
  }

  first_stat_ret <- stats_l[[first_non_NA_idx]]

  if (is.atomic(first_stat_ret) && is.vector(first_stat_ret) && length(first_stat_ret) > 1) {
    len <- length(first_stat_ret)
    warning("It's not recommended for your statistic() to return a vector because it's ",
            "not clear whether your statistic should be treated as a single statistic ",
            "in ", paste0("R^", len), " or ", len, " different statistics (each in R^1). ",
            "It's recommended to return a matrix where the rows are different statistics ",
            "and the number of columns is the dimension of the statistics.")
  }
  # In case I decide on something different (not to always convert):
  # - at the very least, do this if only one column.
  # - might as well do for case where all columns have same type?
  # TODO: give informative error if data.frame has a factor or character in it? tell them to use aux? document this.
  if (is.data.frame(stats_stacked)) {
    stats_stacked <- as.matrix(stats_stacked)
    first_stat_ret <- as.matrix(stats_l[[first_non_NA_idx]])
  }
  if (is.null(rownames(stats_stacked))) {
    snames <- get_statnames_from_stat(first_stat_ret)
    rownames(stats_stacked) <- rep(snames, times = one_nsims)

    # don't remove before, because the col name might be used for the row names (if no row name is set)
    # Without this, the following statistic will cause problems because both the row names and col names will be "blah"
    #   statistics_ <- function(dataf) {
    #     c(blah = mean(dataf))
    #   }
    #
    # We don't want to do this unconditionally. The frcbstats-paper reproduce
    # test, fails for example, because the param (and estimator) are
    # multidimensional.
    if (identical(snames, colnames(stats_stacked))) {
      colnames(stats_stacked) <- NULL
    }
  }

  # Placement: this correction must be before correct_rownames_of_NAs() because
  # that function relies on nrows(stats_stacked) being correct, which is what
  # expand_NA_returns() does.
  stats_stacked <- expand_NA_returns(stats_stacked, stats_l)

  # If the stat_condition_handler turns errors/warnings into NA, it does not know
  # the name to set as the stat name (since the stat name is decided by the
  # statistic itself in the return object). So here, we guess the stat name of the
  # NA element based on the other stat names.
  rownames(stats_stacked) <- correct_rownames_of_NAs(stacked_rownames = rownames(stats_stacked), nsims = one_nsims)

  # these can be anything so we cannot use a more optimized format.
  auxs <- lapply(sims_l, `[[`, "aux")
  # todo: this overwrite is not intuitive. Just use a different name.
  sims_l <- list(stats_m = stats_stacked, aux_l = auxs)
  return(sims_l)
}


gen_sys_stats <- function(systemtime, time_finished) {
  ret_l <-     list(systemtime = systemtime,
                    # available_cores should obviate collecting detect_cores, but just in case...
                    # (https://www.jottr.org/2022/12/05/avoid-detectcores/)
                    detect_cores = parallel::detectCores(),
                    available_cores = parallelly::availableCores(which = "all"),
                    free_connections = parallelly::freeConnections(),
                    mc_cores = getOption("mc.cores"),
                    sys_info = Sys.info(),
                    session_info = sessionInfo(),
                    # useful to know the latest one. see hooks.
                    # use UTC just so don't have to deal with Daylight savings.
                    time_finished = time_finished,
                    future_plan = current_plan(),
                    # set 'test' to FALSE for performance.
                    # todo: add test for this under something like "third-party-interface-tests".
                    future_session_info = capture.output(futureSessionInfo(test = FALSE), type = "message")
                   )
  return(ret_l)
}




# This function is not exported. That way, the statistic has no access
# to global variables, and can only access dgp info through explicit stat_knowledge
# (except of course with tricks).
one_sim <- function(n, dgp_param, dgp, statistic, stat_knowledge) {
  simdata <- dgp(dgp_param = dgp_param, n = n)

  if (is.null(stat_knowledge)) {
    stat_and_aux <- statistic(simdata)
  } else {
    stat_and_aux <- statistic(simdata, dgp_param_shared = dgp_param[stat_knowledge])
  }

  # convenience feature: do not require the user to return aux.
  # Setting it to NA here allows the rest of the code to handle it generally without having to condition each time.
  #
  # a is.list() returns true for a data.frame.
  is_df_or_nonlist <- is.data.frame(stat_and_aux) || !is.list(stat_and_aux)
  if (is_df_or_nonlist) {
    stat_and_aux <- list(stat = stat_and_aux, aux = NA)
  }
  return(stat_and_aux)
}


#' @importFrom methods formalArgs
gen_param_tuples <- function(dgpp_to_poi, dgp_params, nvec, hooks_ba_dgpp_to_poi, seed_initializer, core_seed, verbose) {
  accepts_maxn_arg <- "maxn" %in% formalArgs(dgpp_to_poi)
  # TODO: I haven't documented this to the user yet.  The reason for this
  # feature is that the user might want access to the dgpp_label. e.g., the
  # user might want to create POI label by appending to dgpp_label.
  accepts_dgpp_label_arg <- "dgpp_label" %in% formalArgs(dgpp_to_poi)
# EXPIRE. I used to do the below, but now I want to collect the seed info.
#  if (accepts_maxn_arg) {
#    pois_ <- lapply(X = dgp_params, FUN = dgpp_to_poi, maxn = max(nvec))
#  } else {
#    pois_ <- lapply(X = dgp_params, FUN = dgpp_to_poi)
#  }

  # now that we collect the rng info, it's trickier to think about running things in parallel. We collect the rng info *for each* param. We might add more params on later.
  param_tuples <- list()
  for (i in seq_along(dgp_params)) {
    if (verbose >= 2) message("gen_param_tuples(): Running seed initializer...")
    seed_ <- seed_initializer(base_seed = core_seed, n = n_for_param_tuples(), phash = names(dgp_params)[[i]])
    mc_set_seed(seed_)
    if (verbose >= 2) message("gen_param_tuples(): Running seed initializer... Done.")

    if (verbose >= 2) message("Applying hooks before dgpp_to_poi()...")
    for (hook_idx in seq_along(hooks_ba_dgpp_to_poi)) {
      if (i == 1) {
        dgpp_prev_ <- NULL
      } else {
        dgpp_prev_ <- dgp_params[[i - 1]]
      }
      hook_ret <- hooks_ba_dgpp_to_poi[[hook_idx]](dgpp_prev = dgpp_prev_, dgpp_next = dgp_params[[i]])
    }
    if (verbose >= 2) message("Applying hooks before dgpp_to_poi()... Done.")


    rngstate_before <- rng_snapshot()

    dgp_params_i <- dgp_params[[i]]
    # strip off this attribute just for passing to dgpp_to_poi.
    # The parameter might be an object that should have a fixed
    # set of attributes so I don't want to contaminate it from
    # the user's perspective.
    dgpp_label <- attr(dgp_params_i, "dgpp_label")
    attr(dgp_params_i, "dgpp_label") <- NULL

    dgpp_to_poi_args_l <- list(dgp_params_i)
    if (accepts_maxn_arg) {
      dgpp_to_poi_args_l[["maxn"]] <- max(nvec)
    }
    if (accepts_dgpp_label_arg) {
      dgpp_to_poi_args_l[["dgpp_label"]] <- dgpp_label
    }
    poi_ret <- do.call(dgpp_to_poi, dgpp_to_poi_args_l)
    # this was just for the user
    rm(dgp_params_i)

    rngstate_after <- rng_snapshot()
    is_stochastic <- !identical(rngstate_before, rngstate_after)
    if (!accepts_maxn_arg && is_stochastic && verbose >= 2) {
      message("Your 'dgpp_to_poi' uses randomness. Should it accept and use 'maxn' argument so that the simulated POI converges to the true POI as the number of observations increases? See ?mc_run.")
    }

    # if poi_ret is a list of length 2 we parse as (poi_, label_).
    # But if it is a *vector* of length 2, we parse it as just poi_,
    # e.g., c(beta0, beta1)

    if (is.list(poi_ret) && length(poi_ret) == 2) {
      # This means dgp_to_poi() returned a list of length 2, so we use the
      # second one as the label.
      poi_ <- poi_ret[[1]]
      label_ <- poi_ret[[2]]
    } else {
      # treat the entire return as the parameter.
      poi_ <- poi_ret
      # try to coerce a useful label, since user did not provide one.
      # todo: add unit tests for gen_label.
      label_ <- gen_label(poi_ret,
                          dgpp_label = dgpp_label,
                          i = i,
                          verbose = verbose)
    }
    # todo: change "true_poi" to just "poi" and adapt others? Not sure why, but I prefer
    # simply "poi" here. Not sure if worth the change though.
    one <- list(dgpp = dgp_params[[i]], true_poi = poi_, label = label_, rngstate_before = rngstate_before, rngstate_after = rngstate_after)
    param_tuples <- c(param_tuples, list(one))
  }
  if (verbose >= 2) {
    message("done calling dgpp_to_poi().")
  }

  local({
    label_lengths <- sapply(param_tuples, FUN = function(x) length(x[["label"]]))
    if (!all(label_lengths == 1)) {
      stop("The label returned by dgpp_to_poi() should have length 1.")
    }
  })

  labels <- sapply(param_tuples, `[[`, "label")

  # labels should be unique.
  # We check for duplicate *elements* in dgp_params_to_list().
  if (anyDuplicated(labels) != 0) {
    stop("parameter labels should be unique. There is a duplicate parameter in at least one of the following:", "\n", paste0(labels, collapse = ", "), "\n", "You can set the parameter labels by having your dgpp_to_poi() return a list of length 2, where the first element is the numeric POI and the second element is the label, which can be a string. For more info, see ?mc_run.")
  }

  if (verbose >= 2) message("Applying hooks after dgp_to_poi() all done...")
  for (hook_idx in seq_along(hooks_ba_dgpp_to_poi)) {
    hook_ret <- hooks_ba_dgpp_to_poi[[hook_idx]](dgpp_prev = dgp_params[[length(dgp_params)]], dgpp_next = NULL)
  }
  if (verbose >= 2) message("Applying hooks after dgp_to_poi() all done... Done.")
  return(param_tuples)
}


# todo: why have default arguments here? I think because in some cases this function is called
# directly.
#
# we have mc_run() call do_mc_run() so that we can share code. This way,
# do_mc_run() can support undocumented, unsupported, or preliminary (not yet ready to be shipped) arguments/features.
#' @importFrom parallel clusterExport
do_mc_run <- function(dgp_params = NULL, nvec, dgp, dgpp_to_poi = NULL, statistics, nsims, diagnostics = NULL, partial_results = NA, stat_knowledge = NULL, oloop_is_p = FALSE, parallel = NA, verbose = 1, hooks_pnchunk = list(hook_print_partial, hook_progress_message, hook_disable_other_parallelizations), hooks_ba_dgpp_to_poi = list(), seed_initializer = "seed_init_pnhash", store_globals = TRUE,
  #
  # args not in mc_run (i.e., not exposed in interface).
  init_seeds = NULL,
  # todo/expire: I originally wanted to by default give error if non-function globals
  # i.e., I wanted to have this default to "functions".
  # But I gave up on that, mainly because it was hard to distinguish captured variables. For now, just keep hidden.
  allow_non_local = "all"
  ) {

  # for internal variable name purposes:
  # diagnostics_arg might be a function or a list of functions, but the
  # internal diagnostics is always one function or NULL (if user did not specify).
  diagnostics_arg <- diagnostics
  rm(diagnostics)

  statistics_arg <- statistics
  rm(statistics)

  if (!is.logical(parallel)) {
    stop("The argument 'parallel' must be TRUE, FALSE, or NA.")
  }

  # if NA, nothing to do (we use the user's plan).
  if (!is.na(parallel)) {
    if (parallel && current_plan() == "sequential") {
      #isRStudio <- Sys.getenv("RSTUDIO") == "1"
      #if (Sys.info()['sysname'] == "Windows" || isRStudio) {
      # https://cran.r-project.org/web/packages/future/vignettes/future-1-overview.html
      if (parallelly::supportsMulticore()) {
        plan_ <- "multicore"
      } else {
        plan_ <- "multisession"
      }
    } else if (!parallel && current_plan() != "sequential") {
      plan_ <- "sequential"
    } else {
      # i.e., don't change plan.
      plan_ <- NA
    }

    if (!is.na(plan_)) {
      # This code is based on code in ?plan.
      # plan(plan_) returns the old plan.
      oplan <- plan(plan_)
      on.exit(plan(oplan), add = TRUE)
    }
    if (verbose >= 2) message("'future' plan for simulations is: ", current_plan())
  }
  # no reason for this other than just signal to reader that we don't
  # use the parallel argument for anything other than the above setup.
  rm(parallel)

  if (is.null(dgp_params)) {
    if (verbose >= 2) message("No \"dgp_params\" argument specified so assuming just one DGP that is inlined in \"dgp\".")
    # we still need an object for some of the code to work (e.g., we calculate
    # hash from the dgp param). This is cleaner than continually conditioning
    # on is.null(dgp_params).
    dgp_params <- list(
      "(one unspecified dgpp)"
    )

    # below, we now wrap the user functions in functions that do accept
    # a 'dgp_param' arg. Alternatively, we could just modify the user function
    # to accept a 'dgp_param' arg, but that does not feel right, and also might
    # be confusing when debugging an error (look at stack).

    if (!is.null(dgpp_to_poi)) {
      if ("dgp_param" %in% formalArgs(dgpp_to_poi)) {
        stop("Did you forget to specify a \"dgp_param\" argument to mc_run()? If you meant to omit it, then you also omit a \"dgp_param\" argument to your \"dgpp_to_poi\" function.")
      }
      dgpp_to_poi_user <- dgpp_to_poi
      dgpp_to_poi <- function(dgp_param) {
        dgpp_to_poi_user()
      }
    }

    if ("dgp_param" %in% formalArgs(dgp)) {
      stop("Did you forget to specify a \"dgp_param\" argument to mc_run()? If you meant to omit it, then you also omit a \"dgp_param\" argument to your \"dgp\" function.")
    }
    dgp_user <- dgp
    dgp <- function(dgp_param, n) {
      dgp_user(n = n)
    }
  }

  # sanity check
  if (!is.null(stat_knowledge)) {
    if (!is.list(dgp_params)) {
      stop("If 'stat_knowledge' is specified, 'dgp_params' must be a (named) list.")
    }
    for (param in dgp_params) {
      if (!all(stat_knowledge %in% names(param))) {
        stop("'stat_knowledge' must be named elements of the list elements in 'dgp_params'.")
      }
      if (all(names(param) %in% stat_knowledge)) {
        warning("'stat_knowledge' is giving complete knowledge of the DGP parameters. ",
                "Unless you are just exploring the theoretical properties of an ",
                "*infeasible* statistic, the statistic should not know everything ",
                "that the dgp knows.")
      }
    }
  }

  # TODO: sanity-check seed_initializer argument
  # the reason the argument is a string is because I don't want them to use a
  # custom function. So instead of exporting the seed initializer functions, we
  # just resolve them here. I want to leave open the option of conditioning on
  # the seed_initializer.

  seed_initializer <- get(seed_initializer)


  if (!(length(nsims) %in% c(1, length(nvec)))) {
    stop("'nsims' should have length equal to '1' or to 'length(nvec)'")
  }
  if (length(nsims) == 1) {
    nsims <- rep(nsims, times = length(nvec))
  }

  # do sanity checks that all arguments that should be functions are actually functions.
  if (is.null(dgpp_to_poi)) {
    if (verbose >= 1) {
      message("No dgpp_to_poi specified so using the identity function.")
    }
    dgpp_to_poi <- function(x) x
  }

  if (!is.null(diagnostics_arg)) {
    diagnostics <- process_diagnostic_arg(diagnostics_arg)
  } else {
    diagnostics <- NULL
  }

  statistic <- process_statistics_arg(statistics_arg)

  if (store_globals) {
    if (verbose >= 2) message("Collecting globals...")

    funcs_to_check_l <- list(statistic = statistic, dgp = dgp, dgpp_to_poi = dgpp_to_poi)
    if (!is.null(diagnostics)) {
      funcs_to_check_l <- c(funcs_to_check_l, diagnostics = diagnostics)
    }
    globals_l <- collect_globals(funcs_l = funcs_to_check_l, recursive = TRUE, allow_non_local = allow_non_local, verbose = verbose)
    if (verbose >= 3) message("collected the following globals: ", paste(names(globals_l), collapse = ", "))

    if (verbose >= 2) message("Collecting globals... Done.")
  } else {
    # list(NULL) instead of NULL because it makes setting the attribute a bit easier.
    # Otherwise, can't set it "directly" to NULL with "<-" because assigning NULL
    # to a list means to remove that element (attribute in this case).
    globals_l <- list(NULL)
  }


  # END of sanity checks.



  if (!exists(".Random.seed")) {
    # if I remove this, then I get errors from the tests only *when the tests are run in parallel* !
    # (the error is that .Random.seed does not exist)
    # todo: I'm not sure why the error only happens when the tests are run in parallel. Would be nice to know
    # and document.
    set.seed(NULL)
  }
  core_seed <- .Random.seed


  # Restore user_rngkind on exit. This is because for seed_init_pnhash()
  # we impose a specific RNG kind. We might as well just restore regardless,
  # just in case other seed inits do something similar.
  #
  # alternative: could use on_parent_exit inside seed_init_pnhash().
  user_rngkind <- RNGkind()
  on.exit({
    do.call("RNGkind", as.list(user_rngkind))
  }, add = TRUE)


  if (!is.na(partial_results)) {
    if (verbose >= 1) {
      message("Processing partial results.")
    }
    results <- resolve_user_mc_arg(partial_results, verbose = verbose)
    check_format(results)
    # need to do this before potentially running outside_in() for now, since get_param_tuples()
    # does not currently support the format where nobs is on outer. I'm also not sure outside_in()
    # even preserves that attribute.
    ptups_efile <- get_param_tuples(mc = results)

    # get the "after" seed of the last sim that succeeded, so we can take off from there.
    # We have tests that check this feature works in "test-backup-mechanism.R" (see \ref{test_seed_recovery}).
    # microop: I think this is not needed for seed_init_pnhash since for that seed initializer,
    # it does not use the core seed anyway.
    #
    # This solution should be robust to hooks altering the seed, since we set the seed here
    # before hooks run (so they would alter it in the same way).
    pn_pairs_ <- get_pn_pairs(mc = results, sortby = "time_finished")
    pn_pair_last_successful <- pn_pairs_[[length(pn_pairs_)]]
    rnginfo_last_successful <- get_rnginfo_after(mc = results, pn_pair = pn_pair_last_successful)
    restore_rng(rnginfo_last_successful)
    core_seed <- get_attr(results, "core_seed")

    if (!oloop_is_p) {
      # files saved from montetools are always saved in oloop_is_p format.
      # So if we are in !oloop_is_p mode here, we convert back after reading in.
      results <- outside_in(results)
    }
  } else {
    results <- list()
    class(results) <- "montecarlo"
  }

  dgp_params <- dgp_params_to_list(dgp_params)

  # todo: what if nvec is already named?
  # I think it's fine to use those names. BUT make sure I don't actually rely on those
  # names. At some point I had a hackish solution of relying on the names. Make sure
  # that hack is gone. Then maybe make sure these names are unique?
  names(nvec) <- nvec

  # todo: what if true_poi is the same for many different params...
  #       I guess have to hardcode the values?
  # or pass the entire dgp_params *list* to dgpp_to_poi() so it can optimize internally?

  # first generate the POIs. Reason for this...
  # microop: parallelize over parameters?
  # intermediate: I just wanted to get dgpp_to_poi outside of do_mc_pair.

  if (length(results) == 0) {
    if (verbose >= 2) message("running dgpp_to_poi on all DGP params...")
  } else {
    if (verbose >= 2) message("running dgpp_to_poi on the DGP params not in efile...")
    # todo: why not just calculate POI in the 'for' loop instead of all at once?
    #       disadvantage: can't parallelize (across dgpps)
    #       advantage: it's a bit cleaner with respect to partial file.
    phashes_from_efile <- sapply(X = ptups_efile, FUN = function(x) calc_hash(x[["dgpp"]]))
    phashes_from_this_run <- sapply(X = dgp_params, FUN = calc_hash)
    # EXPIRE: why did I do this complicated thing before? now just use phashes.
    #reuse_these <- sapply(X = seq_along(dgp_params), FUN = function(i) {
    #               identical(ptups_efile[[i]][["dgpp"]], dgp_params[[i]])
    #})
    reuse_these <- phashes_from_this_run %in% phashes_from_efile
    # todo: now avoid running dgpp_to_poi() for elements of reuse_these that are true.
    #       - make tests for this.
  }
  #
  param_tuples <- gen_param_tuples(dgpp_to_poi = dgpp_to_poi, dgp_params = dgp_params, nvec = nvec, hooks_ba_dgpp_to_poi = hooks_ba_dgpp_to_poi, seed_initializer = seed_initializer, core_seed = core_seed, verbose = verbose)


  if (oloop_is_p) {
    outer_l <- dgp_params
    inner_l <- nvec
  } else {
    outer_l <- nvec
    inner_l <- dgp_params
  }

  # We don't have this in hook_progress_message since hooks currently only have
  # access to previous and next chunks. Could easily change that if we want to
  # give hooks more power.
  if (verbose >= 1) {
    param_labels <- sapply(X = param_tuples, `[[`, "label")
    if (identical(param_labels, NA)) {
      # this happens if there is one DGP param.
      param_labels <- "1 unnamed"
    }

    n_pn_chunks <- length(param_labels) * length(nvec)
    message("")
    msg_ <- paste0(n_pn_chunks, " pn-chunks will be run.")
    message(msg_)

    local({
    nsims_unique <- unique(nsims)
    if (length(nsims_unique) == 1) {
      nsims_str <- "  nsims (each chunk): "
      nsims_label <- nsims_unique
      # TODO: also give order. i.e., first, second, third pn-chunks?
    } else {
      nsims_str <- "  nsims: "
      nsims_label <- paste(nsims, collapse = ", ")
    }
    nvec_str <- paste0("  nvec (", length(nvec), "): ")
    param_labels_str <- paste0("  params (", length(param_labels), "): ")
    # we use a padder to get alignment, e.g.,:
    #   nvec:       <blah>
    #   params:     <blah>
    padder <- create_padder(nvec_str, param_labels_str, nsims_str)
    message(padder(param_labels_str), paste(param_labels, collapse = ", "))
    message(padder(nvec_str), paste(nvec, collapse = ", "))
    # nsims corresponds to nvec, so it comes after nvec. But I also want
    # dgp_params to be next to nvec, so we put it at the top. Done.
    message(padder(nsims_str), nsims_label)
    })
  }


  user_args <- list(
    diagnostics = diagnostics,
    dgp = dgp,
    statistic = statistic,
    stat_knowledge = stat_knowledge,
    verbose = verbose
  )

  attrs_l <- list(
    dgpp_to_poi = dgpp_to_poi,
    diagnostics = diagnostics,
    format = get_ver_format(),
    # these are stored for reproducibility
    statistic = statistic,
    dgp = dgp,
    stat_knowledge = stat_knowledge,
    core_seed = core_seed,
    globals = globals_l
  )


  for (o_idx in seq_along(outer_l)) {
    partial_inner_l <- list()
    # mc_one_inner will essentially just be 'list(partial_inner_l)' with the
    # correct (outer element) name so that we can merge it as a pn pair with
    # results.
    mc_one_inner <- list()
    for (i_idx in seq_along(inner_l)) {
      if (oloop_is_p) {
        n_idx <- i_idx
        p_idx <- o_idx
      } else {
        n_idx <- o_idx
        p_idx <- i_idx
      }
      n_ <- nvec[[n_idx]]
      p_ <- dgp_params[[p_idx]]


      # This is where we check if pn-pair already done (i.e., in the partial
      # results). We do so by comparing the p_hash (which is also the name
      # of the parameter element in the dgp_params list.
      p_hash <- names(dgp_params)[[p_idx]]
      if (oloop_is_p) {
        already_done <- p_hash %in% names(results) &&
                        n_ %in% names(results[[p_hash]])
      } else {
        already_done <- n_ %in% names(results) &&
                        # if not as.character(), n_ = 100 will be looked up
                        # as the 100'th element.
                        p_hash %in% names(results[[as.character(n_)]])
      }
      if (already_done) {
        # these results are already in the partial results object
        if (verbose >= 1) {
          # todo: use tuple[["label"]] here... or pair_to_str()
          message("Using the following (p_hash, nobs) pair from partial results: (", p_hash, " , ", n_, ")")
        }
        next
      }

      #  We use do.call to actually call do_mc_pair using this list.
      #  That way, things are centralized, and we may eventually let the user
      #  return a modified version of this list. IF we do that, the following
      #  comments apply:
      #
      #    We should loop through the list and detect what the user changed
      #    (and give as a diagnostics message)
      #
      #    What if multiple hooks change things? give warning if overlap, and
      #    just give precedence to newest.  Give warning if user tries to
      #    change some variables, like p or n.
      mc_next <- list(
        n = n_,
        one_nsims = nsims[[n_idx]],
        ptup = param_tuples[[p_idx]]
      )

      # no use case to expose these to user
      mc_next_private <- list(
        init_seed = init_seeds[[p_hash]][[as.character(n_)]]
      )


      # prepare results_partial to pass to hooks *before* the work because this way after inner_l has run,
      # the outer-loop (over outer_l) commands will have run.
      #
      results_partial <- merge_mc_lists(results, mc_one_inner)
      # todo: ideally mc_one_inner would have this class (and then merge_mc_lists would transfer it).
      #       figure out the root issue.
      class(results_partial) <- "montecarlo"

      if (length(results_partial) > 0) {
        results_partial <- apply_attrs_to_obj(obj = results_partial, attrs = attrs_l)
        if (!oloop_is_p) {
          results_partial <- outside_in(results_partial)
        }
        # this chunk must be after outside_in()
        for (i in seq_along(results_partial)) {
          # todo:  can we assume the same ordering in results_paratial as param_tuples? i.e.,
          #        this position-based loop is OK? I think so.
          #        If not, just double-check hash?
          attr(results_partial[[i]], "param_tuple") <- param_tuples[[i]]
        }
      } else {
        # This is how hooks can recognize it is the first call.
        results_partial <- NULL
      }


      # Do this before the hooks because mc_reproduce() works by appending a
      # hook on the end that will overwrite the seed that is set here.
      if (verbose >= 2) message("Running seed initializer...")
      seed_ <- seed_initializer(base_seed = core_seed, n = n_, phash = p_hash)
      mc_set_seed(seed_)
      if (verbose >= 2) message("Running seed initializer... Done.")


      pn_pair_next <- c(p = p_hash, n = n_)
      args_to_beforehooks_l <- list(
                                    mc_part_done = results_partial,
                                    mc_args_next = mc_next,
                                    user_args = user_args,
                                    pn_pair_next = pn_pair_next
                               )
      if (verbose >= 2) message("Applying hooks before pnchunk...")
      hook_rets_l <- run_pnchunk_hooks(hooks = hooks_pnchunk, hook_args = args_to_beforehooks_l)
      if (verbose >= 2) message("Applying hooks before pnchunk... Done.")

      # This creates some duplication in call to do_mc_pair:
      # We pass both this list, and *additionally* duplicated mc_next and user_args.
      # But seems low cost and keeps things a bit more readable
      args_to_withinhooks_l <- args_to_beforehooks_l

      withinhooks_l <- list()
      for (hookj in hooks_pnchunk) {
        # Hooks that are "within-chunk aware" accept arg 'nsims_dispatched'.
        accepts_nsims_dispatched_arg <- "nsims_dispatched" %in% formalArgs(hookj)
        if (accepts_nsims_dispatched_arg) {
          withinhooks_l <- c(withinhooks_l, hookj)
        }
      }
      # TODO: optimization: if withinhooks_l is empty, then set future_for_with_hook's hook_freq to Inf.

      one <- do.call(do_mc_pair, c(mc_next, mc_next_private, user_args,
                                   list(withinhooks_l = withinhooks_l),
                                   list(args_to_withinhooks_l = args_to_withinhooks_l),
                                   list(attrs_l = attrs_l)
                                 )
             )
      one_l <- list(one)
      # single [] so as to preserve the name.
      names(one_l) <- names(inner_l[i_idx])
      partial_inner_l <- c(partial_inner_l, one_l)

      # We create an object with MC structure, where the outer has just one element, so that we can do an MC merge.
      #
      # if we did not use the partial results, we could do this outside of the
      # inner loop, but we do it here so everything is self-contained when we pass
      # results to the hooks (e.g., partial save).
      mc_one_inner <- list(partial_inner_l)
      names(mc_one_inner) <- names(outer_l[o_idx])
    }
    if (length(partial_inner_l) == 0) {
      if (verbose >= 2) message("Nothing left to do? It seems the partial results file had everything we need.")
      next
    }
    #
    results <- merge_mc_lists(results, mc_one_inner)
    # We set attributes at a different location, but we also set these here so
    # the attributes are in the results passed to the hooks (e.g., for partial
    # save).
    results <- apply_attrs_to_obj(obj = results, attrs = attrs_l)
    # todo: this should be taken care of elsewhere.
    class(results) <- "montecarlo"
  }
  if (!oloop_is_p) {
    results <- outside_in(results)
  }
  # this chunk must be after outside_in()
  for (i in seq_along(results)) {
    # see comment above about 'can we assume the same ordering'.
    attr(results[[i]], "param_tuple") <- param_tuples[[i]]
  }

  # apply hooks one last time. This allows hooks to perform clean up (e.g., the partial save hook
  # may delete the partial results file).
  if (verbose >= 2) message("Applying hooks after all sims are done...")
  args_to_beforehooks_l <- list(mc_part_done = results,
                                mc_args_next = NULL,
                                user_args = user_args,
                                pn_pair_next = NULL)
  hook_rets_l <- run_pnchunk_hooks(hooks = hooks_pnchunk, hook_args = args_to_beforehooks_l)
  if (verbose >= 2) message("Applying hooks after all sims are done... done.")

  # If we don't include this chunk, the test with mc_table() in it in the following
  # test will fail (in test-mc-diags.R):
  # "Handling of errors works well when all errors in small n."
  #
  # We already correct as much as we can above *within* a chunk (e.g., we
  # expand NAs to the proper length based on finding a simulation where
  # statistic() does return without error). However, if all simulations of
  # a chunk have an error, we can only correct *across* chunks.
  results <- correct_NA_chunks_ACROSS_chunks(results)

  return(results)
}


#' @name mc_hooks
#'
#' @title mc hooks
#'
#' @details mc hooks customize display of partial results and other things.
#'
#' This type of hook takes arguments 'mc_part_done', 'mc_args_next', 'user_args', and 'pn_pair_next'.
#'
#' See examples.
NULL

# useful in order to be able to append to default hooks.
#' @export
#' @title Default list of mc hooks
#' @description Default list of mc hooks, useful for appending.
hooks_pnchunk_default <- list(hook_print_partial, hook_progress_message, hook_disable_other_parallelizations)

#' Run a Monte Carlo simulation
#' @param dgp_params The data generating process (DGP) parameters. These are the true parameters used to generate the data that are unknown to the statistic. They are either the parameter of interest or the objects that induce it. If a vector is passed to this argument, each element of the vector will be fed to dgp(); if a data.frame is passed, each row will be fed to dgp() as a one-row data.frame (so that it can contain mixed types, as opposed to a vector); if a list is passed, each element will be fed to dgp(). In addition to an element of the corresponding parameter space, the following may be considered part of (i.e., elements in the list) a DGP parameter: the distribution of the explanatory variables, the distribution of the error term, and the functional form (e.g., an R formula object). You do not need to specify this argument if there is only one DGP parameter and dgp() knows everything it needs to. In this special case, your dgp() argument and your dgpp_to_poi() argument should not accept 'dgp_param' as an argument.
#' @param nvec The vector of sample sizes.
#' @param dgp A function that inputs two arguments: 'dgp_param' (an element of 'dgp_params'); and 'n', the sample size (an element of 'nvec'). It returns one simulated data frame (that will be passed to statistic()).
#' @param dgpp_to_poi A function that inputs 'dgp_param' (an element of 'dgp_params'), and outputs the parameter of interest (POI). If the estimator is estimating the DGP param directly, rather than an induced parameter, then it is not necessary to specify this function. If the estimator is estimating a subset of the DGP param (e.g., only one beta value instead of the entire vector of betas), then it is necessary to specify this function. dgpp_to_poi() is called only once per dgp_param, while dgp() is called once per simulation. Often the parameter in the table is different from the "true" parameter that we are trying to perform inference on. For example, the variance of epsilon might be viewed as a parameter. In addition to the DGP parameter, this function may optionally accept a second argument, "maxn", which is essentially max(nvec) and is useful in cases when the true POI is approximated (e.g., using simulation or numeric approximation methods) for deciding the error tolerance of the POI. The return of this function should be a list of length 2, where the first element is the POI value, and the second element is the label corresponding to the POI (and DGP) that should be used for progress messages and subsequently for mc_table() (e.g., the POI table column). For hypothesis testing, this function could just map to "null" or "alternative". This result will just be useful for labelling/sorting really. The diagnostics function for hypothesis tests will not even depend on the true parameter (as opposed to the case where the statistic is an estimator).
#' @param stat_knowledge A character vector. Each element of this vector is a name of an element of a dgp_param. Only these names of the dgp param elements will be passed to 'statistic()'. If this argument is specified, 'dgp_params' must be a list object. If the statistic is non-parametric, typically 'stat_knowledge' will either be left as NULL or will be the name of the element of a DGP parameter that just gives the name of the y variable in the data and the name(s) of the x variable(s). For parametric statistics, it's common to pass the functional form or the specification, e.g., an R formula object (which contains the specification but not the true coefficients). See examples.
#' @param statistics A statistic is a function that inputs two arguments, 'dataf', which is a data.frame (that is internally produced by calling 'dgp'); and 'dgp_param_shared', which is the subset (determined by 'stat_knowledege') of a 'dgp_params' element that the statistic is allowed to see, and returns either a length-1 numeric of the observed statistic value, or a matrix where each row corresponds to an observed statistic value. For example, if the statistic produces an estimate and a confidence interval, the columns might be "lower", "est", and "upper", and the different rows would be different statistics values that you want to compare (e.g., from different estimators or different ways of bootstrapping to get the confidence interval). A statistic can generally be any function of the data. Examples include p-values, reject/fail-to-reject decisions, and classification predictions. Returning a vector of length k with k > 1 is ambiguous because it is not clear whether it should be interpreted as k different statistics or a single k-dimensional statistic. This argument may be a single statistic, or a list of statistics.
#' @param diagnostics A diagnostic, or list of diagnostics. For more information, see '?mc_diagnostics'. It is best to specify this argument to mc_run in order to use 'hook_print_partial' (which is used by default) to show intermediate results; and to achieve a self-contained MC object. However, this argument is not strictly necessary if you do not want to use 'hook_print_partial'.
#' @param oloop_is_p If TRUE, the outer loop of the MC code is over 'dgp_params', and the inner loop is 'nvec'. This argument does not affect the end result but adds convenience for long simulations. If you want to prioritize getting complete results (i.e., for all of 'nvec') for the first few params before the simulations complete, set to TRUE. Alternatively, in many MCs, the larger sample sizes take considerably more time, and are also more likely to fail (e.g., due to longer time and more RAM they take), so if this argument is set to FALSE, then the outer loop will be over 'nvec' and the inner loop will be over 'dgp_params', meaning that you will get results for all parameters for the smaller sample sizes before the simulations get to larger sample sizes.
#' @param partial_results The file name (or MC object) of previous results (e.g., the partial results file saved with 'hook_save_partial').
#' @param nsims A numeric vector with length equal to 1 or to length(nvec). It may make sense to use a fewer number of simulations for larger number of observations, e.g., (1) if the CPU time is much more and you just want to get an idea of the results; or (2) the variance is lower so don't need to run as many.
#' @param hooks_ba_dgpp_to_poi This argument is used internally by mc_reproduce() to ensure reproducibility of stochastic 'dgp_to_poi' functions. It is not common for the user to use this hook directly.
#' @param hooks_pnchunk A list of hooks. These hooks allow the user to customize display of partial results, making pn-pair dependent settings (like having number of cores depend on nobs), etc. See ?mc_hooks.
#' @param parallel Can be TRUE/FALSE/NA. If TRUE, montetools will use a reasonable parallelization plan depending on your operating system. If FALSE, montetools will set the future::plan() to 'sequential' (if it isn't already). If NA (default), then montetools does not do any configuration and the future::plan() you manually set before calling mc_run() is used. If you did not manually set one, the likely default is 'sequential'. If montetools changes your plan(), the original plan() will be restored on exit from mc_run().
#' @param seed_initializer Must be one of "seed_init_pnhash", "seed_init_add_ints", or "seed_init_pnhash".
#' @param store_globals Whether to capture and store the global objects to make it easier to potentially run mc_reproduce() and mc_extend() later.
#' @eval param_verbose()
#'
#' @return
#' This function returns a "montecarlo" object, which should then be used with mc_diags() which in turn produces input to mc_table(). Each pn pair correponds to a stacked statistics matrix, which contains all of the observed statistic values across simulations for that pn pair.
#'
#' @seealso
#' [mc_extend()] for increasing the number of simulations of an *existing* 'montecarlo' object, [mc_reproduce()] for reproducing a 'montecarlo' object.
#'
#' @export
#' @importFrom utils menu
#' @importFrom parallel mclapply parLapply
#' @importFrom future.apply future_lapply
#' @importFrom future plan
#' @details
#' The following usage example specifies only the essential arguments:
#'
#'   mc_run(dgp_params, nvec, dgp, dgpp_to_poi, stat_knowledge, statistics, nsims)
#'
#' After confirming the above passes without error (use nsims = 1 for a quick
#' test), consider additionally using the 'diagnostics' argument to see results
#' after each set of simulations is done, and 'parallel = TRUE' to enable
#' parallelization.
#'
#' parallel does parallelization over the simulations within each Monte Carlo "column"/chunk
#'          parLapply is not recommended, but can be useful for debugging (e.g., reports the actual error when there is an error. for mclapply(), hard to figure out what the error was)
#'
#' progressr is used to provide a progress bar. You can disable the progress bar by running handlers("void") after loading "montetools". Or you can customize the progress bar. For example, 'handlers(handler_progress(format="[:bar] :percent :eta :message"))'. For more information on how to customize the progress bar, or how to enable it even in non-interactive mode, see https://cran.r-project.org/web/packages/progressr/vignettes/progressr-intro.html and https://furrr.futureverse.org/articles/articles/progress.html.
#'
mc_run <- function(dgp_params = NULL, nvec, dgp, dgpp_to_poi = NULL, stat_knowledge = NULL, statistics, nsims, diagnostics = NULL, partial_results = NA, oloop_is_p = FALSE, parallel = NA,
                       hooks_pnchunk = hooks_pnchunk_default,
                       hooks_ba_dgpp_to_poi = list(),
                       seed_initializer = "seed_init_pnhash",
                       store_globals = TRUE,
                       verbose = 1
                   ) {
  do_mc_run(dgp_params = dgp_params, nvec = nvec, dgp = dgp, dgpp_to_poi = dgpp_to_poi, statistics = statistics, nsims = nsims, diagnostics = diagnostics, partial_results = partial_results, stat_knowledge = stat_knowledge, oloop_is_p = oloop_is_p, parallel = parallel, verbose = verbose, hooks_pnchunk = hooks_pnchunk, hooks_ba_dgpp_to_poi = hooks_ba_dgpp_to_poi, seed_initializer = seed_initializer, store_globals = store_globals)
}


correct_rownames_of_NAs <- function(stacked_rownames, nsims) {
  enable_asserts <- assertsAreEnabled()

  if (!enable_asserts) {
    if (all(stacked_rownames != "")) {
      # there is no problem so do quick return before
      # potentially nontrivial computation time.
      return(stacked_rownames)
    }
  }

  # TODO: comment in parent function that get_statnames() will be invalid
  # until this is run?

  # TODO: really need unit tests for this.
  # Factor out this computation and make unit tests.

  nstats <- length(stacked_rownames)/nsims

  # check it's an integer. i.e., the method of calculating nstats is valid.
  # This will trigger in the tests if we do not do expand_NA_returns() before
  # this function.
  if (!(nstats %% 1 == 0)) {
    internal_error("inferred 'nstats' is not an integer.")
  }

  statnames <- vector(length = nstats, mode = "character")

  for (i in seq_along(statnames)) {
    idx <- rep(FALSE, times = nstats)
    idx[[i]] <- TRUE
    all_rets_i <- stacked_rownames[idx]
    # microop: if this is expensive don't really need to do this
    # except when doing the extra check below. i.e., could condition on 'enable_asserts'.
    candidates <- unique(all_rets_i)
    candidates <- candidates[candidates != ""]
    if (length(candidates) == 0)  {
      # TODO: Could have a function at the end that reconciles. e.g., sees "stat_unnamed1" for one pn-chunk and it is named in another,
      # it can reconcile. If I rely on hardcoded "stat_unnamed" give error at begnning if user tries to use that. Maybe "stat_unknown"?
      stop("No way to get stat name. Return generic name stat1? Might mismatch with a different pn-chunk. Maybe stat_unnamed1?")
    } else if (length(candidates) == 1) {
      statnames[[i]] <- candidates
    } else {
      stop("Multiple candidates for stat name? How did this happen?")
    }
  }

  fixed_rownames <- rep(statnames, times = nsims)

  if (enable_asserts) {
    if (all(stacked_rownames != "")) {
      # this checks that the quick return is equivalent to
      # the non-quick return
      stopifnot(identical(fixed_rownames, stacked_rownames))
    }
  }

  return(fixed_rownames)
}


expand_NA_returns <- function(stacked, unstacked) {
  enable_asserts <- assertsAreEnabled()

  # microop: provide a more efficient method *if* the user supplies stat dim arg to mc_run() ?


  stopifnot(is.matrix(stacked))

  if (all(!is.na(stacked))) {
    # If there is no NA element, do a quick return to avoid
    # further computation.
    return(stacked)
  }

  sim_is_NA <- sapply(unstacked, FUN = function(x) length(x) == 1 && is.na(x))
  # really just need to check for single NAs in unstacked.
  # if a row of stacked has a mix of NAs and numbers, that's fine.
  #
  # We separate this check from above since the above check might be
  # much faster since it works on a matrix rather than looping through
  # list elements.
  if (all(!sim_is_NA)) {
    return(stacked)
  }

  if (all(is.na(stacked))) {
    # TODO: if all NAs... need to give error? hmmm
    # not much else we can do.
    return(stacked)
  }

  first_nonNA_idx <- which(!sim_is_NA)[[1]]
  nstats <- nrow(unstacked[[first_nonNA_idx]])
  statdim <- ncol(unstacked[[first_nonNA_idx]])

  if (enable_asserts) {
    nonNA_nrows <- sapply(X = unstacked[!sim_is_NA], FUN = nrow)
    stopifnot(all(nonNA_nrows == nstats))
  }

  if (nstats == 1) {
    # no correction needed
    return(stacked)
  }


  NA_constructed <- matrix(rep(NA, times = nstats*statdim), nrow = nstats, ncol = statdim)

  # this relies on recycling (the LHS can be a list of many elements and the
  # RHS has length 1)
  unstacked[sim_is_NA] <- list(NA_constructed)

  stacked_new <- do.call(rbind, unstacked)

  if (enable_asserts) {
    if (identical(stacked, stacked_new)) {
      stop("Extra: possibly could have avoided computation.")
    }
  }

  return(stacked_new)
}


#' @import progressr
#' @importFrom parallelly availableCores
#' @importFrom utils capture.output sessionInfo
#' @importFrom future futureSessionInfo
do_mc_pair <- function(n, one_nsims, diagnostics, dgp, stat_knowledge, statistic, ptup, init_seed, withinhooks_l, args_to_withinhooks_l,
                       # need attrs_l to get attributes right for within chunk hooks.
                       attrs_l, verbose) {

  # alt: could instead do this outside of do_mc_pair, but it's nice to keep do_mc_pair() self-contained.
  #      e.g., we can call it from other functions besides mc_run().
  #
  # document why we do this *per* MC col. that way, can reproduce, e.g., just the last column of a table.
  #
  # todo document:
  # need to do saving and setting here rather than before. Don't forget that
  # dgpp_to_poi() can use random seed.
  # ^^^ document this to show that this is not trivial (?).
  # also... need to save things besides .Random.seed


  # todo perhaps document these references somewhere:
  # https://stackoverflow.com/questions/13997444/print-the-current-random-seed-so-that-i-can-enter-it-with-set-seed-later/13997608#13997608
  # http://www.cookbook-r.com/Numbers/Saving_the_state_of_the_random_number_generator/
  rnginfo_before <- rng_snapshot()

  # it is helpful to allow users to set nsims to 0 for a few reasons:
  # e.g., mc_extend().
  if (one_nsims == 0) {
    # we do this to keep the structure so e.g., get_nsims() works (i.e., returns 0).
    sims_l <- list(stats_m = matrix(nrow = 0, ncol = 0), aux_l = list())
    # todo: we could still put the full sys_stats I suppose...
    attr(sims_l, "sys_stats") <- list(time_finished = Sys.time())
    attr(sims_l, "rnginfo_before") <- rnginfo_before
    attr(sims_l, "seed_first") <- list()
    # after = before in this special case.
    attr(sims_l, "rnginfo_after") <- rnginfo_before
    attr(sims_l, "seed_last") <- list()
    return(sims_l)
  }

  fun_ <- function(simidx_notused) {
    # TODO: EXPIRE these two gc(). I doubt these make a difference.
    # do tests and timings (with the different parallelization settings).
    # Note that they also slow things down (for small simulations).
    # After I get benchmarks set up, check if in some strange situation they
    # make a difference and then get rid of them if not.
    #gc()
    one_sim_ <- one_sim(n = n, dgp_param = ptup[["dgpp"]], dgp = dgp, statistic = statistic, stat_knowledge = stat_knowledge)
    #gc()
    return(one_sim_)
  }

  # We allow for this to benchmark future_lapply against mclapply and pbmclapply.
  # A direct comparison isn't fair, since future_lapply supports *reproducible*
  # results (which requires overhead), but a big difference might suggest a regression
  # somewhere.
  override <- getOption("montetools_parallel_override")
  if (!is.null(override)) {
    xapply <- get(override)

    # define these, otherwise get "not found" errors below.
    seed_first <- c()
    seed_last <- c()
  }

  xapply_is_future_derivative <- is.null(override) || override == "future_for_with_hook"
  if (xapply_is_future_derivative) {
    if (verbose >= Inf) {
      options(future.debug = TRUE)
    }
    # we test (see test-make-seeds.R) that using future.seeds_ is same
    # as setting "future.seed" to TRUE. The advantage is that we can store
    # the last element of future.seeds_
    future.seeds_ <- make_seeds(one_nsims, begin_after = init_seed)
    # not sure if we need to save the first, but just in case...
    seed_first <- future.seeds_[[1]]
    seed_last <- future.seeds_[[one_nsims]]

    # default of future.globals is TRUE, but I think this overhead might
    # not be needed if plan is 'multicore' or 'sequential'.
    # see here:
    # todo: I asked whether this is true, and if so why it is not the default:
    # https://github.com/HenrikBengtsson/future/discussions/627
    # TODOSO: If no response there, ask on Stack Overflow?
    future.globals_ <- !(current_plan() %in% c("multicore", "sequential"))

    seq_ <- seq_len(one_nsims)
    future_lapply_args <- list(
      X = seq_,
      future.seed = future.seeds_,
      future.globals = future.globals_,
      # TODO: centralize this process after I figure out progress bar with
      #        future_for_with_hook.
      # the argument "FUN" is changed below if progress bar
      FUN = fun_
    )
  }

  if (is.null(override)) {

    # We could do this in a default hook so that the user could set verbose = 0
    # and still have a progress bar, but I don't imagine there are many users
    # who fall in this category. If a user requests it, then we can consider
    # it.
    #
    # Could use handlers("void") and just use one code path, but it's nice to
    # keep this because it's possible this could be a bit faster (because don't
    # need to signal progress).
    #
    show_progress_bar <- verbose > 0
    #
    if (show_progress_bar) {

      # (https://stackoverflow.com/questions/62916053/r-asynchronous-parallel-lapply)
      st <- system.time(with_progress({
        p <- progressor(along = seq_)
        if (verbose >= 2) message("future_lapply beginning...")
        future_lapply_args[["FUN"]] <- function(x) {
          fun_ret <- fun_(x)
          # todo: I think I prefer to signal progress after completion (where it is now),
          # rather than before fun_(x).
          # What do users prefer though? Allow them to choose?
          # TODO: maybe ask if there is a progressr option for this?
          # e.g., something I can condition on?
          p()
          return(fun_ret)
        }
        sims_l <- do.call(future_lapply, future_lapply_args)
        if (verbose >= 2) message("future_lapply done...")
      }))
    } else {
      st <- system.time(sims_l <- do.call(future_lapply, future_lapply_args))
    }
  } else {
    if (override == "future_for_with_hook") {
      withinhooks_wrapper <- function(nits_dispatched, vs_resolved) {
        # todo: rename lots of places "partial_results" to "pn_chunks_completed" or something like that.
        #       to differentiate from partial *within* results.

        # first copy from the enclosed variable. Don't modify it directly because
        # it is hard to read whether we would be appending to it on each call.
        args_to_wihooks_l <- args_to_withinhooks_l
        args_to_wihooks_l <- c(args_to_wihooks_l, list(nsims_dispatched = nits_dispatched))
        null_idx <- sapply(vs_resolved, is.null)
        sims_resolved <- vs_resolved[!null_idx]
        # TODO: is this the right thing to do? give any message? or still call hooks???
        if (length(sims_resolved) == 0) {
          # none of the sims has completed
          return(NULL)
        }
        # TODO: build the fake mc just once outside of the wrapper, then capture it inside and just replace stats_m each time.
        pn_pair_in_progress <- args_to_wihooks_l[["pn_pair_next"]]
        pmc <- create_pseudo_mc_from_sims_l(sims_l = sims_resolved, pn_pair = pn_pair_in_progress, attrs = attrs_l)
        mc_part_done <- args_to_wihooks_l[["mc_part_done"]]
        results_partial_merged <- merge_mc_lists(mc_part_done, pmc)
        attr(results_partial_merged[[pn_pair_in_progress[["p"]]]], "param_tuple") <- args_to_wihooks_l[["mc_args_next"]][["ptup"]]


        args_to_wihooks_l[["mc_part_done"]] <- results_partial_merged

        hook_rets_l <- list()
        for (i in seq_along(withinhooks_l)) {
          hook_ret <- do.call(withinhooks_l[[i]], args_to_wihooks_l)
          hook_rets_l <- c(hook_rets_l, list(hook_ret))
        }
        return(hook_rets_l)
      }
      future_for_with_hook_args <- c(future_lapply_args,
                                list(hooks = list(withinhooks_wrapper)),
                                list(verbose = verbose)
                              )
      st <- system.time(sims_l <- do.call(future_for_with_hook, future_for_with_hook_args))
    } else {
      st <- system.time(sims_l <- xapply(X = seq_len(one_nsims), FUN = fun_))
      if (override == "pbmclapply") {
        # workaround for https://github.com/kvnkuang/pbmcapply/issues/50,
        # EXPIRE when that is fixed.
        if (one_nsims == 1 && length(sims_l) == 2 && c("value", "warning") == names(sims_l)) {
          sims_l <- sims_l[["value"]]
        }
      }
    }
  }
  time_finished <- Sys.time()
  rnginfo_after <- rng_snapshot()

  sims_l <- format_sims_list(list_of_sims = sims_l)


  # when approximating computational complexity, might want to check that all Monte Carlo's run
  # on same computer.
  # could depend on a package to get more info:
  # https://stackoverflow.com/a/60775989/1376404
  sys_stats <- gen_sys_stats(systemtime = st, time_finished = time_finished)

  attr(sims_l, "sys_stats") <- sys_stats
  attr(sims_l, "rnginfo_before") <- rnginfo_before
  attr(sims_l, "seed_first") <- seed_first
  attr(sims_l, "rnginfo_after") <- rnginfo_after
  attr(sims_l, "seed_last") <- seed_last
  return(sims_l)
}


#' @export
print.montecarlo <- function(x, ...) {
  if (!is.null(attr(x, "diagnostics"))) {
    # TODO: ideally, the object would already be marked as "mcdiags".
    # That is, it would have class "mcdiags" "montecarlo".
    print_mcdiags_table(x)
  } else {
    # TODO: maybe try to guess a diagnostic? The following is pretty useless
    #       if there is a non-trivial amount of sims.
    #       Maybe condition behavior or nsims? e.g., if nsims < 10, just print
    #       everything out? And provide arguments accordingly.
    param_tups <- get_param_tuples(x)
    for (pn in get_pn_pairs(x)) {
      cat(pn_to_label_str(mcx = x, pn_pair = pn), "\n")
      print(mc_stats_m(x, pn))
      cat("\n")
    }
  }
}


#' @export
summary.montecarlo <- function(object, ...) {
  message("(not implemented yet) your monte carlo with confidence intervals/SEs...")
}


#' @export
cbind.montecarlo <- function(..., verbose = 1) {
  # currently this is just a convenience function.
  #
  # todo: eventually, this should only merge if the rows are the same.
  # i.e., check that mc1 and mc2 have the same stats (the stats_m can
  # still have different numbers of rows due to different nsims).
  # If that check fails, recommend they look into the "more general"
  # function mc_merge() ?
  #
  # todo: also implement rbind.montecarlo().
  mc_merge(..., verbose = verbose)
}


# used in validate_montecarlo() *and* validate_mcdiags()
mc_attributes_required <- function() {
  required <- c("names", "class", "dgpp_to_poi", "format", "statistic", "dgp", "globals")
  return(required)
}


#' maybe define the format in this help file?
#' export so other users/packages can build their own montecarlo objects
#' and take advantage of other functions in montetools (e.g., diags or reporting results).
#' @export
#' @eval param_mc()
validate_montecarlo <- function(mc) {

  # todo: add more checks
  stopifnot(is.list(mc))
  stopifnot(is.montecarlo(mc))
  check_format(mc)

  attributes_required <- mc_attributes_required()
  # these do not need to exist.
  # TODO: I put "core_seed" as optional because only needed for one seed_policy.
  #       But maybe better to just have it as required?
  attributes_optional <- c("diagnostics", "stat_knowledge", "core_seed")
  attributes_mc <- names(attributes(mc))
  if (!all(attributes_required %in% attributes_mc)) {
    stop("Not all required attributes present.")
  }
  # warning() because we don't *need* set equality. It's fine if user adds extra attributes.
  if (!all(attributes_mc %in% c(attributes_required, attributes_optional))) {
    warning("Extra attributes detected (not necessarily a problem).")
  }

  return(NULL)
}

#' todo: have args to check only results....
#'  should also check that dgp_to_poi() function body is identical?
#' this ignores some attributes.
#' e.g., should not compare system times
#' If 'target' has the same ptups, but in a different order, all.equal() will return false by default.
#' If you do not want to be sensitive to order, you can use argument "sensitive_to_pn_order".
#'
#' @param target The target MC to compare to.
#' @param current The current MC.
#' @param tolerance The amount by which a number in the current MC's stats_m can differ from the corresponding target MC's stats_m.
#' @param sensitive_to_pn_order A logical. If TRUE, return FALSE if order of current's pn-chunks are different from target's pn-chunks. Analogous to whether c(5, 11) is "equal" to c(11, 5).
#' @param sensitive_to_stat_order A logical. If TRUE, return FALSE if order of current's 'stats_m's are different from target's 'stats_m's.
#' @eval param_verbose()
#' @param ... Not used.
#'
#' @exportS3Method all.equal montecarlo
all.equal.montecarlo <- function(target, current, tolerance = sqrt(.Machine$double.eps), sensitive_to_pn_order = TRUE, sensitive_to_stat_order = TRUE, verbose = 1, ...) {
  validate_montecarlo(target)
  validate_montecarlo(current)

  if (sensitive_to_pn_order) {
    sortby_ <- "none"
  } else {
    sortby_ <- "phash"
  }
  pairs_ <- get_pn_pairs(target, sortby = sortby_)
  if (!identical(pairs_, get_pn_pairs(current, sortby = sortby_))) {
    if (verbose >= 1) {
      message("pn pairs are not the same, or are not in the same order.")
    }
    return(FALSE)
  }

  ptups_target <- get_param_tuples(target, sortby = sortby_)
  ptups_current <- get_param_tuples(current, sortby = sortby_)
  if (length(ptups_target) != length(ptups_current)) {
    if (verbose >= 1) message("different numbers of dgpps")
    return(FALSE)
  }
  for (i in seq_along(ptups_target)) {
    # This is just to make sure I update the names here if I rename them.
    ignore_these <- names(ptups_target[[i]]) %in% c("rngstate_before", "rngstate_after")
    if (sum(ignore_these) != 2) {
      internal_error("rngstate_before and rngstate_after are missing.")
    }
    if (!identical(ptups_target[[i]][!ignore_these], ptups_current[[i]][!ignore_these])) {
      if (verbose >= 1) message("parameter components are not the same. This could be because 'dgp_params' are not the same, or because of different results from dgpp_to_poi.")
      return(FALSE)
    }
  }

  is_all_eq <- TRUE
  for (pn in pairs_) {
    stats_m_target <- mc_stats_m(target, pn)
    stats_m_current <- mc_stats_m(current, pn)
    if (!sensitive_to_stat_order) {
      stats_m_target <- stats_m_target[order(rownames(stats_m_target)), , drop = FALSE]
      stats_m_current <- stats_m_current[order(rownames(stats_m_current)), , drop = FALSE]
    }
    if (!isTRUE(all.equal(
                          stats_m_target,
                          stats_m_current,
                          tolerance = tolerance))) {
      if (verbose >= 1) {
        message("The stats_m of pair '", pn, "' is not equal.")
      }
      is_all_eq <- FALSE
    }
  }

  return(is_all_eq)
}


stats_constant_within <- function(mc) {
  nstats <- get_nstats(mc)
  one_pnpair_has_nondups_statvec <- rep(FALSE, times = nstats)
  for (pn in get_pn_pairs(mc)) {
    stats_m <- mc_stats_m(mc, pn)
    # todo: create a helper function and share with code in diags_aggregator() ?
    for (s in seq.int(nstats)) {
      subset_idx <- rep(FALSE, times = nstats)
      subset_idx[[s]] <- TRUE
      if (!allAreDups(stats_m[subset_idx, , drop = FALSE])) {
        one_pnpair_has_nondups_statvec[[s]] <- TRUE
      }
    }
  }
  statnames <- get_statnames(mc)
  stats_affected <- statnames[!one_pnpair_has_nondups_statvec]
  return(stats_affected)
}


#' Reproduce all, or the first 'nsims' of an MC
#'
#' @eval param_mc()
#' @param nsims The number of sims to reproduce (by default, all).
#' @param reproduce_poi Logical for whether to reproduce the POI. Useful mainly in cases where the dgpp_to_poi is stochastic.
#' @param stop_if_unequal Stop immediately if a pn-chunk is not the same. i.e., don't wait until after all (potentially computationally intensive) pn-chunks are processed to check equality.
#' @param check_nonconstant If TRUE, check that there is non-zero variation in the simulation results within at least one pn-pair. If there is no variation, this could suggest there's a problem, i.e., that the reproduction you're doing does not imply something useful. For example, when the MC statistic is a p-value, and if all of the statistic values are all 0, then that suggests you might want to try to reproduce some of the simulations where the p-value is different. The check does not give a warning when 'nsims' is 1 because it the user likely knows that they are not reproducing a diverse subset. The default is to perform the check when reproducing a proper subset and when there does exist variation in the super set (the original MC).
#' @eval param_restore_globals()
#' @eval param_verbose()
#' @param ... Arguments to pass to mc_run() (e.g., "parallel = TRUE").
#'
#' @details
#' mc_reproduce() restores the .Random.seed from before it is run. If we don't restore, the following is a bit weird: If the user runs mc_reproduce(mcx); abc <- rnorm(1); mc_reproduce(mcx); def <- rnorm(1), then abc would be equal to def. Instead of restoring, we could just reinitialize the seed, but we might as well make mc_reproduce() just not affect the seed.
#
#' @export
mc_reproduce <- function(mc, nsims = get_nsims_vec(mc), reproduce_poi = TRUE, stop_if_unequal = TRUE, check_nonconstant = NULL, restore_globals = TRUE, verbose = 1, ...) {
  # This chunk used to be inside hook_set_seed_factory (with on_parent_exit()
  # instead of on.exit()). But for 'pnhash_and_seed' it needs to be here,
  # because seed_initializer is called before the hook is even run (i.e.,
  # before the hook even has a chance to access the entering seed).
  rng_info <- rng_snapshot()
  # see details in ?mc_reproduce for why, and also the test cases
  # in test-mc-reproduce.R.
  on.exit({
    restore_rng(rng_info)
  }, add = TRUE)

  mc <- resolve_user_mc_arg(mc, verbose = verbose)

  if (length(nsims) > 1 && !all(nsims == get_nsims_vec(mc))) {
    stop("We currently don't support a vector for 'nsims' in mc_reproduce. Please request with a convincing use case.")
  }

  if (!all(nsims <= get_nsims_vec(mc))) {
    stop("nsims should be less than or equal to all elements of get_nsims_vec(mc). If you want to *extend* the sims (i.e., add more sims on top), consider using mc_extend().")
  }

  proper_subset <- !all(nsims == get_nsims_vec(mc))

  if (is.null(check_nonconstant)) {
    # By default, we only perform the check if the superset is nonconstant.
    # i.e., if the user can improve something by choosing a different subset.
    # Our job in mc_reproduce() is not to suggest problems with the original
    # MC; if the original MC is constant, just be silent by default.
    superset_has_nonconstant_subset <- length(stats_constant_within(mc)) == 0
    check_nonconstant <- proper_subset && superset_has_nonconstant_subset
  }

  # EXPIRE: this was an alternative approach to calling mc_run().
  # mc_pairs <- get_pn_pairs(mc)
  #  for (pair in mc_pairs) {
  #    # pull and set the seed... then call
  #    # do_mc_pair() ?
  ##    do_mc_pair_ <- function(n, one_dgp_param, one_nsims, dgpp_to_poi, parallel, diagnostics, dgp, statistic, stat_knowledge, ptup, verbose = verbose) {
  #  }

  if (check_nonconstant) {
    stats_affected <- stats_constant_within(mc)
    if (length(stats_affected) > 0) {
      #
      # if nstats is 1, then don't need to specify which stat has the issue.
      if (get_nstats(mc) > 1) {
        stats_affected <- paste0('"', stats_affected, '"', collapse = ", ")
        msg_beginning <- paste0("For stat(s) ", stats_affected, ", all")
      } else {
        msg_beginning <- "All"
      }
      msg <- paste(msg_beginning,
                   "elements of the MC stat matrix you are about to reproduce",
                   "are the same across simulations within pn pairs. See the",
                   "documentation in ?mc_reproduce() for the argument",
                   "'check_nonconstant' for an explanation of why this could",
                   "signal a problem, or set the argument to FALSE to disable",
                   "this check."
      )
      warning(msg)
    }
  }


  # First, we subset the MC if relevant.
  # We could (and used to) do this later in this function, but it simplifies
  # things and also it might be more efficient to deal with a lighter object.
  if (proper_subset) {
    mc <- mc_first_n_sims(mc, n = nsims)
  }

  mc_args <- get_core_mc_args(mc)

  # TODO: deduplicate. Some of this code is also in mc_extend.
  # don't need to validate the dots arguments. They'll be validated during the call to mc_run().
  dots_ <- list(...)
  dotnames <- names(dots_)
  conflicts <- dotnames[dotnames %in% names(mc_args)]
  if ("diagnostics" %in% conflicts) {
    mc_args[["diagnostics"]] <- NULL
    conflicts <- conflicts[conflicts != "diagnostics"]
  }
  if (length(conflicts) > 0) {
    stop("The following arguments conflict with mc's properties: ", conflicts)
  }
  mc_args <- c(mc_args, list(...))
  # might as well use it for mc_args
  mc_args[["verbose"]] <- verbose

  hooks_ <- c(hooks_pnchunk_default, hook_set_seed_factory(mc, rnginfo_after = FALSE))
  if (stop_if_unequal) {
    # put 'stop' hook first, so it stops before progress message "starting sims # ..."
    # (referring to the *next*).
    # Unfortunately it would be nice to have this *after* the hook that displays
    # partial results, but it's hard to put it after that an before the "starting sims" hook.
    hooks_ <- c(hook_stop_if_not_subset(mc), hooks_)
  }
  mc_args[["hooks_pnchunk"]] <- hooks_
  hooks_dgpp_ <- list()
  # todo: also make a dgpp_to_poi hook analog to 'hook_stop_if_not_subset'. Can make a big difference in some cases.
  if (reproduce_poi) {
    hooks_dgpp_ <- c(hooks_dgpp_, hook_set_seed_factory_dgpp(mc))
  }
  mc_args[["hooks_ba_dgpp_to_poi"]] <- hooks_dgpp_


  # todo: could reduce duplication in code sharing with mc_extend, either
  #       with a helper function or some other way to share code.

  # could be NULL (if not specified), TRUE, or FALSE
  store_globals_from_dots <- list(...)[["store_globals"]]
  #
  if (is.null(store_globals_from_dots)) {
    # This way we are robust against changes to the default of argument
    # "store_globals" of mc_run(), and change of argument name (it will
    # give an error).
    store_globals <- get_default_arg(mc_run, "store_globals")
  } else {
    store_globals <- store_globals_from_dots
  }

  # TODO: document all this to user. Maybe store in a roxygen function
  #       and @eval it to share explanation with mc_extend.
  # If restore_globals == TRUE, globals will not be correctly identified and
  # collected because of the attach(), so we just copy them from original MC.
  # This is also faster since collection of globals can be slow in some cases.
  copy_globals <- FALSE
  if (store_globals && restore_globals && globals_were_stored(mc)) {
    copy_globals <- TRUE
    mc_args[["store_globals"]] <- FALSE
  }

  # DBG
  # mc_args[["parallel"]] <- "future_lapply"

  if (restore_globals) {
    did_attach_globals <- do_attach_globals(mc)
  }
  mcnew <- do.call("mc_run", mc_args)
  if (restore_globals && did_attach_globals) {
    do_detach_globals()
  }

  if (copy_globals) {
    attr(mcnew, "globals") <- attr(mc, "globals")
    # note that attr(mcnew, "core_seed") can be different from attr(mc, "core_seed").
    # e.g., for default pnhash policy it doesn't matter what the seed it.
    # TODO: But for other pnhash policies, should we copy that attribute?
  }

  # todo:  use hook to give error if not equal as soon as possible instead of waiting for all of the sims of a pn-chunk to finish. i.e., check after every e.g., 10 sims?:
  # ^^^ we do stop and check after each pn-chunk, but not after each X simulations.
  #     For that, we might need to do some magic with progressr (?).

  if (!isTRUE(all.equal(mc, mcnew, verbose = verbose))) {
    rds_file <- tempfile(fileext = ".Rds")
    saveRDS(mcnew, file = rds_file)
    stop("The MC files are not equal.", "\n",
         "The new MC is saved to the following file that will be deleted when R exits: ", rds_file)
  }

  # todo document somewhere: what if a hook uses the seed... that's why we need to set the seed for *each* pn-chunk (?) (and as the last hook).

#  mcnew <- mc_run(dgp_params = dgp_params_, nvec = nvec_, dgp, dgpp_to_poi = NULL, statistic, nsims, diagnostics = NULL, partial_results = NA, stat_knowledge = NULL, oloop_is_p = FALSE, parallel = NULL, verbose = 1, hooks_pnchunk = list(hook_print_partial, hook_progress_message, hook_disable_other_parallelizations)) {

  # return the new one. Although they are "equal", they are not
  # identical (e.g., system.time and other info in the attributes).
  return(mcnew)
}


# alternative design: we could have one_sim() log the .Random.seed it starts with and then return that. Then, we could e.g., store just the .Random.seed from the last iteration of one_sim(). This isn't ideal, but I log it here just in case we need to change designs. /sk.
#
#
## much of this function is copied from mc_reproduce(). todo: consider sharing some of the code.
#
#' @title Extend an existing MC object with more simulations
#' @description mc_extend takes the result of a previous MC object created by mc_run(), and runs more simulations.
#' @export
#' @param mc The MC object to create an extension from.
#' @param nsims_additional The number of simulations to extend by. This can be a vector. By default, we use the same number of simulations as the original 'mc' argument used (i.e., the number of simulations after extension will be double that of the original).
#' @eval param_restore_globals()
#' @eval param_verbose()
#' @param ... Arguments that will be passed to the call to mc_run() (e.g., 'parallel = TRUE').
mc_extend <- function(mc, nsims_additional = get_nsims_vec(mc), restore_globals = TRUE, verbose = 1, ...) {
  mc <- resolve_user_mc_arg(mc, verbose = verbose)
  # TODO: first run mc_reproduce() with nsims_additional = 1?
  #        or at least give an option to do this?

  mc_args <- get_core_mc_args(mc)

  dots_ <- list(...)
  dotnames <- names(dots_)
  conflicts <- dotnames[dotnames %in% names(mc_args)]
  # We allow diagnostics to be different, because that does not affect the stat results.
  # For a use case, suppose a referee asks for both more simulations and different
  # diagnostics. We can do this all at once.
  if ("diagnostics" %in% conflicts) {
    mc_args[["diagnostics"]] <- NULL
    conflicts <- conflicts[conflicts != "diagnostics"]
  }
  if (length(conflicts) > 0) {
    stop("The following arguments conflict with mc's properties: ", conflicts)
  }
  mc_args <- c(mc_args, list(...))
  if (!all(nsims_additional == get_nsims_vec(mc))) {
    # ^ this condition is just for readability. Could be removed since it's fine
    # to always set.
    mc_args[["nsims"]] <- nsims_additional
  }
  mc_args[["verbose"]] <- verbose
  #
  # EXPIRE now that we use init_seed arg, we don't need to set the seed like this.
  # hooks_ <- c(hooks_pnchunk_default, hook_set_seed_factory(mc, rnginfo_after = TRUE))
  # mc_args[["hooks_pnchunk"]] <- hooks_
  #
  hooks_dgpp_ <- list()
  # TODO: reproduce POI? Or just copy POI maybe??? arg?
  # todo: also make a dgpp_to_poi hook analog to 'hook_stop_if_not_subset'. Can make a big difference in some cases.
  reproduce_poi <- TRUE
  if (reproduce_poi) {
    hooks_dgpp_ <- c(hooks_dgpp_, hook_set_seed_factory_dgpp(mc))
  }
  mc_args[["hooks_ba_dgpp_to_poi"]] <- hooks_dgpp_
  # DBG
  # mc_args[["parallel"]] <- "future_lapply"
  mc_args[["init_seeds"]] <- get_before_seeds_from_mc(mc)
  store_globals_from_dots <- list(...)[["store_globals"]]
  if (is.null(store_globals_from_dots)) {
    store_globals <- get_default_arg(mc_run, "store_globals")
  } else {
    store_globals <- store_globals_from_dots
  }
  copy_globals <- FALSE
  if (store_globals && restore_globals && globals_were_stored(mc)) {
    copy_globals <- TRUE
    mc_args[["store_globals"]] <- FALSE
  }
  if (restore_globals) {
    did_attach_globals <- do_attach_globals(mc)
  }
  mcnew <- do.call(do_mc_run, mc_args)
  if (restore_globals && did_attach_globals) {
    do_detach_globals()
  }
  if (copy_globals) {
    attr(mcnew, "globals") <- attr(mc, "globals")
  }
  mc_merged <- mc_merge(mc, mcnew, verbose = verbose)
  return(mc_merged)
}
