# https://github.com/HenrikBengtsson/progressr/discussions/121

#' @importFrom future future resolved value
future_for_with_hook <- function(X, FUN, future.seed, future.globals, hooks, hook_freq = 1L, verbose = 1) {
  # TODO: what to do with future.globals?
  rm(future.globals)

  n <- length(X)

  # This chunk was copied from future:::future_xapply().
  # Without this, a hardcoded test (expectedly) fails when putting
  # the following in setup.R:
  #   options(montetools_parallel_override = "future_for_with_hook")
  #
  # TODO: these aren't exported by 'future'. Make feature request?
  # If we use them directly (commented out below), R CMD check would give a note.
  # Instead, use a workaround to show that it is a known issue (so other R CMD check Notes stand out more).
  # https://stackoverflow.com/questions/63023526/unexported-object-imported-by-a-call-tsfeaturesscalets
  # oseed <- future:::next_random_seed()
  oseed <- utils::getFromNamespace("next_random_seed", "future")()
  # on.exit(future:::set_random_seed(oseed))
  on.exit(utils::getFromNamespace("set_random_seed", "future")(oseed))


  #p <- progressr::progressor(steps = n)
  fs <- vector("list", length = n)
  #
  # we want "-1" instead of 0 so that a hook can show something *before*
  # any resolved results.
  n_resolved_prev <- -1
  for (i in seq_len(n)) {
    fs[[i]] <- future({
      FUN(X[[i]])
    }, seed = future.seed[[i]])
    #p()

    ## For every n'th iteration, run hooks on partial results
    if (i %% hook_freq == 0) {
      # TODO: pass 'fs' to hook and have hook decide, based on 'sim_i',
      #       whether it wants to resolve it? Is resolving expensive? Need benchmarks.
      rs <- resolved(fs)
      # TODO: when is it useful to set "signal" arg of value() to FALSE?
      vs_resolved <- value(fs[rs])

      n_resolved <- sum(!sapply(vs_resolved, is.null))
      # This check is binding. It can happen that a new iteration is dispatched before
      # anything new is resolved.
      # TODO: ask on a new 'future' discussion what the technical reason for this is.
      #       Perhaps due to some kind of asynchronous overhead?
      # note to sk: this happens in jury-0fev-sim.R.
      if (n_resolved != n_resolved_prev) {
        # DBG
        # message("n_resolved: ", n_resolved)
        n_resolved_prev <- n_resolved
        if (verbose >= 2) message("Applying hooks within pnchunk, after dispatching sim number ", i, "...")
        args_ <- list(nits_dispatched = i, vs_resolved = vs_resolved)
        for (h in seq_along(hooks)) {
          hook_ret <- do.call(hooks[[h]], args_)
        }
        if (verbose >= 2) message("Applying hooks within pnchunk... Done.")
      } else {
        if (verbose >= 2) message("Skipping hooks within pnchunk because no new resolved.")
      }
    }
  }
  vs <- value(fs)

  # TODO: even if hook_freq is 1, it is called only after *dispatching*. we will not call the hooks while
  #       waiting for the last few workers to finish.
  # TODO: allow to hook on number *completed/resolved* ?? Make a new function for this?
  # would need to resolve often.. expensive?

  return(vs)
}
