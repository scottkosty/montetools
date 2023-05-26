# TODO: overload subset()? i.e., create a method for subset.montecarlo ?
#' Subset on stats
#'
#' Note that the "sys_stats" simulation time is no longer valid after using
#' mc_stats_subset().
#'
#' @export
#' @eval param_mc()
#' @param remove_these The names of the statistics to remove. Cannot be combined with 'keep_these'.
#' @param keep_these The names of the statistics to keep. Cannot be combined with 'remove_these'.
mc_stats_subset <- function(mc, remove_these, keep_these) {
  if (missing(remove_these) + missing(keep_these) != 1) {
    stop("Exactly one of 'remove_these' and 'keep_these' must be specified.")
  }

  validate_montecarlo(mc)

  for (p_idx in seq_along(mc)) {
    p_ <- names(mc)[[p_idx]]
    for (n_idx in seq_along(mc[[p_idx]])) {
      n_ <- names(mc[[p_]])[[n_idx]]
      chunk_m <- mc_stats_m(mc, pn_pair = c(p = p_, n = n_))
      # todo: does this take time? if so, can probably find a different way.
      # TODO: use get_statnames() instead? first add tests before changing.
      stat_names <- unique(rownames(chunk_m))
      if (!missing(keep_these)) {
        not_there <- !(keep_these %in% stat_names)
        if (any(not_there)) {
          stop("The following do not exist (and thus cannot be kept): ",
               keep_these[not_there])
        }
      } else if (!missing(remove_these)) {
        not_there <- !(remove_these %in% stat_names)
        if (any(not_there)) {
          stop("The following do not exist (and thus cannot be removed): ",
               remove_these[not_there])
        }

        keep_these <- stat_names[!(stat_names %in% remove_these)]
        # todo: allow this? Maybe user just wants to store the auxiliary information?
        if (length(keep_these) == 0) {
          stop("All statistics were removed. Other methods will not work.")
        }
      } else {
        # This should be caught in the preamble sanity checks, but we include
        # an extra check here.
        stop("We currently do not support both 'keep_these' and 'remove_these'.")
      }

      chunk_replacement <- chunk_m[stat_names %in% keep_these, , drop = FALSE]
      mc_stats_m(mc, c(p = p_, n = n_)) <- chunk_replacement
    }
  }
  return(mc)
}


do_combine <- function(main, secondary, verbose = 1) {
  check_these_functions <- c("dgpp_to_poi", "dgp")
  for (func_name in check_these_functions) {
    func_main <- get_attr(main, func_name)
    func_secondary <- get_attr(secondary, func_name)
    stopifnot(is.function(func_main), is.function(func_secondary))
    if (!funcs_are_equal(func_main, func_secondary)) {
      warning("The following function is not the same for both 'main' and 'secondary': ",
              func_name, "\n", "We will use the version from 'main', but if there are", "\n",
              "important differences mc_reproduce() might fail.")
    }
  }

  mcret <- main
  # todo: it is a bit strange that "statistic" is singular, where as the user arg
  #       is plural. But it is because "statistic" is internal. "statistics" (the user arg)
  #       can be a list, whereas "statistic" is always the same.
  #       Maybe rename things to make this distinction clearer though?
  statistic_main <- get_attr(main, "statistic")
  statistic_secondary <- get_attr(secondary, "statistic")
  stopifnot(is.function(statistic_main), is.function(statistic_secondary))
  stat_funcs_are_same <- funcs_are_equal(statistic_main, statistic_secondary)

  statm_names_are_same <- identical(get_statnames(main), get_statnames(secondary))
  if (stat_funcs_are_same) {
    merge_sims_of_same_stats <- TRUE
    merge_stats_of_same_sims <- FALSE
    if (!statm_names_are_same) {
      # maybe the functions call third party functions, and that somehow
      # affects the stat names? Seems unlikely someone would do this in
      # practice though.
      if (!is.null(get_statnames(main)) && !is.null(get_statnames(secondary))) {
        # If one is null, it is likely because the stat_m's have 0 rows.
        # comment out the condition and run the tests to get an example.
        stop("Problem: stat functions are the same, but not stat names (from the 'stat_m's). How can this happen? Please send me an example file and explain your use case. Thank you!")
      }
    }
  } else {
    if (statm_names_are_same) {
      if (verbose >= 1) message("The stat functions are not the same, but the names from 'stat_m' are. Perhaps there are minor differences, such as renamings, that do not affect the output? Proceeding as if they are equivalent in functionality, but if not this would cause problems for reproducing, e.g., with mc_reproduce().")
      attr(mcret, "statistic") <- statistic_main
      merge_sims_of_same_stats <- TRUE
    } else {
      if (verbose >= 1) message("'stat_m' names are different, so assuming different statistics")
      merge_sims_of_same_stats <- FALSE
      merge_stats_of_same_sims <- TRUE
      attr(mcret, "statistic") <- list(statistic_main, statistic_secondary)
    }
  }

  # we currently support merging either, but not both.
  # todo: for merging diff stats, check that core seed is the same?
  #
  # 'merge_sims' happens if, e.g., extending with mc_extend().
  stopifnot(merge_sims_of_same_stats + merge_stats_of_same_sims == 1)

  # TODO: clean up the following comments.
  # todo: does this handle non-common elements?
#  test123 <- merge.list(main, secondary)
  # loop through common pairs and combine
#  all_pairs <- get_pn_pairs(X_outer)
  #
  for (p in names(secondary)) {
    if (p %in% names(main)) {
      for (n in names(secondary[[p]])) {
        if (n %in% names(main[[p]])) {
          # pn is in common -> merge

          # TODO: also need to merge aux, and sys_stats, etc.
          stats_main <- mc_stats_m(main, c(p = p, n = n))
          stats_secondary <- mc_stats_m(secondary, c(p = p, n = n))
          if (nrow(stats_secondary) == 0) {
            rbinded <- stats_main
          } else if (nrow(stats_main) == 0) {
            rbinded <- stats_secondary
          } else {
            if (merge_sims_of_same_stats) {
              # we are likely "extending" (e.g., using mc_extend()) an MC with same stat.
              rbinded <- rbind(stats_main, stats_secondary)
            } else {
              stopifnot(merge_stats_of_same_sims)
              # different stats
              #
              # Need to interleave because montetools depends on that
              # structure. e.g., even get_nsims_vec() and get_nstats() probably needs
              # the consistent structure.
              # Similarly, e.g., when comparing first couple of sims, we need correct
              # ordering.
              # TODO: rename "rbinded"
              rbinded <- do_interleave(mat1 = stats_main, mat2 = stats_secondary)
            }
          }
          mc_stats_m(mcret, c(p = p, n = n)) <- rbinded
          # todo: document this somewhere: this is necessary for a double extend to pass.
          # see the test about "mc_extended_x2"
          seed_last_secondary <- get_attr(secondary[[p]][[n]], "seed_last")
          if (!identical(seed_last_secondary, list())) {
            # this happens if secondary had nsims = 0.
            attr(mcret[[p]][[n]], "seed_last") <- seed_last_secondary
          }
        } else {
          # p is in common but not nsims for this p -> just add
          # todo: give verbose message.
          mcret[[p]][[n]] <- secondary[[p]][[n]]
        }
      }
    } else {
      mcret[[p]] <- secondary[[p]]
    }
  }
  return(mcret)
}


#' Combine/merge MC objects, matching on (n, phash) pairs.
#'
#' They can be any combination of MC objects or file names of .Rds files (each containing an MC object).
#' The first argument determines the "main" MC, whose attributes and logged system details will be used in the returned merged MC object; such elements of the other MC objects will not be preserved.
#'
#' @export
#' @param ... The MC objects to be combined.
#' @eval param_verbose()
mc_merge <- function(..., verbose = 1) {
  mcs_l <- list(...)
  stopifnot(length(mcs_l) >= 2)
  mcs_l <- lapply(mcs_l, resolve_user_mc_arg, verbose = verbose)
  # todo: it does matter which one we use to initialize mc_merged.
  #       Those are the attributes that will be used.
  #       In future, should state explicitly "order doesn't matter" in terms of attributes. Also TEST this. the internal order that the sims are stored could be diff (maybe just make this the same as well? Use some arbitrary sorting? hmmm not sure).
  mc_merged <- mcs_l[[1]]
  # todo: each attribute could store a *list*. Then we could just keep track of which elements correspond to which rows of the stats_m.
  if (verbose >= 1) {
    # TODO: at least for POI we can check if they are different. check param_triples?
    message("ignoring some attributes and auxiliary elements like CPU stats. And also the realized POI. Just using the first MC attributes.")
  }
  validate_montecarlo(mcs_l[[1]])
  for (idx in 2:length(mcs_l)) {
    validate_montecarlo(mcs_l[[idx]])
    mc_merged <- do_combine(mc_merged, mcs_l[[idx]], verbose = verbose)
  }
  return(mc_merged)
}


# todo: generalize to allow for more arguments
#merge_mc_lists <- function(...) {
#}
merge_mc_lists <- function(l1, l2) {
  # the following is not sufficent.
  # It will not give the correct results if names(l1) and names(l2)
  # intersect. i.e., it will not merge the sub-lists with the same
  # name.
  # merged_l <- c(l1, l2)

  if (length(l1) > 0 && is.null(names(l1))) {
    stop("merge does not work well if no names. fix merge_mc_lists for this case?")
  }
  if (length(l2) > 0 && is.null(names(l2))) {
    stop("merge does not work well if no names. fix merge_mc_lists for this case?")
  }

  merged_l <- merge.list(l1, l2)
  if (anyDuplicated(names(merged_l))) {
    stop("there should not be duplicates. Indicates problem in montetools.")
  }

  l1_is_trivial <- is.null(l1) || length(l1) == 0
  l2_is_trivial <- is.null(l2) || length(l2) == 0

  if (l1_is_trivial) {
    attributes(merged_l) <- attributes(l2)
  } else if (l2_is_trivial) {
    attributes(merged_l) <- attributes(l1)
  }


  # We don't currently enforce l1 and l2 to be MCs because maybe they are MCs being built
  # in progress.
  # If one has class "montecarlo", also assign class to resulting list.
  if ((length(l1) > 0 && !is.montecarlo(l1)) || (length(l2) > 0 && !is.montecarlo(l2))) {
    if (is.montecarlo(merged_l)) {
      # if caller wants result to be an MC, needs to ensure l1 and l2 are both MCs.
      class(merged_l) <- "list"
    }
  }
  return(merged_l)
}
