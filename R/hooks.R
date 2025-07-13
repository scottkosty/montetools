#' @title MC hook for reporting progress
#'
#' @description
#' Provide progress on what pn pair is currently being processed. If POI label
#' is NA, then the POI label is not output. This is useful when there is only one
#' DGP param, and not an intuitive label for the POI.
#'
#' @export
#' @param mc_part_done An MC object containing the pn-pairs of finished simulations so far.
#' @param mc_args_next The arguments that will be used for the next pn-chunk.
#' @param user_args The arguments given to mc_run() by the user.
#' @param pn_pair_next The pn-pair whose simulations will be run next.
hook_progress_message <- function(mc_part_done, mc_args_next, user_args, pn_pair_next) {
  if (user_args$verbose <= 0) {
    return(NULL)
  }


  if (!is.null(mc_args_next)) {
    message("")
    poi_label <- mc_args_next$ptup[["label"]]
    msg_beginning <- "processing "
    msg_end <- "..."
    msg_ <- paste0(msg_beginning,
                   format_pn_label_str(p_label = mc_args_next$ptup[["label"]], n = mc_args_next$n, nsims = mc_args_next$one_nsims),
                   msg_end
                   )
    message(msg_)
  }
}


#' @title MC hook for showing wall clock duration of previous pn-chunk
#'
#' @description
#' Displays the duration it took for previous pn-chunk to run the simulations.
#'
#' @importFrom lubridate seconds_to_period
#' @export
#' @param mc_part_done An MC object containing the pn-pairs of finished simulations so far.
#' @param mc_args_next The arguments that will be used for the next pn-chunk.
#' @param user_args The arguments given to mc_run() by the user.
#' @param pn_pair_next The pn-pair whose simulations will be run next.
hook_print_duration <- function(mc_part_done, mc_args_next, user_args, pn_pair_next) {
  if (user_args$verbose <= 0) {
    return(NULL)
  }

  if (!is.null(mc_part_done)) {
    pn_pair_most_recent <- get_pn_pair_most_recent(mc_part_done)
    p_ <- pn_pair_most_recent[["p"]]
    n_ <- pn_pair_most_recent[["n"]]

    time_seconds <- get_attr(mc_part_done[[p_]][[n_]], "sys_stats")[["systemtime"]][["elapsed"]]
    time_print <- seconds_to_period(round(time_seconds))
    # For non-default formatting, see here:
    # https://stackoverflow.com/a/27313681
    message("Duration it took this pn-chunk: ", time_print)
  }

  # todo: accept this an an arg? Or support an option (?). Maybe coauthors want to use the same code but
  #       have different displays? (this could be accomplished without options though, e.g., by conditioning
  #       on hostname.
  # Do not display seconds by default.
  time_format <- "%a %d %b %H:%M"

  message()
  message("Current time is: ", format(Sys.time(), time_format))

}


#' the partial result files are useful for several reasons:
#' - if your computer loses power during a long simulation
#' - if your simulation run is more computationally intense than you realize and because of RAM
#'   R exits.
#' - if you want to view partial result tables (e.g., a PDF table can be automatically generated).
#' - ...
#' The backup mechanism relying on partial results is robust to crashes during the write process. That is, the backup file saved after the previous simulation completes will only be overwritten if...
#' 
#'


#' @title hook for saving partial results
#'
#' @description
#' The partial results file is updated to save partial MC results after each pn-chunk finishes (i.e., all of the simulations for a particular combination of a dgp_param and n). See details for the specifics of the partial results. After all simulations succeed, the file is deleted so you may want to immediatedly save the return of mc_run(). If you are concerned about an error after the last simulation (e.g., an error from a hook that comes after this hook), you can set the argument 'rm_if_no_error' to 'FALSE'.
# todo: use 'rm_if_no_error = FALSE' to check: are the partial MC file and final outer_l.Rds equivalent? If not, at least document their differences. Add tests accordingly.
#'
#' @param filename The filename to use. You may choose to save the file to a directory with cloud storage (e.g., ownCloud, Dropbox) if you want robustness respect to your computer not booting after a crash.
#' @param rm_if_no_error If all simulations succeed, delete the partial MC file. If you leave this as TRUE, you may want to save the return of mc_run() as soon as it succeeds.
#' @param snapshot_dir An optional snapshot directory, i.e., the return of mc_archive_new(). This feature is useful if you want to save the partial MC to the snapshot directory rather than the current working directory.
#'
#' @export
gen_hook_save_partial <- function(filename = "mc-bu.Rds", rm_if_no_error = TRUE, snapshot_dir = NA) {
# not indenting for simplicity
hook_save_partial <- function(mc_part_done, mc_args_next, user_args, pn_pair_next) {
  # Notes to user:
  # If the file is large, or you don't want to trigger constant Dropbox notifications,
  # you could save every k'th mc-pair instead of every one.
  # You could also send the file via scp if you prefer.

  if (is.na(snapshot_dir)) {
    bufile <- filename
  } else {
    table_partial_pdf_f <- file.path(snapshot_dir, "table-partial.pdf")
    table_partial_tex_f <- gsub(".pdf$", ".tex", table_partial_pdf_f)
    bufile <- file.path(snapshot_dir, filename)
  }

  if (is.null(mc_part_done) && file.exists(bufile)) {
    # todo: maybe read the file in and give info on which sims are available?
    # allow to rename? hmm no maybe just keep it simple.
    choices <- c(yes = "Yes (overwrite)", no = "No (and exit)")
    msg <-  paste0("Partial MC file (", bufile, ") exists from previous run. Overwrite this previous file to create new partial MC file from fresh start? \n\nIf you want to read in the partial MC file and use those results, exit now (select 'no') and set the 'partial_results' argument of mc_run() to the existing partial MC file name. If you wish to keep the partial results for using at a later point, exit now (select 'no') and rename the file or set the 'filename' argument of gen_hook_save_partial() to a file that does not exist yet. If you do not want to keep the partial results in the existing partial MC file, you may wish to overwrite the file by selecting 'yes'.")
    answer_idx <- menu(choices, title = msg)
    if (answer_idx == 0) {
      # menu() also lets the user enter "0"
      stop("Must enter one of the available choices.")
    }
    user_choice <- names(choices)[answer_idx]
    if (user_choice == "yes") {
      file.remove(bufile)
    } else if (user_choice == "no") {
      stop("Exiting. Please rerun again after renaming the partial MC file, or choose a different 'filename' argument to gen_hook_save_partial() or if you do not want the partial MC mechanism, do not include the hook.")
    }


  }

  # We now remove the partial MC file after last simulation. The idea is that an error in mc_run() is unlikely,
  # and even if it happens, the user can set things up (beforehand) to be able to recover outer_l with debugger() (TODO: document exactly how to set this up in this hook documentation. Also test an error in montetools after this to make sure user can recover).

  # An alternative would be to try to get fancy and run this chunk only if
  # mc_run() exits without error, but it's not clear this increase in
  # complexity is worth it. See: https://stackoverflow.com/a/13603499/1376404.
  if (rm_if_no_error && is.null(mc_args_next)) {
    if (user_args$verbose >= 2) {
      message("Removing partial MC file since all MCs are done: ", bufile)
      message("Also Removing partial table files.")
    }

    # file.remove() would give a warning if the file does not exist, so
    # instead we use file.remove_if_exists() because the user might manually
    # remove a temporary file (e.g., by mistake, or move to a different dir),
    # and we don't want to give an error (e.g., if warn = 2), or even a
    # non-serious warning since it does not cause a problem.
    #
    # Additionally, for table_partial_pdf_f, maybe LaTeX could not compile
    # a pdf because of a LaTeX error (or no LaTeX installed).
    file.remove_if_exists(bufile)
    #
    if (!is.na(snapshot_dir)) {
      file.remove_if_exists(table_partial_tex_f, table_partial_pdf_f)
    }

    # If we don't return, the chunk below will write the file again.
    return(NULL)
  }


  # save partial MC file.
  # TODO: use temp file for this initial save instead!
  bufile2 <- paste0(bufile, "2")
  # this is the intermediate file...
  bufile_old <- paste0(bufile, "_old")
  if (file.exists(bufile)) {
    # protect against (unlikely) crashes during the save operation so at least the
    # previous partial MC file is preserved.
    saveRDS(mc_part_done, file = bufile2)
    file_mv(from = bufile, to = bufile_old)
    file_mv(from = bufile2, to = bufile)
    file.remove(bufile_old)
  } else {
    saveRDS(mc_part_done, file = bufile)
  }
  if (!is.na(snapshot_dir) && !is.null(mc_part_done)) {
    mc_table(diags_or_mc = mc_part_done, output_file = file.path(snapshot_dir, "table-partial.pdf"))
  }
}
return(hook_save_partial)
}


#' Hook to print partial results
#' If verbose >= 1 and user provides diagnostics, calculate diagnostics for pn-chunks as soon as they're available and print them.
#' @export
#' @param mc_part_done An MC object containing the pn-pairs of finished simulations so far.
#' @param mc_args_next The arguments that will be used for the next pn-chunk.
#' @param user_args The arguments given to mc_run() by the user.
#' @param pn_pair_next The pn-pair whose simulations will be run next.
#' @param nsims_dispatched The number of simulations dispatched so far (not necessarily completed).
hook_print_partial <- function(mc_part_done, mc_args_next, user_args, pn_pair_next, nsims_dispatched) {
  if (user_args$verbose <= 0) {
    return(NULL)
  }

  ## don't do anything *within* chunk.
  #if (!is.null(nsims_dispatched) && nsims_dispatched < 2) {
  #  return(NULL)
  #}

  if (length(mc_part_done) == 0) {
    if (user_args$verbose >= 2 && is.null(user_args$diagnostics)) {
      message("User did not specify argument 'diagnostics' so there will be no printing of partial results.")
    }

    return(NULL)
  }

  if (!is.null(user_args$diagnostics)) {
    # todo: use parallelization here? Need to factor out a centralized xapply() type function
    # that handles the special cases of pbmclapply and parLapply, etc. Maybe if parLapply just
    # use lapply here.
    # Does not make much of a difference, but might as well in case user happens to use a
    # diagnostics that takes time.
    # Note that this hook is run sequentially (i.e., montetools is not running any other code) so user can parallelize inside this hook.

    pn_pair_most_recent <- get_pn_pair_most_recent(mc_part_done)

    if (get_nsims(mc_part_done, pn_pair_most_recent) == 0) {
      message("nsims is 0.")
      return(NULL)
    }

    # todo: would be nice to "highlight" (diff color?) the
    #       cells from the most recently completed pn-chunk.

    if (length(user_args$dgp_params) > 1) {
      print_param_col_ <- "always"
    } else {
      print_param_col_ <- NA
    }

    print(mc_part_done,
          # otherwise it looks strange that the param col is not printed for
          # the first (few) chunks, and then it is printed.
          # Not a big issue, but looks better this way.
          print_param_col = print_param_col_
    )
    matrix_for_width <- mc_table(mc_part_done)

    # Used to do:
    # statnames <- get_statnames(mc_part_done)
    # but that doesn't work well if first pn-chunk all had errors (so all NA elements and we never
    # learn the stat names).

    # If user wants to print only the most recent results (this used to be the default
    # behavior of montetools).
    #
    #mc_only_most_recent <- mc_pnpairs_subset(mc_part_done, pn_pairs = list(pn_pair_most_recent))
    ## Used to use a lower-level function, diags_aggregator(), but better to use
    ## mc_diags() since higher-level.
    #mc_diags_ <- mc_diags(mc_only_most_recent)
    #print(mc_diags_)
    #
    # TODO: get rid of this divider thing?
    #for_indiv_print <- get_pn_diags_result(mc_diags_, pn_pair_most_recent)
    ## this is the matrix we will base the width calculation on
    ## (currently the most recently printed)
    #matrix_for_width <- for_indiv_print[[length(for_indiv_print)]]

    width_ <- width_of_printed_obj(matrix_for_width)
    # alternative: just take up whole width
    # width_ <- getOption("width")

    chunk_divider_str <- paste0(rep("-", times = width_))
    message(chunk_divider_str)
  }
}

# TODOSO: I don't want to export hook_disable_other_parallelizations as
#         a function. I want to export it as an *object*, i.e., so it
#         can be used as a (function-object) argument. Same for the other
#         hooks

#' @import RhpcBLASctl
#' @export
#' @title MC hook to disable other parallelizations
#' @description MC hook to disable other common parallelizations.
#' @param mc_part_done An MC object containing the pn-pairs of finished simulations so far.
#' @param mc_args_next The arguments that will be used for the next pn-chunk.
#' @param user_args The arguments given to mc_run() by the user.
#' @param pn_pair_next The pn-pair whose simulations will be run next.
hook_disable_other_parallelizations <- function(mc_part_done, mc_args_next, user_args, pn_pair_next) {
  if (is.null(mc_part_done) && !is.null(user_args$parallel)) {
    # Turn off parallelization by the "boot" package when use.
    # A user might have this option set in .Rprofile.
    boot.ncpus_backup <- getOption("boot.ncpus")
    options(boot.ncpus = 1)

    #  https://github.com/bioFAM/MOFA/issues/21
    OMPThreads_backup <- omp_get_max_threads()
    BLASThreads_backup <- blas_get_num_procs()
    nThreads <- 1
    omp_set_num_threads(nThreads)
    blas_set_num_threads(nThreads)

    # restore options when mc_run() (the parent function) exits
    # This happens if there is an error or if the function exits normally.
    on_parent_exit({
      options(boot.ncpus = boot.ncpus_backup)
      omp_set_num_threads(OMPThreads_backup)
      blas_set_num_threads(BLASThreads_backup)
    })
  }
}


#' Produce a hook to set seed and rng
#'
#' This hook factory is used by mc_reproduce
#'
#' @export
#' @eval param_mc()
#' @param rnginfo_after  If TRUE, set the RNG state to the RNG state from *after* the MC object was run (useful for extending the MC), otherwise use the rnginfo from before the MC object was run (useful for reproducing).
hook_set_seed_factory <- function(mc, rnginfo_after = FALSE) {
  function(mc_part_done, mc_args_next, user_args, pn_pair_next) {
    if (is.null(mc_part_done)) {
      if (!exists(".Random.seed")) {
        set.seed(NULL)
      }
    }

    if (!is.null(mc_args_next)) {
      if (rnginfo_after) {
        seed_info <- get_rnginfo_after(mc, pn_pair = pn_pair_next)
      } else {
        seed_info <- get_rnginfo_before(mc, pn_pair = pn_pair_next)
      }
      restore_rng(seed_info)
    }
    return(NULL)
  }
}


# stop if the stat_m of current sims is not a subset of MC.
# (subset and not equal because might only do e.g., 1 sim).
# See mc_reproduce().
hook_stop_if_not_subset <- function(mc) {
  function(mc_part_done, mc_args_next, user_args, pn_pair_next) {
    if (!is.null(mc_part_done)) {
      pn_pair_most_recent <- get_pn_pair_most_recent(mc_part_done)
      p_ <- pn_pair_most_recent[["p"]]
      n_ <- pn_pair_most_recent[["n"]]

      stat_m_current <- mc_stats_m(mc_part_done, pn_pair = pn_pair_most_recent)
      stat_m_target <- mc_stats_m(mc, pn_pair = pn_pair_most_recent)[1:nrow(stat_m_current), , drop = FALSE]
      if (!isTRUE(all.equal(stat_m_current, stat_m_target))) {
        # todo: could have an argument "on_error" that specifies what to do.
        # document that the reason why it's not obvious is that mc_reproduce() gives an error
        #          so it's not obvious how to return something.
        # options: could drop user into a browser() prompt.
        #          could show a difference
        #          could store the MC object in options("mc_partial")
        #          could just document that user can do debugger().
        rds_file <- tempfile(fileext = ".Rds")
        saveRDS(mc_part_done, file = rds_file)
        stop("The following pn-pair is not equal: ", pn_pair_most_recent, "\n",
             "The partial MC is saved to the following file that will be deleted when R exits: ", rds_file)
      }
    }
    return(NULL)
  }
}


# todo: could implement hook_stop_if_not_equal (analog to
# hook_stop_if_not_subset). This would only be helpful when dgpp_to_poi() is
# very computationally intensive.
hook_set_seed_factory_dgpp <- function(mc) {
  # microop: could in theory check whether rng_before == rng_after. If so, just return function(...) NULL.
  # BUT, dgpp_to_poi() might *internally* reset the seed. (although not sure what a use case for that would be) (also, we could check whether each hook changes the seed). In the end,
  # it's not worth it to do this.
  function(dgpp_prev, dgpp_next) {
    if (is.null(dgpp_prev)) {
      if (!exists(".Random.seed")) {
        set.seed(NULL)
      }
      rng_info <- rng_snapshot()
      # for hook_set_seed_factory_dgpp, the parent function refers to gen_param_tuples(). This is in fact
      # necessary (and one of the motivations for creating the separate gen_param_tuples() function.
      # We need to restore the seed *before* hook_set_seed_factory saves the seed. There are several
      # alternatives to this approach. For example, we could just have hook_set_seed_factory *not* store/restore
      # the seed and count on this hook to do it (and require this hook). Or, instead of using on_parent_exit()
      # here, we could just store/retrieve the seed info from options() on the last run of this hook.
      on_parent_exit({
        restore_rng(rng_info)
      })
    }

    if (!is.null(dgpp_next)) {
      seed_info <- get_dgpp_rnginfo_before(mc, dgpp_next)
      restore_rng(seed_info)
    }
    return(NULL)
  }
}
