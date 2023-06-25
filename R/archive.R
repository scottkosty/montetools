#' @title Make a snapshot of the repository of the current working directory.
#' @description This is useful for mc_archive_store.
#' @export
#' @importFrom git2r discover_repository repository_head diff sha is_detached repository status
repo_snapshot <- function() {
  maybe_repo <- discover_repository()
  is_git <- !is.null(maybe_repo)
  if (!is_git) {
    stop("repo_snapshot currently only works with Git. Please open a feature request on GitHub and describe your use case and setup. Thanks for your time!")
    # todo: if not git, store entire directory? give warning if over 5mb? how
    # could we even detect if this is the root directory of the project though?
    # Need more structure, or guidance from user.
  }

  repo <- repository()

  repository_head_ <- repository_head()
  sha_ <- sha(repository_head_)

  # need to cat() them to print nicely so linebreaks (\n) are converted.
  diff_notstaged <- diff(repo, as_char = TRUE, index = FALSE)
  diff_staged <- diff(repo, as_char = TRUE, index = TRUE)

  # We don't want to save untracked files (they could be huge), but the
  # presence (and contents) of untracked files could potentially affect
  # results, so we at least note their presence by storing 'git status'.
  #
  # could set staged to TRUE since we have the diff, but it's cheap so we
  # leave it at the default TRUE.
  status_ <- status()

  if (is_detached(repo)) {
    # TODO: merge with the other error asking to open an enhancement request.
    stop("Not sure what to do with a detached repository since the SHA might not be retrievable later on. Please open a feature request.")
  }

  ret_l <- list(sha = sha_, status = status_, diff_notstaged = diff_notstaged, diff_staged = diff_staged)
  return(ret_l)
}


#' @title Begin a new MC archive entry
#' @param archive_dir The directory where archive files are stored.
#' @eval param_verbose()
#' @details Returns the names of directories in the snapshot dir, sorted by mtime.
#' @export
mc_archive_ls <- function(archive_dir = getOption("montetools_archive_dir"), verbose = 1) {
  # Can't find it based on the name of the directory, because it's hard to
  # compare, e.g., compname1_5 to compname2_3.

  # todo: file.info() has a lot of helpful info. Allow different sorting criteria?

  dirs_and_times <- sapply(list.files(archive_dir, full.names = TRUE), FUN = file.mtime)
  sorted_ <- sort(dirs_and_times, decreasing = TRUE)
  sorted_paths <- names(sorted_)
  return(sorted_paths)
}


#' @export
#' @title Begin a new MC archive entry
#' @param archive_dir The directory where to store the archive files.
#' @param name The name of the snapshot directory. The actual directory will suffix "_1" to it, or if that directory exists will keep incrementing the suffix.
#' @eval param_verbose()
#' @details This function takes a snapshot of the repository state. The user should call this function at beginning of the script. That way, if the repository changes in the mean time (while the simulations are running), the snapshot taken at the beginning is still valid (i.e., an accurate representation of the repository at the time when the simulations were *started*).
#' @importFrom tinytex latexmk
mc_archive_new <- function(name = NA, archive_dir = getOption("montetools_archive_dir"), verbose = 1) {
  snapshot_name <- name

  if (is.null(archive_dir)) {
    archive_dir <- "montetools_archive"
  }

  # if not, things work fine, but when we return the path, it might be
  # "~/blah", and if user calls browseURL() on it, it doesn't work.
  # So might as well just expand now.
  #
  # If there is a case where this is not sufficient, consider normalizePath().
  archive_dir <- path.expand(archive_dir)

  if (!dir.exists(archive_dir)) {
    # todo: document that dir will be created *recursively*.
    dir.create(archive_dir, recursive = TRUE)
  }

  if (is.na(snapshot_name)) {
    snapshot_name <- paste(Sys.Date(),
                           # e.g., hostname, although ?Sys.info() says it is not
                           # reliable. However, the user can override this by
                           # providing the "snapshot_name" argument.
                           Sys.info()[["nodename"]],
                           sep = "_")
  }
  snapshot_dir <- file.path(archive_dir, snapshot_name)
  snapshot_dir <- file_increment_suffix(snapshot_dir)
  dir.create(snapshot_dir)
  if (verbose >= 1) {
    message("snapshot directory: ", snapshot_dir)
  }


  repo_snapshot_ <- repo_snapshot()
  saveRDS(repo_snapshot_, file = file.path(snapshot_dir, "repo_snapshot.Rds"))

  # return the path so if user wants they can open the directory, or
  # open the table inside the directory.
  # consistent with tinytex::latexmk
  return(snapshot_dir)
}


# mc_archive_stash, mc_archive_checkin, mc_archive_save, mc_archive_store
# mc_archive_commit
#' @export
#' @title Store an mc in the mc_archive_dir.
#' @eval param_mc()
#' @param snapshot_dir The snapshot directory, i.e., the return of mc_archive_new().
mc_archive_store <- function(mc, snapshot_dir) {
  file_ <- "mc.Rds"

  saveRDS(mc, file = file.path(snapshot_dir, file_))

  # EXPIRE: maybe accept dots and save those in a big list .Rds also?
  #       hmmm what are some use cases?
  #
  # We do not attempt to compile if no 'diagnostics' argument.
  if (!is.null(attr(mc, "diagnostics"))) {
    mc_table(diags_or_mc = mc, output_file = file.path(snapshot_dir, c("table.tex", "table.pdf")))
  }
}
