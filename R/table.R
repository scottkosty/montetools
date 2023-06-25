# TODO: make a function "in_null" that accepts a "parameter"
# and returns true or false.
# pass as arg. That way can use hline, or dashed line, etc to separate rows that correspond to
# true null hypothesis from a false null hypothesis.
# i.e., where we are looking at size versus looking at power.


# TODO: would be nice to compare competing statistics.
# i.e., group by operator on *inner*! (?)
# allow both ways...


# TODO:  does this work also when statistics have duplicate entries? For example,
#        with different confidence intervals, often the point estimates are the same.
# Format entries of a column that potentially has duplicate entries. We
# represent duplicates by empty strings.
#
# This function is especially useful for "outer" columns, which typically contain duplicates.
#
# Example: if a 2x1 stat return (i.e., 2 1-d stats), and two diagnostics, will return
# something like:
#   c("diag1", "", "diag2", "")
#
# Similarly, for parameters e.g. when there is more than one diagnostics.
# Example: for one parameter and 3 diagnostics, we get something like this:
#   c("1 (-1.25)", "", "")
#
# This function is mostly a non-op on columns that don't have duplicates.
# For 1x1 stat return and 1x1 diagnostics, the return is identical to as.character(rowlevels)
# (as.character just removes the names of the elements)
format_column_output <- function(rowlevels, dataf) {

  nrows_within <- nrow(dataf)/length(rowlevels)

  retrnames <- c()
  for (uo in rowlevels) {
    retrnames <- c(retrnames, uo, rep("", times = nrows_within - 1))
  }

  return(retrnames)
}


mcp_to_table <- function(l, aggregators) {

  # this is not elegant. Will keep this until we figure out how to handle multiple aggregators
  for (n_idx in seq_along(l)) {
    l[[n_idx]] <- l[[n_idx]][[aggregators]]
  }


  cn <- colnames(l[[1]])
  if (is.null(cn)) {
    internal_error()
  }


  nstats <- nrow(l[[1]])
  ll1 <- list()
  for (cname in cn) {
    jopee <- do.call(cbind, lapply(l, '[', , cname, drop = FALSE))
    colnames(jopee) <- names(l)
    ll1 <- c(ll1, list(jopee))
  }
  # matrix of diagnostics for this p, across all nobs.
  p_mat <- do.call(rbind, ll1)

  retrnames <- format_column_output(cn, p_mat)

  nnn <- rownames(p_mat)
  rownames(p_mat) <- NULL

  # convert to data frame; otherwise, matrix would be converted to character.
  p_df <- as.data.frame(p_mat)
  p_df <- cbind(diag_internal_colname = retrnames, stat_internal_colname = nnn, p_df)

  return(p_df)
}

# TODO: when using knitr, does user need to wrap in
#   paste(abc, collapse = "\n")
# or does it work as is?
# If it needs the collapse, maybe give arg. Or make format = "knitr" that does it.

#' Document feature that it supports *partial* tables (e.g., if not all sims
#' done for all nobs or for all params).
#' @eval param_diags_or_mc()
#' @param output_file A vector of file names to output. Leave as NA (default) to print to the screen (useful when using knitr). The vector may have length greater than 1 if you want to output multiple formats. Currently, only ".tex" and ".pdf" are supported. If you specify a ".pdf" extension, you must have a TeX distribution installed (e.g., TeX Live or MiKTeX). If you don't have one installed, consider running tinytex::install_tinytex() in R, which installs a relatively lightweight (~250MB) TeX distribution.
#' @param colname_stat Set to NA to remove the column. By default, "stat.", or if number of statistics is 1 it is NA.
#' @param colname_diag Set to NA to remove the column. By default, "diag.", or if number of diagnostics is 1 it is NA.
#' @param colname_poi Set to NA to remove the column. By default, "param.", or if number of parameters is 1 it is NA.
#' @param escape If TRUE, escape special characters in LaTeX code. Also known as "sanitizing" the code.
#' @param aggregators What function should be used to summarize the diagnostics of each simulation (typically the default "mean").
#' @param display_nsims Can be "auto", "none", "row", or "multicol".
#' @param rounder The rounder to be used.
#' @eval param_verbose()
#' @importFrom plyr rbind.fill
#' @importFrom xtable xtable
#' @importFrom tinytex tinytex_root
#' @export
#' @details If a .pdf is specified in output_file, if the compilation fails an error is not given.
mc_table <- function(diags_or_mc, output_file = NA, aggregators, colname_poi = "parameter", colname_stat, colname_diag,
                     # Default to FALSE for now, since allows LaTeX in column names.
                     #
                     # A default to TRUE might be more user-friendly.
                     # For example, a diag with name "covers_l" will
                     # lead to a LaTeX error.
                     # This can be addressed by using "covers\\_l" or (what we currently do)
                     # by using "covers.l" (which is more space-efficient anyway), but
                     # it is one extra complication for the user.
                     # After we add other table backends, we should revisit.
                     escape = FALSE,
                     # TODO: can be "auto", "none", "row", "multicol"
                     display_nsims = "row",
                     rounder = gen_rounder_fixed(digits = 3),
                     verbose = 1
                     ) {

  # this chunk should be near the top, so that, e.g., nsims_vec below is valid.
  mcdiags <- resolve_diags_or_mc_arg(diags_or_mc = diags_or_mc)

  # TODO: should aggregators be passed as an attribute?
  if (missing(aggregators)) {
    aggregators <- "mean"
  }
  if (length(aggregators) > 1) {
    # todo: eventually want to have an option to print one table with multiple aggregators, but not there yet.
    stop("We currently only support one aggregator at a time.")
  }

  nsims_vec <- get_nsims_vec(mcdiags)
  if (display_nsims == "auto") {
    if (length(unique(nsims_vec)) > 1) {
      display_nsims <- "row"
    } else {
      display_nsims <- "multicol"
    }
  }


  # TODO: use a get method to create abstract interface?
  # shouldn't matter which aggregator, but generalize (by using it here) just in case.
  nstats <- nrow(mcdiags[[1]][[1]][[aggregators]])
  ndiags <- ncol(mcdiags[[1]][[1]][[aggregators]])
  nparams <- get_nparams(mcdiags)

  empty_line_str <- '\\\\'

  # these can have length > 1 if multiple lines. e.g.,
  #sep_param_lines <- c(empty_line_str, empty_line_str)
  # TODO: give example with hline!
  sep_diag_lines <- NA
  sep_param_lines <- NA

  # explain why can't combine both... need to be added at different spots.
  # number of centimeters
  sep_diag_cm <- NA
  sep_param_cm <- NA

  sep_diag_lines <- NA
  sep_param_lines <- '\\hline'

  # example
  sep_diag_lines <- NA
  sep_param_lines <- NA
  if (nstats == 1) {
    sep_diag_cm <- 0
    sep_param_cm <- 0.2
  } else {
    sep_diag_cm <- 0.2
    sep_param_cm <- 0.4
  }


  #example
  ## todo: document that you need to add \usepackage{arydshln} to preamble
  #sep_diag_lines <- '\\hdashline'
  #sep_param_lines <- '\\hline'
  #sep_diag_cm <- NA
  #sep_param_cm <- NA

  # TODO: ask why this doesn't work. how to get relative to text height?
  #relattempt <- 0.5
  #sep_param_latex <- paste0('\\vspace{', relattempt, '\\the\\baselineskip}')

  if (sep_param_cm != 0) {
    sep_param_latex <- paste0('\\vspace{', sep_param_cm, 'cm}')
  } else {
    sep_param_latex <- ""
  }
  if (sep_diag_cm != 0) {
    sep_diag_latex <- paste0('\\vspace{', sep_diag_cm, 'cm}')
  } else {
    sep_diag_latex <- ""
  }


  # TODO: without this, I get unwanted lines after the tabular, and the document doesn't
  #       combine. Will take a while to debug. For now, just set these to NA.
  if (ndiags == 1) {
    sep_diag_cm <- NA
    sep_param_cm <- NA
  }


  if (missing(colname_stat)) {
    if (nstats == 1) {
      colname_stat <- NA
    } else {
      colname_stat <- "stat."
    }
  }

  if (missing(colname_diag)) {
    if (ndiags == 1) {
      colname_diag <- NA
    } else {
      colname_diag <- paste0("diag. ", "(", aggregators, ")")
    }
  }

  param_labels <- sapply(X = get_param_tuples(mcdiags), `[[`, "label")
  # sanity check
  stopifnot(length(param_labels) == nparams)
  #
  if (missing(colname_poi)) {
    if (nparams == 1) {
      colname_poi <- NA
    } else {
      colname_poi <- "param."
    }
  }

  mcdiags <- apply_diag_labels_to_colnames(mcdiags, format = "latex")

  hat <- lapply(X = mcdiags, FUN = mcp_to_table, aggregators = aggregators)

  scarf <- do.call(rbind.fill, hat)
  rownames(scarf) <- NULL

  if (!is.na(colname_poi)) {
    # better way to assign the name than this? hmmm
    noj <- data.frame(format_column_output(param_labels, scarf))
    names(noj) <- colname_poi
    scarf <- cbind(noj, scarf)
  }

  # todo: deduplicate?
  col <- "stat_internal_colname"
  if (!(col %in% names(scarf))) {
    stop("Internal error: can't find stat col. Please report.")
  }
  if (is.na(colname_stat)) {
    scarf[, col] <- NULL
  } else {
    names(scarf)[names(scarf) == col] <- colname_stat
  }
  col <- "diag_internal_colname"
  if (!(col %in% names(scarf))) {
    stop("Internal error: can't find diag col. Please report.")
  }
  if (is.na(colname_diag)) {
    scarf[, col] <- NULL
  } else {
    names(scarf)[names(scarf) == col] <- colname_diag
  }


  # TODO: use a get method
  ncol_nobs <- length(mcdiags[[1]])
  ncol_other <- ncol(scarf) - ncol_nobs

  nobs_idx <- (ncol_other + 1):ncol(scarf)

  # keywords: thousands separator, comma
  #
  # todo: In a quick test, this actually didn't make a difference in many
  #       situations. Document in which case it does make a difference.
  # Replace "10000" with "10{,}000" or 10,000 depending on "escape".
  # We could alternatively do this substitution after the call to xtable,
  # and thus always use {,}.
  if (escape) {
    big_mark_ <- ","
  } else {
    big_mark_ <- "{,}"
  }
  #
  nobs_orig <- colnames(scarf)[nobs_idx]
  nobs_formatted <- format(as.numeric(nobs_orig), big.mark = big_mark_)
  colnames(scarf)[nobs_idx] <- nobs_formatted


  # This breaks up by diag and dgpp I think.
  # could take argument to compare across dgpps. That would make sense.
  #
  # parts of the partition
  parts <- slice(seq_len(nrow(scarf)), nstats)
  # scarf2 will be converted to character on first modification. Need to keep scarf because
  # we want to keep feeding numeric.
  scarf2 <- scarf
  for (part in parts) {
    # convert to matrix, because easier for rounder to deal with.
    scarf2[part, nobs_idx] <- rounder(as.matrix(scarf[part, nobs_idx]))
  }
  scarf <- scarf2


  hlines <- c(-1, -1, 0, nrow(scarf), nrow(scarf))

  # Should we attempt to get rid of xtable since it is not actively developed?
  # Or support multiple table-packages as backends? As well as possibly some custom
  # routines written directly in montetools?
  # -- note that xtable also exports to HTML

  # User might not want to escape text if they want to use LaTeX commands,
  # e.g., \beta, in col names.
  # keyword: sanitize
  if (escape) {
    escape_text_ <- NULL
  } else {
    escape_text_ <- function(x) x
  }
  # Don't save to file here, because we need to hack in some stuff first
  xtable_print <- print(xtable(scarf, digits = 3), sanitize.text.function = escape_text_, include.rownames = FALSE, floating = FALSE,
               hline.after = hlines, print.results = FALSE,
               # we omit the comment in the same spirit as "reproducible builds"
               # (since it contains the xtable version number).
               comment = FALSE)
  # use "\\R" to match newline. Hopefully platform independent.
  #       https://stackoverflow.com/a/61448006/1376404
  #       https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
  # todo: double-check same result on Windows. Use hard-coded output tests.
  table_lines <- strsplit(xtable_print, "\\R", perl = TRUE)[[1]]

  #nobs_label <- "sample size"
  nobs_label <- "$n$"
  nobs_tex1 <- '\\multicolumn{'
  nobs_tex2 <- ncol_other
  # TODO: if display_nsims is "multicol", change '}{l}{}' to '}{c}{nsims = <nsims>}' below
  nobs_tex3 <- '}{l}{} & \\multicolumn{'
  nobs_tex4 <- ncol_nobs
  nobs_tex5 <- '}{c}{'
  nobs_tex7 <- '} \\\\'
  nobs_line <- paste0(nobs_tex1, nobs_tex2, nobs_tex3, nobs_tex4, nobs_tex5,
                     nobs_label, nobs_tex7)

  table_lines2 <- append(table_lines, nobs_line, after = 3)

  table_lines3 <- table_lines2
  first_stat_row <- 7

  # todo: make this recursive? support more depths?
  # we don't use "i" or "j" directly. the running var is "start_stat_row"
  # not "nparams - 1" because still to loop through last for the *inner* loop to do its thing.
  start_stat_row <- first_stat_row
  for (i in 1:(nparams)) {
    for (j in 1:(ndiags - 1)) {
      # last stat row *of this chunk*
      last_stat_row <- start_stat_row + (nstats - 1)
      if (!is.na(sep_diag_cm)) {
        table_lines3[last_stat_row] <- paste0(sep_diag_latex, table_lines3[last_stat_row])
      }
      if (!identical(sep_diag_lines, NA)) {
        table_lines3 <- append(table_lines3, sep_diag_lines, after = last_stat_row)
        # need to track the lines just added
        jump <- nstats + length(sep_diag_lines)
      } else {
        jump <- nstats
      }
      start_stat_row <- start_stat_row + jump
    }

    if (i == nparams) {
      next
    }

    # points start_stat_row to first row in next diag
    # we have to jump over the last diag, since we only added space in-between above
    start_stat_row <- start_stat_row + (nstats)

    before_stat_row <- start_stat_row - 1
    if (!is.na(sep_param_cm)) {
      table_lines3[before_stat_row] <- paste0(sep_param_latex, table_lines3[before_stat_row])
    }

    if (!identical(sep_param_lines, NA)) {
      table_lines3 <- append(table_lines3, sep_param_lines, after = before_stat_row)
      start_stat_row <- start_stat_row + length(sep_param_lines)
    }
  }

  if (display_nsims == "row") {
    if (length(unique(nsims_vec)) == 1) {
      nsims_ <- nsims_vec[[1]]
      # keywords: thousands separator, comma
      nsims_ <- format(nsims_, big.mark = big_mark_)
    } else {
      stop("Need to add feature to append nsims row when nsims is not constant.")
    }
    nsims_row1 <- "\\hline"

    nsims_row2a <- "nsims "
    # "-1" because it is *in-between* the columns.
    nsims_row2b <- paste(rep("& ", times = ncol_other - 1), collapse = "")
    nsims_row2c <- paste(rep(paste0(" & ", nsims_), times = ncol_nobs), collapse = "")
    nsims_row2d <- " \\\\"
    nsims_row2 <- paste0(nsims_row2a, nsims_row2b, nsims_row2c, nsims_row2d, collapse = "")

    #nsims_row2_alt <- paste0("nsims &  &  & 2 & 2 \\\\ "
    nsims_rows <- c(nsims_row1, nsims_row2)
    # the '- 3' is because of the hlines.
    table_lines3 <- append(table_lines3, nsims_rows, after = length(table_lines3) - 3)
  }

  table_lines_final <- table_lines3

  # TODO: look at how xtable and print.xtable() work. Make sure this works
  # smoothly with knitr
  # TODO: need to make "mc_table" class and print.mc_table method.

  na_idx <- which(is.na(output_file))
  tex_idx <- grep("\\.tex$", output_file)
  pdf_idx <- grep("\\.pdf$", output_file)
  png_idx <- grep("\\.png$", output_file)
  if (length(tex_idx) + length(pdf_idx) + length(png_idx) + length(na_idx) != length(output_file)) {
    stop("For the 'output_file' argument, we currently only support .tex and .pdf extensions, and 'NA' to return the code. Please open a feature request for other extensions.")
  }

  if (length(tex_idx) > 1 || length(pdf_idx) > 1 || length(png_idx) > 1) {
    stop("Currently we don't support multiple outputs of same format. Please open a feature request with details of your use case.")
  }

  if (length(tex_idx) == 1) {
    writeLines(table_lines_final, output_file[[tex_idx]])
  }

  # TODO: clean up this code.. should be something like <<if (ext == "pdf")>>

  # TODO: "png" is work in progress. I haven't documented it yet. Need to find
  #       a way to crop PDF (use pdfcrop?) and to convert to png.
  if (length(pdf_idx) == 1) {
    if (tinytex_root() == "") {
      stop("'tinytex' (the R package that handles LaTeX compilation) could not find a LaTeX installation. You can ask 'tinytex' to install one by running tinytex::install_tinytex() in R, but note that this will take (1) a few minutes and (2) a considerable amount of space (~250MB).")
    }

    tex_standalone <- make_table_standalone(tex_lines = table_lines_final)
    # Even if user additionally wants a .tex file, we still have to use
    # a temporary file because we need to compile a *standalone* .tex.
    tex_standalone_f <- tempfile(fileext = ".tex")
    if (verbose >= 2) {
      # Useful to debug issues where R uses a temp file name with non-standard
      # characters. e.g., I have seen the following temp file, which causes
      # problems due to the ~ in the user name (and thus the path):
      #   C:/Users/ANON~1/AppData/Local/Temp/RtmpWSFhmD/file4fbc2b2943bf.tex
      message("intermediate TeX file location: ", tex_standalone_f)
    }
    writeLines(tex_standalone, con = tex_standalone_f)
    pdf_file <- output_file[[pdf_idx]]
    if (verbose >= 2) message("output PDF file (future) location: ", pdf_file)
    try_tex_compile(tex_f = tex_standalone_f, pdf_file = pdf_file, verbose = verbose)
  }

  if (length(na_idx) == 1) {
    return(table_lines_final)
  }
}


make_table_standalone <- function(tex_lines) {
  tex_begin <- c("\\documentclass{article}", "\\begin{document}",
                 # no need for page number. It also makes it more difficult to automate
                 # cropping with pdfcrop.
                 "\\thispagestyle{empty}")
  tex_end <- "\\end{document}"
  tex_ <- c(tex_begin, tex_lines, tex_end)
  return(tex_)
}


# We wrap in "try" because we run this automatically in mc_archive(), and
# don't want to give an error if it fails.
# todo: could make that a user arg though to give them the choice, but
# we don't export this anyway currently.
#
# Not exporting because user should always use mc_table() (?).
#
#' @importFrom tools file_ext
#' @title MC table compilation
#' @param tex_f The path (with .tex extension) to the input file.
#' @param pdf_file The path (with .pdf extension) to the output file.
#' @param chdir Change directory to where 'tex_f' is and perform compilation there on the base filename. Otherwise, if 'tex_f' has, e.g., a tilde in it, LaTeX compilation could fail.
#' @eval param_verbose()
try_tex_compile <- function(tex_f, pdf_file, chdir = TRUE, verbose = 1) {
  # TODO: also accept args to pass to mc_diags ?

  if (chdir) {
    dir_for_tex_comp <- dirname(tex_f)

    wd_orig <- getwd()
    setwd(dir_for_tex_comp)
    # we change back below, but just in case something goes wrong in-between
    on.exit(setwd(wd_orig), add = TRUE)

    file_ <- basename(tex_f)
    pdf_file_ <- gsub("tex$", "pdf", file_)
  } else {
    file_ <- tex_f
    pdf_file_ <- pdf_file
  }

  silent_ <- (verbose <= 1)
  try_ <- try(
              tinytex::latexmk(file = file_, pdf_file = pdf_file_, install_packages = FALSE),
              silent = silent_
  )
  if (inherits(try_, "try-error") && verbose >= 1) {
    message("LaTeX compilation of the table failed.")
  }

  if (chdir) {
    setwd(wd_orig)
    from_ <- file.path(dir_for_tex_comp, pdf_file_)
    file.rename(from = from_, to = pdf_file)
  }

  return(NULL)
}
