# https://stackoverflow.com/a/2437259
# TODO: rename? is slice appropriate?
slice <- function(x, n) {
  N <- length(x)
  lapply(seq(1, N, n), FUN = function(i) x[i:min(i+n-1, N)])
}


# turns the inner list into the outer list
outside_in <- function(l) {
  if (length(l) == 0) {
    return(l)
  }
  # TODO: assert that all inner names are the same?
  #       we should not require that... right? add tests for if diff inner names.

  # We do not use create_empty_mc() because that assumes nobs_is_outer is TRUE.
  #
  # set up a skeleton just to get names of outer and inner list elements right.
  # Will copy over the contents later.
  # todo: assumes that [[1]] contains all of the names. This is true currently in montetools,
  #       but might want to generalize.
  ret_outer <- as.list(names(l[[1]]))
  names(ret_outer) <- names(l[[1]])
  ret_inner <- as.list(names(l))
  names(ret_inner) <- names(l)
  for (reti in seq_along(ret_outer)) {
    these <- c()
    for (j in seq_along(l)) {
      if (names(l[[1]])[[reti]] %in% names(l[[j]])) {
        these <- c(these, names(l)[[j]])
      }
    }
    ret_outer[[reti]] <- ret_inner[these]
  }

  for (o in names(l)) {
    for (i in names(l[[o]])) {
      ret_outer[[i]][[o]] <- l[[o]][[i]]
    }
  }

  # keep the names attribute but assign all others.
  # TODO: centralize this in a helper function.
  attributes(ret_outer) <- c(attributes(ret_outer), attributes(l)[names(attributes(l)) != "names"])
  return(ret_outer)
}


#' Construct a data.frame from RHS of formula
#'
#' the LHS variable of 'form' does not need to be available.
#' @importFrom stats as.formula update model.matrix
#' @param formula The formula object.
#' @param ... The data arguments.
#' @export
construct_rhs_df <- function(formula, ...) {
  # this way, formula can be something like the following (which is of type character):
  # formula <- "y ~ poly(l, degree = 2, raw = TRUE)*poly(k, degree = 2, raw = TRUE)"
  formula <- as.formula(formula)
  dataf <- data.frame(...)

  # TODO: add a bunch of tests, and document support of '.' in this way.
  # See Doug's example file.
  if (length(formula) == 3 && as.character(formula[[3]]) == ".") {
    return(as.matrix(dataf))
  }

  # remove the LHS so model.frame() does not look for it.
  form_rhs <- update(formula, new = NULL ~ .)

  rhs_m <- model.matrix(object = form_rhs, data = dataf)
  return(rhs_m)
}


#' Construct f(X)
#'
#' TODO: rename this to 'construct_f_of_x' to avoid thinking this makes the partial derivative of f with respect to x (i.e., f_x).
#'
#' @param beta The vector of coefficients.
#' @param formula The formula object.
#' @param ... The objects used to construct the data (passed to construct_rhs_df).
#'
#' @export
construct_fX <- function(beta, formula, ...) {
  fx_m <- construct_rhs_df(formula = formula, ...)
  if (length(beta) != ncol(fx_m)) {
    stop("length of beta (", length(beta), ") does not equal the
          number of columns (", ncol(fx_m), ") induced by formula.")
  }
  y <- c(fx_m %*% beta)
  return(y)
}


# TODO: create tests for this.
# TODO: describe input using comments
# todo: rename 'keke' variable
vecs_l_to_matrices_l <- function(keke) {
  # The elements within a list do not have to have the same order since we look them up
  # by name.
  process_one_row_in_all_lists <- function(rname) {
    m <- do.call(rbind, lapply(X = keke, FUN = `[[`, rname))
    return(m)
  }

  # assume all lists have the same number of elements and
  # TODO: what if no names?
  # create an argument for that? or just switch to it automatically?
  #
  #process_ith_row_in_all_lists <- function(i) {
  #  m <- do.call(rbind, lapply(X = keke, FUN = `[[`, i))
  #  return(m)
  #}

  # todo: allow user to pass arg to check?
  matrices_l <- lapply(X = names(keke[[1]]), FUN = process_one_row_in_all_lists)

  names(matrices_l) <- names(keke[[1]])
  return(matrices_l)
}


# currently only works for matrices
width_of_printed_obj <- function(obj) {
  # solution based on this: https://stackoverflow.com/a/31123436/1376404
  # Not sure what the point of this line is. Initializing to 0 instead seemed to
  # work better.
  #dashes <- max( nchar( rownames( obj ) ) ) + length ( obj )
  dashes <- 0
  # TODO: replaced length with ncol
  for (i in 1:ncol(obj)) {
    x <- nchar(colnames(obj))[i]
    if (any(is.na(obj[, i]))) {
      # TODO: should take max of 2 (NA values) and
      #     max(nchar(obj[!is.na(obj[, i]), i]))
      #     i.e., test with c(NA, 2.342143, NA).
      # ^^^^ add test
      y <- 2
    } else {
      y <- max(nchar(obj[, i]))
    }
    if (x > y) {
      dashes <- dashes + x
    } else {
      dashes <- dashes + y
    }
  }
  width_ <- dashes
}


# I'm not sure why I do all that work with names. I think the names
# end up getting replaced with hashes in dgp_params_to_list().
#' @importFrom stats setNames
obj_to_list <- function(obj) {
  if (is.matrix(obj)) {
    # (https://stackoverflow.com/a/2471243/1376404)
    ret <- as.list(data.frame(t(obj)))
    names(ret) <- sapply(ret, FUN = paste, collapse = ",")
  } else if (is.data.frame(obj)) {
    # Instead of making a list of vectors (like the case of a matrix),
    # here we make a list of one-row data frames. The reasoning is that
    # this way users can mix types; reducing to a vector would require
    # that the type of all elements be the same.
    #
    # https://stackoverflow.com/a/14370455/1376404
    # the setNames preserves the names.
    ret <- setNames(split(obj, seq(nrow(obj))), rownames(obj))
  } else if (is.atomic(obj)) {
    ret <- as.list(obj)
    if (is.null(names(obj))) {
      names(ret) <- obj
    } else {
      # if not a named vector, then use parameter values also as the name
      if (any(names(ret) == "")) {
        stop("Your 'dgp_params' vector has names for some elements but not others. ",
             "It must have names for all elements or none.")
      }
      names(ret) <- names(obj)
    }
  } else if (is.list(obj)) {
    ret <- obj
    # We do not assign names because this DGP list given by the user might have functions
    # or any type of R object, so best not to attempt any magic.
    #if (is.null(names(obj))) {
    #  names(ret) <- sapply(ret, FUN = paste)
    #}
  } else {
    stop("Object of type ", class(obj), " not supported yet.")
  }
  return(ret)
}


# Change all detected functions inside of obj (including obj itself) into the corresponding *body*
# of the function. This way the hash of the object will not change if a function inside 'obj' is
# used or compiled.
# We look for interior functions by recursing through all list elements and all attributes.
func_to_body <- function(obj) {
  # representation
  if (is.function(obj)) {
    attributes_ <- attributes(obj)
    for (a in names(attributes_)) {
      attr(obj, a) <- func_to_body(attr(obj, a))
    }
    obj <- body(obj)
  } else if (is.list(obj)) {
    for (i in seq_along(obj)) {
      attributes_ <- attributes(obj[[i]])
      for (a in names(attributes_)) {
        attr(obj[[i]], a) <- func_to_body(attr(obj[[i]], a))
      }
      obj[[i]] <- func_to_body(obj[[i]])
    }
  }
  return(obj)
}


# TODO: add hardcoded tests that will essentially check if
#       digest() changes default values.
# This is a separate function so easy to change hash engine.
#' @importFrom digest digest
calc_hash <- function(obj) {
  # the goal is to avoid problems with byte compiling.
  digest(func_to_body(obj))
#  output_ <- capture.output(obj)
#  digest(output_)
  # https://github.com/eddelbuettel/digest/issues/92
  # https://github.com/jhrcook/mustashe/issues/8
  # alternative: could *compile* the function:
  #     https://stackoverflow.com/questions/47825393/r-digest-of-closure-changes-after-repeated-call
  # but then if different compiler options, could lead to different end hashes.
}


# todo: if I do more than check for globals, change this name
#' @importFrom codetools checkUsage
#' @importFrom globals globalsOf
check_for_globals <- function(f, verbose = 0) {
  # I took a lot of code from here: https://stackoverflow.com/questions/6216968/r-force-local-scope

  if (verbose >= 2) {
    # I think this prints potentially useful info.
    codetools::checkUsage(f)
  }

  # We currently rely on globals::globalsOf(). This chunk shows an alternative.
  #vars <- codetools::findGlobals(f)
  #found <- !vapply(vars, exists, logical(1), envir=as.environment(2))

  # Without mustExist set to FALSE, we get an error when the function
  # was created by another function. e.g., for diagnostics when
  # it was created with 'gen_diags_ci' we get an error about "diags_l".
  # (currently there is no test that uses 'gen_diags_ci').
  # Similarly, a statistic wrapped in 'ret_na_if_error' will give an
  # error about "func".
  # (we do have a test for this one: see test-mc-diags.R, mc_ci_always_error)
  #
  # We still get an error when setting 'locals = FALSE'.
  #
  # One alternative would be to check for variables like this:
  #
  # Browse[1]> ls(envir = environment(f))
  #  [1] "diags_l" "ret_fn"
  #
  # mustExist set to TRUE seems like a good default for most use cases of
  # globalsOf, but in montetools's use here we are not trying to ensure there
  # will not be an error when the function is run. Indeed, if there is an error
  # that's fine; the user will see it and fix it. No need to warn/error them in
  # advance.
  #
  # todo: try different 'method' arguments of globalsOf() ?
  #
  # setting to recursive = TRUE can add a lot of computation time, e.g., 5
  # minutes (see frcbstats-paper) and I'm not sure if it solves a problem so
  # leave as FALSE until we find a use case that fails. Perhaps the reason is
  # that montetools's recursive checks superseed the need for this.
  #
  # setting 'locals = FALSE' is not necessary, and I'm not aware of how it
  # changes behavior for montetools's purposes, but it might be a bit faster
  # with it in some cases.
  vars <- globals::globalsOf(f, recursive = FALSE, mustExist = FALSE, locals = FALSE)
  # TODO: explain what this command does
  found <- !vapply(names(vars), exists, logical(1), envir=as.environment(2))
  globals_varnames <- names(found)[found]
  # TODO: probably there is an arg of globalsOf that can do this for me?
  #       Maybe file an enhancement request?
  ret_l <- list()
  # need as.logical because if sapply returns empty, it is an empty *list* for
  # some reason.
  exists_standard <- as.logical(sapply(globals_varnames, exists))
  ret_l <- c(ret_l, get_globals(globals_varnames[exists_standard], envir = environment()))

  # Now search in the environment of f.
  # (needed for testthat, and maybe other strange workflow/environment setups)
  env_f <- environment(f)
  exists_other <- as.logical(sapply(globals_varnames, exists, envir = env_f))
  # '!exists_standard' because no need to get variables that we already got above.
  globals_varnames_f <- globals_varnames[exists_other & !exists_standard]
  ret_l <- c(ret_l, get_globals(globals_varnames_f, envir = env_f))

  # DBG
  # This means we detected a global but it doesn't exist anywhere. Should give
  # the user an error during mc_run() when their function is used. Not
  # our responsibility. But this is useful when debugging.
  #if (any(!exists_standard && !exists_other)) browser()
  #stopifnot(any(!exists_standard && !exists_other))
  return(ret_l)
}


# a duplicate is defined as having the same name and
# same element.
# Equivalent elements with different names are not removed.
remove_dups_from_list <- function(l) {
  l_ret <- l
  dup_names <- duplicated(names(l))
  # duplicated does not care whether name is different. Only checks
  # the element.
  dup_entries <- duplicated(l)
  # if names are duplicates *and* elements are duplicates, remove:
  remove_these <- dup_names & dup_entries
  # if names are duplicates and elements are not duplicates, we
  # have a problem since I don't think they'll be restored
  # correctly
  if (any(dup_names & !dup_entries)) {
    warning("Possible problem: mc_reproduce() likely won't work since
             there are two distinct global functions with different names.
             Please report this issue with an example and motivate a use case for this setup.")
  }
  return(l[!remove_these])
}


# uses non-standard evaluation, checking sys.call
sanity_check <- function(var, possible_values) {
  if (!(var %in% possible_values)) {
    user_arg_name <- sys.call(0)[[2]]
    vals <- paste0("\"", possible_values, "\"")
    stop("The argument \"", paste0(user_arg_name), "\" cannot be set to \"", var, "\".", "\n",
         "It must be set to one of the following values: ",
         paste(vals, collapse = ", "), ".")
  }
}


# this does not give an error if the global does not exist (e.g., 'statistic' calls a
# global that doesn't exist).
collect_globals <- function(funcs_l, exclude_these = c(), recursive = TRUE, allow_non_local = "functions", verbose = 0) {
  parents <- funcs_l

  # TODO: only detect global if its environment (if function) or the environment where it was found (if object) is equal to the environment of one of the parents (?).

  # TODO: could try to collect only objects in global environment. But that might be fragile.
  #  in_global_env <- sapply(globals_l, function(x) identical(environment(x), .GlobalEnv))
  #
  # exclude objects that are in a package ? e.g.,:
  # <environment: namespace:montetools>
  # i.e., exclude those?

  # check all '...' are functions:
  parents_are_funcs <- sapply(parents, FUN = is.function)
  stopifnot(all(parents_are_funcs))

  sanity_check(allow_non_local, c("functions", "all"))

  globals_l <- list()
  for (parent in parents) {
    globals_ <- check_for_globals(parent, verbose = verbose)
    globals_l <- c(globals_l, globals_)
  }
  unique_globals_l <- remove_dups_from_list(globals_l)
  unique_globals_l <- unique_globals_l[!(names(unique_globals_l) %in% exclude_these)]

  global_is_func <- as.logical(sapply(unique_globals_l, FUN = is.function))
  unique_global_funcs_l <- unique_globals_l[global_is_func]
  # TODO: give warnings about weird objects, like connections.

  if (allow_non_local != "all") {
    if (any(!global_is_func)) {
      # todo: would be user-friendly to say *which* of the user's function.
      stop("The following variable(s) are defined outside the local scope of one (or more) of the functions you passed as arguments: ", "\n",
           paste0(paste(names(unique_globals_l)[!global_is_func], collapse = ", "), "."), "\n",
           "It is generally not recommended to rely on non-local variables. You can override this error by setting the argument \"allow_non_local = 'all'\". For more information, see ?mc_run."
      )
    }
  }

  if (recursive) {
    if (length(unique_globals_l) == 0) {
      return(list())
    } else {
      # We use 'exclude_these' to avoid infinite recursion.
      # we already collected those in 'exclude_these' so no need to collect them again.
      # todo: could be a problem that same name but different function. Just document
      # that is an expected problem? At least detect and give a warning?
      exclude_ <- c(exclude_these, names(unique_globals_l))
      # we only call collect_globals on functions.
      child_globals_l <- collect_globals(funcs_l = unique_global_funcs_l, exclude_these = exclude_,
                                         # needed in case default of 'recursive' is changed to FALSE.
                                         recursive = TRUE,
                                         allow_non_local = allow_non_local,
                                         verbose = verbose)
      unique_child_globals_l <- remove_dups_from_list(child_globals_l)
      unique_globals_l <- c(unique_globals_l, unique_child_globals_l)
      return(unique_globals_l)
    }
  } else {
    return(unique_globals_l)
  }
}


get_globals <- function(v, envir) {
  # cannot use sapply because the global might itself be a list and sapply does
  # not do the right thing in this case when v has, e.g., two lists.
  # If we change "lapply" to "sapply", the test \ref{global-list} fails.
  # For a real use case, frcbstats-paper currently encounters this situation
  # (as of 4 June 2022).
  l <- lapply(v, FUN = get, envir = envir)
  names(l) <- v
  return(l)
}


# If check is TRUE, we give error if there is no consistent ordering
# of the columns. If FALSE, we assume consistent ordering (more efficient).
infer_col_order <- function(one_sim_res, check = FALSE) {
  order_on_sum <- order(colSums(one_sim_res))

  if (!check) {
    return(order_on_sum)
  }

  order_m <- apply(X = one_sim_res[, order_on_sum, drop = FALSE], MARGIN = 1, FUN = order)
  # check that all columns are equal.
  # (https://stackoverflow.com/questions/26262583/quickest-way-in-r-to-check-that-all-columns-are-identical)
  order_is_consistent <- all(order_m == order_m[, 1])

  if (!order_is_consistent) {
    stop("No consistent ordering between columns. Each column should be either weakly greater than or weakly less than the other columns.")
  }
  return(order_on_sum)
}


# This function is not currently used. If it is used, be careful:
# we might remove NA row of one stat, but not others. That will invalidate
# get_nstats, get_statnames and everything that relies on them (?).
# first get nstats of MC, then make sure we're removing an entire sim.
drop_row_if_any_NA <- function(stats_m) {
  row_has_NA <- apply(X = stats_m, MARGIN = 1, FUN = function(row_) any(is.na(row_)))
  # TODO: we might support a 0-row stats_m in the future, but for now it causes
  #       problems later on so we just return back all of the NAs.
  if (all(row_has_NA)) {
    # Instead of a message, we have a hook so users can customize
    # behavior. See hook_print_NA_report.
    # message("All rows of stats_m have an NA.")
    return(FALSE)
  }
  return(row_has_NA)
}


# make unit tests for this.
# essentially checks the *bodies* of functions (?).
funcs_are_equal <- function(func1, func2) {
  # alternatives:
  # https://stackoverflow.com/questions/9638372/how-to-compare-functions
  # https://stackoverflow.com/a/39538503
  # see ?identical.
  # TODO: add unit tests.
  # TODO: rely on func_to_body(), or check consistent?
  #
  # TODO: This one returns FALSE sometimes when there's no difference, not
  # sure why. But we use it rather than the one below because of the change of
  # behavior in R. See local directory "funcs_are_equal_change_of_behavior" /sk
  ret <- identical(func1, func2, ignore.environment = TRUE)
  # this had a change of behavior starting with R 4.1 I think.
  #ret <- isTRUE(all.equal(func1, func2))
  return(ret)
}


get_attr <- function(obj, attr, error_if_null = TRUE) {
  # check both if attribute exists and whether NULL. Two different things...
  # currently, no attribute should be set to NULL so don't need to distinguish between does not exists and exists but set to null.
  attr_ <- attr(x = obj, which = attr, exact = TRUE)
  if (error_if_null) {
    stopifnot(!is.null(attr_))
  }
  return(attr_)
}


# copied from xfun::exit_call to avoid the import
# other relevant URLs:
# https://stackoverflow.com/questions/8546262/add-on-exit-expr-to-parent-call
# https://yihui.org/en/2017/12/on-exit-parent/
# sk: I try to keep the interface similar to on.exit(). e.g.,
#     require the caller to specify add = TRUE (even though it is indeed best practice).
#     the argument is an expression, not a function.
#' @importFrom rlang enquo
on_parent_exit <- function(expr, n = 2, ...) {
  do.call(on.exit, list(enquo(expr),
          ...), envir = parent.frame(n))
}


rng_snapshot <- function() {
  if (!exists(".Random.seed")) {
    set.seed(NULL)
  }
  list(seed = .Random.seed, rngkind = RNGkind())
}


# This function should not be called in concurrent code.
# The argument "seed_info" can be the "rnginfo_before" or "rnginfo_after" attribute
# from an mcpn.
restore_rng <- function(seed_info) {
  # The order of the following is important
  do.call("RNGkind", as.list(seed_info[["rngkind"]]))
  # It is important to assign to the global environment:
  assign(".Random.seed", seed_info[["seed"]], .GlobalEnv)
}


internal_error <- function(context_msg = "") {
  print(sys.calls())
  stop("montetools internal error: ", context_msg, " this should not happen. I'm sorry about that. could you please make a small example on GitHub (https://github.com/scottkosty/montetools) that reproduces this error so I can fix it?")
}


# This is Henrik's recommended way:
# https://github.com/HenrikBengtsson/future/issues/625#issuecomment-1136380976
#
# futureSessionInfo() does not seem meant for parsing.
current_plan <- function() {
  plan_ <- attributes(plan())[["class"]][[2]]
  if (is.null(plan_)) {
    stop("Need to change detection of plan.")
  }
  return(plan_)
}


# I use a separate function so can make unit tests to check that the functionality
# hasn't changed.
#' @importFrom utils getFromNamespace
#' @importFrom parallel nextRNGStream
make_seeds <- function(count, begin_after = NULL) {
  stopifnot(count >= 0)
  #  see here for various ways of dealing with seeds:
  # https://rdrr.io/cran/future.apply/src/tests/future_lapply,RNG.R
  if (is.null(begin_after)) {
    seed_ <- TRUE
    # TODO: figure out how the *initial* seed is created inside make_rng_seeds.
    #       Then can copy that and can combine these branches better.
    # use un-exported function from another package:
    # https://stackoverflow.com/a/46098814/1376404
    make_rng_seeds_ <- utils::getFromNamespace("make_rng_seeds", "future.apply")
    make_rng_seeds_(count = count, seed = seed_)
  } else {
    seeds <- vector("list", length = count)
    seeds[[1]] <- nextRNGStream(begin_after)
    if (count > 1) {
      for (i in 2:count) {
        seeds[[i]] <- nextRNGStream(seeds[[i - 1]])
      }
    }
    return(seeds)
  }
}


# copied from 'taRifx' package
merge.list <- function(x, y, ...) {
  res <- x
  for (nm in names(y)) {
    if (is.null(x[[nm]])) {
      res[[nm]] <- y[[nm]]
    } else {
      for (yname in names(y[[nm]])) {
        res[[nm]][[yname]] <- y[[nm]][[yname]]
      }
    }
  }
  res
}


# todo: do some tests
#
# If x is length 1, we return FALSE since the first one is not a duplicate, so
# not all are duplicates.
allAreDups <- function(x) {
  #
  # inefficient for a couple of reasons:
  # (1) duplicated() looks at the *whole* vector.
  # An efficient function would stop early, like
  # anyDuplicated().
  # (2) sum() also looks at the whole vector.
  # (2) could be solved by one of these sols:
  # https://stackoverflow.com/questions/20246903/faster-way-to-find-the-first-true-value-in-a-vector
  # (3) '!' also works on the entire vector.
  # could be addressed by not using '!' and comparing to length(x).

  if (length(x) <= 1) {
    ret <- FALSE
  } else {
    ret <- identical(sum(!duplicated(x)), 1L)
  }
  return(ret)
}


# TODO: add unit tests for this!
#
#' stop_on_these_classes: In some cases, we do usually want to give an error (and not
#' return NA). For example, it is usually nice to error out on
#' 'packageNotFoundError'. That particular error occurs when running mc_reproduce
#' without a required package.
#'
#' @export
#' @param expr The expression to try.
#' @param na_on_these_messages The pattern of messages to "allow", i.e., return NA without error. 'stop_on_these_messages' overrides these messages.
#' @param stop_on_these_classes The classes of error that should result in an error (i.e., not return NA).
#' @param stop_on_these_messages The pattern of messages that should cause an error (i.e., not return NA).
#' @eval param_verbose()
try_or_na <- function(expr, na_on_these_messages = c("*"), stop_on_these_classes = c("packageNotFoundError"), stop_on_these_messages = c("could not find function"), verbose = 1) {
  # TODO: what if user sets 'warn' option *inside* statistic?
  warn2error <- getOption('warn') == 2
  # if there is an error, it returns NA. TODO make retOnNA = NA a parameter?
  #
  # TODO: condition on options("warn") for whether to catch warnings also. if 2, then capture!
  # need lots of tests for different options. And document!
  if (warn2error) {
    try_ <- tryCatch(assign("ret_", eval(expr)), error = function(e) e, warning = function(w) w)
  } else {
    try_ <- tryCatch(assign("ret_", eval(expr)), error = function(e) e)
  }

  if (inherits(try_, what = stop_on_these_classes)) {
    msg_ <- try_[["message"]]
    stop("try_or_na: not swallowing error since return inherits from one of 'stop_on_these_classes'. Error message: ",
          msg_)
  }
  if (inherits(try_, what = "condition")) {
    msg_ <- try_[["message"]]
    matches <- sapply(stop_on_these_messages, grepl, msg_)
    if (any(matches)) {
      stop("try_or_na: not swallowing error since the following message matches one of 'stop_on_these_messages': ",
           msg_)
    }
  }

  if (inherits(try_, what = stop_on_these_classes)) {
    stop("try_or_na: not swallowing error since return inherits from one of 'stop_on_these_classes': ",
         paste(stop_on_these_classes, collapse = ", "))
  }

  # If the package is not found (e.g., when doing mc_reproduce()), the class is:
  # Browse[1]> class(try_)
  # [1] "packageNotFoundError" "error" "condition"
  caught_error <- inherits(try_, what = c("simpleError", "error"))
  caught_warning <- inherits(try_, "simpleWarning")

  enable_asserts <- assertsAreEnabled()
  if (enable_asserts) {
    if (caught_error && caught_warning) {
      stop("Detected both a warning and an error. When does this happen and does montetools handle this situation correctly?")
    }
  }

  # These checks are because it's not easy to ensure we detected the error. Some
  # errors set the class to "simpleError", some to "error", and it wouldn't be
  # surprising if we're missing another one.
  # todo: maybe grep for "error" in class(try_)?
  #
  caught_de_facto_error <- caught_error || (warn2error && caught_warning)
  if (caught_de_facto_error && exists("ret_")) {
    stop("'ret_' should not exist if there was an error.")
  }
  #
  if (!caught_de_facto_error && !exists("ret_")) {
    stop("Since 'ret_' does not exist, there was likely an undetected error.")
  }

  if (caught_error || caught_warning) {
    if (caught_error) {
      prefix <- "Error "
    } else if (caught_warning) {
      prefix <- "Error (converted from warning) "
    }

    msg_ <- try_[["message"]]
    matches <- sapply(na_on_these_messages, grepl, msg_)
    if (any(matches)) {
      if (verbose >= 1) {
        message(prefix, "that try_or_na swallowed: ", msg_)
      }
      return(NA)
    } else {
      # EXPIRE this comment: this used to be "message" (and protected by
      # verbose >= 1), but I think "stop" is correct since it's not in 'na_on_these_messages'.
      stop(prefix, "not escaped: ", msg_)
    }
  }
  return(ret_)
}


#' @export
#' @title Return NA if error
#' @description Function wrapper to return NA if error.
#' @param func The function that will be wrapped.
#' @eval param_verbose()
ret_na_if_error <- function(func, verbose = 1) {
  # expire alternative: instead of '...' could copy args explicitly? Not sure of benefit.
  func_captured <- func
  func2 <- function(...) {
    ret_or_na <- try_or_na(func(...), verbose = verbose)
    return(ret_or_na)
  }
  return(func2)
}


mc_set_seed <- function(seed) {
  # we could alternatively have seed_initializer set the seed, but it might be
  # nice to have access to the seed itself, and not just .Random.seed.
  if (!is.null(seed)) {
    # seed_initializer is allowed to return "NULL" to mean "don't do
    # anything, I'll take care of it." In that way, it can either set the
    # seed itself, or it can choose to not set the seed.
    set.seed(seed)
  }
}


# TODO: make a vignette called "debugging", where I describe this option.
# Otherwise, no need to mention it.
assertsAreEnabled <- function() {
  if (isTRUE(getOption("montetools_asserts"))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


get_default_arg <- function(f, arg) {
  default_ <- formals(mc_run)[[arg]]
  stopifnot(!is.null(default_))
  return(default_)
}


file_increment_suffix <- function(d) {
  inc <- 1
  candidate <- paste0(d, "_", inc)
  while (file.exists(candidate)) {
    inc <- inc + 1
    candidate <- paste0(d, "_", inc)
  }
  return(candidate)
}


create_padder <- function(...) {
  l <- list(...)
  nchar_ <- sapply(l, nchar)
  f <- function(s) {
    npad <- max(nchar_) - nchar(s)
    padding <- paste0(rep(" ", times = npad), collapse = "")
    s_padded <- paste0(s, padding, collapse = "")
    return(s_padded)
  }
  return(f)
}


# file.remove() gives a warning by default if it doesn't
# exist. Sometimes I don't want it to give a warning
# (e.g., maybe a user manually removes a file by accident).
file.remove_if_exists <- function(...) {
  for (file_ in list(...)) {
    if (file.exists(file_)) {
      file.remove(file_)
    }
  }
}


apply_attrs_to_obj <- function(obj, attrs) {
  attrs_combined <- c(attributes(obj), attrs)
  attributes(obj) <- attrs_combined
  return(obj)
}


do_interleave <- function(mat1, mat2) {
  nr <- nrow(mat1)
  nc <- ncol(mat1)
  stopifnot(identical(nr, nrow(mat2)))
  stopifnot(identical(nc, ncol(mat2)))
  combined_mat_alt <- matrix(nrow = 2*nr, ncol = nc,
                         # initialize rownames so can replace them.
                         dimnames = list(rep(NA, 2*nr)))
  # interleave
  idx_mat1 <- seq(from = 1, to = 2*nr, by = 2)
  idx_mat2 <- seq(from = 2, to = 2*nr, by = 2)
  combined_mat_alt[idx_mat1, ] <- mat1
  rownames(combined_mat_alt)[idx_mat1] <- rownames(mat1)
  combined_mat_alt[idx_mat2, ] <- mat2
  rownames(combined_mat_alt)[idx_mat2] <- rownames(mat2)

  #if (assertsAreEnabled()) {
  #  # this works well but might as well avoid the dependency.
  #  # I wrote a simple implementation below instead.
  #  combined_mat <- gdata::interleave(mat1, mat2)
  #  stopifnot(identical(combined_mat_alt, combined_mat))
  #}

  return(combined_mat_alt)
}


# file.rename() fails in the case of renaming across file systems.
# So instead, we copy and then remove.
# Similar issue to the ones reported here:
#   https://github.com/Bioconductor/BiocFileCache/issues/9
#   https://github.com/wch/vtest/issues/14
file_mv <- function(from, to) {
  file.copy(from = from, to = to)
  file.remove(from)
}
