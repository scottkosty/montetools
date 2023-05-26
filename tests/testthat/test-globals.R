dgp_params_ <- c(1, 2, 3)

nvec_ <- c(5, 9)
nsims_1 <- 3

dgp_ <- function(n, dgp_param) {
  # use dgp_param just to trigger error if dgp_param is missing.
  if (identical(dgp_param, 3.11111)) {
    rnorm(n)
  } else {
    rnorm(n, sd = 1.01)
  }
}


statistic_4 <- function(dataf) {
  stat <- mean(dataf)
  return(c("mystatname" = stat))
}


mc_args_l <- list(
  dgp_params = dgp_params_,
  nvec = nvec_,
  nsims = nsims_1,
  dgp = dgp_,
  statistics = statistic_4,
  verbose = 0
)


# todo: do I really need to loop through all seed_inits? Maybe just do:
# seed_inits_l <- "seed_init_pnash"
test_that("Globals are stored and restored correctly.", {
  for (si in seed_inits_l) {
  
    set.seed(7)
    mc_args_local_l <- mc_args_l
    mc_args_local_l[["seed_initializer"]] <- si
    # not needed for this test:
    # mc <- do.call(mc_run, mc_args_local_l)
  
    glob1 <- function(x) {
      mean_ <- mean(x)
      return(mean_)
    }
  
    statistic_4_calls_glob1 <- function(dataf) {
      stat <- glob1(dataf)
      return(c("mystatname" = stat))
    }
  
    mc_args_local_l2 <- mc_args_local_l
    mc_args_local_l2[["statistics"]] <- statistic_4_calls_glob1
    expect_error(mc_orig <- do.call(mc_run, mc_args_local_l2), NA)
    # remove the global so it can't be found. It should have been collected
    # by the mc_run() if handling globals works correctly.
    rm(glob1)
    #
    rm(statistic_4_calls_glob1)
    expect_error(mc_repd <- mc_reproduce(mc_orig, verbose = 0), NA)
    #
    # When we create the reproduced MC, we don't (re)collect globals, so
    # here we test that we copied them over correctly.
    # i.e., we test that we can "reproduce the reproduced MC".
    expect_identical(attr(mc_orig, "globals"), attr(mc_repd, "globals"))
    expect_error(mc_reproduce(mc_repd, verbose = 0), NA)




    # now try nested globals
    glob5 <- function(x) {
      mean_ <- mean(x)
      return(mean_)
    }

    glob4 <- function(x) {
      mean_ <- glob5(x)
      return(mean_)
    }

    glob3 <- function(x) {
      mean_ <- glob4(x)
      return(mean_)
    }

    glob2 <- function(x) {
      mean_ <- glob3(x)
      return(mean_)
    }

    statistic_4_calls_glob2 <- function(dataf) {
      stat <- glob2(dataf)
      return(c("mystatname" = stat))
    }
    mc_args_local_l2 <- mc_args_local_l
    mc_args_local_l2[["statistics"]] <- statistic_4_calls_glob2
    expect_error(mc_orig <- do.call(mc_run, mc_args_local_l2), NA)
    rm(glob2, glob3, glob4, glob5)
    rm(statistic_4_calls_glob2)
    expect_error(mc_repd <- mc_reproduce(mc_orig, verbose = 0), NA)
    expect_identical(attr(mc_orig, "globals"), attr(mc_repd, "globals"))
    expect_error(mc_reproduce(mc_repd, verbose = 0), NA)



    # now try globals that call each other
    glob6 <- function(x) {
      mean_ <- glob7(x)
      return(mean_)
    }

    glob7 <- function(x) {
      mean_ <- mean(x)
      if (mean(x) > 1000) {
        return(mean_)
      } else {
        mean_ <- mean_ + 100
        return(glob6(mean_))
      }
    }
    # first just testing no infinite loop or other error:
    expect_error(glob6(1:10), NA)

    statistic_4_calls_glob6 <- function(dataf) {
      stat <- glob6(dataf)
      return(c("mystatname" = stat))
    }
    mc_args_local_l2 <- mc_args_local_l
    mc_args_local_l2[["statistics"]] <- statistic_4_calls_glob6
    expect_error(mc_orig <- do.call(mc_run, mc_args_local_l2), NA)
    rm(glob6, glob7)
    rm(statistic_4_calls_glob6)
    expect_error(mc_repd <- mc_reproduce(mc_orig, verbose = 0), NA)
    expect_identical(attr(mc_orig, "globals"), attr(mc_repd, "globals"))
    expect_error(mc_reproduce(mc_repd, verbose = 0), NA)


    # now try recursive global
    glob8 <- function(x) {
      mean_ <- mean(x)
      if (mean(x) > 1000) {
        return(mean_)
      } else {
        mean_ <- mean_ + 100
        return(glob8(mean_))
      }
    }
    # first just testing no infinite loop or other error:
    expect_error(glob8(1:10), NA)

    statistic_4_calls_glob8 <- function(dataf) {
      stat <- glob8(dataf)
      return(c("mystatname" = stat))
    }
    mc_args_local_l2 <- mc_args_local_l
    mc_args_local_l2[["statistics"]] <- statistic_4_calls_glob8
    expect_error(mc_orig <- do.call(mc_run, mc_args_local_l2), NA)
    rm(glob8)
    rm(statistic_4_calls_glob8)
    expect_error(mc_repd <- mc_reproduce(mc_orig, verbose = 0), NA)
    expect_identical(attr(mc_orig, "globals"), attr(mc_repd, "globals"))
    expect_error(mc_reproduce(mc_repd, verbose = 0), NA)


    # now try gobal *variable* (and function)
    # We use globvar1 twice, once in glob2 and once
    # directly in statistic.
    globvar1 <- 5
    glob2 <- function(x) {
      mean_ <- mean(x) + globvar1
      return(mean_)
    }
    statistic_4_uses_globvar1 <- function(dataf) {
      stat <- glob2(dataf)*globvar1
      return(c("mystatname" = stat))
    }
    mc_args_local_l2 <- mc_args_local_l
    mc_args_local_l2[["statistics"]] <- statistic_4_uses_globvar1
    expect_error(mc_orig <- do.call(mc_run, mc_args_local_l2), NA)
    rm(globvar1)
    rm(glob2)
    rm(statistic_4_uses_globvar1)
    expect_error(mc_repd <- mc_reproduce(mc_orig, verbose = 0), NA)
    expect_identical(attr(mc_orig, "globals"), attr(mc_repd, "globals"))
    expect_error(mc_reproduce(mc_repd, verbose = 0), NA)


    # now try a function with a keyname (i.e., a name that is used internally in montetools)
    # "verbose" is used as an object in a lot of parts of montetools.
    globvar2 <- 10
    verbose <- function(x) {
      mean_ <- mean(x) + globvar2
      return(mean_)
    }
    statistic_4_uses_verbosefn <- function(dataf) {
      stat <- verbose(dataf)*globvar2
      return(c("mystatname" = stat))
    }
    mc_args_local_l2 <- mc_args_local_l
    mc_args_local_l2[["statistics"]] <- statistic_4_uses_verbosefn
    expect_error(mc_orig <- do.call(mc_run, mc_args_local_l2), NA)
    rm(globvar2)
    rm(verbose)
    rm(statistic_4_uses_verbosefn)
    expect_error(mc_repd <- mc_reproduce(mc_orig, verbose = 0), NA)
    expect_identical(attr(mc_orig, "globals"), attr(mc_repd, "globals"))
    expect_error(mc_reproduce(mc_repd, verbose = 0), NA)


    # \label{global-list}
    #
    # now try gobal variable that happens to be a *list*.
    # This test makes sure when collecting the globals that we don't accidentally
    # unwind the list, as can happen with c(list1, list2).
    globlist1a <- list(a = 1, b = 2, c = 3)
    globlist2a <- list(a = 10, b = 20, c = 30)
    glob2a <- function(x) {
      mean_ <- mean(x) + mean(unlist(globlist1a)) + mean(unlist(globlist2a))
      return(mean_)
    }
    statistic_4_uses_globlist1a <- function(dataf) {
      stat <- glob2a(dataf)
      return(c("mystatname" = stat))
    }
    mc_args_local_l2 <- mc_args_local_l
    mc_args_local_l2[["statistics"]] <- statistic_4_uses_globlist1a
    expect_error(mc_orig <- do.call(mc_run, mc_args_local_l2), NA)
    rm(globlist1a)
    rm(glob2a)
    rm(statistic_4_uses_globlist1a)
    expect_error(mc_repd <- mc_reproduce(mc_orig, verbose = 0), NA)
    expect_identical(attr(mc_orig, "globals"), attr(mc_repd, "globals"))
    expect_error(mc_reproduce(mc_repd, verbose = 0), NA)


    # EXPIRE: this was when I was trying to give an error if non-local
    #         non-function. I suspended those efforts though.
    ## now try a factory function. Not really a global variable!
    ## todo: test several nests...
    #glob9a <- function() {
    #  not_a_global <- 10
    #  func_ <- function(x) x^2 + not_a_global
    #  return(func_)
    #}
    #glob10 <- glob9a()


    #statistic_4_calls_glob10 <- function(dataf) {
    #  stat <- glob10(dataf)
    #  return(c("mystatname" = stat))
    #}
    #mc_args_local_l2 <- mc_args_local_l
    #mc_args_local_l2[["statistics"]] <- statistic_4_calls_glob10
    ## of course with "all" it passes
    #mc_args_local_l2[["allow_non_local"]] <- "all"
    #expect_error(mc_orig <- do.call(mc_run, mc_args_local_l2), NA)
    #rm(glob9a, glob10, statistic_4_calls_glob10)
    #expect_error(mc_reproduce(mc_orig, verbose = 0), NA)
  }
})
