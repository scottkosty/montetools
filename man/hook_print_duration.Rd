% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hooks.R
\name{hook_print_duration}
\alias{hook_print_duration}
\title{MC hook for showing wall clock duration of previous pn-chunk}
\usage{
hook_print_duration(mc_part_done, mc_args_next, user_args, pn_pair_next)
}
\arguments{
\item{mc_part_done}{An MC object containing the pn-pairs of finished simulations so far.}

\item{mc_args_next}{The arguments that will be used for the next pn-chunk.}

\item{user_args}{The arguments given to mc_run() by the user.}

\item{pn_pair_next}{The pn-pair whose simulations will be run next.}
}
\description{
Displays the duration it took for previous pn-chunk to run the simulations.
}
