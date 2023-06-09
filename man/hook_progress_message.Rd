% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hooks.R
\name{hook_progress_message}
\alias{hook_progress_message}
\title{MC hook for reporting progress}
\usage{
hook_progress_message(mc_part_done, mc_args_next, user_args, pn_pair_next)
}
\arguments{
\item{mc_part_done}{An MC object containing the pn-pairs of finished simulations so far.}

\item{mc_args_next}{The arguments that will be used for the next pn-chunk.}

\item{user_args}{The arguments given to mc_run() by the user.}

\item{pn_pair_next}{The pn-pair whose simulations will be run next.}
}
\description{
Provide progress on what pn pair is currently being processed. If POI label
is NA, then the POI label is not output. This is useful when there is only one
DGP param, and not an intuitive label for the POI.
}
