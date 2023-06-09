% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnostics.R
\name{diags_ci}
\alias{diags_ci}
\alias{diag_ci_length}
\alias{diag_ci_covers}
\alias{diag_ci_covers_l}
\alias{diag_ci_covers_u}
\title{Diagnostics for statistics of the form c(lower, est, upper).}
\usage{
diag_ci_length(lower, est, upper, true_poi)

diag_ci_covers(lower, est, upper, true_poi)

diag_ci_covers_l(lower, est, upper, true_poi)

diag_ci_covers_u(lower, est, upper, true_poi)
}
\arguments{
\item{lower}{The vector of lower bounds of the confidence band estimates.}

\item{est}{The vector of point estimates.}

\item{upper}{The vector of upper bounds of the confidence band estimates.}

\item{true_poi}{The vector of parameter values.}
}
\description{
Often used as arguments to [gen_diags_ci()], these diagnostics calculate the following: lower/upper/combined coverage.
}
\seealso{
See gen_diags_ci() to create diagnostic functions for estimators with confidence intervals. See diags_est for estimator diagnostics.
}
