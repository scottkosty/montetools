# todo: can we detect whether it is an estimation MC?
# this way can adapt the default behavior.
# e.g., we could make a plot() generic.

# todo: tell user they need to subset if they want different plots.

#' @importFrom ggplot2 ggplot geom_density aes facet_grid geom_vline
#' @importFrom stats reformulate
mc_plot_density <- function(mc, n_on_columns = TRUE) {
  stats_m_stacked <- get_stacked_stats_m(mc)

  names_ <- names(stats_m_stacked)
  if (! ("value" %in% names_)) {
    stop("expected name 'value'")
  }
  names(stats_m_stacked)[names_ == "value"] <- "estimate"
  if (! ("statname" %in% names_)) {
    stop("expected name 'statname'")
  }
  names(stats_m_stacked)[names_ == "statname"] <- "estimator"

  # This addresses R CMD check notes about global bindings for "estimate" and "estimator".
  # https://stackoverflow.com/a/69981433/1376404
  # expire: alternative is to use rlang::sym() instead of quote().
  plot_ <- ggplot(data = stats_m_stacked, aes(x = !!quote(estimate), color = !!quote(estimator))) +
    geom_density() +
    # https://stackoverflow.com/a/49868869/1376404
    geom_vline(mapping = aes(xintercept = !!quote(poi)))

  # todo: I would prefer to not plot the values when the function is 0.
  #       ^ This is the problem of the reason why there's the colored lines at the bottom.
  # Maybe follow this approach:
  # https://stackoverflow.com/questions/66893182/ggplot2-density-with-multiple-groups-and-trim-true-density-does-not-go-to-0
  # or other answer:
  # https://stackoverflow.com/questions/66893182/ggplot2-density-with-multiple-groups-and-trim-true-density-does-not-go-to-0?noredirect=1#comment136441499_66893182

  if (n_on_columns) {
    col_facet <- "n"
    row_facet <- "dgpp_label"
  } else {
    col_facet <- "dgpp_label"
    row_facet <- "n"
  }

  # https://stackoverflow.com/questions/21588096/pass-string-to-facet-grid-ggplot2
  # the order must be reversed (wrt facet_grid()) for some reason.
  plot_facetted <- plot_ + facet_grid(reformulate(col_facet, row_facet))
  return(plot_facetted)
}
