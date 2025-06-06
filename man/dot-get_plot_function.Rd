% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-core.r, R/utils-core.r
\name{.get_plot_function}
\alias{.get_plot_function}
\title{Determine Appropriate Plotting Function Based on Data and Parameters}
\usage{
.get_plot_function(
  data,
  plot_type,
  plot_specific_args,
  verbose = TRUE,
  debug = FALSE
)

.get_plot_function(
  data,
  plot_type,
  plot_specific_args,
  verbose = TRUE,
  debug = FALSE
)
}
\description{
Internal router that selects the specialized plotting function based on data
characteristics (number of countries/indicators) and plot specifications.
Central decision point for all plotting operations.

Used by: .plot_core
Uses: No direct function calls

Returns references to specialized plotting functions:
\itemize{
\item For series: .plot_one_ctry_area, .plot_multi_indic_one_ctry, etc.
\item For bar: .plot_one_indic_bar, .plot_multi_indic_bar
\item For scatter: .plot_scatter_two_vars, .plot_scatter_multi_vars
}

Internal router that selects the specialized plotting function based on data
characteristics (number of countries/indicators) and plot specifications.
Central decision point for all plotting operations.

Used by: .plot_core
Uses: No direct function calls

Returns references to specialized plotting functions:
\itemize{
\item For series: .plot_one_ctry_area, .plot_multi_indic_one_ctry, etc.
\item For bar: .plot_one_indic_bar, .plot_multi_indic_bar
\item For scatter: .plot_scatter_two_vars, .plot_scatter_multi_vars
}
}
\keyword{internal}
