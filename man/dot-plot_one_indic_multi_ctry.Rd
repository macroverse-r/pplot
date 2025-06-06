% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-series.r, R/pp_plot_series.r
\name{.plot_one_indic_multi_ctry}
\alias{.plot_one_indic_multi_ctry}
\title{Create Multi-Line Plot for Multiple Countries}
\usage{
.plot_one_indic_multi_ctry(
  data,
  y_axis = NULL,
  key_dates = NULL,
  theme = list(base_size = 16)
)

.plot_one_indic_multi_ctry(
  data,
  y_axis = NULL,
  key_dates = NULL,
  theme = list(base_size = 16)
)
}
\description{
Internal function that creates a line plot comparing one indicator across
multiple countries. Used for cross-country comparison over time.

Used by: pp_plot_series (via .plot_core), .plot_multi_by_indic
Uses: .scale_y, .scale_x, .theme_plot, .add_date_markers

Features:
\itemize{
\item One line per country
\item Color coding by country
\item Single scale
\item Optional key date markers
}

Internal function that creates a line plot comparing one indicator across
multiple countries. Used for cross-country comparison over time.

Used by: pp_plot_series (via .plot_core), .plot_multi_by_indic
Uses: .scale_y, .scale_x, .theme_plot, .add_date_markers

Features:
\itemize{
\item One line per country
\item Color coding by country
\item Single scale
\item Optional key date markers
}
}
\keyword{internal}
