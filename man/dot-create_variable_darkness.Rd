% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pp_plot_bar.r
\name{.create_variable_darkness}
\alias{.create_variable_darkness}
\title{Create Darkness Scale for Variables}
\usage{
.create_variable_darkness(
  variables,
  base_color = "gray50",
  verbose = TRUE,
  debug = FALSE
)
}
\description{
Internal function that creates a scale of darkness levels for variables.
Used for differentiating variables when color coding isn't used.

Used by: .plot_multi_indic_bar
Uses: No direct function calls

Features:
\itemize{
\item Graduated darkness scale
\item Consistent spacing
\item Alpha adjustment
\item Base color modification
}
}
\keyword{internal}
