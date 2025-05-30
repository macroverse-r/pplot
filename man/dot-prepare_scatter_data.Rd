% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pp_plot_scatter.r
\name{.prepare_scatter_data}
\alias{.prepare_scatter_data}
\title{Prepare Data for Scatter Plot Creation}
\usage{
.prepare_scatter_data(data, verbose = TRUE, debug = FALSE)
}
\description{
Internal function that reshapes and prepares data for scatter plot creation.
Converts long format data to wide format suitable for scatter plotting.
Essential preprocessing step for all scatter plots.

Used by: .plot_scatter_two_vars
Uses: No direct function calls

Processing steps:
\enumerate{
\item Validate variable count
\item Reshape data to wide format
\item Rename columns to match variables
\item Return both data and variable names
}
}
\keyword{internal}
