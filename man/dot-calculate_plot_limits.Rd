% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pp_plot_bar.r
\name{.calculate_plot_limits}
\alias{.calculate_plot_limits}
\title{Calculate Plot Limits and Margins}
\usage{
.calculate_plot_limits(data, show_values)
}
\description{
Internal function that determines appropriate plot limits and margins based on data
and label placement. Ensures proper spacing for value labels and data visibility.

Used by: .plot_one_indic_bar, .plot_multi_indic_bar
Uses: No direct function calls

Features:
\itemize{
\item Automatic margin calculation
\item Label position consideration
\item Bottom/top label accommodation
\item Dynamic spacing adjustment
}
}
\keyword{internal}
