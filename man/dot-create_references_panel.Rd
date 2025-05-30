% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-components.r
\name{.create_references_panel}
\alias{.create_references_panel}
\title{Create References Citation Panel}
\usage{
.create_references_panel(
  reference_col,
  dim,
  font_size = 16,
  theme = list(),
  verbose = TRUE,
  debug = TRUE
)
}
\description{
Internal function that creates a formatted panel for reference citations.
Handles text wrapping and layout of source citations.

Used by: .plot_core
Uses: .format_references

Features:
\itemize{
\item Automatic text wrapping
\item Width-aware formatting
\item Character count optimization
\item Consistent styling
}
}
\keyword{internal}
