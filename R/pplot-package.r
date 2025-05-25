#' pplot: Panel Data Visualization Tools
#'
#' @description
#' The pplot package provides specialized visualization tools for panel and
#' longitudinal data. It creates publication-quality plots including time series,
#' bar charts, scatter plots, and multi-panel layouts. Part of the macroverse ecosystem.
#'
#' @details
#' ## Main Plotting Functions
#'
#' ### Time Series Plots
#' - \code{\link{pp_plot_series}}: Create time series visualizations
#'
#' ### Bar Charts  
#' - \code{\link{pp_plot_bar}}: Create bar charts for comparative analysis
#'
#' ### Scatter Plots
#' - \code{\link{pp_plot_scatter}}: Create scatter plots with trend analysis
#'
#' ### Combined Plots
#' - \code{\link{pp_plot_combine}}: Combine multiple plots into layouts
#'
#' @section Plot Customization:
#' All plotting functions support extensive customization through various parameters:
#' - Color schemes using mvcommon color palettes
#' - Custom themes and styling
#' - Flexible axis scaling
#' - Date markers and annotations
#' - Multi-panel layouts
#' - Export to various formats
#'
#' @section Dependencies:
#' The pplot package relies on:
#' - ggplot2 for core plotting functionality
#' - scales for axis formatting
#' - patchwork for combining plots
#' - mvcommon for utilities and validation
#'
#' @section See Also:
#' Other macroverse packages:
#' \itemize{
#'   \item \code{mvcommon}: Common utilities and validation
#'   \item \code{isomapper}: ISO codes and country mapping
#'   \item \code{macrodata}: Data loading and processing
#'   \item \code{mvlazy}: Convenience functions
#'   \item \code{macroverse}: Umbrella package
#' }
#'
#' @examples
#' \dontrun{
#' # Load data using macrodata
#' data <- macrodata::md_data("GDP_C", years = 2010:2020)
#' 
#' # Create time series plot
#' pp_plot_series(data, variable = "GDP_C")
#' 
#' # Create bar chart
#' pp_plot_bar(data, year = 2020)
#' 
#' # Create scatter plot
#' pp_plot_scatter(data, x = "year", y = "GDP_C")
#' }
#'
#' @docType package
#' @name pplot-package
#' @aliases pplot
#' @keywords internal
"_PACKAGE"