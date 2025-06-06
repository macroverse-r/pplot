% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pp_plot_combine.r
\name{pp_plot_combine}
\alias{pp_plot_combine}
\title{Combine Multiple PPplot Plots with Advanced Layout Options}
\usage{
pp_plot_combine(
  plot_list,
  dim = c(12, 12),
  print = TRUE,
  filename = NULL,
  reference = NULL,
  fontsize_base = 12,
  ref_scale = 1.2,
  title_scale = 2,
  title = NULL,
  subtitle = NULL,
  ncol = NULL,
  nrow = NULL,
  widths = NULL,
  heights = NULL,
  legend = NULL,
  guides = "collect",
  theme = list()
)
}
\arguments{
\item{plot_list}{list of ggplot2 objects; plots to combine, typically created by
PPplot plotting functions}

\item{dim}{numeric vector of length 2; output dimensions in inches:
- First element: width
- Second element: height
Default is c(12, 12).}

\item{print}{logical; whether to display the combined plot:
- TRUE: display plot (default)
- FALSE: create but don't display}

\item{filename}{character; path for saving plot:
- Specify path without extension
- Saves as both PNG and PDF
- NULL/FALSE for no file output (default)}

\item{reference}{character; citation text for data sources:
- Appears as figure caption
- NULL for no reference (default)}

\item{fontsize_base}{numeric; base font size in points:
- Controls text size throughout plot
- Default is 12}

\item{ref_scale}{numeric; scaling factor for reference text:
- Multiplied by fontsize_base
- Default is 1.2}

\item{title_scale}{numeric; scaling factor for title text:
- Multiplied by fontsize_base
- Default is 2}

\item{title}{character; main title for combined plot:
- NULL for no title (default)}

\item{subtitle}{character; subtitle for combined plot:
- NULL for no subtitle (default)}

\item{ncol}{integer; number of columns in plot grid:
- NULL for automatic calculation (default)}

\item{nrow}{integer; number of rows in plot grid:
- NULL for automatic calculation (default)}

\item{widths}{numeric vector; relative widths of columns:
- Length must match number of columns
- NULL for equal widths (default)}

\item{heights}{numeric vector; relative heights of rows:
- Length must match number of rows
- NULL for equal heights (default)}

\item{guides}{character; method for handling plot guides/legends:
- "collect": combine legends (default)
- "keep": maintain separate legends}
}
\value{
A patchwork object containing the combined plots
}
\description{
Combines multiple plots created by PPplot plotting functions (pp_plot_series, pp_plot_bar,
pp_plot_scatter) into a single figure with customizable layout, shared guides, and
consistent formatting. Particularly useful for creating multi-panel visualizations
of economic data with shared legends and coordinated styling.
}
\details{
The function uses the patchwork package for plot composition and provides several
features for creating publication-ready figures:
\itemize{
\item Layout Control:
\itemize{
\item Grid-based arrangement with flexible dimensions
\item Optional control over relative sizes
\item Automatic space optimization
}
\item Visual Consistency:
\itemize{
\item Unified theming across panels
\item Coordinated margins and spacing
\item Optional shared legends
}
\item Text Handling:
\itemize{
\item Hierarchical text sizing
\item Flexible title placement
\item Caption support for references
}
}

The function automatically adjusts spacing and margins to create a cohesive
visualization while preserving the integrity of individual plots.
}
\examples{
Create data
ISO <- c("USA", "FRA", "DEU", "CHN")
years <- c(1990, 2020)
data_gdp <- macrodata::md_data(ISO = ISO, years = years, formula = "GDP_R_2015/1e12")
data_trade <- macrodata::md_data(ISO = ISO, years = years, formula = "100*TB_C/GDP_C")
data_exports <- macrodata::md_data(ISO = ISO[1], years = years, 
                        formula = c("100*(EXg_C-IMg_C)/GDP_C", "100*EXg_C/GDP_C", "100*IMg_C/GDP_C"),
                        variable = c("Net Goods Trade", "Exports of Goods", "Imports of Goods")
)



# Create sample plots
plot1 <- pp_plot_series(data_gdp, y_axis = "Trillion 2015 USD", title = "Real GDP", reference = FALSE)
plot2 <- pp_plot_bar(data_trade, y_axis = "\% of GDP", title = "Trade Balance", reference = FALSE, show_values = FALSE, legend = FALSE)
plot3 <- pp_plot_series(data_gdp, y_axis = "Billion 2023 USD", title = "Real GDP - Larger plot", reference = FALSE)
plot4 <- pp_plot_series(data_exports, y_axis = "\% of GDP", title = "Trade of Goods -- United States", reference = FALSE, area = TRUE)


plot_list = list(plot1, plot2, plot3, plot4)


# Basic combination with default layout
pp_plot_combine(
    plot_list = plot_list
)

# Custom layout with titles and reference
pp_plot_combine(
    plot_list = plot_list,
    title = "Economic Indicators",
    reference = "Source: World Bank, IMF",
    dim = c(20, 14),
    ncol = 2,
    heights = c(1, 2)
)

}
\seealso{
\code{\link{pp_plot_series}} for creating time series plots
\code{\link{pp_plot_bar}} for creating bar plots
\code{\link{pp_plot_scatter}} for creating scatter plots

Other plotting functions: 
\code{\link{pp_plot_bar}()},
\code{\link{pp_plot_scatter}()},
\code{\link{pp_plot_series}()}
}
\concept{plotting functions}
