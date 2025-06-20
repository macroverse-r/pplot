
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pplot

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: AGPL
v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
<!-- badges: end -->

The goal of pplot is to provide specialized visualization tools for
panel and longitudinal data. It creates publication-quality plots
including time series, bar charts, scatter plots, and multi-panel
layouts.

This package is part of the [macroverse
ecosystem](https://github.com/macroverse-r/macroverse).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("macroverse-r/pplot")
```

## Examples

### Time Series Plots

``` r
library(pplot)
library(macrodata)  # For data loading

# Load some data
data <- md_data(
  ISO = c("USA", "DEU", "JPN"),
  formula = "GDP_C",
  years = c(2010, 2023)
)

# Create time series plot
pp_plot_series(
  data,
  title = "GDP Comparison",
  y_axis = "Billion USD",
  area = TRUE,  # Area plot
  key_dates = data.frame(
    Event = "COVID-19",
    Date = as.Date("2020-03-11")
  )
)
```

### Bar Charts

``` r
# Create bar chart for latest year
pp_plot_bar(
  data,
  year = 2023,
  title = "GDP in 2023",
  horizontal = TRUE,
  sort = TRUE
)
```

### Scatter Plots

``` r
# Load two variables
data <- md_data(
  ISO = "OECD",
  formula = c("GDP_PC_C", "UNEMP_R"),
  variable = c("GDP per capita", "Unemployment rate"),
  years = c(2019, 2019)
)

# Create scatter plot with regression
pp_plot_scatter(
  data,
  x = "GDP per capita",
  y = "Unemployment rate",
  interpolation = "Linear",
  r_squared = TRUE,
  ISO = "Both"  # Show both points and labels
)
```

### Combining Plots

``` r
# Create multiple plots
p1 <- pp_plot_series(data1, title = "Panel A")
p2 <- pp_plot_bar(data2, title = "Panel B")

# Combine them
pp_plot_combine(
  list(p1, p2),
  ncol = 2,
  title = "Combined Analysis"
)
```

## Key Features

- **Time Series Visualization**: Line plots, area plots, multi-panel
  layouts
- **Bar Charts**: Horizontal/vertical, grouped, sorted
- **Scatter Plots**: With regression lines, R², custom grouping
- **Customization**: Themes, colors, labels, annotations
- **Export**: High-quality PNG, PDF, and other formats
- **Integration**: Works seamlessly with macrodata package

## Plot Customization

All plotting functions support extensive customization:

``` r
pp_plot_series(
  data,
  # Styling
  area = TRUE,
  stacked = FALSE,
  log_scale = FALSE,
  
  # Axes
  y_axis = "Custom Label",
  right_axis = "Variable2",  # Dual axis
  
  # Appearance
  title = "My Title",
  subtitle = "My Subtitle",
  caption = "Data source: ...",
  
  # Output
  filename = "my_plot.png",
  width = 10,
  height = 6,
  dpi = 300
)
```

## macroverse Ecosystem

The pplot package works best with other macroverse packages: -
**[mvcommon](https://github.com/macroverse-r/mvcommon)**: Common
utilities and validation - **pplot**: Panel data visualization (this
package) - **[isomapper](https://github.com/macroverse-r/isomapper)**:
ISO codes and country mapping -
**[macrodata](https://github.com/macroverse-r/macrodata)**: Data loading
and processing - **[mvlazy](https://github.com/macroverse-r/mvlazy)**:
Convenience functions -
**[macroverse](https://github.com/macroverse-r/macroverse)**:
Meta-package loading all components

## License

This package is licensed under AGPL-3.0.
