#' Combine Multiple PPplot Plots with Advanced Layout Options
#' 
#' @description
#' Combines multiple plots created by PPplot plotting functions (pp_plot_series, pp_plot_bar, 
#' pp_plot_scatter) into a single figure with customizable layout, shared guides, and 
#' consistent formatting. Particularly useful for creating multi-panel visualizations
#' of economic data with shared legends and coordinated styling.
#'
#' @param plot_list list of ggplot2 objects; plots to combine, typically created by 
#'        PPplot plotting functions
#' @param dim numeric vector of length 2; output dimensions in inches:
#'        - First element: width
#'        - Second element: height
#'        Default is c(12, 12).
#' @param print logical; whether to display the combined plot:
#'        - TRUE: display plot (default)
#'        - FALSE: create but don't display
#' @param filename character; path for saving plot:
#'        - Specify path without extension
#'        - Saves as both PNG and PDF
#'        - NULL/FALSE for no file output (default)
#' @param reference character; citation text for data sources:
#'        - Appears as figure caption
#'        - NULL for no reference (default)
#' @param fontsize_base numeric; base font size in points:
#'        - Controls text size throughout plot
#'        - Default is 12
#' @param ref_scale numeric; scaling factor for reference text:
#'        - Multiplied by fontsize_base
#'        - Default is 1.2
#' @param title_scale numeric; scaling factor for title text:
#'        - Multiplied by fontsize_base
#'        - Default is 2
#' @param title character; main title for combined plot:
#'        - NULL for no title (default)
#' @param subtitle character; subtitle for combined plot:
#'        - NULL for no subtitle (default)
#' @param ncol integer; number of columns in plot grid:
#'        - NULL for automatic calculation (default)
#' @param nrow integer; number of rows in plot grid:
#'        - NULL for automatic calculation (default)
#' @param widths numeric vector; relative widths of columns:
#'        - Length must match number of columns
#'        - NULL for equal widths (default)
#' @param heights numeric vector; relative heights of rows:
#'        - Length must match number of rows
#'        - NULL for equal heights (default)
#' @param guides character; method for handling plot guides/legends:
#'        - "collect": combine legends (default)
#'        - "keep": maintain separate legends
#'
#' @return A patchwork object containing the combined plots
#'
#' @details
#' The function uses the patchwork package for plot composition and provides several
#' features for creating publication-ready figures:
#'
#' * Layout Control:
#'   - Grid-based arrangement with flexible dimensions
#'   - Optional control over relative sizes
#'   - Automatic space optimization
#'
#' * Visual Consistency:
#'   - Unified theming across panels
#'   - Coordinated margins and spacing
#'   - Optional shared legends
#'
#' * Text Handling:
#'   - Hierarchical text sizing
#'   - Flexible title placement
#'   - Caption support for references
#'
#' The function automatically adjusts spacing and margins to create a cohesive
#' visualization while preserving the integrity of individual plots.
#'
#' @examples
#' Create data
#' ISO <- c("USA", "FRA", "DEU", "CHN")
#' years <- c(1990, 2020)
#' data_gdp <- macrodata::md_data(ISO = ISO, years = years, formula = "GDP_R_2015/1e12")
#' data_trade <- macrodata::md_data(ISO = ISO, years = years, formula = "100*TB_C/GDP_C")
#' data_exports <- macrodata::md_data(ISO = ISO[1], years = years, 
#'                         formula = c("100*(EXg_C-IMg_C)/GDP_C", "100*EXg_C/GDP_C", "100*IMg_C/GDP_C"),
#'                         variable = c("Net Goods Trade", "Exports of Goods", "Imports of Goods")
#' )
#'
#'
#'
#' # Create sample plots
#' plot1 <- pp_plot_series(data_gdp, y_axis = "Trillion 2015 USD", title = "Real GDP", reference = FALSE)
#' plot2 <- pp_plot_bar(data_trade, y_axis = "% of GDP", title = "Trade Balance", reference = FALSE, show_values = FALSE, legend = FALSE)
#' plot3 <- pp_plot_series(data_gdp, y_axis = "Billion 2023 USD", title = "Real GDP - Larger plot", reference = FALSE)
#' plot4 <- pp_plot_series(data_exports, y_axis = "% of GDP", title = "Trade of Goods -- United States", reference = FALSE, area = TRUE)
#'
#'
#' plot_list = list(plot1, plot2, plot3, plot4)
#'
#'
#' # Basic combination with default layout
#' pp_plot_combine(
#'     plot_list = plot_list
#' )
#'
#' # Custom layout with titles and reference
#' pp_plot_combine(
#'     plot_list = plot_list,
#'     title = "Economic Indicators",
#'     reference = "Source: World Bank, IMF",
#'     dim = c(20, 14),
#'     ncol = 2,
#'     heights = c(1, 2)
#' )
#'
#' @seealso 
#' \code{\link{pp_plot_series}} for creating time series plots
#' \code{\link{pp_plot_bar}} for creating bar plots
#' \code{\link{pp_plot_scatter}} for creating scatter plots
#'
#' @family plotting functions
#' @export
pp_plot_combine <- function(plot_list, 
                           dim = c(12,12), 
                           print = TRUE, 
                           filename = NULL, 
                           reference = NULL,
                           fontsize_base = 12,
                           ref_scale = 1.2,
                           title_scale = 2,
                           title = NULL,
                           subtitle = NULL,
                           # layout of the plot
                           ncol = NULL,
                           nrow = NULL,
                           widths = NULL,
                           heights = NULL,
                           legend = NULL,
                           # design = NULL, # do not current work
                           guides = "collect",
                           theme = list()
                           ) {

    plot_list <- lapply(plot_list, function(p) {
            p + ggplot2::theme(
                plot.title = ggplot2::element_text(size = 30)  # or whatever size you want
            )
        })

    wrapped_plots <- lapply(seq_along(plot_list), function(i) {
        # Modify the plot BEFORE wrapping it
        modified_plot <- plot_list[[i]] + 
            ggplot2::theme(
                plot.margin = ggplot2::margin(0, 0, 0, 0),
                panel.spacing = ggplot2::unit(0, "lines"),
                axis.title = ggplot2::element_text(margin = ggplot2::margin(0, 0, 0, 0)),
                axis.text = ggplot2::element_text(margin = ggplot2::margin(0, 0, 0, 0))
            )
        
        # Then wrap the modified plot
        patchwork::wrap_elements(modified_plot)
    })

    # Combine all wrapped plots 
    plot_combined <- patchwork::wrap_plots(wrapped_plots)


    # Use regular grid layout with optional parameters
    plot_combined <- plot_combined + 
      patchwork::plot_layout(
        ncol = ncol,
        nrow = nrow,
        widths = widths,
        heights = heights,
        guides = guides
      )

    # Alternative: Use custom design layout (currently do not work to combined area and non-area plots)
    # plot_combined <- plot_combined + 
    #   patchwork::plot_layout(design = design, guides = guides)

    if (!is.null(legend)) {
        if (is.null(theme$legend_heights)) { theme$legend_heights <- c(20,2) }
        plot_combined <- plot_combined / legend +
              patchwork::plot_layout(heights = theme$legend_heights )
    }

    # Add title, subtitle, and reference as a caption
    plot_combined <- plot_combined + 
        patchwork::plot_annotation(caption = reference, title = title, subtitle = subtitle) &
        ggplot2::theme(
            plot.caption = ggplot2::element_text(
                size = fontsize_base*ref_scale,
                hjust = 0        # Align text to the left
                ),
            plot.title = ggplot2::element_text(
                size = fontsize_base*title_scale,
                margin = ggplot2::margin(b = 10),
                hjust = 0.5 # center the title
                ),
            plot.subtitle = ggplot2::element_text(
                size = fontsize_base,
                margin = ggplot2::margin(b = 10),
                hjust = 0        # Align text to the left
                ),
            # apply theme modifications to all plots
            panel.spacing = ggplot2::unit(0, "lines"),  # Remove space between panels
            legend.margin = ggplot2::margin(0, 0, 0, 0), # Remove legend margins
            # Add title theming here
            legend.spacing = ggplot2::unit(0, "lines")   # Remove space around legends
            )


    # Print the combined plot
    .print_and_save(plot_combined, print = print, filename = filename, dim = dim)

    invisible(plot_combined)

}