#' Create Consistent Theme for All Plots
#' 
#' @description
#' Internal function that establishes a consistent visual theme across all plot types.
#' Based on theme_minimal with custom modifications for better readability and consistency.
#' 
#' Used by: All plotting functions
#' Uses: theme_minimal, theme, element_text, element_line, element_blank, element_rect
#' 
#' Features:
#' - Black axis text for clarity
#' - Angled x-axis labels
#' - Bottom legend positioning
#' - Minimal grid lines
#' - Consistent margins
#' - Transparent backgrounds
#'
#' @keywords internal
.theme_plot <- function(theme = NULL, bg = "transparent") {         # numeric: base font size that scales all text elements


    if (is.null(theme$base_size)) { theme$base_size <- 16 }
    if (is.null(theme$x_axis_angle)) { theme$x_axis_angle <- 45 }

    ggplot2::theme_minimal(base_size = theme$base_size) +
        ggplot2::theme(
            # Main plot elements
            legend.position = "bottom",
            legend.box = "horizontal",
            axis.text = ggplot2::element_text(color = "black"),  # Set all axis text to black
            axis.text.x = ggplot2::element_text(angle = theme$x_axis_angle, hjust = 1, size = theme$base_size),
            axis.text.y = ggplot2::element_text(size = theme$base_size),
            axis.title.y = ggplot2::element_text(size = theme$base_size * 1.15),
            axis.title.y.right = ggplot2::element_text(size = theme$base_size * 1.15, angle = 90),
            axis.text.y.right = ggplot2::element_text(size = theme$base_size, angle = 0),
            legend.text = ggplot2::element_text(
                size = theme$base_size,
                margin = ggplot2::margin(t = 0, r = 30, b = 0, l = 0, unit = "pt")
            ),
            
            # Panel elements
            panel.grid.major = ggplot2::element_line(color = "gray50", size = 0.05),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_rect(fill = bg, color = NA),
            plot.background = ggplot2::element_rect(fill = bg, color = NA),
            plot.margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 5, unit = "pt"),
            panel.ontop = TRUE,
            
            # Strip text (for faceting)
            strip.text = ggplot2::element_text(size = theme$base_size, face = "bold")
            
            # Caption styling
            # plot.caption = ggplot2::element_text(
            #     size = theme$base_size * ref_scale,
            #     # family = "CM Roman",
            #     hjust = 0,
            #     vjust = 1,
            #     lineheight = 1.2,
            #     margin = ggplot2::margin(t = 0, b = 0)
            # ),
            # plot.caption.position = "plot"
            
        )
}