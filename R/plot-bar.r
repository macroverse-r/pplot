#' Create Bar Plot for Single Indicator
#' 
#' @description
#' Internal function that creates a bar plot for one indicator across multiple countries.
#' Handles sophisticated styling with optional statistical elements like ranges and medians.
#' 
#' Used by: pp_plot_bar (via .plot_core)
#' Uses: .process_bar_data, .create_bar_labels, .create_color_mapping, .calculate_plot_limits
#'       .create_bar_legend, .get_category_labels, .get_legend_columns, .get_ordered_groups
#'       .theme_plot
#' 
#' Features:
#' - Mean or latest value display
#' - Optional min-max error bars
#' - Optional median markers
#' - Flexible color coding by region/income/etc
#' - Value labels with year information
#' - Customizable ordering
#'
#' @keywords internal
.plot_one_indic_bar <- function(data,                    # data.frame: input data for single indicator
                                 y_axis = NULL,            # string/NULL: y-axis label
                                 type = "Mean",            # string: "Mean"/"Last" - determines value to display
                                 order = "Variable",       # string/numeric: "Variable"/"Alphabetical"/"Input" - controls bar ordering
                                 show_values = "Bottom",   # string/FALSE: "Bottom"/"Top" - position of value labels
                                 show_year = 1,            # numeric: 0/1/2 - year display format in labels
                                 show_range = TRUE,        # logical: whether to show min-max range indicators
                                 show_median = TRUE,       # logical: whether to show median markers
                                 color = "Subregion",      # string/FALSE: color grouping scheme
                                 theme = NULL,           # numeric: base font size in points
                                 dimensions = c(10, 7)) {  # numeric vector: plot width and height in inches
    
    # 1. Process data
    all_data <- .process_bar_data(data, type, order)

    # 2. Create labels
    all_data <- .create_bar_labels(all_data, show_year, type)
    
    # 3. Handle colors
    color_result <- .create_color_mapping(all_data, color, theme$base_size)
    if (!is.null(color_result)) {
        all_data <- color_result$data
        color_mapping <- color_result$mapping
        dark_color_mapping <- color_result$dark_mapping
    }
    
    # 4. Calculate plot limits
    limits <- .calculate_plot_limits(all_data, show_values)
    
    # 5. Create legend information
    legend_info <- .create_bar_legend(all_data, type, show_median, show_range)
   
    # 6. Create base plot
    plot <- ggplot2::ggplot() +
        {if(!is.null(color) && !isFALSE(color))
            ggplot2::geom_bar(data = all_data, 
                    ggplot2::aes(x = Country, y = display_value, fill = color_group_label), 
                    stat = "identity", width = 0.5)
        else 
            ggplot2::geom_bar(data = all_data, 
                    ggplot2::aes(x = Country, y = display_value, fill = "Values"), 
                    stat = "identity", width = 0.5, fill = "gray50")
        }

    # 7. Add statistical elements
    if(show_range) {
        plot <- plot +
            ggplot2::geom_errorbar(
                data = all_data,
                ggplot2::aes(x = Country, ymin = min_value, ymax = max_value),
                width = 0.25,
                color = if(!is.null(color) && !isFALSE(color)) 
                    sapply(all_data$color_group, function(g) dark_color_mapping[g]) 
                else "gray20"
            )
    }

    if(show_median && type == "Mean") {
        plot <- plot +
            ggplot2::geom_point(
                data = all_data,
                ggplot2::aes(x = Country, y = median_value),
                size = 3,
                color = if(!is.null(color) && !isFALSE(color)) 
                    sapply(all_data$color_group, function(g) dark_color_mapping[g]) 
                else "gray20"
            )
    }

    # # 8. Add unified legend
    plot <- plot +
        ggplot2::geom_point(
            data = legend_info$data, 
            ggplot2::aes(x = x, y = y, shape = type), 
            show.legend = TRUE, 
            alpha = 0
        ) +
        ggplot2::scale_shape_manual(
            values = setNames(
                c(15, if(show_median && type == "Mean") 19 else NULL, if(show_range) 95 else NULL),
                legend_info$elements
            ),
            breaks = legend_info$elements,
            name = NULL,
            guide = ggplot2::guide_legend(
                order = 1,
                direction = "horizontal",
                override.aes = list(
                    alpha = 1,
                    size = c(5, if(show_median && type == "Mean") 3 else NULL, if(show_range) 12 else NULL),
                    linetype = c("blank", if(show_median && type == "Mean") "blank" else NULL, if(show_range) "solid" else NULL),
                    linewidth = c(0.5, if(show_median && type == "Mean") 0.5 else NULL, if(show_range) 3 else NULL),
                    color = c("gray50", if(show_median && type == "Mean") "gray20" else NULL, if(show_range) "gray20" else NULL)
                )
            )
        )



    # 9. Add color legend
    if(!is.null(color) && !isFALSE(color)) {
        category_labels <- .get_category_labels()
        display_color_mapping <- color_mapping
        names(display_color_mapping) <- sapply(names(color_mapping), function(g) {
            if (g == "Other") return("Other")
            return(category_labels[[g]])
        })
        
        ncol <- .get_legend_columns(
            legend_items = color_mapping, 
            plot_width_inches = dimensions[1]*0.6, 
            base_size = theme$base_size
        )
        
        plot <- plot +
            ggplot2::scale_fill_manual(
                values = display_color_mapping,
                name = NULL,
                breaks = sapply(
                    .get_ordered_groups(color, color_mapping, category_labels), 
                    function(g) if(g == "Other") "Other" else category_labels[[g]]
                ),
                guide = ggplot2::guide_legend(
                    order = 2,
                    ncol = ncol,
                    byrow = TRUE,
                    override.aes = list(shape = NA)
                )
            )
    } else {
        plot <- plot +
            ggplot2::scale_fill_manual(
                values = c(Values = "transparent"),
                name = NULL,
                guide = ggplot2::guide_legend(
                    order = 2,
                    ncol = 1,
                    byrow = TRUE,
                    override.aes = list(shape = NA)
                )
            )
    }

    # 10. Add value labels if requested
    if(!isFALSE(show_values)) {
        value_pos <- if(identical(show_values, "Bottom")) 0 else all_data$display_value
        value_vjust <- if(identical(show_values, "Bottom")) 1.5 else -0.5
        
        plot <- plot +
            ggplot2::geom_text(
                data = all_data,
                ggplot2::aes(x = Country, y = value_pos, label = sprintf("%.1f", display_value)),
                vjust = value_vjust,
                size = theme$base_size/3
            )
    }

    limits <- c(min(limits$y_min - limits$y_margin_bottom,0), max(limits$y_max + limits$y_margin_top,0))
    # 11. Complete plot formatting
    plot <- plot +
        ggplot2::scale_y_continuous(
            name = y_axis,
            labels = scales::comma_format(scale = 1),
            breaks = scales::pretty_breaks(n = 8),
            limits = limits,
            expand = ggplot2::expansion(mult = c(0.02, 0.02))
        ) +
        ggplot2::scale_x_discrete(labels = all_data$label) +
        ggplot2::labs(x = NULL) +
        .theme_plot(theme = theme) +
        ggplot2::theme(
            legend.direction = "horizontal",
            legend.box = "vertical",
            legend.spacing = grid::unit(0.1, "cm"),
            legend.margin = ggplot2::margin(0, 0, 0, 0)
        )
    
    return(plot)
}



#' Create Bar Plot for Multiple Indicators
#' 
#' @description
#' Internal function that creates a grouped bar plot comparing multiple indicators
#' across countries. Each country gets a group of bars, one per indicator.
#' 
#' Used by: pp_plot_bar (via .plot_core)
#' Uses: .process_multi_bar_data, .calculate_plot_limits, .create_variable_darkness,
#'       .get_legend_columns, .theme_plot, .print_debug
#' 
#' Features:
#' - Grouped bars by country
#' - Color coding by indicator
#' - Mean or latest value display
#' - Optional range and median indicators
#' - Dodged positioning for clarity
#' - Automatic width adjustment
#'
#' @keywords internal
.plot_multi_indic_bar <- function(data,                    # data.frame: input data for multiple indicators
                                   y_axis = NULL,            # string/NULL: y-axis label
                                   type = "Mean",            # string: "Mean"/"Last" - determines value to display
                                   order = "Variable",       # string/numeric: ordering method for bars
                                   show_values = "Bottom",   # string/FALSE: position of value labels
                                   show_range = TRUE,        # logical: whether to show min-max range indicators
                                   show_median = TRUE,       # logical: whether to show median markers
                                   color = "Variable",       # string: color scheme (typically "Variable" for multi-indicator)
                                   theme = list(base_size = 16), # list
                                   dimensions = c(10, 7),    # numeric vector: plot dimensions
                                   verbose = TRUE,           # logical: controls information messages
                                   debug = FALSE) {          # logical: controls debug messages
    
    .print_debug("Starting multi-indicator bar plot", verbose, debug, type = "info", "MAIN")
    
    # 1. Process data
    all_data <- .process_multi_bar_data(data, type, order, verbose, debug)
    variables <- unique(all_data$Variable)
    n_vars <- length(variables)
    
    # 2. Set up position adjustment for grouped bars
    bar_width <- 0.5
    group_space <- bar_width * 0.4
    position_dodge_width <- bar_width + (bar_width / n_vars)
    
    .print_debug(paste("Bar positioning:", 
                     "width =", bar_width,
                     "space =", group_space,
                     "dodge =", position_dodge_width), 
               FALSE, debug, type = "info", "POSITION")
    
    # 3. Handle colors
    if(is.null(color) || isFALSE(color)) {
        # Use gray scales for variables
        color_mapping <- .create_variable_darkness(variables, verbose = verbose, debug = debug)
    } else {
        # Use distinct colors for variables, ensuring we skip the first color
        color_mapping <- setNames(custom_colors[2:(n_vars+1)], variables)
    }
    
    # Create darker versions for error bars and median points
    dark_color_mapping <- sapply(color_mapping, function(col) {
        rgb_values <- col2rgb(col)
        rgb(rgb_values[1]/512, rgb_values[2]/512, rgb_values[3]/512)
    })
    
    .print_debug("Color mapping created", verbose, debug, type = "info", "COLORS")
    
    # 4. Calculate plot limits
    limits <- .calculate_plot_limits(all_data, show_values)
    
    # 5. Create base plot
    plot <- ggplot2::ggplot(all_data, ggplot2::aes(x = Country, y = display_value, group = Variable))
    
    # Add bars with appropriate aesthetics
    plot <- plot + 
        ggplot2::geom_bar(ggplot2::aes(fill = Variable),
                stat = "identity",
                position = ggplot2::position_dodge(width = position_dodge_width),
                width = bar_width)
    
    .print_debug("Base plot created", verbose, debug, type = "info", "PLOT")
    
    # 6. Add statistical elements
    if(show_range) {
        plot <- plot +
            ggplot2::geom_errorbar(
                ggplot2::aes(ymin = min_value, 
                    ymax = max_value,
                    group = Variable),
                position = ggplot2::position_dodge(width = position_dodge_width),
                width = bar_width * 0.5,
                color = sapply(all_data$Variable, function(v) dark_color_mapping[v])
            )
    }
    
    if(show_median && type == "Mean") {
        plot <- plot +
            ggplot2::geom_point(
                ggplot2::aes(y = median_value,
                    group = Variable),
                position = ggplot2::position_dodge(width = position_dodge_width),
                size = 3,
                color = sapply(all_data$Variable, function(v) dark_color_mapping[v])
            )
    }
    
    # 7. Add value labels if requested
    if(!isFALSE(show_values)) {
        value_pos <- if (identical(show_values, "Bottom")) 0 else all_data$display_value
        value_vjust <- if (identical(show_values, "Bottom")) 1.5 else -0.5
        
        plot <- plot +
            ggplot2::geom_text(
                ggplot2::aes(y = value_pos,
                    label = sprintf("%.1f", display_value),
                    group = Variable),
                position = ggplot2::position_dodge(width = position_dodge_width),
                vjust = value_vjust,
                size = theme$base_size/3
            )
    }
    
    # 8. Add statistical legend elements
    legend_elements <- c()
    legend_elements <- c(legend_elements, if(type == "Mean") "Mean values" else "Latest values")
    if (show_median && type == "Mean") legend_elements <- c(legend_elements, "Median values")
    if (show_range) legend_elements <- c(legend_elements, "Range (Min to Max)")
    
    if (length(legend_elements) > 0) {
        legend_data <- data.frame(
            x = rep(1, length(legend_elements)),
            y = rep(1, length(legend_elements)),
            type = factor(legend_elements, levels = legend_elements),
            Variable = variables[1]
        )
        
        plot <- plot +
            ggplot2::geom_point(
                data = legend_data,
                ggplot2::aes(x = x, y = y, shape = type),
                show.legend = TRUE,
                alpha = 0
            ) +
            ggplot2::scale_shape_manual(
                values = setNames(
                    c(15, if(show_median && type == "Mean") 19 else NULL, if(show_range) 95 else NULL),
                    legend_elements
                ),
                breaks = legend_elements,
                name = NULL,
                guide = ggplot2::guide_legend(
                    order = 1,
                    direction = "horizontal",
                    override.aes = list(
                        alpha = 1,
                        size = c(5, if(show_median && type == "Mean") 3 else NULL, if(show_range) 12 else NULL),
                        linetype = c("blank", if(show_median && type == "Mean") "blank" else NULL, if(show_range) "solid" else NULL),
                        linewidth = c(0.5, if(show_median && type == "Mean") 0.5 else NULL, if(show_range) 3 else NULL),
                        color = c("gray50", if(show_median && type == "Mean") "gray20" else NULL, if(show_range) "gray20" else NULL)
                    )
                )
            )
    }
    
    # 9. Add color scale
    ncol <- .get_legend_columns(
        legend_items = variables, 
        plot_width_inches = dimensions[1]*0.6, 
        base_size = theme$base_size
    )
    
    plot <- plot +
        ggplot2::scale_fill_manual(
            values = color_mapping,
            name = NULL,
            guide = ggplot2::guide_legend(
                order = 2,
                ncol = ncol,
                byrow = TRUE,
                override.aes = list(shape = NA)
            )
        )
    
    # 10. Complete plot formatting
    limits <- c(min(limits$y_min - limits$y_margin_bottom,0), max(limits$y_max + limits$y_margin_top,0))
    plot <- plot +
        ggplot2::scale_y_continuous(
            name = y_axis,
            labels = scales::comma_format(scale = 1),
            breaks = scales::pretty_breaks(n = 8),
            limits = limits,
            expand = ggplot2::expansion(mult = c(0.02, 0.02))
        ) +
        ggplot2::labs(x = NULL) +
        .theme_plot(theme = theme) +
        ggplot2::theme(
            legend.direction = "horizontal",
            legend.box = "vertical",
            legend.spacing = grid::unit(0.1, "cm"),
            legend.margin = ggplot2::margin(0, 0, 0, 0)
        )
    
    .print_debug("Plot formatting completed", verbose, debug, type = "info", "PLOT")
    
    return(plot)
}