#' Create Multi-Line Plot for Single Country
#' 
#' @description
#' Internal function that creates a line plot with multiple indicators for one country.
#' Each indicator gets its own line with distinct color coding.
#' 
#' Used by: pp_plot_series (via .plot_core), .plot_multi_by_ctry
#' Uses: .scale_y, .scale_x, .theme_plot, .add_date_markers
#' 
#' Features:
#' - Multiple lines (one per indicator)
#' - Color coding by indicator
#' - Single scale
#' - Optional key date markers
#'
#' @keywords internal
.plot_multi_indic_one_ctry <- function(data,                    # data.frame: input data for single country
                                           y_axis = NULL,            # string/NULL: y-axis label
                                           key_dates = NULL,         # data.frame/NULL: event markers data
                                           theme = list(base_size = 16)) {         # numeric: base font size in points
    
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = Date, y = Value, color = Variable)) +
        ggplot2::geom_line(linewidth = 1.2) +
        .scale_y(y_axis, n_breaks = theme$n_breaks) +
        ggplot2::scale_color_manual(values = custom_colors[1:length(unique(data$Variable))]) +
        .scale_x(data) +
        ggplot2::labs(x = NULL, color = "") +
        .theme_plot(theme = theme)
    
    # Add key dates if specified
    plot <- .add_date_markers(plot, key_dates)
    
    return(plot)
}



#' Create Multi-Line Plot for Multiple Countries
#' 
#' @description
#' Internal function that creates a line plot comparing one indicator across
#' multiple countries. Used for cross-country comparison over time.
#' 
#' Used by: pp_plot_series (via .plot_core), .plot_multi_by_indic
#' Uses: .scale_y, .scale_x, .theme_plot, .add_date_markers
#' 
#' Features:
#' - One line per country
#' - Color coding by country
#' - Single scale
#' - Optional key date markers
#'
#' @keywords internal
.plot_one_indic_multi_ctry <- function(data,                    # data.frame: input data for multiple countries
                                             y_axis = NULL,            # string/NULL: y-axis label
                                             key_dates = NULL,         # data.frame/NULL: event markers data
                                             theme = list(base_size = 16)) {         # numeric: base font size in points
    
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = Date, y = Value, color = Country)) +
        ggplot2::geom_line(linewidth = 1.2) +
        .scale_y(y_axis, n_breaks = theme$n_breaks) +
        ggplot2::scale_color_manual(values = custom_colors[1:length(unique(data$Country))]) +
        .scale_x(data) +
        ggplot2::labs(x = NULL,
             y = y_axis,
             color = "") +
        .theme_plot(theme = theme)
    
    # Add key dates if specified
    plot <- .add_date_markers(plot, key_dates)
    
    return(plot)
}



#######################################################################################################################################################
# BASIC MULTI PLOTS


#' Create Panel Plot with Indicators as Facets
#' 
#' @description
#' Internal function that creates a multi-panel plot with one panel per indicator.
#' Each panel shows cross-country comparison for one indicator.
#' 
#' Used by: pp_plot_series (via .plot_core)
#' Uses: .plot_one_indic_multi_ctry, .add_date_markers, wrap_plots
#' 
#' Features:
#' - One panel per indicator
#' - Consistent scales across panels
#' - Panel titles
#' - Shared legend
#' - Optional key date markers
#'
#' @keywords internal
.plot_multi_by_indic <- function(data,                    # data.frame: input data
                                     y_axis = NULL,            # vector/NULL: y-axis labels for each panel
                                     key_dates = NULL,         # data.frame/NULL: event markers data
                                     theme = list(base_size = 14),  # list: must contain base font size
                                     subfig_title = TRUE) {    # logical/character: panel titles

    
    indicators_list <- unique(data$Variable)
    n_indicators <- length(indicators_list)
    
    # Assuming subfig_title can be NULL, TRUE, FALSE, or a character vector
    add_subfig_title <- isTRUE(subfig_title) || is.character(subfig_title)

    # If not specified but TRUE, use column Variable
    subfig_title <- if (isTRUE(subfig_title)) indicators_list else subfig_title
    y_axis <- if (isTRUE(y_axis)) indicators_list else y_axis


    # Create plots without legends
    plot_list <- lapply(1:n_indicators, function(i) {
        subdata <- data[data$Variable == indicators_list[i],]
        plot <- .plot_one_indic_multi_ctry(
            data = subdata,
            y_axis = y_axis[i],
            key_dates = NULL,
            theme = theme
        )
        # Add key dates if specified
        plot <- .add_date_markers(plot, key_dates, add_subfig_title)

        # Add title with extra margin if key_dates are present
        if (add_subfig_title) {
            plot <- plot + ggplot2::ggtitle(subfig_title[i]) +
                ggplot2::theme(plot.title = ggplot2::element_text(
                                            size = theme$base_size,  # Scale title size
                                            margin = ggplot2::margin(b = ifelse(!is.null(key_dates), 25, 5), t = 0, unit = "pt")
                                            ))
        }
        
        # Remove legend
        plot + ggplot2::theme(legend.position = "none")
    })
    
    # Combine plots in a grid
    combined_plot <- patchwork::wrap_plots(plot_list)
    
    return(combined_plot)
}


#' Create Panel Plot with Countries as Facets
#' 
#' @description
#' Internal function that creates a multi-panel plot with one panel per country.
#' Each panel shows multiple indicators for one country.
#' 
#' Used by: pp_plot_series (via .plot_core)
#' Uses: .plot_multi_indic_one_ctry, .add_date_markers, wrap_plots
#' 
#' Features:
#' - One panel per country
#' - Consistent scales across panels
#' - Panel titles
#' - Shared legend
#' - Optional key date markers
#'
#' @keywords internal
.plot_multi_by_ctry <- function(data,                    # data.frame: input data
                                    y_axis = NULL,            # vector/NULL: y-axis labels for each panel
                                    key_dates = NULL,         # data.frame/NULL: event markers data
                                    theme = list(base_size = 14), # list: must contain base font size
                                    subfig_title = TRUE) {    # logical/character: panel titles
    
    # Get list of unique countries
    countries_list <- unique(data$Country)
    n_countries <- length(countries_list)

    # Assuming subfig_title can be NULL, TRUE, FALSE, or a character vector
    add_subfig_title <- isTRUE(subfig_title) || is.character(subfig_title)

    # If not specified but TRUE, use column Country
    subfig_title <- if (isTRUE(subfig_title)) countries_list else subfig_title
    y_axis <- if (isTRUE(y_axis)) countries_list else y_axis
    
    # Create plots without legends, one for each country
    plot_list <- lapply(1:n_countries, function(i) {
        # Filter data for current country
        subdata <- data[data$Country == countries_list[i],]
        
        # Use existing .plot_multi_indic_one_ctry function
        plot <- .plot_multi_indic_one_ctry(
            data = subdata,
            y_axis = y_axis[i],
            key_dates = NULL,
            theme = theme
        )
        # Add key dates if specified
        plot <- .add_date_markers(plot, key_dates, add_subfig_title)

        # Add title only if title parameter is TRUE
        if (add_subfig_title) {
            plot <- plot + ggplot2::ggtitle(subfig_title[i]) +
                ggplot2::theme(plot.title = ggplot2::element_text(
                                            size = theme$base_size,  # Scale title size
                                            margin = ggplot2::margin(b = ifelse(!is.null(key_dates), 25, 5), t = 0, unit = "pt")
                                            ))
        }
        
        # Remove legend
        plot + ggplot2::theme(legend.position = "none")

    })
    
    # Combine plots in a grid
    combined_plot <- patchwork::wrap_plots(plot_list)
    
    return(combined_plot)
}




#######################################################################################################################################################
# AREA PLOTS




#' Create Stacked Area Plot for One Country
#' 
#' @description
#' Internal function that creates a stacked area plot with first variable as line.
#' Used for showing composition over time for multiple indicators in one country.
#' 
#' Used by: pp_plot_series (via .plot_core)
#' Uses: reshape, .scale_y, .scale_x, .theme_plot, .add_date_markers
#' 
#' Features:
#' - First variable plotted as line on top
#' - Remaining variables as stacked areas
#' - Custom color palette with black line for first variable
#' - Optional key date markers
#'
#' @keywords internal
.plot_one_ctry_area <- function(data,                    # data.frame: input data for single country
                                    y_axis = NULL,            # string/NULL: y-axis label
                                    key_dates = NULL,         # data.frame/NULL: event markers data
                                    theme = list(base_size = 16) # list: must contain base font size
                                    ) { 

    # Ensure data is ordered by Date
    data <- data[order(data$Date), ]
    variable_order <- unique(data$Variable)
    sum_variable <- variable_order[1]
    
    # Prepare data for area plot
    wide_data <- reshape(data[, c("Date", "Country", "Variable", "Value")],
                        direction = "wide",
                        idvar = c("Date", "Country"),
                        timevar = "Variable",
                        v.names = "Value")
    
    colnames(wide_data) <- gsub("Value\\.", "", colnames(wide_data))
    
    data_area <- reshape(wide_data,
                             direction = "long",
                             varying = variable_order,
                             v.names = "Value",
                             timevar = "Variable",
                             times = variable_order)
    
    data_area$Variable <- factor(data_area$Variable, levels = variable_order)
    
    color_palette <- c("black", custom_colors[2:length(variable_order)])
    names(color_palette) <- variable_order
    
    # Create plot
    plot <- ggplot2::ggplot(data_area, ggplot2::aes(x = Date, y = Value)) +
        ggplot2::geom_area(data = subset(data_area, Variable != sum_variable),
                 ggplot2::aes(fill = Variable), position = "stack") +
        ggplot2::geom_line(data = subset(data_area, Variable == sum_variable),
                 ggplot2::aes(color = Variable), linewidth = 1.2) +
        .scale_y(y_axis, n_breaks = theme$n_breaks) +
        .scale_x(data) +
        ggplot2::scale_fill_manual(values = color_palette[-1], breaks = variable_order[-1]) +
        ggplot2::scale_color_manual(values = color_palette[1], breaks = variable_order[1]) +
        ggplot2::labs(x = NULL, fill = "", color = "") +
        .theme_plot(theme = theme) +
        ggplot2::guides(fill = ggplot2::guide_legend(order = 2, override.aes = list(linetype = 0, shape = 15)),
               color = ggplot2::guide_legend(override.aes = list(linetype = 1, shape = NA)))
    
    # Add key dates if specified
    plot <- .add_date_markers(plot, key_dates)
    
    return(plot)
}


#' Create Multi-Panel Area Plots
#' 
#' @description
#' Internal function that creates a multi-panel plot with stacked areas for multiple
#' countries. Each panel shows composition over time for one country.
#' 
#' Used by: pp_plot_series (via .plot_core)
#' Uses: .plot_one_ctry_area, .add_date_markers, wrap_plots
#' 
#' Features:
#' - One stacked area panel per country
#' - Consistent scales across panels
#' - Optional panel titles
#' - Shared legend
#' - Key date markers support
#' - First variable as line on areas
#'
#' @keywords internal
.plot_multi_area <- function(data,                    # data.frame: input data for multiple countries
                              y_axis = NULL,            # vector/NULL: y-axis labels for each panel
                              key_dates = NULL,         # data.frame/NULL: event markers data
                              theme = list(base_size = 14), # list: must contain base font size
                              subfig_title = TRUE) {    # logical/character: panel titles
    
    # Get list of unique countries
    countries_list <- unique(data$Country)
    n_countries <- length(countries_list)

    # Assuming subfig_title can be NULL, TRUE, FALSE, or a character vector
    add_subfig_title <- isTRUE(subfig_title) || is.character(subfig_title)

    # If not specified but TRUE, use column Country
    subfig_title <- if (isTRUE(subfig_title)) countries_list else subfig_title
    y_axis <- if (isTRUE(y_axis)) countries_list else y_axis
    
    # Create plots without legends, one for each country
    plot_list <- lapply(1:n_countries, function(i) {
        # Filter data for current country
        subdata <- data[data$Country == countries_list[i],]
        
        # Use existing .plot_multi_indic_one_ctry function
        plot <- .plot_one_ctry_area(
            data = subdata,
            y_axis = y_axis[i],
            key_dates = NULL,
            theme = theme
        )
        # Add key dates if specified
        plot <- .add_date_markers(plot, key_dates, add_subfig_title)

        # Add title only if title parameter is TRUE
        if (add_subfig_title) {
            plot <- plot + ggplot2::ggtitle(subfig_title[i]) +
                ggplot2::theme(plot.title = ggplot2::element_text(
                                            size = theme$base_size,  # Scale title size
                                            margin = ggplot2::margin(b = ifelse(!is.null(key_dates), 25, 5), t = 0, unit = "pt")
                                            ))
        }
        
        # Remove legend
        plot + ggplot2::theme(legend.position = "none")

    })
    
    # Combine plots in a grid
    combined_plot <- patchwork::wrap_plots(plot_list)
    
    return(combined_plot)
}



#######################################################################################################################################################
# DUAL-AXIS PLOTS


#' @keywords internal
.plot_one_indic_multi_ctry_two_axes <- function(data,                    # data.frame: input data for single country
                                                    y_axis = NULL,            # vector/NULL: labels for both axes
                                                    key_dates = NULL,         # data.frame/NULL: event markers data
                                                    right_axis = TRUE,        # logical/character: variables for right axis
                                                    theme = list(base_size = 16, align_zeros = TRUE), # list: must contain base font size
                                                    verbose = TRUE,           # logical: controls information messages
                                                    debug = FALSE) {          # logical: controls debug messages



    data$Variable <- data$Country
    data$ISO <- "USA"
    data$Country <- "USA"

    args <- list(
        data = data,
        y_axis = y_axis,
        key_dates = key_dates,
        right_axis = right_axis,
        theme = theme,
        verbose = verbose,
        debug = debug
    )
    plot <- do.call(.plot_multi_indic_one_ctry_two_axes, args)
    plot$type_legend <- "Country"
    return(plot)

}



#' Create Dual-Axis Plot for One Country
#' 
#' @description
#' Internal function that creates a plot with two y-axes for different scales.
#' Handles automatic or manual assignment of variables to axes based on scales.
#' 
#' Used by: pp_plot_series (via .plot_core)
#' Uses: .assign_axes, .scale_x, .theme_plot, .add_date_markers
#' 
#' Features:
#' - Two independent y-axes
#' - Automatic scale normalization
#' - Manual or automatic variable assignment
#' - Legend with axis indication
#'
#' @keywords internal
.plot_multi_indic_one_ctry_two_axes <- function(data,                    # data.frame: input data for single country
                                                    y_axis = NULL,            # vector/NULL: labels for both axes
                                                    key_dates = NULL,         # data.frame/NULL: event markers data
                                                    right_axis = TRUE,        # logical/character: variables for right axis
                                                    theme = list(base_size = 16, align_zeros = TRUE), # list: must contain base font size
                                                    verbose = TRUE,           # logical: controls information messages
                                                    debug = FALSE) {          # logical: controls debug messages
    

    dd <<- data

    if (is.null(theme$align_zeros)) { 
        theme$align_zeros <- TRUE 
    }

    # Validate inputs
    if(length(unique(data$Variable)) < 2) {
        stop("Need at least 2 indicators for two-axis plot")
    }
    
    if(!is.null(y_axis) && length(y_axis) != 2) {
        warning("y_axis must be NULL or a vector of length 2 - input ignored")
        y_axis <- NULL
    }
    
    # Determine right axis indicators if not specified
    if(isTRUE(right_axis)) {
        right_indicators <- .assign_axes(data, verbose, debug)
    } else {
        right_indicators <- right_axis
    }
    
    # Validate right_axis
    if(!all(right_indicators %in% unique(data$Variable))) {
        stop("Invalid right_axis specified")
    }
    
    # Split data into left and right axis indicators
    left_data <- subset(data, !Variable %in% right_indicators)
    right_data <- subset(data, Variable %in% right_indicators)
    
    # Calculate scaling factor for right axis only if align_zeros is TRUE
    left_range <- range(left_data$Value, na.rm = TRUE)
    right_range <- range(right_data$Value, na.rm = TRUE)
    # print(paste0(" - left_max = ", left_max, " | left_range = ", left_range))
    # print(paste0(" - right_max = ", right_max, " | right_range = ", right_range))

    # print(" - left_range = ")
    # print(left_range)
    # print(" - right_range = ")
    # print(right_range)

    if (isTRUE(theme$align_zeros)) {
        # Calculate scaling factor for aligned zeros
        left_max <- max(abs(left_data$Value), na.rm = TRUE)
        right_max <- max(abs(right_data$Value), na.rm = TRUE)
        scale_factor <- left_max / right_max
        
        # Create aligned secondary axis
        sec_axis_obj <- ggplot2::sec_axis(
            ~./scale_factor,
            name = if(!is.null(y_axis)) y_axis[2] else NULL,
            labels = scales::comma_format(scale = 1),
            breaks = scales::pretty_breaks(n = 9)
        )
        
        # Create base plot with scaled right axis
        plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = Date, y = Value, colour = Variable)) +
            ggplot2::geom_line(data = left_data, linewidth = 1.2) +
            ggplot2::geom_line(data = right_data,
                              ggplot2::aes(y = Value * scale_factor),
                              linewidth = 1.2)

    } else if (is.numeric(theme$align_zeros)) {


        offset <- theme$align_zeros
        print(paste0("Offset = ", offset))
        # Compute the range and offset
        left_range <- range(left_data$Value, na.rm = TRUE)
        right_range <- range(right_data$Value, na.rm = TRUE)

        # Compute the scale factor based on the difference from 100
        scale_factor <- (left_range[2] - offset) / (right_range[2] - offset)

        # Create aligned secondary axis
        sec_axis_obj <- ggplot2::sec_axis(
                                          ~ (.-offset) / scale_factor + offset,  # Reverse the transformation
                                          name = if (!is.null(y_axis)) y_axis[2] else NULL,
                                          labels = scales::comma_format(scale = 1),
                                          breaks = scales::pretty_breaks(n = 9)
                                          )

        # Create base plot
        plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = Date, y = Value, colour = Variable)) +
            ggplot2::geom_line(data = left_data, linewidth = 1.2) +
            ggplot2::geom_line(data = right_data, 
                              ggplot2::aes(y = (Value - offset) * scale_factor + offset),
                              linewidth = 1.2)
    } else {
        # No alignment - independent scales
        scale_factor <- diff(left_range) / diff(right_range)
        offset <- left_range[1] - right_range[1] * scale_factor
        
        # Create secondary axis
        sec_axis_obj <- ggplot2::sec_axis(
            ~ (. - offset) / scale_factor,
            name = if(!is.null(y_axis)) y_axis[2] else NULL,
            labels = scales::comma_format(scale = 1),
            breaks = scales::pretty_breaks(n = 9)
        )
        
        # Create base plot
        plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = Date, y = Value, colour = Variable)) +
            ggplot2::geom_line(data = left_data, linewidth = 1.2) +
            ggplot2::geom_line(data = right_data,
                              ggplot2::aes(y = Value * scale_factor + offset),
                              linewidth = 1.2)
    }
    
    # Add scales and theme
    plot <- plot +
        .scale_x(data) +
        ggplot2::scale_y_continuous(
            name = if(!is.null(y_axis)) y_axis[1] else NULL,
            labels = scales::comma_format(scale = 1),
            breaks = scales::pretty_breaks(n = 9),
            sec.axis = sec_axis_obj
        ) +
        ggplot2::scale_color_manual(
            values = custom_colors[1:length(unique(data$Variable))],
            labels = function(x) {
                axis_indicator <- ifelse(x %in% right_indicators, " (R)", " (L)")
                paste0(x, axis_indicator)
            }
        ) +
        ggplot2::labs(x = NULL, color = "") +
        .theme_plot(theme = theme)
    
    # Add key dates if specified
    plot <- .add_date_markers(plot, key_dates)
    
    # Type legend parameter is used elsewhere to identify legend type
    plot$type_legend <- "Variable"
    
    return(plot)
}