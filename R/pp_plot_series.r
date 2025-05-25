

#' Create Time Series Plots with Multiple Configuration Options
#' 
#' @description
#' Creates time series plots with support for single or multiple indicators,
#' single or multiple countries, stacked area plots, dual axes, and various visual
#' customizations (e.g., references and legend). 
#' The function automatically determines the most appropriate plot type
#' based on the input data structure and specified parameters.
#' 
#' @param data data.frame that must contain the following columns:
#'        - ISO: ISO 3-letter country codes (required)
#'        - Date: dates in Date format (required)
#'        - Variable: indicator names (required)
#'        - Value: numeric values (required)
#'        - Country: country names (optional, auto-generated from ISO if missing)
#'        - Reference: source citations (required if reference=TRUE)
#' @param y_axis Single string, vector of strings, TRUE, FALSE, or NULL:
#'        - Single string: common y-axis label for all plots
#'        - Vector of strings: individual labels for each plot/axis
#'        - TRUE: use Variable names as labels
#'        - FALSE/NULL: no axis labels
#'        For dual axis plots, must be vector of length 2 for left and right axes.
#'        For multi-indicator plots, length must match number of indicators.
#' @param right_axis controls the right y-axis behavior:
#'        - NULL/FALSE: no right axis (default)
#'        - TRUE: automatically assigns variables to right axis based on scale
#'        - character vector: names of variables to assign to right axis
#'        Only available for plots with 2 or more indicators.
#'        Cannot be used with area=TRUE.
#' @param key_dates data.frame for marking significant events:
#'        - Must have exactly 2 columns
#'        - Column 1: event descriptions
#'        - Column 2: corresponding dates in Date format
#'        - Events outside plot date range are automatically filtered
#'        - NULL for no event markers
#' @param area logical; if TRUE, creates a stacked area plot:
#'        - Only for single-country, multi-indicator plots
#'        - First variable shown as line on top of stacked areas
#'        - Cannot be used with right_axis=TRUE
#'        Default is FALSE.
#' @param by_indicator logical; controls faceting in multi-panel plots:
#'        - TRUE: creates separate panels for each indicator
#'        - FALSE: creates separate panels for each country
#'        - Ignored for single country or single indicator plots
#'        Default is TRUE.
#' @param filename character string for saving plots:
#'        - Specify name without extension
#'        - Plots saved as both PNG and PDF in 'img/' directory
#'        - NULL/FALSE for no file output
#' @param print logical; controls plot display:
#'        - TRUE: displays the plot
#'        - FALSE: creates but doesn't display the plot
#'        Default is TRUE.
#' @param legend logical; controls legend display:
#'        - TRUE: includes a legend
#'        - FALSE: omits the legend
#'        Default is TRUE.
#' @param reference logical; controls reference panel:
#'        - TRUE: includes reference citations panel (requires Reference column)
#'        - FALSE: omits reference panel
#'        - Automatically set to FALSE if Reference column missing/empty
#'        Default is TRUE.
#' @param title character string for main plot title:
#'        - NULL/FALSE for no title
#' @param subtitle character string for plot subtitle:
#'        - NULL/FALSE for no subtitle
#' @param subfig_title controls subplot titles in multi-panel plots:
#'        - TRUE: uses default titles (Variable or Country names)
#'        - FALSE: no subplot titles
#'        - Character vector: custom titles (length must match number of panels)
#'        - Ignored for single panel plots
#'        Default is TRUE.
#' @param verbose logical; controls information messages:
#'        - TRUE: prints processing information
#'        - FALSE: suppresses information messages
#'        Default is TRUE.
#' @param debug logical; controls debugging output:
#'        - TRUE: prints detailed debugging information
#'        - FALSE: suppresses debugging information
#'        Default is FALSE.
#' @param size numeric or NULL; controls plot dimensions:
#'        - 1: small (10x7 inches)
#'        - 2: medium (15x7 inches)
#'        - 3: large (15x10 inches)
#'        - 4: extra large (20x10 inches)
#'        - NULL: auto-sizes based on data
#' @param theme list
#' @param bg character; controls plot background color:
#'        - "transparent": transparent background (default)
#'        - Any valid color string: sets background to that color
#'
#' @return A ggplot2 object containing the time series plot
#'
#' @details
#' The function automatically selects the appropriate plotting implementation based on
#' the data structure and parameters:
#'
#' * Single country, multiple indicators:
#'   - Basic line plot (default)
#'   - Stacked area plot (if area = TRUE)
#'   - Dual axis plot (if right_axis specified)
#'
#' * Multiple countries, single indicator:
#'   - Multi-line plot with color-coded countries
#'
#' * Multiple countries, multiple indicators:
#'   - Faceted plot by indicator (if by_indicator = TRUE)
#'   - Faceted plot by country (if by_indicator = FALSE)
#'
#' The function includes multiple features for customization:
#' - Automatic axis scaling and formatting
#' - Event markers with customizable labels
#' - Reference panel for data sources
#' - Flexible legend positioning
#' - Consistent theme across all plot types
#'
#' @examples
#' # Basic time series for one country and multiple indicators
#' data <- data.frame(
#'   Date = rep(seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by = "year"), 2),
#'   ISO = "FRA",
#'   Variable = rep(c("GDP", "Inflation"), each = 21),
#'   Value = c(rnorm(21, 2, 0.5), rnorm(21, 3, 1)),
#'   Reference = "World Bank"
#' )
#' pp_plot_series(data, y_axis = "Percent")
#'
#' # Dual axis plot with manual axis assignment
#' pp_plot_series(data, 
#'                y_axis = c("GDP Growth (%)", "Inflation (%)"),
#'                right_axis = "Inflation")
#'
#' # Stacked area plot
#' pp_plot_series(data, 
#'                y_axis = "Percent",
#'                area = TRUE)
#'
#' # Multiple countries with event markers
#' data_multi <- data.frame(
#'   Date = rep(seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by = "year"), 4),
#'   ISO = rep(c("FRA", "DEU"), each = 42),
#'   Variable = rep(rep(c("GDP", "Inflation"), each = 21), 2),
#'   Value = rnorm(84, 2, 0.5),
#'   Reference = "World Bank"
#' )
#' events <- data.frame(
#'   Event = c("Financial Crisis", "Euro Crisis"),
#'   Date = as.Date(c("2008-09-15", "2010-05-01"))
#' )
#' pp_plot_series(data_multi,
#'                y_axis = "Percent",
#'                key_dates = events,
#'                by_indicator = TRUE)
#'
#' @seealso 
#' \code{\link{pp_plot_bar}} for bar plots
#' \code{\link{pp_plot_scatter}} for scatter plots
#' \code{\link{isomapper::im_from_iso}} for ISO code to country name conversion
#'
#' @family plotting functions
#' @export
pp_plot_series <- function(data,                # data.frame with required columns: ISO, Date, Variable, Value; Country auto-generated if missing; Reference needed if reference=TRUE
                          y_axis = NULL,        # string/vector/TRUE/FALSE/NULL: axis labels (single, dual, or per-indicator)
                          right_axis = NULL,    # TRUE/FALSE/NULL/char vector: variables for right axis; TRUE for auto-assignment
                          key_dates = NULL,     # data.frame with 2 cols or NULL: event descriptions and dates
                          area = FALSE,         # logical: create stacked area plot for single country multi-indicator
                          by_indicator = TRUE,  # logical: facet by indicator (TRUE) or country (FALSE) for multi-panel plots
                          filename = NULL,      # string/NULL/FALSE: output filename without extension (saved as PNG and PDF)
                          print = TRUE,         # logical: display the plot
                          legend = TRUE,        # logical: include legend
                          reference = TRUE,     # logical: include reference panel (requires Reference column)
                          title = NULL,         # string/NULL/FALSE: main title
                          subtitle = NULL,      # string/NULL/FALSE: subtitle
                          subfig_title = NULL,  # TRUE/FALSE/char vector: titles for multi-panel plots
                          verbose = TRUE,       # logical: print processing information
                          debug = FALSE,        # logical: print detailed debugging information
                          size = NULL,          # 1-4 or NULL: plot size presets or auto-sizing
                          theme = list(base_size = 16), # list: must contain base font size
                          title_scale = 1.15,      # numeric: scaling factor for title
                          title_face = "bold",     # string: "plain", "bold", "italic", or "bold.italic"
                          subtitle_scale = 1,      # numeric: scaling factor for subtitle
                          ref_scale = 1,          # numeric: scaling factor for reference
                          bg = "transparent") {     # character: background color for plot ("transparent" or color string)
    
    args <- as.list(environment())
    args$plot_type <- "series"
    
    # Validate args
    validated_args <- .validate_series_args(args)
    
    # Call core plotting function
    do.call(.plot_core, validated_args)
}




#######################################################################################################################################################
# BASIC PLOTS


#' Create Multi-Line Plot for One Country
#' 
#' @description
#' Internal function that creates a simple multi-line plot for multiple indicators
#' in a single country. Basic implementation for time series comparison.
#' 
#' Used by: pp_plot_series (via .plot_core), .plot_multi_by_ctry
#' Uses: .scale_y, .scale_x, .theme_plot, .add_date_markers
#' 
#' Features:
#' - Multiple indicators as separate lines
#' - Consistent color coding
#' - Single y-axis scaling
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
        ggplot2::scale_color_manual(values = mvcommon::mv_get_colors()[1:length(unique(data$Variable))]) +
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
        ggplot2::scale_color_manual(values = mvcommon::mv_get_colors()[1:length(unique(data$Country))]) +
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
    
    color_palette <- c("black", mvcommon::mv_get_colors()[2:length(variable_order)])
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

        # Create the base plot with aligned and scaled axes
        plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = Date, y = Value, colour = Variable)) +
            ggplot2::geom_line(data = left_data, linewidth = 1.2) +
            ggplot2::geom_line(
                               data = right_data,
                               ggplot2::aes(y = (Value - offset) * scale_factor + offset),
                               linewidth = 1.2
                               ) +
            ggplot2::scale_y_continuous(
                                        name = if (!is.null(y_axis)) y_axis[1] else NULL,
                                        sec.axis = sec_axis_obj
            )


    } else {
        # Create transformation formula for independent scales
        trans_formula <- function(x) {
            # Transform from primary scale to secondary scale
            primary_pos <- (x - left_range[1]) / diff(left_range)  # Position in [0,1]
            right_range[1] + primary_pos * diff(right_range)       # Map to right scale
        }
        
        # Create independent secondary axis
        sec_axis_obj <- ggplot2::sec_axis(
            ~trans_formula(.),
            name = if(!is.null(y_axis)) y_axis[2] else NULL,
            breaks = scales::pretty_breaks(n = 9),
            labels = scales::comma_format(scale = 1)
        )
        
        # Create base plot with proper scaling for right axis data
        plot <- ggplot2::ggplot(mapping = ggplot2::aes(x = Date, colour = Variable)) +
            # Left axis data at original scale
            ggplot2::geom_line(data = left_data,
                              ggplot2::aes(y = Value),
                              linewidth = 1.2) +
            # Right axis data transformed to left axis scale
            ggplot2::geom_line(data = right_data,
                              ggplot2::aes(y = (Value - right_range[1])/diff(right_range) * 
                                           diff(left_range) + left_range[1]),
                              linewidth = 1.2)
    }
    
    # Set up common scales and aesthetics
    plot <- plot +
        .scale_x(data) +
        ggplot2::scale_color_manual(
            values = mvcommon::mv_get_colors()[1:length(unique(data$Variable))],
            breaks = unique(data$Variable),
            labels = function(x) {
                paste0(x, " (", ifelse(x %in% right_indicators, "Right", "Left"), ")")
            }
        )
    
    # Add y-axis scales and labels
    plot <- plot +
        ggplot2::scale_y_continuous(
            name = if(!is.null(y_axis)) y_axis[1] else NULL,
            labels = scales::comma_format(scale = 1),
            breaks = scales::pretty_breaks(n = 9),
            sec.axis = sec_axis_obj
        )
    
    
    # Add theme and labels with consistent axis text rotation
    plot <- plot + .theme_plot(theme = theme) + 
                ggplot2::labs(x = NULL, color = "")
    
    # Add Date at the location used by .add_date_markers()
    plot$data$Date <- plot$layers[[1]]$data$Date

    # Add key dates if specified
    plot <- .add_date_markers(plot, key_dates)
    
    return(plot)
}


#' Determine Right Axis Variable Assignment
#' 
#' @description
#' Internal function that analyzes variable ranges to determine optimal
#' axis assignment for dual-axis plots. Ensures sensible scale combinations.
#' 
#' Used by: .plot_multi_indic_one_ctry_two_axes
#' Uses: No direct function calls
#' 
#' Assignment logic:
#' - Two variables: Larger range to left axis
#' - Multiple variables: Uses gap analysis to split into two groups
#' - Preserves scale relationships
#'
#' @keywords internal
.assign_axes <- function(data,                    # data.frame: input data
                          verbose = TRUE,           # logical: controls information messages
                          debug = FALSE) {          # logical: controls debug messages

    # Get unique indicators
    indicators <- unique(data$Variable)
    if(length(indicators) < 2) {
        stop("Need at least 2 indicators for two-axis plot")
    }
    
    # Calculate ranges for each indicator
    ranges <- sapply(indicators, function(ind) {
        values <- data$Value[data$Variable == ind]
        max_abs <- max(abs(values), na.rm = TRUE)
        return(max_abs)
    })
    
    # If we have exactly two indicators, put larger range on left
    if(length(indicators) == 2) {
        right_indicators <- names(ranges)[which.min(ranges)]
    } else {
        # For more than two indicators:
        sorted_ranges <- sort(ranges, decreasing = TRUE)
        gaps <- diff(sorted_ranges)
        split_point <- which.max(gaps)
        right_indicators <- names(ranges)[ranges <= sorted_ranges[split_point + 1]]
    }
    
    if(verbose) {
        msg <- paste0("Automatic axis assignment - Right axis: ", paste(right_indicators, collapse = ", "))
        mvcommon::mv_debug(msg, verbose, debug, type = "info", "AXES")
    }
    
    return(right_indicators)
}






#######################################################################################################################################################
# DATE MARKERS



#' Add Event Markers to Time Series Plots
#' 
#' @description
#' Internal function that adds vertical lines and labels for significant dates.
#' Handles positioning and appearance of event markers.
#' 
#' Used by: All time series plotting functions
#' Uses: No direct function calls
#' 
#' Features:
#' - Vertical lines at event dates
#' - Angled text labels
#' - Automatic date range filtering
#' - Margin adjustment for labels
#' - Title-aware positioning
#'
#' @keywords internal
.add_date_markers <- function(plot,                    # ggplot object: base plot to add markers to
                               key_dates,                # data.frame/NULL: event data with dates and labels
                               title = FALSE) {          # logical: whether plot has title (affects margins)

    # If no key dates, return plot unchanged
    if (is.null(key_dates) || nrow(key_dates) == 0) {
        return(plot)
    }

    # mvcommon::mv_debug("add key dates", TRUE, FALSE, type = "info", "DATES")


    if (!is.null(key_dates) && nrow(key_dates) > 0) {

        # Extract date range from plot data
        start_date <- min(plot$data$Date, na.rm = TRUE)
        end_date <- max(plot$data$Date, na.rm = TRUE)

        # Create data frame of key dates
        key_dates_df <- data.frame(
            event = key_dates[,1],
            date = as.Date(key_dates[,2])
        )

        # Filter key dates to only include those within the plot's date range
        key_dates_df <- key_dates_df[
            key_dates_df$date >= start_date & 
            key_dates_df$date <= end_date,
        ]

        # If no dates remain after filtering, return plot unchanged
        if (nrow(key_dates_df) == 0) {
            return(plot)
        }

        # Get the current theme
        current_theme <- plot$theme

        extra_top_margin <- if (title) 0 else 20
        
        # Extract the current margins if they exist
        if (!is.null(current_theme$plot.margin)) {
            current_margins <- as.numeric(current_theme$plot.margin)
            new_margins <- ggplot2::margin(
                t = current_margins[1] + extra_top_margin,       # Set top margin for key dates
                r = current_margins[2],  # Preserve right margin
                b = current_margins[3],  # Preserve bottom margin
                l = current_margins[4],  # Preserve left margin
                unit = "pt"
            )
        } else {
            # If no margins exist, create new ones
            new_margins <- ggplot2::margin(t = 20, r = 0, b = 0, l = 0, unit = "pt")
        }
        
        plot <- plot +
            ggplot2::geom_vline(data = key_dates_df, 
                      ggplot2::aes(xintercept = date), 
                      color = "black", 
                      alpha = 0.5, 
                      size = 0.5) +
            ggplot2::geom_text(data = key_dates_df, 
                     ggplot2::aes(x = date, y = Inf, label = event),
                     vjust = -0.2,
                     hjust = 0.8,
                     angle = 0,
                     size = 4.5,
                     color = "black",
                     nudge_x = -1.) +
            ggplot2::coord_cartesian(clip = "off") +
            ggplot2::theme(plot.margin = new_margins)
    }
    
    return(plot)
}