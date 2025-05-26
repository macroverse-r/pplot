
#' Create Bar Plots with Statistical Indicators
#' 
#' @description
#' Creates bar plots for comparing values across countries, with support for mean or latest values,
#' statistical indicators (range, median), and various visual customizations including
#' color grouping and value labels. The function automatically determines the most appropriate
#' plot type based on the number of indicators and specified parameters.
#' 
#' @param data data.frame that must contain the following columns:
#'        - ISO: ISO 3-letter country codes (required)
#'        - Date: dates in Date format (required)
#'        - Variable: indicator names (required)
#'        - Value: numeric values (required)
#'        - Country: country names (optional, auto-generated from ISO if missing)
#'        - Reference: source citations (required if reference=TRUE)
#' @param y_axis Single string, TRUE, FALSE, or NULL:
#'        - String: y-axis label
#'        - TRUE: use Variable name as label
#'        - FALSE/NULL: no axis label
#' @param type Controls which value to display:
#'        - "Mean": average over time period (default)
#'        - "Last": most recent value
#'        Statistical indicators (range, median) adjust accordingly.
#' @param order Controls bar ordering:
#'        - "Variable": order by value (default)
#'        - "Alphabetical": order by country name
#'        - "Input": order based on the order in data
#'        - Numeric: order by nth variable (for multi-indicator plots)
#'        - TRUE: same as "Variable"
#'        - FALSE: same as "Alphabetical"
#' @param show_values Controls value label position:
#'        - "Bottom": labels below bars
#'        - "Top": labels above bars
#'        - FALSE: no labels
#'        Default is "Bottom".
#' @param show_year Controls year display in labels:
#'        - 0: no year
#'        - 1: short year format (YY)
#'        - 2: full year format (YYYY)
#'        Only applies when type="Last". Default is 1.
#' @param show_range Logical; whether to show min-max range:
#'        - TRUE: adds range indicators
#'        - FALSE: no range indicators
#'        Default is TRUE.
#' @param show_median Logical; whether to show median value:
#'        - TRUE: adds median markers when type="Mean"
#'        - FALSE: no median markers
#'        - Ignored when type="Last"
#'        Default is TRUE.
#' @param color Controls bar coloring scheme:
#'        For single indicator:
#'        - "Subregion": colors by geographical subregion (default)
#'        - "Region": colors by continent/major region
#'        - "Center-Periphery": colors by economic classification
#'        - "Hydrocarbon": colors by hydrocarbon exporter/importer status
#'        For multiple indicators:
#'        - "Variable": different color per indicator
#'        - FALSE/NULL: gray bars
#' @param filename Character string for saving plots:
#'        - Specify name without extension
#'        - Plots saved as both PNG and PDF in 'img/' directory
#'        - NULL/FALSE for no file output
#' @param print Logical; controls plot display:
#'        - TRUE: displays the plot
#'        - FALSE: creates but doesn't display the plot
#'        Default is TRUE.
#' @param legend Logical; controls legend display:
#'        - TRUE: includes a legend
#'        - FALSE: omits the legend
#'        Default is TRUE.
#' @param reference Logical; controls reference panel:
#'        - TRUE: includes reference citations panel (requires Reference column)
#'        - FALSE: omits reference panel
#'        Default is TRUE.
#' @param title Character string for main plot title:
#'        - NULL/FALSE for no title
#' @param subtitle Character string for plot subtitle:
#'        - NULL/FALSE for no subtitle
#' @param subfig_title For future use; currently ignored.
#' @param verbose Logical; controls information messages:
#'        - TRUE: prints processing information
#'        - FALSE: suppresses information messages
#'        Default is TRUE.
#' @param debug Logical; controls debugging output:
#'        - TRUE: prints detailed debugging information
#'        - FALSE: suppresses debugging information
#'        Default is FALSE.
#' @param size Numeric or NULL; controls plot dimensions:
#'        - 1: small (10x7 inches)
#'        - 2: medium (15x7 inches)
#'        - 3: large (15x10 inches)
#'        - 4: extra large (20x10 inches)
#'        - NULL: auto-sizes based on data
#' @param theme List
#' @param bg character; controls plot background color:
#'        - "transparent": transparent background (default)
#'        - Any valid color string: sets background to that color
#'
#' @return A ggplot2 object containing the bar plot
#'
#' @details
#' The function automatically selects the appropriate plotting implementation based on
#' the number of indicators:
#'
#' * Single indicator:
#'   - Simple bar plot with optional color grouping
#'   - Statistical indicators (range, median)
#'   - Flexible value and year labeling
#'
#' * Multiple indicators:
#'   - Grouped bar plot
#'   - Color-coded by indicator
#'   - Consistent statistical indicators across groups
#'
#' The function includes multiple features for customization:
#' - Automatic axis scaling and formatting
#' - Flexible ordering options
#' - Statistical annotations
#' - Reference panel for data sources
#' - Consistent theme across all variations
#'
#' @examples
#' # Basic bar plot with single indicator
#' data <- data.frame(
#'   Date = rep(seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by = "year"), 3),
#'   ISO = rep(c("FRA", "DEU", "ITA"), each = 21),
#'   Variable = "GDP Growth",
#'   Value = rnorm(63, 2, 0.5),
#'   Reference = "World Bank"
#' )
#' pp_plot_bar(data, y_axis = "Percent")
#'
#' # Bar plot with mean values and all statistical indicators
#' pp_plot_bar(data,
#'            type = "Mean",
#'            show_range = TRUE,
#'            show_median = TRUE,
#'            color = "Region")
#'
#' # Latest values with year labels
#' pp_plot_bar(data,
#'            type = "Last",
#'            show_year = 2,
#'            order = "Alphabetical")
#'
#' # Multiple indicators
#' data_multi <- data.frame(
#'   Date = rep(seq(as.Date("2000-01-01"), as.Date("2020-01-01"), by = "year"), 6),
#'   ISO = rep(rep(c("FRA", "DEU", "ITA"), each = 21), 2),
#'   Variable = rep(c("GDP Growth", "Inflation"), each = 63),
#'   Value = rnorm(126, 2, 0.5),
#'   Reference = "World Bank"
#' )
#' pp_plot_bar(data_multi,
#'            order = 1,  # Order by first indicator
#'            show_values = "Top")
#'
#' @seealso 
#' \code{\link{pp_plot_series}} for time series plots
#' \code{\link{pp_plot_scatter}} for scatter plots
#' \code{\link{isomapper::im_from_iso}} for ISO code to country name conversion
#'
#' @family plotting functions
#' @export
pp_plot_bar <- function(data,                    # data.frame with required columns: ISO, Date, Variable, Value
                       y_axis = NULL,            # string/TRUE/FALSE/NULL: y-axis label or auto-label
                       type = "Mean",            # "Mean"/"Last": display average or most recent value
                       order = "Variable",       # "Variable"/"Alphabetical"/"Input"/numeric/TRUE/FALSE: bar ordering
                       show_values = "Bottom",   # "Bottom"/"Top"/FALSE: value label position
                       show_year = 1,            # 0/1/2: no year/short year/full year (for type="Last")
                       show_range = TRUE,        # logical: display min-max range indicators
                       show_median = TRUE,       # logical: display median markers (for type="Mean")
                       color = "Subregion",      # "Subregion"/"Region"/"Center-Periphery"/"Variable"/FALSE: color scheme
                       filename = NULL,          # string/NULL/FALSE: output filename without extension
                       print = TRUE,             # logical: display the plot
                       legend = TRUE,            # logical: include legend
                       reference = TRUE,         # logical: include reference panel
                       title = NULL,             # string/NULL/FALSE: main title
                       subtitle = NULL,          # string/NULL/FALSE: subtitle
                       subfig_title = TRUE,      # Currently ignored in bar plots
                       verbose = TRUE,           # logical: print processing information
                       debug = FALSE,            # logical: print debugging information
                       size = NULL,              # 1/2/3/4/NULL: plot size presets or auto-sizing
                       theme = list(base_size = 14), # list: must contain base font size
                       title_scale = 1.15,      # numeric: scaling factor for title
                       title_face = "bold",     # string: "plain", "bold", "italic", or "bold.italic"
                       subtitle_scale = 1,      # numeric: scaling factor for subtitle
                       ref_scale = 1,          # numeric: scaling factor for reference
                       bg = "transparent") {     # character: background color for plot ("transparent" or color string)
    


    args <- as.list(environment())
    args$plot_type <- "bar"
    
    # Validate args
    validated_args <- .validate_bar_args(args)
    
    # Call core plotting function
    do.call(.plot_core, validated_args)
}




#######################################################################################################################################################
# PLOT SINGLE BAR

#' Create Single Indicator Bar Plot with Statistics
#' 
#' @description
#' Internal function that creates a bar plot for a single indicator across countries.
#' Handles mean/latest values, statistical indicators, and color grouping.
#' Core implementation for simple bar plots.
#' 
#' Used by: pp_plot_bar (via .plot_core)
#' Uses: .process_bar_data, .create_bar_labels, .create_color_mapping,
#'       .calculate_plot_limits, .create_bar_legend, .theme_plot
#' 
#' Produces bar plot with optional:
#' - Range indicators (min/max)
#' - Median markers
#' - Geographic/economic color grouping
#' - Value labels
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



#######################################################################################################################################################
# HELPER FUNCTIONS FOR PLOT BAR


# 1. Data Processing Module

#' Process Data for Bar Plot Creation
#' 
#' @description
#' Internal function that prepares and structures data for bar plots.
#' Handles data ordering, country name formatting, and factor level setting.
#' Essential preprocessing step for consistent bar plot creation.
#' 
#' Used by: .plot_one_indic_bar
#' Uses: .calculate_bar_statistics
#' 
#' Processing steps:
#' 1. Calculate statistics (mean/latest values)
#' 2. Order data based on specified method
#' 3. Format country names
#' 4. Set factor levels for consistent ordering
#'
#' @keywords internal
.process_bar_data <- function(data,                    # data.frame: raw input data
                               type,                     # string: "Mean"/"Last" - determines statistic calculation
                               order = "Variable") {      # string: "Variable"/"Alphabetical" - controls data ordering
    # Calculate statistics
    all_data <- .calculate_bar_statistics(data, type)
    
    # Order data based on order parameter
    if(is.numeric(order) || identical(order, "Variable")) {
        # For numeric or "Variable" order in single indicator case, 
        # always order by the variable values
        new_order <- order(all_data$display_value, decreasing = FALSE)
        all_data <- all_data[new_order, ]
    } else if(identical(order, "Alphabetical")) {
        all_data <- all_data[order(all_data$Country), ]
    } else if(identical(order, "Input")) {
        all_data <- all_data[order(match(all_data$Country, unique(data$Country))), ]
    }
    
    # Process country names
    all_data$Country <- gsub("Republic", "Rep.", all_data$Country)
    
    # Set factor levels based on the current order
    all_data$Country <- factor(all_data$Country, 
                             levels = unique(all_data$Country),
                             ordered = TRUE)
    
    return(all_data)
}


# 2. Label Creation Module


#' Create Labels for Bar Plot
#' 
#' @description
#' Internal function that generates formatted labels for bar plots.
#' Handles year display and country name formatting.
#' 
#' Used by: .plot_one_indic_bar, .plot_multi_indic_bar
#' Uses: No direct function calls
#' 
#' Features:
#' - Flexible year display options
#' - Conditional label formatting
#' - Support for short/long year formats
#' - Country name handling
#'
#' @keywords internal
.create_bar_labels <- function(data,                    # data.frame: data containing values to label
                               show_year,                 # numeric: 0/1/2 controlling year display format
                               type) {                    # string: "Mean"/"Last" determining value type

    create_labels <- function(country, year_short, year, show_year, type) {
        labels <- country
        if (show_year == 1 && type == "Last") {
            labels <- paste0(country, " (", year_short, ")")
        } else if (show_year == 2 && type == "Last") {
            labels <- paste0(country, "\n", year)
        }
        return(labels)
    }
    
    data$label <- create_labels(
        country = data$Country,
        year_short = data$YearShort,
        year = data$Year,
        show_year = show_year,
        type = type
    )
    
    return(data)
}

# 3. Plot Limits Calculator

#' Calculate Plot Limits and Margins
#' 
#' @description
#' Internal function that determines appropriate plot limits and margins based on data
#' and label placement. Ensures proper spacing for value labels and data visibility.
#' 
#' Used by: .plot_one_indic_bar, .plot_multi_indic_bar
#' Uses: No direct function calls
#' 
#' Features:
#' - Automatic margin calculation
#' - Label position consideration
#' - Bottom/top label accommodation
#' - Dynamic spacing adjustment
#'
#' @keywords internal
.calculate_plot_limits <- function(data,                    # data.frame: data containing values to plot
                                   show_values) {             # string/FALSE: "Bottom"/"Top" - value label position

    y_max <- max(c(data$display_value, data$max_value), na.rm = TRUE)
    y_min <- min(c(data$display_value, data$min_value), na.rm = TRUE)



    y_margin_top <- if (show_values %in% c(TRUE, "Top")) y_max * 0.08 else 0
    y_margin_bottom <- if (identical(show_values, "Bottom")) abs(y_min) * 0.08 else 0
    
    list(
        y_max = y_max,
        y_min = y_min,
        y_margin_top = y_margin_top,
        y_margin_bottom = y_margin_bottom
    )
}


# 4. Legend Creation Module

#' Create Legend for Bar Plot Statistics
#' 
#' @description
#' Internal function that creates a unified legend for bar plots showing statistical elements.
#' Handles mean/latest values, range indicators, and median markers.
#' 
#' Used by: .plot_one_indic_bar, .plot_multi_indic_bar
#' Uses: No direct function calls
#' 
#' Elements:
#' - Mean/latest value indicators
#' - Range bars (optional)
#' - Median markers (optional)
#' - Consistent ordering
#'
#' @keywords internal
.create_bar_legend <- function(data,                    # data.frame: data for legend creation
                                 type,                     # string: "Mean"/"Last" - determines legend text
                                 show_median,              # logical: whether to include median in legend
                                 show_range) {             # logical: whether to include range in legend

    legend_elements <- c(if(type == "Mean") "Mean values" else "Latest values")
    if(show_median && type == "Mean") legend_elements <- c(legend_elements, "Median values")
    if(show_range) legend_elements <- c(legend_elements, "Range (Min to Max)")
    
    legend_data <- data.frame(
        x = rep(1, length(legend_elements)),
        y = rep(1, length(legend_elements)),
        type = factor(legend_elements, levels = legend_elements)
    )
    
    list(
        data = legend_data,
        elements = legend_elements
    )
}




#' Calculate Statistical Measures for Bar Plots
#' 
#' @description
#' Internal function that computes statistical measures for each country/variable
#' combination. Handles both mean and latest value calculations along with
#' supporting statistics like median, min, and max.
#' 
#' Used by: .process_bar_data, .process_multi_bar_data
#' Uses: No direct function calls
#' 
#' Calculates for each group:
#' - Mean value
#' - Median value
#' - Latest value
#' - Min/Max values
#' - Number of data points
#' - Date information for latest values
#'
#' @keywords internal
.calculate_bar_statistics <- function(data,                    # data.frame: input data to calculate statistics from
                                      type = "Mean") {           # string: "Mean"/"Last" - determines primary statistic to calculate
    # Split data by Country and ISO
    split_data <- split(data, list(data$Country, data$ISO))
    
    # Process each group
    results <- lapply(split_data, function(group) {
        # Filter out NA values
        valid_data <- group[!is.na(group$Value), ]
        
        if (nrow(valid_data) == 0) {
            return(NULL)
        }
        
        # Calculate statistics
        mean_val <- mean(valid_data$Value)
        median_val <- median(valid_data$Value)
        last_idx <- which.max(valid_data$Date)
        last_val <- valid_data$Value[last_idx]

        # TODO improve to make it possible to choose between quartile and full range
        # min_val <- min(valid_data$Value)
        # max_val <- max(valid_data$Value)

        min_val <- quantile(valid_data$Value, 0.25)
        max_val <- quantile(valid_data$Value, 0.75)


        last_date <- max(valid_data$Date)
        n_pts <- nrow(valid_data)
        
        # Get Country and ISO (they're the same for the whole group)
        country <- valid_data$Country[1]
        iso <- valid_data$ISO[1]
        
        # Create row
        data.frame(
            Country = country,
            ISO = iso,
            mean_value = mean_val,
            median_value = median_val,
            last_value = last_val,
            min_value = min_val,
            max_value = max_val,
            last_date = last_date,
            n_points = n_pts,
            stringsAsFactors = FALSE
        )
    })
    
    # Combine results
    stats_df <- do.call(rbind, results[!sapply(results, is.null)])

    if (is.null(stats_df)) {
            stats_df <- data.frame(
                        Country = unique(data$Country), # character(),
                        ISO = unique(data$ISO), #character(),
                        mean_value = NA, # numeric(),
                        median_value = NA, #numeric(),
                        last_value = NA, # numeric(),
                        min_value = NA, # numeric(),
                        max_value = NA, #numeric(),
                        last_date = NA, # as.Date(character()),
                        n_points = NA, # integer(),
                        stringsAsFactors = FALSE
            )
    }

    # Add display value and year columns
    stats_df$display_value <- if(type == "Last") stats_df$last_value else stats_df$mean_value
    stats_df$Year <- if(type == "Last")    format(stats_df$last_date, "%Y") else ""
    stats_df$YearShort <- if(type == "Last") format(stats_df$last_date, "%y") else ""
    
    # Ensure row names are sequential
    rownames(stats_df) <- NULL

    return(stats_df)
}




#' Create Color Mapping for Bar Plots
#' 
#' @description
#' Internal function that creates color mappings for bar plots.
#' Handles both standard schemes and custom groupings.
#' 
#' Used by: .plot_one_indic_bar, .plot_multi_indic_bar
#' Uses: .get_color_group, .get_category_labels, .get_color_palettes
#' 
#' Features:
#' - Color assignment by group
#' - Dark color variants for statistics
#' - Group labeling
#' - "Other" category handling
#' 
#' Example of list for color:
#' color_groups <- list(
#'   "Core" = c("DEU", "FRA"),
#'   "Periphery" = c("ITA", "ESP")
#' )
#'
#' @keywords internal
.create_color_mapping <- function(data,                    # data.frame: data to create mapping for
                                  color,                     # string/list/NULL: color scheme to use
                                  nase_size = 16) {          # numeric: base font size in points

    if (is.null(color) || isFALSE(color)) return(NULL)
    
    category_labels <- .get_category_labels()
    color_palettes <- .get_color_palettes()
    
    # Add color grouping to data
    data$color_group <- sapply(data$ISO, function(iso) .get_color_group(iso, color))
    data$color_group_label <- sapply(data$color_group, function(g) {
        if (g == "Other") return("Other")
        return(category_labels[[g]])
    })
    
    color_mapping <- color_palettes[[color]]
    # Add gray for "Other" category if needed
    if ("Other" %in% data$color_group) {
        color_mapping <- c(color_mapping, Other = "gray50")
    }
    
    # Keep only color groups that exist in the data
    present_groups <- unique(data$color_group)
    color_mapping <- color_mapping[names(color_mapping) %in% present_groups]
    
    # Create darker versions for error bars and median points
    dark_color_mapping <- sapply(color_mapping, function(col) {
        rgb_values <- col2rgb(col)
        rgb(rgb_values[1]/512, rgb_values[2]/512, rgb_values[3]/512)
    })
    
    list(
        mapping = color_mapping,
        dark_mapping = dark_color_mapping,
        data = data
    )
}




#' Order Groups for Legend Display
#' 
#' @description
#' Internal function that determines the optimal ordering of groups for legend display.
#' Ensures consistent and logical grouping particularly for geographical regions.
#' 
#' Used by: .create_color_mapping
#' Uses: No direct function calls
#' 
#' Features:
#' - Special handling for Subregion scheme
#' - Geographical hierarchy preservation
#' - Consistent "Other" category placement
#' - Maintains original order for non-geographic schemes
#'
#' @keywords internal
.get_ordered_groups <- function(color_type,              # string: type of color scheme being used
                                color_mapping,             # named vector: color assignments for groups
                                category_labels) {         # list: display labels for categories

    if (color_type == "Subregion") {
        # Define region order and subregion order within regions
        region_order <- c("Africa", "Americas", "Asia", "Europe", "Oceania")
        subregion_order <- list(
            Africa = c("NORTH_AFRICA", "EAST_AFRICA", "MIDDLE_AFRICA", "WEST_AFRICA", "SOUTH_AFRICA"),
            Americas = c("NORTH_AMERICA", "CARIBBEAN", "CENTRAL_AMERICA", "SOUTH_AMERICA"),
            Asia = c("EAST_ASIA", "SOUTHEAST_ASIA", "SOUTH_ASIA", "WEST_ASIA", "CENTRAL_ASIA"),
            Europe = c("WESTERN_EUROPE", "EASTERN_EUROPE"),
            Oceania = c("AUSTRALIA_NZ", "OTHER_OCEANIA")
        )
        
        # Get present subregions in the correct order
        ordered_groups <- unlist(subregion_order)
        present_groups <- names(color_mapping)
        ordered_groups <- ordered_groups[ordered_groups %in% present_groups]
        
        # Add "Other" at the end if present
        if ("Other" %in% present_groups) {
            ordered_groups <- c(ordered_groups, "Other")
        }
        
        return(ordered_groups)
    } else {
        # For other color types, keep original order
        return(names(color_mapping))
    }
}






#######################################################################################################################################################
# PLOT MULTI BAR

#' Create Multiple Indicator Bar Plot with Statistics
#' 
#' @description
#' Internal function that creates a grouped bar plot for multiple indicators.
#' Handles bar positioning, statistics, and color coding by indicator.
#' Core implementation for complex bar comparisons.
#' 
#' Used by: pp_plot_bar (via .plot_core)
#' Uses: .process_multi_bar_data, .calculate_plot_limits, .theme_plot,
#'       .get_legend_columns
#' 
#' Features:
#' - Grouped bars by country
#' - Consistent spacing and positioning
#' - Statistical indicators per group
#' - Color coding by indicator
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
    
    mvcommon::mv_debug("Starting multi-indicator bar plot", verbose, debug, type = "info", "MAIN")
    
    # 1. Process data
    all_data <- .process_multi_bar_data(data, type, order, verbose, debug)
    variables <- unique(all_data$Variable)
    n_vars <- length(variables)
    
    # 2. Set up position adjustment for grouped bars
    bar_width <- 0.5
    group_space <- bar_width * 0.4
    position_dodge_width <- bar_width + (bar_width / n_vars)
    
    mvcommon::mv_debug(paste("Bar positioning:", 
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
        color_mapping <- setNames(mvcommon::mv_get_colors()[2:(n_vars+1)], variables)
    }
    
    # Create darker versions for error bars and median points
    dark_color_mapping <- sapply(color_mapping, function(col) {
        rgb_values <- col2rgb(col)
        rgb(rgb_values[1]/512, rgb_values[2]/512, rgb_values[3]/512)
    })
    
    mvcommon::mv_debug("Color mapping created", verbose, debug, type = "info", "COLORS")
    
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
    
    mvcommon::mv_debug("Base plot created", verbose, debug, type = "info", "PLOT")
    
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
    
    mvcommon::mv_debug("Plot formatting completed", verbose, debug, type = "info", "PLOT")
    
    return(plot)
}


#######################################################################################################################################################
# HELPER FUNCTIONS FOR PLOT MULTI BAR

#' Create Darkness Scale for Variables
#' 
#' @description
#' Internal function that creates a scale of darkness levels for variables.
#' Used for differentiating variables when color coding isn't used.
#' 
#' Used by: .plot_multi_indic_bar
#' Uses: No direct function calls
#' 
#' Features:
#' - Graduated darkness scale
#' - Consistent spacing
#' - Alpha adjustment
#' - Base color modification
#'
#' @keywords internal
.create_variable_darkness <- function(variables,             # character vector: variables to create scale for
                                      base_color = "gray50",   # string: base color to modify
                                      verbose = TRUE,          # logical: controls information messages
                                      debug = FALSE) {         # logical: controls debug messages


    n_vars <- length(variables)
    
    # Create darkness levels from darkest to lightest
    alphas <- seq(1, 0.3, length.out = n_vars)
    
    # Create color mapping
    colors <- sapply(alphas, function(a) {
        if(base_color == "gray50") {
            # For grayscale, adjust the darkness directly
            paste0("gray", round(20 + (80 * (1-a))))
        } else {
            # For other colors, adjust alpha
            adjustcolor(base_color, alpha.f = a)
        }
    })
    
    names(colors) <- variables
    
    if(verbose) {
        msg <- paste0("Variables: ", paste(variables, collapse = ", "), 
                     "\nColor mappings: ", paste(names(colors), colors, collapse = "; "))
        mvcommon::mv_debug(msg, verbose, debug, type = "info", "COLORS")
    }
    
    return(colors)
}

#' Process Data for Multi-Indicator Bar Plot
#' 
#' @description
#' Internal function that prepares and structures data for multi-indicator bar plots.
#' Handles data ordering, statistic calculation, and country name formatting.
#' 
#' Used by: .plot_multi_indic_bar
#' Uses: .calculate_bar_statistics, mvcommon::mv_debug
#' 
#' Features:
#' - Variable-based ordering
#' - Multi-indicator statistics
#' - Country name standardization
#' - Consistent factor level maintenance
#' - Custom ordering options (variable/alphabetical)
#'
#' @keywords internal
.process_multi_bar_data <- function(data,                    # data.frame: raw input data for multiple indicators
                                    type,                     # string: "Mean"/"Last" - type of statistics to calculate
                                    order = "Variable",       # string: "Variable"/"Alphabetical"/numeric - ordering method
                                    verbose = TRUE,           # logical: controls information message output
                                    debug = FALSE) {          # logical: controls debugging message output

    if(verbose) {
        mvcommon::mv_debug("Starting data processing", verbose, debug, type = "info", "DATA")
    }
    
    # Get unique variables
    variables <- unique(data$Variable)
    n_vars <- length(variables)
    
    if(verbose) {
        mvcommon::mv_debug(paste("Variables found:", paste(variables, collapse = ", ")), 
                   verbose, debug, type = "info", "DATA")
    }
    
    # Calculate statistics for each variable
    all_stats <- lapply(variables, function(var) {
        var_data <- subset(data, Variable == var)
        stats <- .calculate_bar_statistics(var_data, type)
        stats$Variable <- var
        return(stats)
    })
    
    # Combine all statistics
    all_data <- do.call(rbind, all_stats)
    
    # Determine country order based on order parameter
    if (is.numeric(order)) {
        # Handle numeric order
        var_index <- min(order, n_vars)  # If order > n_vars, use first variable
        if(verbose) {
            mvcommon::mv_debug(paste("Ordering by variable", var_index, ":", variables[var_index]), 
                       verbose, debug, type = "info", "ORDER")
        }
        ordering_var <- variables[var_index]
        var_data <- subset(all_data, Variable == ordering_var)
        var_data <- var_data[order(var_data$display_value, decreasing = FALSE), ]
        country_order <- var_data$Country
        
    } else if(identical(order, "Variable")) {
        if(verbose) {
            mvcommon::mv_debug("Ordering by first variable values", verbose, debug, type = "info", "ORDER")
        }
        first_var <- variables[1]
        first_var_data <- subset(all_data, Variable == first_var)
        first_var_data <- first_var_data[order(first_var_data$display_value, decreasing = FALSE), ]
        country_order <- first_var_data$Country
        
    } else if(identical(order, "Alphabetical")) {
        if(verbose) {
            mvcommon::mv_debug("Ordering alphabetically", verbose, debug, type = "info", "ORDER")
        }
        country_order <- sort(unique(all_data$Country))
    } else if(identical(order, "Input")) {
        if(verbose) {
            mvcommon::mv_debug("Ordering based on input", verbose, debug, type = "info", "ORDER")
        }
        country_order <- unique(data$Country)
    }
    
    if(verbose) {
        mvcommon::mv_debug(paste("Country order:", paste(country_order, collapse = ", ")), 
                   verbose, debug, type = "info", "ORDER")
    }
    
    # Set country factor levels explicitly for ordering
    all_data$Country <- factor(all_data$Country, 
                             levels = country_order,
                             ordered = TRUE)
    
    # Sort the dataframe by Country and Variable
    all_data <- all_data[order(all_data$Country, all_data$Variable), ]
    
    # Process country names
    all_data$Country <- gsub("Republic", "Rep.", as.character(all_data$Country))
    # Maintain the factor levels after name processing
    all_data$Country <- factor(all_data$Country, 
                             levels = gsub("Republic", "Rep.", country_order),
                             ordered = TRUE)
    
    if(verbose) {
        mvcommon::mv_debug(paste("Final data rows:", nrow(all_data)), 
                   verbose, debug, type = "info", "DATA")
        mvcommon::mv_debug(paste("Final country order:", 
                        paste(levels(all_data$Country), collapse = ", ")), 
                   verbose, debug, type = "info", "ORDER")
    }
    
    return(all_data)
}

#' Create Legend for Multi-Indicator Bar Plot
#' 
#' @description
#' Internal function that creates a unified legend for multi-indicator bar plots.
#' Handles both indicator colors and statistical markers in a single legend panel.
#' 
#' Used by: .plot_multi_indic_bar
#' Uses: mvcommon::mv_debug
#' 
#' Features:
#' - Combined statistical and indicator legend
#' - Support for mean/latest values
#' - Optional median markers
#' - Range indicators
#' - Consistent ordering
#'
#' @keywords internal
.create_multi_bar_legend <- function(data,                    # data.frame: data for legend creation
                                     type,                     # string: "Mean"/"Last" - value type displayed
                                     show_median,              # logical: whether to include median markers
                                     show_range,               # logical: whether to include range indicators
                                     variables,                # character vector: variable names for legend
                                     verbose = TRUE,           # logical: controls information message output
                                     debug = FALSE) {          # logical: controls debugging message output


    if(verbose) {
        mvcommon::mv_debug("Creating multi-bar legend", verbose, debug, type = "info", "LEGEND")
    }
    
    # Create statistical elements legend data
    stat_elements <- c(if(type == "Mean") "Mean values" else "Latest values")
    if(show_median && type == "Mean") stat_elements <- c(stat_elements, "Median values")
    if(show_range) stat_elements <- c(stat_elements, "Range (Min to Max)")
    
    if(verbose) {
        mvcommon::mv_debug(paste("Statistical elements:", paste(stat_elements, collapse = ", ")), 
                   verbose, debug, type = "info", "LEGEND")
    }
    
    legend_data <- data.frame(
        x = rep(1, length(stat_elements)),
        y = rep(1, length(stat_elements)),
        type = factor(stat_elements, levels = stat_elements),
        Variable = variables[1]  # Add Variable to avoid the error
    )
    
    return(list(
        data = legend_data,
        elements = stat_elements
    ))
}
