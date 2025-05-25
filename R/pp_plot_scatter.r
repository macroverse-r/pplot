#' Create Scatter Plots with Advanced Visualization Options
#' 
#' @description
#' Creates scatter plots for comparing two or more variables across countries,
#' with support for various visual customizations including color grouping,
#' ISO label placement, trend lines, and statistical annotations.
#' The function automatically determines the most appropriate plot type
#' based on the number of variables and specified parameters.
#' 
#' @param data data.frame that must contain the following columns:
#'        - ISO: ISO 3-letter country codes (required)
#'        - Variable: indicator names (required)
#'        - Value: numeric values (required)
#'        - Country: country names (optional, auto-generated from ISO if missing)
#'        - Reference: source citations (required if reference=TRUE)
#' @param y_axis Single string, vector of strings, TRUE, FALSE, or NULL:
#'        - Single string: common y-axis label for all plots
#'        - Vector of strings: individual labels for each plot
#'        - TRUE: use Variable names as labels
#'        - FALSE/NULL: no axis labels
#'        For multi-variable plots, length must match number of plot panels.
#' @param x_axis Similar to y_axis, controls x-axis labeling:
#'        - Single string: common x-axis label for all plots
#'        - Vector of strings: individual labels for each plot
#'        - TRUE: use Variable names as labels
#'        - FALSE/NULL: no axis labels
#' @param color Controls point coloring scheme:
#'        - "Subregion": colors by geographical subregion
#'        - "Region": colors by continent/major region
#'        - "Center-Periphery": colors by economic classification
#'        - "Hydrocarbon": colors by hydrocarbon exporter/importer status
#'        - list: custom grouping with named groups of countries
#'        - FALSE/NULL: no color grouping (all points gray)
#'        Default is "Subregion".
#' @param ISO Controls country label display:
#'        - TRUE: replaces points with ISO codes
#'        - "Both": shows both points and repelled ISO labels
#'        - FALSE/NULL: shows only points
#'        Default is "Both".
#' @param interpolation Controls trend line fitting:
#'        - TRUE/"Linear": linear regression line
#'        - "Square": quadratic regression curve
#'        - "Cubic": cubic regression curve
#'        - "Spline": smoothed spline curve
#'        - FALSE: no trend line
#'        Default is FALSE.
#' @param spline_bw Numeric value for spline smoothing bandwidth:
#'        - Only used when interpolation="Spline"
#'        - Lower values create more flexible curves
#'        - NULL for automatic bandwidth selection
#'        Default is NULL.
#' @param r_squared Controls R-squared statistic display:
#'        - 0: no R-squared display
#'        - 1: shows R-squared value
#'        - 2: shows R-squared and adjusted R-squared
#'        - 3: shows equation and both R-squared values
#'        Default is 0.
#' @param r_squared_pos Controls R-squared text position:
#'        - "flexible": automatically chooses best position
#'        - "topleft": fixed top-left position
#'        - "topright": fixed top-right position
#'        - "bottomleft": fixed bottom-left position
#'        - "bottomright": fixed bottom-right position
#'        Default is "flexible".
#' @param label_nudge Numeric value for ISO label positioning:
#'        - Controls distance between point and label
#'        - Only used when ISO="Both"
#'        - Larger values increase separation
#'        Default is 0.005.
#' @param highlight_zero Logical; whether to add x=0 and y=0 reference lines:
#'        - TRUE: adds both reference lines
#'        - FALSE: no reference lines
#'        Default is FALSE.
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
#' @param subfig_title Controls subplot titles in multi-variable plots:
#'        - TRUE: uses default titles (pairs of variables)
#'        - FALSE: no subplot titles
#'        - Character vector: custom titles (length must match number of panels)
#'        - Ignored for two-variable plots
#'        Default is TRUE.
#' @param verbose Logical; controls information messages:
#'        - TRUE: prints processing information
#'        - FALSE: suppresses information messages
#'        Default is TRUE.
#' @param debug Logical; controls debugging output:
#'        - TRUE: prints detailed debugging information
#'        - FALSE: suppresses debugging information
#'        Default is FALSE.
#' @param no_other Logical; controls treatment of unclassified countries:
#'        - TRUE: excludes countries not in any color group
#'        - FALSE: includes unclassified countries in gray
#'        Default is TRUE.
#' @param size Numeric or NULL; controls plot dimensions:
#'        - 1: small (10x7 inches)
#'        - 2: medium (15x7 inches)
#'        - 3: large (15x10 inches)
#'        - 4: extra large (20x10 inches)
#'        - NULL: auto-sizes based on data
#' @param theme List
#' @param common_var Controls variable pairing in multi-variable plots:
#'        - TRUE: uses first variable as common x-axis
#'        - Character: specified variable used as common x-axis
#'        - FALSE: sequential pairing of variables
#'        Default is TRUE.
#' @param bg character; controls plot background color:
#'        - "transparent": transparent background (default)
#'        - Any valid color string: sets background to that color
#'
#' @return A ggplot2 object containing the scatter plot(s)
#'
#' @details
#' The function automatically selects the appropriate plotting implementation based on
#' the number of variables:
#'
#' * Two variables:
#'   - Simple scatter plot with optional trend lines
#'   - Color grouping by region or custom groups
#'   - Flexible ISO code labeling options
#'
#' * Multiple variables:
#'   - Matrix of scatter plots
#'   - Option for common x-axis variable
#'   - Consistent scales and formatting across panels
#'
#' The function includes multiple features for customization:
#' - Automatic axis scaling and formatting
#' - Flexible color grouping options
#' - Statistical annotations (R-squared, trend lines)
#' - Reference panel for data sources
#' - Consistent theme across all plot types
#'
#' @examples
#' # Basic scatter plot with two variables
#' data <- data.frame(
#'   ISO = c("FRA", "DEU", "ITA"),
#'   Variable = rep(c("GDP", "Inflation"), each = 3),
#'   Value = c(2.1, 1.8, 1.5, 3.2, 2.8, 2.5),
#'   Reference = "World Bank"
#' )
#' pp_plot_scatter(data, y_axis = "Inflation Rate (%)")
#'
#' # Scatter plot with color groups and trend line
#' pp_plot_scatter(data,
#'                color = "Region",
#'                interpolation = "Linear",
#'                r_squared = 2)
#'
#' # Multiple variables with common x-axis
#' data_multi <- data.frame(
#'   ISO = rep(c("FRA", "DEU", "ITA"), 3),
#'   Variable = rep(c("GDP", "Inflation", "Debt"), each = 3),
#'   Value = runif(9, 0, 5),
#'   Reference = "World Bank"
#' )
#' pp_plot_scatter(data_multi,
#'                common_var = "GDP",
#'                color = "Subregion",
#'                ISO = "Both")
#'
#' # Custom color grouping
#' color_groups <- list(
#'   "Core" = c("DEU", "FRA"),
#'   "Periphery" = c("ITA", "ESP")
#' )
#' pp_plot_scatter(data,
#'                color = color_groups,
#'                highlight_zero = TRUE)
#'
#' @seealso 
#' \code{\link{pp_plot_series}} for time series plots
#' \code{\link{pp_plot_bar}} for bar plots
#' \code{\link{isomapper::im_from_iso}} for ISO code to country name conversion
#'
#' @family plotting functions
#' @export
pp_plot_scatter <- function(data,                    # data.frame with required columns: ISO, Variable, Value
                           y_axis = NULL,            # string/vector/TRUE/FALSE/NULL: axis labels (single or per-panel)
                           x_axis = NULL,            # string/vector/TRUE/FALSE/NULL: x-axis labels (single or per-panel)
                           color = "Subregion",      # "Subregion"/"Region"/"Center-Periphery"/list/FALSE/NULL: color grouping
                           ISO = "Both",             # TRUE/"Both"/FALSE/NULL: country label display mode
                           interpolation = FALSE,     # TRUE/"Linear"/"Square"/"Cubic"/"Spline"/FALSE: trend line type
                           spline_bw = NULL,         # numeric: bandwidth for spline smoothing
                           r_squared = 0,            # 0/1/2/3: R-squared display options
                           r_squared_pos = "flexible", # "flexible"/"topleft"/"topright"/"bottomleft"/"bottomright"
                           label_nudge = 0.005,      # numeric: distance for repelled ISO labels
                           highlight_zero = FALSE,    # logical: show x=0 and y=0 reference lines
                           filename = NULL,          # string/NULL/FALSE: output filename without extension
                           print = TRUE,             # logical: display the plot
                           legend = TRUE,            # logical: include legend
                           reference = TRUE,         # logical: include reference panel
                           title = NULL,             # string/NULL/FALSE: main title
                           subtitle = NULL,          # string/NULL/FALSE: subtitle
                           subfig_title = FALSE,     # TRUE/FALSE/char vector: titles for multi-panel plots
                           verbose = TRUE,           # logical: print processing information
                           debug = FALSE,            # logical: print debugging information
                           no_other = TRUE,          # logical: exclude unclassified countries
                           size = 4,                 # 1/2/3/4/NULL: plot size presets or auto-sizing
                           theme = list(base_size = 16), # list: must contain base font size
                           common_var = TRUE,        # TRUE/char/FALSE: variable pairing mode for multi-variable plots
                           title_scale = 1.15,      # numeric: scaling factor for title
                           title_face = "bold",     # string: "plain", "bold", "italic", or "bold.italic"
                           subtitle_scale = 1,      # numeric: scaling factor for subtitle
                           ref_scale = 1,          # numeric: scaling factor for reference
                           bg = "transparent") {     # character: background color for plot ("transparent" or color string)
    



    args <- as.list(environment())
    args$plot_type <- "scatter"
    
    # Validate args  
    validated_args <- .validate_scatter_args(args)
    
    # Call core plotting function
    do.call(.plot_core, validated_args)
}



#######################################################################################################################################################
# MAIN SCATTER PLOT


#' Create Two-Variable Scatter Plot
#' 
#' @description
#' Internal function that creates a scatter plot comparing two variables.
#' Handles point plotting, color grouping, trend lines, and statistical annotations.
#' Core implementation for basic scatter plots.
#' 
#' Used by: pp_plot_scatter (via .plot_core)
#' Uses: .prepare_scatter_data, .calculate_r_squared, .scale_y, .theme_plot,
#'       .create_scatter_colors
#' 
#' Features:
#' - Color grouping by region/custom groups
#' - Linear/polynomial/spline trend lines
#' - R-squared statistics
#' - ISO code labels with flexible positioning
#' - Zero reference lines
#'
#' @keywords internal
.plot_scatter_two_vars <- function(data,                    # data.frame: input data for two variables
                                    y_axis = NULL,            # string/NULL: y-axis label
                                    x_axis = NULL,            # string/NULL: x-axis label
                                    color = NULL,             # string/list/NULL: color grouping scheme
                                    ISO = NULL,               # string/logical: "Both"/TRUE/FALSE - country label display
                                    interpolation = FALSE,    # string/logical: trend line type
                                    spline_bw = NULL,         # numeric: bandwidth for spline smoothing
                                    r_squared = 0,            # numeric: 0/1/2/3 - R-squared display options
                                    r_squared_pos = "flexible", # string: position for R-squared text
                                    label_nudge = 0.05,       # numeric: distance for repelled labels
                                    highlight_zero = TRUE,    # logical: whether to show zero reference lines
                                    theme = list(base_size = 16), # list: must contain base font size
                                    no_other = FALSE,         # logical: whether to exclude ungrouped countries
                                    verbose = TRUE,           # logical: controls information messages
                                    debug = FALSE) {          # logical: controls debug messages
    
    mvcommon::mv_debug("Starting two-variable scatter plot", verbose, debug, type = "info", "SCATTER")
    
    # 1. Data Preparation
    data_prep <- .prepare_scatter_data(data, verbose, debug)

    plot_data <- data_prep$data
    x_var <- data_prep$x_var
    y_var <- data_prep$y_var
    
    # 2. Initialize Base Plot
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]))
    
    # 3. Add Zero Lines (if requested and within data range)
    if(highlight_zero) {
        # Calculate ranges and margins
        x_range <- range(plot_data[[x_var]], na.rm = TRUE)
        y_range <- range(plot_data[[y_var]], na.rm = TRUE)
        
        # Calculate 10% margins
        x_margin <- diff(x_range) * 0.1
        y_margin <- diff(y_range) * 0.1
        
        # Add horizontal line only if 0 is within y-axis range + margin
        if((y_range[1] - y_margin) <= 0 && 0 <= (y_range[2] + y_margin)) {
            plot <- plot + ggplot2::geom_hline(yintercept = 0, color = "gray80", size = 0.6)
        }
        
        # Add vertical line only if 0 is within x-axis range + margin
        if((x_range[1] - x_margin) <= 0 && 0 <= (x_range[2] + x_margin)) {
            plot <- plot + ggplot2::geom_vline(xintercept = 0, color = "gray80", size = 0.6)
        }
    }
    
    # 4. Handle Color Mapping and Points/Labels
    if (!is.null(color)) {
        original_color <- color  # Save the original color list
        if (is.list(color)) {
            # Convert the list-based coloring to use the "List" scheme
            color <- "List"
            
            # Create color groups based on the original list
            plot_data$color_group <- "Other"  # Default group
            for (group_name in names(original_color)) {
                group_countries <- original_color[[group_name]]
                plot_data$color_group[plot_data$ISO %in% group_countries] <- 
                    paste0("Group ", LETTERS[which(names(original_color) == group_name)])
            }
            
            # Get color palettes and category labels, passing the original color parameter
            color_palettes <- .get_color_palettes(original_color)
            category_labels <- .get_category_labels(original_color)
            
        } else {
            # Original code for non-list color schemes
            plot_data$color_group <- sapply(plot_data$ISO, function(iso) .get_color_group(iso, color))
            color_palettes <- .get_color_palettes()
            category_labels <- .get_category_labels()
        }

        # Get palette
        palette <- color_palettes[[color]]
        
        # Keep appropriate color groups based on no_other parameter
        present_groups <- unique(plot_data$color_group)
        if (no_other) {
            present_groups <- present_groups[present_groups != "Other"]
        }
        color_mapping <- palette[names(palette) %in% present_groups]
        
        # If including "Other", add it to the color mapping
        if (!no_other && "Other" %in% present_groups) {
            color_mapping <- c(color_mapping, Other = "gray50")
        }
        
        # Create labels mapping
        labels <- sapply(present_groups, function(x) {
            if (x == "Other") return("Other")
            return(category_labels[[x]])
        })
        names(labels) <- present_groups

        if(verbose) {
            mvcommon::mv_debug(paste("Labels created:", paste(names(labels), labels, collapse="; ")), 
                       verbose, debug, type = "info", "LABELS")
        }
        
        # Add points/labels based on ISO parameter
        if (!identical(ISO, TRUE)) {
            if (no_other) {
                # Split plotting for labeled and Other groups
                plot <- plot + 
                    ggplot2::geom_point(data = subset(plot_data, color_group != "Other"),
                              ggplot2::aes(color = color_group),
                              size = 3) +
                    ggplot2::geom_point(data = subset(plot_data, color_group == "Other"),
                              color = "gray50",
                              size = 3)
            } else {
                # Plot all points with color mapping
                plot <- plot + 
                    ggplot2::geom_point(data = plot_data,
                              ggplot2::aes(color = color_group),
                              size = 3)
            }
        } else {
            if (no_other) {
                # Split text labels for labeled and Other groups
                plot <- plot + 
                    ggplot2::geom_text(data = subset(plot_data, color_group != "Other"),
                             ggplot2::aes(label = ISO, color = color_group),
                             size = theme$base_size/3) +
                    ggplot2::geom_text(data = subset(plot_data, color_group == "Other"),
                             ggplot2::aes(label = ISO),
                             color = "gray50",
                             size = theme$base_size/3)
            } else {
                # All text labels with color mapping
                plot <- plot + 
                    ggplot2::geom_text(data = plot_data,
                             ggplot2::aes(label = ISO, color = color_group),
                             size = theme$base_size/3)
            }
        }
        
        # Add color scale
        plot <- plot + 
            ggplot2::scale_color_manual(
                values = color_mapping,
                name = NULL,
                labels = labels,
                breaks = names(color_mapping) # define the order of the legend
            )
        
        # Add repel labels if ISO = "Both"
        if (identical(ISO, "Both")) {
            if (no_other) {
                # Create ISO to color mapping (including gray for "Other")
                iso_colors <- color_mapping[plot_data$color_group]
                iso_colors[plot_data$color_group == "Other"] <- "gray50"
                names(iso_colors) <- plot_data$ISO
            } else {
                # Use color mapping directly
                iso_colors <- color_mapping[plot_data$color_group]
                names(iso_colors) <- plot_data$ISO
            }
            
            plot <- plot + 
                ggrepel::geom_text_repel(
                    data = plot_data,
                    ggplot2::aes(label = ISO),
                    size = theme$base_size/3,
                    color = iso_colors[plot_data$ISO],
                    nudge_x = label_nudge,
                    nudge_y = label_nudge
                )
        }
    } else {
        # No color mapping case
        plot <- plot + ggplot2::geom_point(size = 3, color = "black")
        
        if (identical(ISO, TRUE)) {
            plot <- plot + 
                ggplot2::geom_text(ggplot2::aes(label = ISO),
                         size = theme$base_size/3,
                         color = "black")
        } else if (identical(ISO, "Both")) {
            plot <- plot + 
                ggrepel::geom_text_repel(
                    ggplot2::aes(label = ISO),
                    size = theme$base_size/3,
                    color = "gray40",
                    nudge_x = label_nudge,
                    nudge_y = label_nudge
                )
        }
    }
    
    # 5. Add Interpolation
    if (!isFALSE(interpolation)) {
        # Determine polynomial degree
        degree <- switch(interpolation,
                        "Linear" = 1,
                        "Square" = 2,
                        "Cubic" = 3,
                        1)  # Default to linear for TRUE or other values
        
        if (interpolation %in% c(TRUE, "Linear", "Square", "Cubic")) {
            # Polynomial interpolation
            plot <- plot + 
                ggplot2::geom_smooth(
                    method = "lm",
                    formula = as.formula(sprintf("y ~ poly(x, %d, raw = TRUE)", degree)),
                    color = "black",
                    linetype = "longdash",
                    size = 1,
                    alpha = 0
                )
        } else if (identical(interpolation, "Spline")) {
            # Spline interpolation
            if (is.null(spline_bw)) {
                # Default bandwidth calculation
                x_range <- diff(range(plot_data[[x_var]], na.rm = TRUE))
                spline_bw <- x_range / 4
            }
            
            plot <- plot + 
               ggplot2::geom_smooth(
                    method = "loess",
                    span = spline_bw,
                    color = "black",
                    linetype = "dashed",
                    size = 0.8,
                    alpha = 0.1
                )
        }
    }
    
    # 6. Add R-squared if requested
    if (r_squared > 0) {
        stats <- .calculate_r_squared(
            plot_data[[x_var]], 
            plot_data[[y_var]], 
            degree = if(!isFALSE(interpolation)) degree else 1
        )
        
        # Create R-squared text based on option
        r2_text <- switch(r_squared,
                         sprintf("R\u00b2 = %.3f", stats$r_squared),
                         sprintf("R\u00b2 = %.3f\nAdj. R\u00b2 = %.3f", stats$r_squared, stats$adj_r_squared),
                         sprintf("%s\nR\u00b2 = %.3f\nAdj. R\u00b2 = %.3f", stats$equation, stats$r_squared, stats$adj_r_squared))
        
        # Determine position
        if (r_squared_pos == "flexible") {
            pos <- if (stats$r_squared < 0.5) "topleft" else "topright"
        } else {
            pos <- r_squared_pos
        }
        
        # Build plot to get actual dimensions
        built <- ggplot2::ggplot_build(plot)
        plot_dims <- built$layout$panel_params[[1]]
        
        # Get actual plot limits
        x_limits <- plot_dims$x.range
        y_limits <- plot_dims$y.range
        
        # Calculate position coordinates using actual plot limits
        x_pos <- ifelse(grepl("right", pos), x_limits[2], x_limits[1])
        y_pos <- ifelse(grepl("top", pos), y_limits[2], y_limits[1])
        
        # Calculate margins based on plot range
        x_range <- diff(x_limits)
        y_range <- diff(y_limits)
        margin_x <- x_range * 0.005 * ifelse(grepl("right", pos), -1, 1)
        margin_y <- y_range * 0.005 * ifelse(grepl("top", pos), -1, 1)
        
        # Add text annotation
        plot <- plot +
            ggplot2::annotate("text",
                    x = x_pos + margin_x,
                    y = y_pos + margin_y,
                    label = r2_text,
                    hjust = ifelse(grepl("right", pos), 1, 0),
                    vjust = ifelse(grepl("top", pos), 1, 0),
                    size = theme$base_size/2.5)
    }
    
    # 7. Add Final Plot Elements

    x_lab <- if (is.null(x_axis) || x_axis == TRUE) {
            x_var
        } else if (x_axis == FALSE) {
            NULL
        } else {
            x_axis
        }
    y_lab <- if (is.null(y_axis) || y_axis == TRUE) {
            y_var
        } else if (y_axis == FALSE) {
            NULL
        } else {
            y_axis
        }

    plot <- plot +
        ggplot2::labs(x = x_lab, y = y_lab) +
        .theme_plot(theme = theme) + 
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 0, hjust = 1, size = theme$base_size)
        )



    return(plot)
}



#######################################################################################################################################################
# MULTI SCATTER PLOT


#' Create Multi-Variable Scatter Plot Matrix
#' 
#' @description
#' Internal function that creates a matrix of scatter plots for multiple variables.
#' Handles panel creation, consistent formatting, and optional common variable plotting.
#' Core implementation for complex scatter plot matrices.
#' 
#' Used by: pp_plot_scatter (via .plot_core)
#' Uses: .plot_scatter_two_vars
#' 
#' Features:
#' - Matrix layout of scatter plots
#' - Common variable option (one variable vs all others)
#' - Consistent styling across panels
#' - Optional panel titles
#' - Legend sharing across panels
#'
#' @keywords internal
.plot_scatter_multi_vars <- function(data,                    # data.frame: input data for multiple variables
                                     variables = NULL,          # character vector: variables to plot
                                     common_var = TRUE,         # logical/string: use common x-axis variable
                                     color = NULL,             # string/list/NULL: color grouping scheme
                                     ISO = NULL,               # string/logical: country label display
                                     interpolation = NULL,     # string/logical: trend line type
                                     spline_bw = NULL,         # numeric: bandwidth for spline smoothing
                                     r_squared = FALSE,        # numeric: R-squared display options
                                     label_nudge = 0.05,       # numeric: distance for repelled labels
                                     highlight_zero = TRUE,    # logical: whether to show zero reference lines
                                     x_axis = NULL,            # string/vector: x-axis labels
                                     y_axis = NULL,            # string/vector: y-axis labels
                                     subfig_title = FALSE,     # logical/character: panel titles
                                     size = 1,                 # numeric: plot size preset
                                     theme = list(base_size = 14), # list: must contain base font size
                                     print = TRUE,             # logical: whether to display the plot
                                     verbose = TRUE,           # logical: controls information messages
                                     debug = FALSE) {          # logical: controls debug messages
  
    
    # extract variable names from data
    variables <- unique(data$Variable)
    if (verbose) {
        message("Using variables from data: ", paste(variables, collapse = ", "))
    }
    
    # Input validation
    if (length(variables) < 2) {
        stop("At least two variables must be provided")
    }
    
    # Debug mode setup
    .print_debug <- function(msg, type = "info") {
        if (debug) {
            if (type == "warning") {
                warning(msg, immediate. = TRUE)
            } else {
                message(msg)
            }
        }
    }
    
    # Handle common_var parameter
    if (is.null(common_var)) {
        common_var <- TRUE  # Default behavior
    }
    use_common_var <- is.character(common_var) || isTRUE(common_var)
    
    # If common_var is a string, validate it exists in variables
    if (is.character(common_var)) {
            if (!common_var %in% variables) {
                stop(sprintf("Specified common variable '%s' not found in data", common_var))
            }
            x_var <- common_var
            y_vars <- variables[variables != common_var]
            # Create pairs with common variable as second element
            plot_pairs <- lapply(y_vars, function(y) c(y, x_var))
    } else if (isTRUE(common_var)) {
        # Use first variable as common variable (original same_var = TRUE behavior)
        x_var <- variables[1]
        y_vars <- variables[-1]
        plot_pairs <- lapply(y_vars, function(y) c(y, x_var))
        
        if (verbose) {
            .print_debug(sprintf("Using first variable '%s' as common variable", x_var))
        }
    } else {
        # Sequential pairing (original same_var = FALSE behavior)
        if (length(variables) %% 2 != 0) {
            .print_debug(paste("Last variable", variables[length(variables)], 
                            "will be ignored as number of variables is odd"), 
                      "warning")
            variables <- variables[1:(length(variables) - 1)]
        }
        plot_pairs <- split(variables, ceiling(seq_along(variables)/2))
        
        if (verbose) {
            .print_debug("Using sequential variable pairing")
        }
    }
    

    # Assuming subfig_title can be NULL, TRUE, FALSE, or a character vector
    add_subfig_title <- isTRUE(subfig_title) || is.character(subfig_title)



    # Create individual plots
    plots <- vector("list", length(plot_pairs))
    for(i in seq_along(plot_pairs)) {
        pair <- plot_pairs[[i]]
        # Filter data for current pair of variables
        plot_data <- data[data$Variable %in% pair, ]
        # Ensure consistent variable order by explicitly ordering
        plot_data <- plot_data[order(match(plot_data$Variable, pair)), ]
        
        if (is.character(x_axis)){ x_axis_in <- x_axis[i] }else{ x_axis_in <- x_axis }
        if (is.character(y_axis)){ y_axis_in <- y_axis[i] }else{ y_axis_in <- y_axis }
        
        plot <- .plot_scatter_two_vars(
            data = plot_data,
            color = color,
            ISO = ISO,
            interpolation = interpolation,
            spline_bw = spline_bw,
            r_squared = r_squared,
            label_nudge = label_nudge,
            highlight_zero = highlight_zero,
            x_axis = x_axis_in,
            y_axis = y_axis_in,
            theme = theme,
            verbose = verbose,
            debug = debug
        )
        
        plot <- plot + ggplot2::theme(legend.position = "none")
        


        # Add subplot title if requested
        if (add_subfig_title) {

            # If not specified but TRUE, use pair
            plot_title <- if (isTRUE(subfig_title)) paste(pair[1], "vs", pair[2]) else subfig_title[i]

            plot <- plot + 
                ggplot2::ggtitle(plot_title) +
                ggplot2::theme(plot.title = ggplot2::element_text(size = rel(0.8)))
        }
        
        plots[[i]] <- plot
    }
    
    # Combine plots using patchwork
    if (length(plots) > 1) {
        # Calculate grid dimensions
        # n_plots <- length(plots)
        # n_cols <- min(3, n_plots)  # Maximum 3 columns
        # n_rows <- ceiling(n_plots / n_cols)
        
        # Combine plots
        final_plot <- patchwork::wrap_plots(plots)
    } else {
        final_plot <- plots[[1]]
    }
    
    # Return plot invisibly
    invisible(final_plot)
}



#######################################################################################################################################################
# HELPER FUNCTIONS



#' Prepare Data for Scatter Plot Creation
#' 
#' @description
#' Internal function that reshapes and prepares data for scatter plot creation.
#' Converts long format data to wide format suitable for scatter plotting.
#' Essential preprocessing step for all scatter plots.
#' 
#' Used by: .plot_scatter_two_vars
#' Uses: No direct function calls
#' 
#' Processing steps:
#' 1. Validate variable count
#' 2. Reshape data to wide format
#' 3. Rename columns to match variables
#' 4. Return both data and variable names
#'
#' @keywords internal
.prepare_scatter_data <- function(data,                    # data.frame: long format input data
                                   verbose = TRUE,           # logical: controls information messages
                                   debug = FALSE) {          # logical: controls debug messages

    mvcommon::mv_debug("Preparing data for scatter plot", verbose, debug, type = "info", "DATA")
    
    # Get variable names
    variables <- unique(data$Variable)
    if (length(variables) != 2) {
        stop("Exactly 2 variables required for scatter plot")
    }
    
    mvcommon::mv_debug(paste("Variables:", paste(variables, collapse = " vs ")), 
               verbose, debug, type = "info", "DATA")
    
    # Reshape data to wide format
    wide_data <- reshape(data,
                        direction = "wide",
                        timevar = "Variable",
                        idvar = c("Country", "ISO", "Date"),
                        v.names = "Value")
    
    # Rename columns to match variables
    colnames(wide_data) <- gsub("Value\\.", "", colnames(wide_data))
    
    # Return both the reshaped data and original variable names
    list(
        data = wide_data,
        x_var = variables[1],
        y_var = variables[2]
    )
}


#' Create Color Mapping for Scatter Plots
#' 
#' @description
#' Internal function that creates color mappings for scatter plots.
#' Supports both standard schemes and custom groupings.
#' 
#' Used by: .plot_scatter_two_vars
#' Uses: .get_color_group, .get_color_palettes
#' 
#' Features:
#' - Custom group color assignment
#' - Standard scheme mapping
#' - Default gray for ungrouped
#' - Consistent color selection
#'
#' @keywords internal
.create_scatter_colors <- function(data,                    # data.frame: data to create mapping for
                                   color,                     # string/list/NULL: color scheme to use
                                   verbose = TRUE,            # logical: controls information messages
                                   debug = FALSE) {           # logical: controls debug messages


    mvcommon::mv_debug("Creating color mapping", verbose, debug, type = "info", "COLOR")
    
    if (is.null(color) || isFALSE(color)) {
        mvcommon::mv_debug("No color mapping requested", verbose, debug, type = "info", "COLOR")
        return(NULL)
    }
    
    if (is.list(color)) {
        mvcommon::mv_debug("Creating custom color groups", verbose, debug, type = "info", "COLOR")
        # Custom color groups
        color_mapping <- rep("gray50", nrow(data))  # Default color
        mvcommon::mv_get_colors() <- c("#4472C4", "#70AD47", "#C00000", "#FFC000", "#7030A0")
        
        for (i in seq_along(color)) {
            group_name <- names(color)[i]
            countries <- color[[i]]
            if(verbose) {
                mvcommon::mv_debug(paste("Group:", group_name, "Countries:", 
                                paste(countries, collapse=", ")), 
                           verbose, debug, type = "info", "COLOR")
            }
            color_mapping[data$Country %in% countries] <- mvcommon::mv_get_colors()[i]
        }
        
        if(verbose) {
            mvcommon::mv_debug(paste("Unique colors in mapping:", 
                            paste(unique(color_mapping), collapse=", ")), 
                       verbose, debug, type = "info", "COLOR")
        }
        
        return(color_mapping)
    } else {
        mvcommon::mv_debug(paste("Using predefined color scheme:", color), 
                   verbose, debug, type = "info", "COLOR")
        # Use existing color mapping functions
        color_group <- sapply(data$ISO, function(iso) .get_color_group(iso, color))
        
        if(verbose) {
            mvcommon::mv_debug(paste("Unique color groups:", 
                            paste(unique(color_group), collapse=", ")), 
                       verbose, debug, type = "info", "COLOR")
        }
        
        color_palettes <- .get_color_palettes()
        color_mapping <- color_palettes[[color]][color_group]
        color_mapping[is.na(color_mapping)] <- "gray50"  # Default for "Other"
        
        if(verbose) {
            mvcommon::mv_debug(paste("Unique colors after mapping:", 
                            paste(unique(color_mapping), collapse=", ")), 
                       verbose, debug, type = "info", "COLOR")
        }
        
        return(color_mapping)
    }
}

#' Format Regression Coefficients for Display
#' 
#' @description
#' Internal function that formats numerical coefficients for equation display.
#' Handles different numerical ranges with appropriate precision.
#' Ensures consistent and readable equation formatting.
#' 
#' Used by: .calculate_r_squared
#' Uses: No direct function calls
#' 
#' Formatting rules:
#' - Very small numbers (<0.001): Scientific notation
#' - Small numbers (<1): Variable decimal places
#' - Medium numbers (<10): Up to 2 decimals
#' - Large numbers (>=1000): No decimals
#' - Removes trailing zeros
#'
#' @keywords internal
.format_coef <- function(x) {                    # numeric: coefficient to format

    if (abs(x) == 0) return("0")
    
    # For very small numbers (use scientific notation for very small values)
    if (abs(x) < 0.001) {
        return(sprintf("%.1e", x))
    }
    
    # For small numbers (show more decimals)
    if (abs(x) < 1) {
        # Find first non-zero decimal place
        decimals <- max(3, -floor(log10(abs(x))))
        return(sprintf(paste0("%.", decimals, "f"), x))
    }
    

    # For medium numbers (show up to 3 decimals)
    if (abs(x) < 10) {
        # Remove trailing zeros after decimal point
        return(sub("0+$", "", sprintf("%.2f", x)))
    }

    # For medium numbers (show up to 3 decimals)
    if (abs(x) < 1000) {
        # Remove trailing zeros after decimal point
        return(sub("0+$", "", sprintf("%.0f", x)))
    }
    
    # For large numbers (no decimals)
    return(sprintf("%.0f", x))
}

#' Calculate R-squared Statistics and Trend Line Equations
#' 
#' @description
#' Internal function that computes R-squared statistics and generates trend line equations.
#' Handles different polynomial degrees and formatting of coefficients.
#' Essential for statistical annotations in scatter plots.
#' 
#' Used by: .plot_scatter_two_vars
#' Uses: .format_coef
#' 
#' Capabilities:
#' - Linear regression
#' - Quadratic regression
#' - Cubic regression
#' - R-squared and adjusted R-squared calculation
#' - Equation formatting with proper signs
#'
#' @keywords internal
.calculate_r_squared <- function(x,                          # numeric vector: x coordinates
                                 y,                           # numeric vector: y coordinates
                                 degree = 1) {                # numeric: polynomial degree (1/2/3)

    # Remove NA values
    complete_cases <- complete.cases(x, y)
    x <- x[complete_cases]
    y <- y[complete_cases]
    
    if (degree == 1) {
        # Linear model
        model <- lm(y ~ x)
        coef <- coef(model)
        slope <- .format_coef(coef[2])
        intercept <- .format_coef(coef[1])
        
        # Format equation with proper sign for intercept
        if (coef[1] >= 0) {
            equation <- sprintf("y = %sx + %s", slope, intercept)
        } else {
            equation <- sprintf("y = %sx - %s", slope, sub("-", "", intercept))
        }
    } else if (degree == 2) {
        # Quadratic model
        model <- lm(y ~ poly(x, 2, raw = TRUE))
        coef <- coef(model)
        a <- .format_coef(coef[3])
        b <- .format_coef(coef[2])
        c <- .format_coef(coef[1])
        
        # Build equation term by term with proper signs
        equation <- sprintf("y = %sx\u00b2", a)
        if (coef[2] > 0) equation <- paste0(equation, " + ", b, "x")
        if (coef[2] < 0) equation <- paste0(equation, " - ", sub("-", "", b), "x")
        if (coef[1] > 0) equation <- paste0(equation, " + ", c)
        if (coef[1] < 0) equation <- paste0(equation, " - ", sub("-", "", c))
    } else if (degree == 3) {
        # Cubic model
        model <- lm(y ~ poly(x, 3, raw = TRUE))
        coef <- coef(model)
        a <- .format_coef(coef[4])
        b <- .format_coef(coef[3])
        c <- .format_coef(coef[2])
        d <- .format_coef(coef[1])
        
        # Build equation term by term with proper signs
        equation <- sprintf("y = %sx\u00b3", a)
        if (coef[3] > 0) equation <- paste0(equation, " + ", b, "x\u00b2")
        if (coef[3] < 0) equation <- paste0(equation, " - ", sub("-", "", b), "x\u00b2")
        if (coef[2] > 0) equation <- paste0(equation, " + ", c, "x")
        if (coef[2] < 0) equation <- paste0(equation, " - ", sub("-", "", c), "x")
        if (coef[1] > 0) equation <- paste0(equation, " + ", d)
        if (coef[1] < 0) equation <- paste0(equation, " - ", sub("-", "", d))
    }
    
    # Calculate R-squared and adjusted R-squared
    r_squared <- summary(model)$r.squared
    adj_r_squared <- summary(model)$adj.r.squared
    
    list(
        r_squared = r_squared,
        adj_r_squared = adj_r_squared,
        equation = equation,
        model = model
    )
}