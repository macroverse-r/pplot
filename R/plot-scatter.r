#' Create Scatter Plot for Two Variables
#' 
#' @description
#' Internal function that creates a scatter plot comparing two variables across countries.
#' Supports sophisticated features like trend lines, color grouping, and statistical annotations.
#' 
#' Used by: pp_plot_scatter (via .plot_core), .plot_scatter_multi_vars
#' Uses: .prepare_scatter_data, .get_color_group, .get_color_palettes, .get_category_labels,
#'       .calculate_r_squared, .theme_plot, .print_debug
#' 
#' Features:
#' - Multiple interpolation types (linear, polynomial, spline)
#' - Country grouping by color schemes
#' - Country labels (points/text/repelled)
#' - R-squared display options
#' - Zero reference lines
#' - Flexible positioning
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
    
    .print_debug("Starting two-variable scatter plot", verbose, debug, type = "info", "SCATTER")
    
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
            .print_debug(paste("Labels created:", paste(names(labels), labels, collapse="; ")), 
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
                        ifelse(interpolation == TRUE, 1, NA))  # Default TRUE to linear
        
        if (!is.na(degree)) {
            # Polynomial regression
            plot <- plot + 
                ggplot2::geom_smooth(method = "lm", 
                           formula = y ~ poly(x, degree, raw = TRUE),
                           se = FALSE,
                           linewidth = 1.2,
                           color = "black")
            
            if (r_squared %in% c(1, 2)) {
                r2_result <- .calculate_r_squared(plot_data, x_var, y_var, method = "lm", degree = degree)
            }
        } else if (interpolation == "Spline") {
            # Spline interpolation
            if (!is.null(spline_bw)) {
                plot <- plot + 
                    ggplot2::geom_smooth(method = "loess", 
                               se = FALSE,
                               span = spline_bw,
                               linewidth = 1.2,
                               color = "black")
            } else {
                plot <- plot + 
                    ggplot2::geom_smooth(method = "loess", 
                               se = FALSE,
                               linewidth = 1.2,
                               color = "black")
            }
            
            if (r_squared %in% c(1, 2)) {
                r2_result <- .calculate_r_squared(plot_data, x_var, y_var, method = "loess", span = spline_bw)
            }
        }
        
        # Add R-squared text if requested
        if (r_squared %in% c(1, 2) && exists("r2_result")) {
            r2_text <- sprintf("R² = %.3f", r2_result$r_squared)
            
            # Determine position
            if (r_squared_pos == "flexible") {
                # Position based on negative values
                x_has_negative <- any(plot_data[[x_var]] < 0, na.rm = TRUE)
                y_has_negative <- any(plot_data[[y_var]] < 0, na.rm = TRUE)
                
                if (!x_has_negative && !y_has_negative) {
                    r_squared_pos <- "bottomright"
                } else if (!x_has_negative && y_has_negative) {
                    r_squared_pos <- "topright"
                } else if (x_has_negative && !y_has_negative) {
                    r_squared_pos <- "bottomleft"
                } else {
                    r_squared_pos <- "topleft"
                }
            }
            
            # Calculate position
            x_range <- range(plot_data[[x_var]], na.rm = TRUE)
            y_range <- range(plot_data[[y_var]], na.rm = TRUE)
            
            x_pos <- if (grepl("left", r_squared_pos)) {
                x_range[1] + diff(x_range) * 0.05
            } else {
                x_range[2] - diff(x_range) * 0.05
            }
            
            y_pos <- if (grepl("top", r_squared_pos)) {
                y_range[2] - diff(y_range) * 0.05
            } else {
                y_range[1] + diff(y_range) * 0.05
            }
            
            hjust <- if (grepl("left", r_squared_pos)) 0 else 1
            vjust <- if (grepl("top", r_squared_pos)) 1 else 0
            
            plot <- plot + 
                ggplot2::annotate("text", 
                        x = x_pos, 
                        y = y_pos,
                        label = r2_text,
                        hjust = hjust,
                        vjust = vjust,
                        size = theme$base_size/3)
        }
    }
    
    # 6. Add Scales and Labels
    plot <- plot +
        ggplot2::scale_x_continuous(
            name = if(!is.null(x_axis)) x_axis else x_var,
            labels = scales::comma_format(scale = 1),
            breaks = scales::pretty_breaks(n = 8)
        ) +
        ggplot2::scale_y_continuous(
            name = if(!is.null(y_axis)) y_axis else y_var,
            labels = scales::comma_format(scale = 1),
            breaks = scales::pretty_breaks(n = 8)
        ) +
        ggplot2::labs(x = if(!is.null(x_axis)) x_axis else x_var,
             y = if(!is.null(y_axis)) y_axis else y_var) +
        .theme_plot(theme = theme)
    
    return(plot)
}


#' Create Multi-Panel Scatter Plot for Multiple Variables
#' 
#' @description
#' Internal function that creates multiple scatter plots for comparing several variables.
#' Supports pairwise comparisons or comparisons against a common variable.
#' 
#' Used by: pp_plot_scatter (via .plot_core)
#' Uses: .plot_scatter_two_vars, wrap_plots
#' 
#' Features:
#' - Common variable mode (all vs one)
#' - Sequential pairing mode
#' - Flexible variable specification
#' - Panel titles
#' - Consistent formatting across panels
#' - Grid layout optimization
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