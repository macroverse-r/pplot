
#######################################################################################################################################################
# REFERENCE



#' Create References Citation Panel
#' 
#' @description
#' Internal function that creates a formatted panel for reference citations.
#' Handles text wrapping and layout of source citations.
#' 
#' Used by: .plot_core
#' Uses: .format_references
#' 
#' Features:
#' - Automatic text wrapping
#' - Width-aware formatting
#' - Character count optimization
#' - Consistent styling
#'
#' @keywords internal
.create_references_panel <- function(reference_col,        # character: column containing references
                                     dim,                    # numeric vector: plot dimensions
                                     font_size = 16,         # numeric: base font size in points
                                     theme = list(),
                                     verbose = TRUE,         # logical: controls information messages
                                     debug = TRUE) {           # logical: controls debug messages


    if (is.null(theme$ref_prefix)) {
        theme$ref_prefix <- "Reference: "
    }
    if (is.null(theme$ref_width_adj_scale)) {
        theme$ref_width_adj_scale <- 1
    }

    references <- .format_references(reference_col)
    mvcommon::mv_debug(references, FALSE, debug, type='info', text_type = 'ADD_REF')
    
    # Calculate characters per inch based on scaled font size
    chars_per_inch <- floor(9 * (16/font_size))
    
    margin_reduction <- 0.75
    effective_width <- dim[1] - margin_reduction
    max_chars <- floor(effective_width * chars_per_inch * theme$ref_width_adj_scale)
    
    # Text wrapping function remains the same...
    wrap_text_block <- function(text, width) {
        content <- sub("^Reference: ", "", text)
        
        # Split the content by explicit newlines first
        paragraphs <- strsplit(content, "\n", fixed = TRUE)[[1]]
        
        all_lines <- character(0)
        
        # Process each paragraph separately
        for (p_index in seq_along(paragraphs)) {
            words <- strsplit(paragraphs[p_index], "(?<=;) |(?<=[^;]) ", perl = TRUE)[[1]]
            words <- if (identical(words, character(0))) " " else words
            
            lines <- character(0)
            # Only use prefix for the first line of the first paragraph
            current_line <- if (p_index == 1) theme$ref_prefix else ""
            remaining_width <- width - if (p_index == 1) nchar(theme$ref_prefix) else 0
            
            for (word in words) {
                if (current_line == theme$ref_prefix || current_line == "") {
                    if (nchar(word) <= remaining_width) {
                        current_line <- paste0(current_line, word)
                    } else {
                        if (current_line != "") {
                            lines <- c(lines, substr(current_line, 1, nchar(current_line)-1))
                        }
                        current_line <- word
                    }
                } else {
                    test_line <- paste0(current_line, " ", word)
                    if (nchar(test_line) <= width) {
                        current_line <- test_line
                    } else {
                        lines <- c(lines, current_line)
                        current_line <- word
                    }
                }
            }
            
            if (nchar(current_line) > 0) {
                lines <- c(lines, current_line)
            }
            
            all_lines <- c(all_lines, lines)
        }
        
        paste(all_lines, collapse = "\n")
    }
    
    wrapped_refs <- wrap_text_block(references, max_chars)
    
    ggplot2::ggplot() + 
        ggplot2::theme_void() +
        ggplot2::labs(caption = wrapped_refs) +
        ggplot2::theme(
            plot.caption = ggplot2::element_text(
                hjust = 0,
                vjust = 1,
                # family = "CM Roman",
                size = font_size,
                lineheight = 1.2,
                margin = ggplot2::margin(t = 0, b = 0)
            ),
            plot.caption.position = "plot"
        )
}


#' Format Reference Citations
#' 
#' @description
#' Internal function that processes and formats reference citations.
#' Handles deduplication and consistent formatting of citations.
#' 
#' Used by: .create_references_panel
#' Uses: No direct function calls
#' 
#' Processing:
#' - Split by semicolon
#' - Remove duplicates
#' - Sort alphabetically
#' - Consistent separator
#'
#' @keywords internal
.format_references <- function(references) {            # character: semicolon-separated reference strings

    all_refs <- unlist(strsplit(references, split = ";\\s*"))
    unique_refs <- unique(all_refs)
    sorted_refs <- sort(unique_refs)
    paste0(sorted_refs, collapse = "; ")
}


#######################################################################################################################################################
# LEGEND


#' Create Plot Legend Panel with Optimal Layout
#' 
#' @description
#' Internal function that generates a standalone legend panel with optimized layout.
#' Handles different plot types and legend styles with automatic sizing.
#' 
#' Used by: .plot_core
#' Uses: .get_legend_type, .get_legend_columns, get_legend (cowplot)
#' 
#' Features:
#' - Adaptive column count
#' - Plot-type specific styling
#' - Size-responsive layout
#' - Support for fill and color scales
#'
#' @keywords internal
.create_legend_panel <- function(base_plot,                 # ggplot object: plot to extract legend from
                                 data,                       # data.frame: data for legend creation
                                 plot_type,                  # string: type of plot for legend styling
                                 legend_fill = FALSE,        # logical: whether to include fill legend
                                 plot_width_inches = 10,     # numeric: plot width for layout calculation
                                 base_size = 16,             # numeric: base font size in points
                                 verbose = TRUE,             # logical: controls information messages
                                 debug = TRUE) {             # logical: controls debug messages
    
    # Get appropriate legend type based on plot type
    if (plot_type == "scatter") {
        # For scatter plots, use the scale breaks as legend items
        color_scale <- base_plot$scales$scales[[which(sapply(base_plot$scales$scales, function(x) "colour" %in% x$aesthetics))]]
        if (!is.null(color_scale)) {
            # Get the actual breaks (group names) that will appear in the legend
            legend_items <- color_scale$breaks
            if (is.null(legend_items)) {
                # If breaks not set, use unique values from the color grouping
                color_group_data <- unique(data$color_group)
                legend_items <- color_group_data[!is.na(color_group_data)]
            }

            legend_items_display <- color_scale$labels
            n_items <- length(legend_items)
        } else {
            legend_items_display <- "Default"
            n_items <- 1
        }
    } else {
        # For other plot types, use .get_legend_type to determine what to group by
        legend_type <- .get_legend_type(base_plot)
        legend_items_display <- unique(data[[legend_type]])
        n_items <- length(legend_items_display)
    }
    
    if (verbose) {
        mvcommon::mv_debug(paste("Legend items (display):", paste(legend_items_display, collapse=", ")), 
                   verbose, debug, type = "info", "LEGEND")
    }
    
    # Calculate optimal number of columns using display names
    legend_cols <- .get_legend_columns(
        legend_items = legend_items_display,
        plot_width_inches = plot_width_inches,
        base_size = base_size
    )
    
    # Create appropriate guides based on plot type and fill
    if (legend_fill) {
        legend_guides <- ggplot2::guides(
            color = ggplot2::guide_legend(byrow = TRUE, ncol = 1),
            fill = ggplot2::guide_legend(byrow = TRUE, ncol = legend_cols - 1)
        )
    } else {
        legend_guides <- ggplot2::guides(
            color = ggplot2::guide_legend(byrow = TRUE, ncol = legend_cols)
        )
    }
    
    # Get legend panel
    legend_panel <- cowplot::get_legend(
        base_plot + 
        ggplot2::theme(
            legend.position = "bottom",
            legend.text = ggplot2::element_text(size = base_size)
        ) +
        legend_guides
    )
    
    # Calculate legend height
    legend_height <- ceiling(n_items/legend_cols) * (base_size/12)
    if(legend_fill) {
        legend_height <- legend_height + (base_size/16)
    }
    
    if(verbose) {
        msg <- paste0("plot_type = '", plot_type, 
                     "' | legend_cols = ", legend_cols,
                     "' | legend_height = ", legend_height, 
                     "' | n_items = ", n_items)
        mvcommon::mv_debug(msg, verbose, debug, type = "info", "LEGEND")
    }
    
    return(list(
        panel = legend_panel,
        height = legend_height
    ))
}



#' Determine Legend Variable Type
#' 
#' @description
#' Internal function that determines what variable to use for legend grouping.
#' Examines plot mappings to find color or fill aesthetics.
#' 
#' Used by: .create_legend_panel
#' Uses: No direct function calls
#' 
#' Logic:
#' - Checks for color mapping first
#' - Falls back to fill if color not found
#' - Defaults to "Country" if no mapping found
#'
#' @keywords internal
.get_legend_type <- function(plot) {                    # ggplot object: plot to examine for legend mapping


    if (!is.null(plot$type_legend)) {
        return(plot$type_legend)
    }

    # Check if color mapping exists
    if ("colour" %in% names(plot$mapping)) {
        return(as.character(plot$mapping$colour)[2])
    }
    
    # Check scales for fill aesthetic
    scales <- plot$scales$scales
    scale_names <- sapply(scales, function(x) x$aesthetics[1])
    if ("fill" %in% scale_names) {
        return("Variable")  # In area plots, fill is always mapping Variables
    }
    
    # Default fallback
    return("Country")
}




#' Calculate Optimal Number of Legend Columns
#' 
#' @description
#' Internal function that determines the optimal number of columns for legend layout.
#' Considers plot width, text length, and item count.
#' 
#' Calculate optimal number of legend columns based on:
#' \enumerate{
#'   \item Plot width in inches (dim\[1\])
#'   \item Length of legend items
#'   \item Font size and approximate character width
#' }
#'
#' Used by: .create_legend_panel, .plot_multi_indic_bar
#' Uses: No direct function calls
#' 
#' Calculations:
#' - Character width estimation
#' - Space for legend symbols
#' - Padding consideration
#' - Width constraints
#'
#' @keywords internal
.get_legend_columns <- function(legend_items,            # vector: items to display in legend
                                plot_width_inches,         # numeric: available width for layout
                                base_size = 16) {          # numeric: base font size for text measurement

    # Calculate approximate width of each legend item in inches
    # Assuming each character is roughly 60% of font size in points
    # 72 points = 1 inch

    char_width_inches <- (base_size * 0.36) / 66
    
    # Add some padding for the legend key/symbol (roughly 1.5x character height)
    key_width_inches <- (base_size * 1.5) / 72
    
    # Calculate width needed for each legend item
    item_widths <- sapply(legend_items, function(item) {
        # Width = key + text + padding
        (nchar(as.character(item)) * char_width_inches) + key_width_inches + (char_width_inches * 2)
    })
    
    # Find the width of the widest item
    max_item_width <- max(item_widths)
    
    # Calculate maximum possible columns given plot width
    # Leave 10% margin on each side
    effective_width <- plot_width_inches * 0.8
    # print(paste0("effective_width = ", effective_width))
    max_possible_cols <- floor(effective_width / max_item_width)
    
    # Ensure at least 1 column and no more than number of items
    n_items <- length(legend_items)
    optimal_cols <- min(max(1, max_possible_cols), n_items)
    # print(paste0("n_items = ", n_items))
    # print(paste0("optimal_cols = ", optimal_cols))
    
    # If we have very few items, don't use all available width
    if (n_items <= 3) {
        optimal_cols <- min(optimal_cols, n_items)
    }
    
    return(optimal_cols)
}





#######################################################################################################################################################
# TITLE



#' Create Title and Subtitle Panel
#' 
#' @description
#' Internal function that creates a formatted title panel.
#' Handles both title and subtitle with consistent styling.
#' 
#' Used by: .plot_core
#' Uses: plot_annotation (patchwork)
#' 
#' Features:
#' - Scaled text sizes
#' - Consistent margins
#' - Font family control
#' - Optional subtitle
#'
#' @keywords internal
.create_title_panel <- function(title = NULL,           # string/NULL: main title text
                                 subtitle = NULL,         # string/NULL: subtitle text
                                 title_size = 18,          # numeric: base font size for scaling
                                 subtitle_size = 16,      # numeric: scaling factor for title
                                 title_face = "bold",     # string: "plain", "bold", "italic", or "bold.italic"
                                 subtitle_scale = 1) {    

    theme_settings <- ggplot2::theme(
        plot.margin = ggplot2::margin(t = 15, r = 0, b = 0, l = 0, unit = "pt")
    )
    
    if (!is.null(title)) {
        theme_settings <- theme_settings + 
            ggplot2::theme(
                plot.title = ggplot2::element_text(
                    size = title_size,
                    face = title_face,
                    margin = ggplot2::margin(b = 10)
                )
            )
    }
    
    if (!is.null(subtitle)) {
        theme_settings <- theme_settings + 
            ggplot2::theme(
                plot.subtitle = ggplot2::element_text(
                    size = subtitle_size,
                    margin = ggplot2::margin(b = 10)
                )
            )
    }
    
    list(
        annotation = patchwork::plot_annotation(
            title = title,
            subtitle = subtitle,
            theme = theme_settings
        ),
        has_title = !is.null(title) || !is.null(subtitle)
    )
}



#######################################################################################################################################################
# THEME



#' Create Consistent Theme for All Plots
#' 
#' @description
#' Internal function that defines the common visual theme used across all plot types.
#' Ensures consistent styling and professional appearance of all plots.
#' 
#' Used by: All plotting functions
#' Uses: theme_minimal
#' 
#' Theme elements:
#' - Grid: Major lines only, minimal appearance
#' - Panel: transparent background, no border
#' - Text: Consistent sizing and positioning
#' - Legend: Bottom position with spacing rules
#' - Margins: Optimized for readability
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

#' Create Consistent X-Axis Scale for Time Series
#' 
#' @description
#' Internal function that creates a standardized date-based x-axis scale.
#' Automatically determines appropriate break intervals based on date range.
#' 
#' Used by: All time series plotting functions
#' Uses: scale_x_date, difftime
#' 
#' Features:
#' - Adaptive date breaks
#' - Year-based labels
#' - Zero expansion at ends
#' - Range based on data extents
#'
#' @keywords internal
.scale_x <- function(data) {                    # data.frame: input data containing Date column

    start_date <- min(data$Date)
    end_date <- max(data$Date)
    years_difference <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25
    breaks_years <- max(round(years_difference/12), 1)
    
    ggplot2::scale_x_date(date_breaks = paste0(breaks_years," years"), 
                 date_labels = "%Y",
                 limits = c(start_date, end_date),
                 expand = ggplot2::expansion(mult = c(0, 0)))
}



#' Create Consistent Y-Axis Scale
#' 
#' @description
#' Internal function that creates a standardized continuous y-axis scale.
#' Handles axis labeling and break determination.
#' 
#' Used by: All plotting functions
#' Uses: ggplot2::scale_y_continuous, scales::comma_format, scales::pretty_breaks
#' 
#' Features:
#' - Comma formatting for large numbers
#' - Pretty break intervals
#' - Optional axis label
#' - Consistent break count
#'
#' @keywords internal
.scale_y <- function(y_axis = NULL,             # string/NULL: axis label
                      n_breaks = 8) {             # numeric: target number of axis breaks
    if (!is.null(y_axis) && (isFALSE(y_axis) || is.na(y_axis))) {
        y_axis <- NULL
    }
    ggplot2::scale_y_continuous(
        name = y_axis,
        labels = scales::comma_format(scale = 1),
        breaks = scales::pretty_breaks(n = n_breaks)
    )
}





#######################################################################################################################################################
# PRINT AND SAVE


#' Handle Plot Display and File Output
#' 
#' @description
#' Internal function that manages plot display and file saving operations.
#' Handles both interactive display and file output in multiple formats.
#' Ensures consistent output handling across all plot types.
#' 
#' Used by: .plot_core
#' Uses: dev.new, ggsave
#' 
#' File output details:
#' - PNG: 300 DPI with background controlled by parameter bg
#' - PDF: Uses cairo_pdf device for better font handling
#' - Both files saved in 'img/' directory
#'
#' @keywords internal
.print_and_save <- function(plot,                    # ggplot2 object: the plot to display/save
                             print = TRUE,              # logical: whether to display the plot in current device
                             filename = NULL,           # string/NULL: output filename without extension
                             dim,                       # numeric vector: plot dimensions c(width, height) in inches
                             bg = "transparent") {      # background color
    
    
    if (print) {
        # Create new device with specified dimensions and consistent DPI
        grDevices::dev.new(width = dim[1], 
                          height = dim[2], 
                          unit = "in", 
                          noRStudioGD = TRUE,
                          res = 300)  # Set resolution to match ggsave default
        print(plot)
    }
    
    # Save if filename provided
    if (!is.null(filename) && filename != '') {
        # Save PNG with explicit device settings
        # Used grDevices::png with type = "cairo" for better text rendering in PNG files
        ggplot2::ggsave(paste0(filename, ".png"), 
                        plot = plot, 
                        width = dim[1], 
                        height = dim[2], 
                        bg = bg,
                        dpi = 300,
                        device = grDevices::png,
                        type = "cairo")  # Use cairo for better text rendering
        
        # Save PDF with consistent settings
        ggplot2::ggsave(paste0(filename, ".pdf"), 
                        plot = plot, 
                        width = dim[1], 
                        height = dim[2], 
                        device = grDevices::cairo_pdf,
                        bg = bg)
    }
}