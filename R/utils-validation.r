#' Validate Common Plot Arguments
#' 
#' @description
#' Internal function that validates and normalizes arguments common to all plot types.
#' First validation step for all plotting functions to ensure data integrity
#' and argument consistency.
#' 
#' Used by: .validate_bar_args, .validate_series_args, .validate_scatter_args
#' Uses: mvcommon::mv_debug, isomapper::im_from_iso
#' 
#' Validation steps:
#' - Required data columns check
#' - Logical parameter normalization
#' - File output parameter handling
#' - Size and dimension validation
#' - Date handling
#' - Reference column check
#'
#' @keywords internal
.validate_common_args <- function(args) {                    # list: full set of function arguments to validate

    # Check for required data parameter
    if (is.null(args$data)) {
        stop("'data' parameter is required")
    }
    

    # TODO enable an option for verbose = 0, 1, 2, 3 (different degree of printing on the terminal)
    # mvcommon::mv_debug("Starting pp_plot_scatter", verbose, debug, type = "info", "MAIN")


    # Validate and standardize logical parameters
    logical_params <- c("print", "legend", "reference", "verbose", "debug")
    for (param in logical_params) {
        if (!is.null(args[[param]]) && !is.logical(args[[param]])) {
            args[[param]] <- as.logical(args[[param]])
            if (is.na(args[[param]])) { args[[param]] <- TRUE }
            mvcommon::mv_debug(sprintf("Parameter '%s' coerced to logical: %s", param, args[[param]]), 
                       args$verbose, args$debug, type = "warning")
        } else if (is.null(args[[param]])) {
            args[[param]] <- FALSE
        }
    }
    

    # Validate bg parameter
    if (!is.null(args$bg)) {
        if (!is.character(args$bg)) {
            mvcommon::mv_debug("Invalid 'bg' parameter type. Must be character. Using default: 'transparent'", 
                       args$verbose, args$debug, type = "warning")
            args$bg <- "transparent"
        }
        # Try to convert the color string - this will throw an error if invalid
        tryCatch({
            if (!args$bg %in% colors() && !grepl("^#[0-9A-Fa-f]{6}$", args$bg) && args$bg != "transparent") {
                mvcommon::mv_debug("Invalid color value for 'bg'. Using default: 'transparent'", 
                           args$verbose, args$debug, type = "warning")
                args$bg <- "transparent"
            }
        }, error = function(e) {
            mvcommon::mv_debug("Error parsing 'bg' color value. Using default: 'transparent'", 
                       args$verbose, args$debug, type = "warning")
            args$bg <- "transparent"
        })
    } else {
        args$bg <- "transparent"  # Set default value if not provided
    }

    if (is.null(args$print)) {
        args$print <- TRUE
        mvcommon::mv_debug("Parameter 'print' was NULL, converted to TRUE", 
                   args$verbose, args$debug, type = "info", "CONVERT")
    } else if (!is.null(args$print) && !is.logical(args$print)) {
        args$print <- as.logical(args$print)
        if (is.na(args$print)) { args$print <- FALSE }
        mvcommon::mv_debug(sprintf("Parameter 'print' coerced to logical: %s", args$print), 
                   args$verbose, args$debug, type = "warning")
    }


    # Validate required columns in data
    required_cols <- c("Date", "Variable", "Value", "ISO")
    missing_cols <- required_cols[!required_cols %in% names(args$data)]
    if (length(missing_cols) > 0) {
        stop(sprintf("Missing required columns in data: %s", paste(missing_cols, collapse = ", ")))
    }

    # Add the column Country (Country names) in data if missing
    if(!"Country" %in% names(args$data)) {
        mvcommon::mv_debug(sprintf("Column Country added to data"), args$verbose, args$debug, type = "info", "COUNTRY")
        args$data$Country <- isomapper::im_from_iso(args$data$ISO, opt = "name", verbose = FALSE)
    }

    
    # Check Reference column if reference = TRUE
    if (isTRUE(args$reference)) {
        if (!"Reference" %in% names(args$data)) {
            mvcommon::mv_debug("'Reference' column is missing but reference = TRUE. Setting reference = FALSE", 
                       args$verbose, args$debug, type = "warning")
            args$reference <- FALSE
        } else if (all(is.na(args$data$Reference))) {
            mvcommon::mv_debug("'Reference' column contains only NA values. Setting reference = FALSE", 
                       args$verbose, args$debug, type = "warning")
            args$reference <- FALSE
        }
    }
    
    # Convert FALSE to NULL for certain parameters
    null_if_false <- c("right_axis", "key_dates", "title", "subtitle", "filename", "size")
    if (args$plot_type != "scatter" ) {
        null_if_false <- c(null_if_false, "y_axis")
    }
    for (param in null_if_false) {
        if (!is.null(args[[param]]) && isFALSE(args[[param]])) {
            # TODO check all "<- NULL"
            args[param] <- list(NULL)
            mvcommon::mv_debug(sprintf("Parameter '%s' was FALSE, converted to NULL", param), 
                       args$verbose, args$debug, type = "info", "CONVERT")
        }
    }
    


    # Validate size parameter
    if (!is.null(args$size) && !args$size %in% c(1, 2, 3, 4)) {
        mvcommon::mv_debug("Invalid 'size' parameter. Should be 1, 2, 3, or 4. Using default: 1", 
                   args$verbose, args$debug, type = "warning")
        args$size <- 1
    }
    
    # TODO need to be replaced by theme
    # # Validate base_size parameter
    # # Attempt to convert args$base_size to numeric, or set to NULL if it's not provided
    # base_size_value <- if (!is.null(args$base_size) && !is.logical(args$base_size)) as.numeric(args$base_size) else NULL
    #
    # # Check if the base_size_value is NULL, NA, or non-positive
    # if (is.null(base_size_value) || is.na(base_size_value) || base_size_value <= 0 || is.logical(base_size_value)) {
    #     mvcommon::mv_debug("Invalid 'base_size' parameter. Using default: 16", 
    #                  args$verbose, args$debug, type = "warning")
    #     args$base_size <- 16
    # } else {
    #     args$base_size <- base_size_value
    # }

    # Validate key_dates if provided
    if (!is.null(args$key_dates)) {
        if (!(is.data.frame(args$key_dates) || is.matrix(args$key_dates))) {
            mvcommon::mv_debug("key_dates must be a data frame or matrix. Setting to NULL", 
                       args$verbose, args$debug, type = "warning")
            args["key_dates"] <- list(NULL)
        } else if (ncol(args$key_dates) != 2) {
            mvcommon::mv_debug("key_dates must have exactly 2 columns. Setting to NULL", 
                       args$verbose, args$debug, type = "warning")
            args["key_dates"] <- list(NULL)
        } 
    }

    # Validate plot_type (if present)
    if (!is.null(args$plot_type) && !args$plot_type %in% c("series", "bar", "scatter")) {
        stop(sprintf("Invalid plot_type: %s. Must be one of: series, bar, scatter", args$plot_type))
    }
    
    # Return validated and normalized arguments
    return(args)
}




#' Validate Time Series Plot Arguments
#' 
#' @description
#' Internal function that validates and normalizes arguments specific to time series plots.
#' Ensures proper configuration for time series creation after common validation.
#' 
#' Used by: pp_plot_series
#' Uses: .validate_common_args, mvcommon::mv_debug
#' 
#' Validation:
#' - Right axis configuration
#' - Area plot compatibility
#' - Panel arrangement (by_indicator)
#' - Subplot titles
#' - Key dates format
#' - Multi-country/indicator logic
#'
#' @keywords internal
.validate_series_args <- function(args) {                    # list: full set of time series arguments to validate


    # First validate common arguments
    args <- .validate_common_args(args)
    
    # Get number of indicators and countries for validation logic
    n_indicators <- length(unique(args$data$Variable))
    n_countries <- length(unique(args$data$Country))
    
    if (args$verbose) {
        msg <- paste0("n_countries = ", n_countries, 
                     " | n_indicators = ", n_indicators,
                     " | plot_type = series")
        mvcommon::mv_debug(msg, args$verbose, args$debug, type = "info", "CONFIG")
    }
    
    # Validate and normalize series-specific logical parameters
    series_logical_params <- c("area", "by_indicator")
    for (param in series_logical_params) {
        if (!is.null(args[[param]]) && !is.logical(args[[param]])) {
            args[[param]] <- as.logical(args[[param]])

            if (is.na(args[[param]])) {
                args[[param]] <- FALSE
            }
            mvcommon::mv_debug(sprintf("Parameter '%s' coerced to logical: %s", param, args[[param]]), 
                       args$verbose, args$debug, type = "warning")
        }
    }

    # Validate by_indicator parameter in context
    if (!is.null(args$by_indicator)) {
        if (n_countries == 1 || n_indicators == 1) {
            mvcommon::mv_debug("by_indicator parameter ignored for single country or single indicator", 
                       args$verbose, args$debug, type = "warning")
            args$by_indicator <- NULL
        }
    }
    

    # Validate subfig_title parameter
    # print(paste0("args$subfig_title = ", paste0(args$subfig_title, collapse = ", ")))
    if (!is.null(args$subfig_title)) {
        if (n_countries == 1 || n_indicators == 1) {
            mvcommon::mv_debug("subfig_title parameter ignored for single country or single indicator", 
                       args$verbose, args$debug, type = "warning")
            args$subfig_title <- NULL
        } else if (is.character(args$subfig_title)) {
            N <- length(args$subfig_title)
            if (isTRUE(args$by_indicator)) {
                args$subfig_title <- if (N < n_indicators) c(args$subfig_title, rep("", n_indicators - N)) else head(args$subfig_title, n_indicators)
            } else if (isFALSE(args$by_indicator)) {
                args$subfig_title <- if (N < n_countries) c(args$subfig_title, rep("", n_countries - N)) else head(args$subfig_title, n_countries)
            } else {
                stop(sprintf("Invalid subfig_title: %s and by_indicator: %s combination. ", args$subfig_title, args$by_indicator))
            }
        } else if (!isTRUE(args$subfig_title) && !isFALSE(args$subfig_title)){
            mvcommon::mv_debug(sprintf("Invalid subfig_title: %s. Setting to default (subfig_title = TRUE)", args$subfig_title),
                       args$verbose, args$debug, type = "warning")
            args$subfig_title <- TRUE
        }
    }else {
        args$subfig_title <- TRUE
    }

    
    # Validate right_axis if provided
    if (!is.null(args$right_axis)) {
        # if (n_indicators < 2) {
        #     mvcommon::mv_debug("right_axis requires at least 2 indicators. Setting right_axis = NULL", 
        #                args$verbose, args$debug, type = "warning")
            # args$right_axis <- NULL
        # }else 
        if (!is.character(args$right_axis) && !isTRUE(args$right_axis)) {
            mvcommon::mv_debug("Invalid right_axis parameter. Must be TRUE or character. Setting to TRUE", 
                       args$verbose, args$debug, type = "warning")
            args$right_axis <- TRUE
        }else if (is.character(args$right_axis)) {
            # Check if specified indicators exist in data
            missing_indicators <- args$right_axis[!args$right_axis %in% unique(args$data$Variable)]
            if (length(missing_indicators) > 0) {
                mvcommon::mv_debug(sprintf("Indicators not found for right_axis: %s. Setting to TRUE", 
                                  paste(missing_indicators, collapse = ", ")), 
                           args$verbose, args$debug, type = "warning")
                args$right_axis <- TRUE
            }
        }
    }
    
    
    # Check for incompatible parameter combinations
    if (isTRUE(args$area) && isTRUE(args$right_axis)) {
        mvcommon::mv_debug("Parameters 'area' and 'right_axis' cannot be used together. Setting right_axis = NULL", 
                   args$verbose, args$debug, type = "warning")
        args["right_axis"] <- list(NULL)
    }
    
    # Remove irrelevant bar and scatter parameters if present
    bar_params <- c("type", "order", "show_values", "show_year", "show_range", 
                   "show_median", "color")
    scatter_params <- c("x_axis", "ISO", "interpolation", "spline_bw", "r_squared", 
                       "r_squared_pos", "label_nudge", "highlight_zero", "common_var")
    
    irrelevant_params <- c(bar_params, scatter_params)
    present_irrelevant <- irrelevant_params[irrelevant_params %in% names(args)]
    
    if (length(present_irrelevant) > 0) {
        mvcommon::mv_debug(sprintf(
            "The following parameters are not used in series plots and will be ignored: %s",
            paste(present_irrelevant, collapse = ", ")
        ), args$verbose, args$debug, type = "warning")
        args[present_irrelevant] <- NULL
    }
    
    

    return(args)
}



#' Validate Bar Plot Arguments
#' 
#' @description
#' Internal function that validates and normalizes arguments specific to bar plots.
#' Ensures proper configuration for bar plot creation after common validation.
#' 
#' Used by: pp_plot_bar
#' Uses: .validate_common_args, mvcommon::mv_debug
#' 
#' Validation:
#' - Type parameter ("Mean"/"Last")
#' - Order parameter (Variable/Alphabetical)
#' - Value display options
#' - Statistical indicators
#' - Color scheme assignment
#' - Multi-indicator handling
#'
#' @keywords internal
.validate_bar_args <- function(args) {                    # list: full set of bar plot arguments to validate


    # add column "Date" if it does not exist
    if (!"Date" %in% names(args$data)) {
        args$data$Date <- as.Date("2000-01-01")
    }


    # First validate common arguments
    args <- .validate_common_args(args)
    
    # Get number of indicators and countries for validation logic
    n_indicators <- length(unique(args$data$Variable))
    n_countries <- length(unique(args$data$Country))
    
    if (args$verbose) {
        msg <- paste0("n_countries = ", n_countries, 
                     " | n_indicators = ", n_indicators,
                     " | plot_type = bar")
        mvcommon::mv_debug(msg, args$verbose, args$debug, type = "info", "CONFIG")
    }
    
    # Validate and normalize bar-specific logical parameters
    bar_logical_params <- c("show_range", "show_median")
    for (param in bar_logical_params) {
        if (!is.null(args[[param]]) && !is.logical(args[[param]])) {
            args[[param]] <- as.logical(args[[param]])
            mvcommon::mv_debug(sprintf("Parameter '%s' coerced to logical: %s", param, args[[param]]), 
                       args$verbose, args$debug, type = "warning")
        }
    }
    
    # Validate type parameter
    if (!is.null(args$type) && !args$type %in% c("Mean", "Last")) {
        mvcommon::mv_debug("Invalid 'type' parameter. Using default: 'Mean'", 
                   args$verbose, args$debug, type = "warning")
        args$type <- "Mean"
    }
    
    # Validate show_values parameter
    if (!is.null(args$show_values) && !args$show_values %in% c("Bottom", "Top", FALSE)) {
        mvcommon::mv_debug("Invalid 'show_values' parameter. Using default: 'Bottom'", 
                   args$verbose, args$debug, type = "warning")
        args$show_values <- "Bottom"
    }
    
    # Validate show_year parameter
    if (!is.null(args$show_year) && !args$show_year %in% c(0, 1, 2)) {
        mvcommon::mv_debug("Invalid 'show_year' parameter. Using default: 1", 
                   args$verbose, args$debug, type = "warning")
        args$show_year <- 1
    }
    
    # Validate order parameter
    if (!is.null(args$order)) {
        if (is.logical(args$order)) {
            if (is.na(args$order)) {
                mvcommon::mv_debug("NA value in order parameter. Using default: 'Variable'", 
                          args$verbose, args$debug, type = "warning")
                args$order <- "Variable"
            } else {
                args$order <- if(isTRUE(args$order)) "Variable" else "Alphabetical"
            }
        } else if (is.numeric(args$order)) {
            if (args$order < 1) {
                mvcommon::mv_debug("Order value less than 1. Using order = 1", 
                          args$verbose, args$debug, type = "warning")
                args$order <- 1
            }
        } else if (!args$order %in% c("Alphabetical", "Variable", "Input")) {
            mvcommon::mv_debug("Invalid 'order' parameter. Using default: 'Variable'", 
                       args$verbose, args$debug, type = "warning")
            args$order <- "Variable"
        }
    } else {
        args$order <- "Variable"  # Default value
    }
    
    # Validate color parameter with context awareness
    if (!is.null(args$color)) {
        if (n_indicators > 1) {
            # For multiple indicators, set color to "Variable" if it's TRUE or a region-based value
            if (isTRUE(args$color)) {
                args$color <- "Variable"
            } else if (args$color %in% c("Subregion", "Region", "Center-Periphery", "Hydrocarbon")) {
                args$color <- "Variable"
                mvcommon::mv_debug("Setting color to 'Variable' for multi-indicator bar plot", 
                          args$verbose, args$debug, type = "warning")
            }
        } else {
            # For single indicator, set color to "Subregion" if it's TRUE or "Variable"
            if (isTRUE(args$color)) {
                args$color <- "Subregion"
            } else if (args$color == "Variable") {
                args$color <- "Subregion"
                mvcommon::mv_debug("Setting color to 'Subregion' for single-indicator bar plot", 
                          args$verbose, args$debug, type = "warning")
            }
        }
        
        # Validate final color value
        # if (!args$color %in% c("Subregion", "Hydrocarbon", "Variable", "Region", "Center-Periphery", FALSE)) {
        #     mvcommon::mv_debug("Invalid 'color' parameter. Using default: NULL", 
        #                args$verbose, args$debug, type = "warning")
        #     args["color"] <- list(NULL)
        # }
    }
    
    # Remove irrelevant series and scatter parameters if present
    series_params <- c("right_axis", "key_dates", "area", "by_indicator", "subfig_title")
    scatter_params <- c("x_axis", "ISO", "interpolation", "spline_bw", "r_squared", 
                       "r_squared_pos", "label_nudge", "highlight_zero", "common_var", "subfig_title")
    
    irrelevant_params <- c(series_params, scatter_params)
    present_irrelevant <- irrelevant_params[irrelevant_params %in% names(args)]
    
    if (length(present_irrelevant) > 0) {
        mvcommon::mv_debug(sprintf(
            "The following parameters are not used in bar plots and will be ignored: %s",
            paste(present_irrelevant, collapse = ", ")
        ), args$verbose, args$debug, type = "warning")
        args[present_irrelevant] <- NULL
    }
    
    return(args)
}



#' Validate Scatter Plot Arguments
#' 
#' @description
#' Internal function that validates and normalizes arguments specific to scatter plots.
#' Ensures proper configuration for scatter plot creation after common validation.
#' 
#' Used by: pp_plot_scatter
#' Uses: .validate_common_args, .validate_color_list, mvcommon::mv_debug
#' 
#' Validation:
#' - Variable count check
#' - ISO label options
#' - Interpolation settings
#' - R-squared display
#' - Color grouping
#' - Common variable handling
#' - Multi-variable arrangement
#'
#' @keywords internal
.validate_scatter_args <- function(args) {                    # list: full set of scatter plot arguments to validate


    # add column "Date" if it does not exist
    if (!"Date" %in% names(args$data)) {
        args$data$Date <- as.Date("2000-01-01")
    }

    # First validate common arguments
    args <- .validate_common_args(args)
    
    # Get number of indicators and countries for validation logic
    n_indicators <- length(unique(args$data$Variable))
    n_countries <- length(unique(args$data$Country))
    
    if (args$verbose) {
        msg <- paste0("n_countries = ", n_countries, 
                     " | n_indicators = ", n_indicators,
                     " | plot_type = scatter")
        mvcommon::mv_debug(msg, args$verbose, args$debug, type = "info", "CONFIG")
    }
    
    # Validate minimum number of variables
    if (n_indicators < 2) {
        stop("Scatter plots require at least 2 variables")
    }
    

    # Validate subfig_title parameter
    # print(paste0("args$subfig_title = ", paste0(args$subfig_title, collapse = ", ")))
    if (!is.null(args$subfig_title)) {
        if (n_indicators < 2) {
            mvcommon::mv_debug("subfig_title parameter ignored for two indicators", 
                       args$verbose, args$debug, type = "warning")
            args$subfig_title <- NULL
        } else if (is.character(args$subfig_title)) {
            N <- length(args$subfig_title)
            args$subfig_title <- if (N < n_indicators) c(args$subfig_title, rep("", n_indicators - N)) else head(args$subfig_title, n_indicators)
        } else if (!isTRUE(args$subfig_title) && !isFALSE(args$subfig_title)){
            mvcommon::mv_debug(sprintf("Invalid subfig_title: %s. Setting to default (subfig_title = TRUE)", args$subfig_title),
                       args$verbose, args$debug, type = "warning")
            args$subfig_title <- TRUE
        }
    }else {
        args$subfig_title <- FALSE
    }

    # Validate and normalize scatter-specific logical parameters
    scatter_logical_params <- c("highlight_zero", "no_other")
    for (param in scatter_logical_params) {
        if (!is.null(args[[param]]) && !is.logical(args[[param]])) {
            args[[param]] <- as.logical(args[[param]])
            mvcommon::mv_debug(sprintf("Parameter '%s' coerced to logical: %s", param, args[[param]]), 
                       args$verbose, args$debug, type = "warning")
        }
    }
    
    # Validate ISO parameter
    if (!is.null(args$ISO) && !args$ISO %in% c(TRUE, "Both", FALSE)) {
        mvcommon::mv_debug("Invalid 'ISO' parameter. Using default: NULL", 
                   args$verbose, args$debug, type = "warning")
        args$ISO <- NULL
    }
    
    # Validate interpolation parameter
    if (!is.null(args$interpolation)) {
        if (isTRUE(args$interpolation)) {
            args$interpolation <- "Linear"
        } else if (!args$interpolation %in% c("Linear", "Square", "Cubic", "Spline", FALSE)) {
            mvcommon::mv_debug("Invalid 'interpolation' parameter. Using default: FALSE", 
                       args$verbose, args$debug, type = "warning")
            args$interpolation <- FALSE
        }
    }
    
    # Validate spline_bw parameter (only relevant if interpolation = "Spline")
    if (!is.null(args$spline_bw)) {
        args$spline_bw <- as.numeric(args$spline_bw)
        if (is.na(args$spline_bw) || args$spline_bw <= 0) {
            mvcommon::mv_debug("Invalid 'spline_bw' parameter. Using default: NULL", 
                       args$verbose, args$debug, type = "warning")
            args$spline_bw <- NULL
        }
    }
    
    # Validate r_squared parameter
    if (!is.null(args$r_squared) && !args$r_squared %in% c(0, 1, 2, 3)) {
        mvcommon::mv_debug("Invalid 'r_squared' parameter. Using default: 0", 
                   args$verbose, args$debug, type = "warning")
        args$r_squared <- 0
    }
    
    # Validate r_squared_pos parameter
    valid_positions <- c("flexible", "topleft", "topright", "bottomleft", "bottomright")
    if (!is.null(args$r_squared_pos) && !args$r_squared_pos %in% valid_positions) {
        mvcommon::mv_debug("Invalid 'r_squared_pos' parameter. Using default: 'flexible'", 
                   args$verbose, args$debug, type = "warning")
        args$r_squared_pos <- "flexible"
    }
    
    # Validate label_nudge parameter
    if (!is.null(args$label_nudge)) {
        args$label_nudge <- as.numeric(args$label_nudge)
        if (is.na(args$label_nudge) || args$label_nudge < 0) {
            mvcommon::mv_debug("Invalid 'label_nudge' parameter. Using default: 0.05", 
                       args$verbose, args$debug, type = "warning")
            args$label_nudge <- 0.05
        }
    }
    
    # Validate color parameter
    if (!is.null(args$color)) {
        if (is.list(args$color)) {
            mvcommon::mv_debug("Validating color list", args$verbose, args$debug, type = "info", "COLOR")
            
            # Get unique countries in data
            data_countries <- unique(args$data$Country)
            
            # Check each group in the list
            for (group_name in names(args$color)) {
                countries <- args$color[[group_name]]
                if (!is.character(countries)) {
                    stop(sprintf("Color list group '%s' must contain character values", group_name))
                }
                
                # Check for countries not in data
                invalid_countries <- countries[!countries %in% data_countries]
                if (length(invalid_countries) > 0) {
                    warning_msg <- sprintf("Countries not found in data for group '%s': %s", 
                                         group_name, 
                                         paste(invalid_countries, collapse = ", "))
                    mvcommon::mv_debug(warning_msg, args$verbose, args$debug, type = "warning", "COLOR")
                }
                
                # Check for duplicate countries across groups
                for (other_group in names(args$color)[1:which(names(args$color) == group_name) - 1]) {
                    duplicates <- intersect(countries, args$color[[other_group]])
                    if (length(duplicates) > 0) {
                        warning_msg <- sprintf(
                            "Countries %s already assigned to group '%s', ignoring in '%s'",
                            paste(duplicates, collapse = ", "),
                            other_group,
                            group_name)
                        mvcommon::mv_debug(warning_msg, args$verbose, args$debug, type = "warning", "COLOR")
                        # Remove duplicates from current group
                        args$color[[group_name]] <- setdiff(countries, duplicates)
                    }
                }
            }
        } else if (!args$color %in% c(TRUE, "Subregion", "Region", "Center-Periphery", "Hydrocarbon")) {
            mvcommon::mv_debug("Invalid 'color' parameter. Using default: 'Subregion'", 
                       args$verbose, args$debug, type = "warning")
            args$color <- "Subregion"
        }
    }
    
    # Validate common_var parameter
    if (!is.null(args$common_var)) {
        if (is.character(args$common_var)) {
            if (!args$common_var %in% unique(args$data$Variable)) {
                stop(sprintf("Specified common variable '%s' not found in data", args$common_var))
            }
        } else if (!is.logical(args$common_var)) {
            mvcommon::mv_debug("Invalid 'common_var' parameter. Using default: TRUE", 
                       args$verbose, args$debug, type = "warning")
            args$common_var <- TRUE
        }
    }
    
    # Remove irrelevant series and bar parameters if present
    series_params <- c("right_axis", "key_dates", "area", "by_indicator")
    bar_params <- c("type", "order", "show_values", "show_year", "show_range", "show_median")
    
    irrelevant_params <- c(series_params, bar_params)
    present_irrelevant <- irrelevant_params[irrelevant_params %in% names(args)]
    
    if (length(present_irrelevant) > 0) {
        mvcommon::mv_debug(sprintf(
            "The following parameters are not used in scatter plots and will be ignored: %s",
            paste(present_irrelevant, collapse = ", ")
        ), args$verbose, args$debug, type = "warning")
        args[present_irrelevant] <- NULL
    }
    
    return(args)
}










#' Validate Custom Color Grouping List
#' 
#' @description
#' Internal function that checks and validates custom color group assignments.
#' Ensures no conflicts or invalid assignments in custom groupings.
#' 
#' Used by: .validate_scatter_args
#' Uses: mvcommon::mv_debug
#' 
#' Validation:
#' - Country existence in data
#' - Duplicate country checks
#' - Group name validity
#' - Warning generation
#'
#' @keywords internal
# .validate_color_list <- function(color_list,            # list: custom color grouping list
#                                  data,                    # data.frame: data containing countries
#                                  verbose = TRUE,          # logical: controls information messages
#                                  debug = FALSE) {         # logical: controls debug messages
#
#
#     mvcommon::mv_debug("Starting color list validation", verbose, debug, type = "info", "COLOR")
#     
#     # Get unique countries in data
#     data_countries <- unique(data$Country)
#     
#     # Check each country in the list
#     for (group_name in names(color_list)) {
#         countries <- color_list[[group_name]]
#         if (!is.character(countries)) {
#             stop(sprintf("Color list group '%s' must contain character values", group_name))
#         }
#         
#         # Check for countries not in data
#         invalid_countries <- countries[!countries %in% data_countries]
#         if (length(invalid_countries) > 0) {
#             warning_msg <- sprintf("Countries not found in data for group '%s': %s", 
#                                  group_name, 
#                                  paste(invalid_countries, collapse = ", "))
#             mvcommon::mv_debug(warning_msg, verbose, debug, type = "warning", "COLOR")
#         }
#         
#         # Check for duplicate countries across groups
#         for (other_group in names(color_list)[1:which(names(color_list) == group_name) - 1]) {
#             duplicates <- intersect(countries, color_list[[other_group]])
#             if (length(duplicates) > 0) {
#                 warning_msg <- sprintf("Countries %s already assigned to group '%s', ignoring in '%s'",
#                                      paste(duplicates, collapse = ", "),
#                                      other_group,
#                                      group_name)
#                 mvcommon::mv_debug(warning_msg, verbose, debug, type = "warning", "COLOR")
#                 # Remove duplicates from current group
#                 color_list[[group_name]] <- setdiff(countries, duplicates)
#             }
#         }
#     }
#     
#     return(color_list)
# }