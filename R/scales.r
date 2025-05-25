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