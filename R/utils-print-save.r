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