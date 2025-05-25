

#' Get Display Labels for Categories
#' 
#' @description
#' Internal function that provides display names for category codes.
#' Handles both standard categories and custom group labels.
#' 
#' Used by: .create_color_mapping, .create_scatter_colors
#' Uses: No direct function calls
#' 
#' Label types:
#' - Region names
#' - Subregion names
#' - Economic classifications
#' - Custom group names
#' - Default "Other" category
#'
#' @keywords internal
.get_category_labels <- function(color = NULL) {       # list/NULL: custom color grouping list
    
    # If no color argument or not a list, return category labels
    if (is.null(color) || !is.list(color)) {
        return(category_labels)
    }
    
    # Create group names (Group A, Group B, etc.)
    group_names <- paste0("Group ", LETTERS[1:length(color)])
    
    # Create list labels with descriptive names from the color list
    list_labels <- names(color)
    names(list_labels) <- group_names
    
    # Add list labels to base labels
    category_labels <- c(category_labels, list_labels)
    
    return(category_labels)
}


#' Get Color Palettes for Different Grouping Schemes
#' 
#' @description
#' Internal function that defines and returns color palettes for various grouping schemes.
#' Handles both standard schemes and custom list-based groupings.
#' 
#' Used by: .create_color_mapping, .create_scatter_colors
#' Uses: No direct function calls
#' 
#' Palettes:
#' - Region: 5 colors for major regions
#' - Subregion: 18 colors for detailed regions
#' - Center-Periphery: 3 colors for economic groups
#' - Hydrocarbon: 2 colors for exporter/importer
#' - Custom: Colors for user-defined groups
#'
#' @keywords internal
.get_color_palettes <- function(color = NULL) {        # list/NULL: custom color grouping list

    # Base palettes remain unchanged
    base_palettes <- list(
        Region = c(
            AFRICA = "#4472C4",    # Blue
            AMERICAS = "#70AD47",   # Green
            ASIA = "#C00000",      # Red
            EUROPE = "#FFB347",    # Yellow
            OCEANIA = "#7030A0"    # Purple
        ),
        Subregion = c(
            # Africa
            NORTH_AFRICA = "#00B5D8",     # Cyan
            EAST_AFRICA = "#5B8BE6",      # Lighter Blue
            MIDDLE_AFRICA = "#264478",    # Dark Blue
            WEST_AFRICA = "#4472C4",      # Light Blue
            SOUTH_AFRICA = "#1F3A66",     # Navy Blue
            # Americas
            NORTH_AMERICA = "#70AD47",    # Green
            CARIBBEAN = "#A4D65E",        # Light Green
            CENTRAL_AMERICA = "#2A7031",  # Dark Green
            SOUTH_AMERICA = "#ADEBAD",    # Pale Green
            # Asia
            EAST_ASIA = "#C00000",        # Red
            SOUTHEAST_ASIA = "#FF5A5F",   # Light Red
            SOUTH_ASIA = "#800000",       # Dark Red
            WEST_ASIA = "#FF9999",        # Pink
            CENTRAL_ASIA = "#4B0000",     # Deep Red
            # Europe
            WESTERN_EUROPE = "#C68642",   # Burnished Gold
            EASTERN_EUROPE = "#FFB347",   # Deep Amber
            # Oceania
            AUSTRALIA_NZ = "#7030A0",     # Purple
            OTHER_OCEANIA = "#B083D6"     # Light Purple
        ),
        `Center-Periphery` = c(
            CTR = "#4472C4",    # Blue
            SMP = "#70AD47",    # Green
            PERI = "#C00000"    # Red
        ),
        Hydrocarbon = c(
            HYD_EXP = "#C00000",    # Red for exporters
            HYD_IMP = "#4472C4"     # Blue for importers
        )
    )
    
    # If no color argument or not a list, return base palettes
    if (is.null(color) || !is.list(color)) {
        return(base_palettes)
    }
    
    # Define colors for custom groups
    # Using a subset of existing colors that work well together
    custom_colors <- c(
        "#4472C4",    # Blue
        "#70AD47",    # Green
        "#C00000",    # Red
        "#FFB347",    # Yellow
        "#7030A0",    # Purple
        "#00B5D8",    # Cyan
        "#2A7031",    # Dark Green
        "#FF5A5F",    # Light Red
        "#C68642",    # Burnished Gold
        "#B083D6"     # Light Purple
    )
    
    # Create names for the groups (Group A, Group B, etc.)
    group_names <- paste0("Group ", LETTERS[1:length(color)])
    names(group_names) <- names(color)
    
    # Create the List palette
    list_palette <- custom_colors[1:length(color)]
    names(list_palette) <- group_names
    
    # Add List palette to base palettes
    base_palettes$List <- list_palette
    
    return(base_palettes)
}

#' Determine Color Group for Country
#' 
#' @description
#' Internal function that assigns a color group to a country based on its ISO code.
#' Supports multiple classification schemes (regional, economic, etc.).
#' 
#' Used by: .create_color_mapping, .create_scatter_colors
#' Uses: .get_ctry_categories
#' 
#' Classification schemes:
#' - Region: Major geographical regions
#' - Subregion: Detailed geographical regions
#' - Center-Periphery: Economic classification
#' - Hydrocarbon: Exporter/importer status
#'
#' @keywords internal
.get_color_group <- function(iso_code,                 # string: country ISO code
                              scheme) {                  # string/NULL/FALSE: classification scheme to use

    if (is.null(scheme) || isFALSE(scheme)) {
        return("Default")
    }
    
    categories <- .get_ctry_categories(iso_code)
    
    if (scheme == "Region") {
        regions <- c("AFRICA", "AMERICAS", "ASIA", "EUROPE", "OCEANIA")
        region <- categories[categories %in% regions]
        return(if(length(region) > 0) region[1] else "Other")
    } else if (scheme == "Subregion") {
        subregions <- c(
            "NORTH_AFRICA", "EAST_AFRICA", "MIDDLE_AFRICA", "WEST_AFRICA", "SOUTH_AFRICA",
            "NORTH_AMERICA", "CARIBBEAN", "CENTRAL_AMERICA", "SOUTH_AMERICA",
            "EAST_ASIA", "SOUTHEAST_ASIA", "SOUTH_ASIA", "WEST_ASIA", "CENTRAL_ASIA",
            "WESTERN_EUROPE", "EASTERN_EUROPE",
            "AUSTRALIA_NZ", "OTHER_OCEANIA"
        )
        subregion <- categories[categories %in% subregions]
        return(if(length(subregion) > 0) subregion[1] else "Other")
    } else if (scheme == "Center-Periphery") {
        if ("CTR" %in% categories) return("CTR")
        if ("SMP" %in% categories) return("SMP")
        if ("PERI" %in% categories) return("PERI")
        return("Other")
    } else if (scheme == "Hydrocarbon") {
        if ("HYD_EXP" %in% categories) return("HYD_EXP")
        if ("HYD_IMP" %in% categories) return("HYD_IMP")
        return("Other")
    }
    return("Other")
}

