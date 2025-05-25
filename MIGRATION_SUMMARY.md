# Migration Summary

## Files Created/Updated

### Core Plotting Functions
- `plot-core.r`: Contains `.plot_core`, `.get_plot_function`, `.get_plot_dimensions`, `.filter_args`
- `plot-series.r`: Contains all series plotting functions (`.plot_multi_indic_one_ctry`, `.plot_one_indic_multi_ctry`, `.plot_multi_by_indic`, `.plot_multi_by_ctry`, `.plot_multi_area`, `.plot_one_ctry_area`, `.plot_one_indic_multi_ctry_two_axes`, `.plot_multi_indic_one_ctry_two_axes`)
- `plot-bar.r`: Contains bar plotting functions (`.plot_one_indic_bar`, `.plot_multi_indic_bar`)
- `plot-scatter.r`: Contains scatter plotting functions (`.plot_scatter_two_vars`, `.plot_scatter_multi_vars`)

### Theme and Styling
- `theme.r`: Contains `.theme_plot` function
- `scales.r`: Contains `.scale_x` and `.scale_y` functions
- `colors.r`: Contains `custom_colors` palette

### Utilities
- `utils-print-save.r`: Contains `.print_and_save` function
- `utils-core.r`: Updated to include `.print_debug` and `.create_color` functions

### Previously Migrated
- `pp_plot_series.r`: Main series plotting function (already migrated)
- `pp_plot_bar.r`: Main bar plotting function (already migrated)
- `pp_plot_scatter.r`: Main scatter plotting function (already migrated)
- `pp_plot_combine.r`: Plot combination function (already migrated)
- `utils-components.r`: Legend, reference, and title panel functions (already migrated)
- `utils-bar-scatter.r`: Bar and scatter plot utilities (already migrated)
- `utils-validation.r`: Validation functions (already migrated)

## Function Renaming

All internal functions have been renamed from `in_*` to `.*` pattern:
- `in_plot_core` → `.plot_core`
- `in_plot_multi_indic_one_ctry` → `.plot_multi_indic_one_ctry`
- `in_plot_one_indic_multi_ctry` → `.plot_one_indic_multi_ctry`
- `in_plot_multi_by_indic` → `.plot_multi_by_indic`
- `in_plot_multi_by_ctry` → `.plot_multi_by_ctry`
- `in_plot_multi_area` → `.plot_multi_area`
- `in_plot_one_ctry_area` → `.plot_one_ctry_area`
- `in_plot_one_indic_bar` → `.plot_one_indic_bar`
- `in_plot_multi_indic_bar` → `.plot_multi_indic_bar`
- `in_plot_scatter_two_vars` → `.plot_scatter_two_vars`
- `in_plot_scatter_multi_vars` → `.plot_scatter_multi_vars`
- `in_theme_plot` → `.theme_plot`
- `in_scale_x` → `.scale_x`
- `in_scale_y` → `.scale_y`
- `in_print_and_save` → `.print_and_save`
- `in_print_debug` → `.print_debug`
- `in_create_color` → `.create_color`

## Cross-References Updated

All function calls within the migrated code have been updated to use the new `.` prefix naming convention.

## Notes

- The `custom_colors` object is now in its own file (`colors.r`)
- All internal functions use the `.` prefix convention
- Public functions maintain the `pp_` prefix
- Documentation has been updated to reflect the new package name and function references