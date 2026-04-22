# pplot — TODO

## Known minor issues

- **Scatter legend count divergence under `waiver()` labels**
  `R/utils-components.r` `.create_legend_panel` — scatter branch: `n_items` is
  computed from `color_scale$breaks`, while the downstream `.get_legend_columns`
  call receives `legend_items_display` (from `color_scale$labels`). If a scale
  ever produces `waiver()`-valued labels, the two counts could diverge
  (the `n_items == 0` guard would fire on one but not the other, or the
  column calculation would see a different length than the guard did).
  No current pplot code path triggers this, but worth harmonizing if/when
  we add more color-scheme variants.
  Fix: unify the scatter branch to derive `n_items` from `legend_items_display`,
  or add a parallel `length(legend_items_display) == 0` check in the guard.

## Future refactors

- **`pp_plot_combine` legend composition (architectural)**
  Today each child plot arrives with its legend baked in as a separate bottom
  panel (extracted during construction by `.create_legend_panel`). Patchwork's
  `guides = "collect"` mechanism only works on ggplot guide-boxes, not on
  flat panel grobs inside a patchwork hierarchy, so the default `collect`
  behavior is effectively a no-op for the typical combine use case.
  Two directions, pick one:
  - *Level 2 (ergonomic win)*: make `pp_plot_combine` detect when all child
    plots have identical legend panels, keep one copy, and stack it at the
    bottom via `theme$legend_heights`. Needs a sensible "identical" test
    that does not produce wrong results when legends differ subtly.
  - *Level 3 (architectural fix)*: restructure `.plot_core` to return plots
    whose legends are still in the native ggplot guide-box form (legend
    extraction only happens when a standalone render is requested, e.g. via
    an `intent = "standalone" | "compose"` flag). Then `pp_plot_combine` +
    patchwork's `guides = "collect"` work natively, eliminating the whole
    class of "legends don't compose" surprises.

- **Build plot gtable once, share between legend extraction and final render**
  Today `.extract_guide_box` (called by `.create_legend_panel`) runs
  `ggplot2::ggplotGrob(plot)` to pluck the legend grob, and `.print_and_save`
  later renders the plot again. Any geom warnings
  (e.g. `Removed N rows containing missing values ...`) therefore emit twice.
  Pre-existing behavior under cowplot; not introduced by the cowplot→ggplotGrob
  migration. Clean fix is to restructure `.plot_core` + `.print_and_save`
  to build the gtable once and reuse it, which eliminates the duplicate work
  and the duplicate warning in one shot.
