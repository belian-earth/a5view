# Update a5_view layer data without full re-render

Sends new cell data to an existing a5_view widget via a Shiny custom
message, avoiding the full widget teardown/rebuild cycle. Much faster
for interactive updates. Visual options (opacity, basemap, borders) are
left as they are; only the data, colours, legend and tooltip change.

## Usage

``` r
a5_view_update(
  session,
  outputId,
  cells,
  fill = "#74ac90ff",
  fill_identity = FALSE,
  palette = "Viridis",
  tooltip = TRUE,
  aggregate = c("none", "rep_child", "mean"),
  lod_step = 1L
)
```

## Arguments

- session:

  The Shiny session object.

- outputId:

  The output ID of the a5_view widget.

- cells:

  An
  [a5R::a5_cell](https://belian-earth.github.io/a5R/reference/a5_cell.html)
  or
  [a5R::a5_covering](https://belian-earth.github.io/a5R/reference/a5_covering.html)
  vector, or a data frame / tibble containing a column of either. A
  covering is drawn as the cells it stores, at their mixed resolutions;
  uncompact it with
  [`a5R::a5_uncompact()`](https://belian-earth.github.io/a5R/reference/a5_compaction.html)
  first to draw its cells at one resolution. Each row of a covering
  column is repeated once per stored cell. Pyramids (`aggregate` other
  than `"none"`) need cells at one resolution.

- fill:

  Fill colour specification. One of:

  - A single hex colour string (e.g. `"#3388ff"`) for uniform fill.

  - A numeric vector (same length as cells) mapped to `palette`.

  - A character vector of hex colours (same length as cells) for
    per-cell colours.

  - An unquoted column name when `cells` is a data frame.

  - An expression evaluated against the columns of `cells`, in the style
    of
    [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
    This makes the colour helpers
    [`cells_rgb()`](https://belian-earth.github.io/a5view/dev/reference/cells_rgb.md)
    and
    [`cells_pca_rgb()`](https://belian-earth.github.io/a5view/dev/reference/cells_pca_rgb.md)
    usable directly inside the call, e.g.
    `a5_view(df, fill = cells_rgb(r, g, b))` or
    `a5_view(df, fill = cells_pca_rgb(embedding))`. The helpers tag
    their output so `fill_identity` is auto-enabled. Default:
    `"#74ac90ff"`.

- fill_identity:

  Logical. When `TRUE`, treat `fill` values as literal colours rather
  than mapping through `palette`. Accepts packed RGB integers
  (`(R << 16) | (G << 8) | B`) or hex colour strings. Auto-enabled when
  `fill` is produced by
  [`cells_rgb()`](https://belian-earth.github.io/a5view/dev/reference/cells_rgb.md)
  /
  [`cells_pca_rgb()`](https://belian-earth.github.io/a5view/dev/reference/cells_pca_rgb.md)
  or any vector tagged with `attr(x, "a5_identity") <- TRUE`. Default:
  `FALSE`.

- palette:

  Colour palette used when `fill` is numeric. Either a palette name
  accepted by
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html)
  (e.g. `"viridis"`, `"inferno"`, `"plasma"`, `"turbo"`, `"rocket"`) or
  a character vector of hex colours (at least 2). Default: `"Viridis"`.

- tooltip:

  Logical, or a character vector of column names from `cells`. `TRUE`
  (default) shows the hovered cell's id plus its fill value when `fill`
  is a numeric mapping. A character vector adds those columns to the
  tooltip (requires `aggregate = "none"`). `FALSE` disables the tooltip.
  The tooltip only appears over cells present in the data (see the
  section on hover and click below).

- aggregate:

  How parent cells are summarised when zooming out. One of:

  - `"none"` (default): no precomputed pyramid. All cells are shipped as
    Arrow IPC and rendered through deck.gl's `A5Layer`. Cheapest to
    build, suitable for small to medium datasets.

  - `"rep_child"`: each parent inherits the row of the child whose
    payload (RGBA + optional `fill_value`/`elevation`) is closest in
    Euclidean distance to its parent's mean. Preserves real values, no
    blending. Use for embedding/RGB visualisations.

  - `"mean"`: each numeric payload column is averaged independently
    within the parent group. Best for scalar fields; blends colours in
    RGB space, which can mute embedding visualisations. When set to
    `"rep_child"` or `"mean"`, the pyramid is serialised to parquet with
    a spatial row-group index. The browser decodes only the row groups
    intersecting the viewport at the level of detail matching the
    current zoom. Inside Shiny the parquet file is served over HTTP with
    byte-range support, so only the footer and the visible row groups
    are ever transferred; elsewhere it is embedded in the widget as
    base64.

- lod_step:

  Integer \>= 1, gap between successive precomputed LODs (only used when
  `aggregate != "none"`). `1` (default) precomputes every level from the
  data resolution down to the floor (LOD 2); larger values trade size
  for fewer levels and more visible popping at zoom transitions.
