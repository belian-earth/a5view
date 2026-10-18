# View A5 cells on an interactive map

Renders A5 cells with deck.gl over a MapLibre basemap. Cells are drawn
as filled pentagons; the viewer provides a basemap switcher, an opacity
slider, a legend for numeric fills, hover tooltips, and an optional
polygon-draw tool.

## Usage

``` r
a5_view(
  cells,
  fill = "#74ac90ff",
  fill_identity = FALSE,
  palette = "Viridis",
  opacity = 0.3,
  tooltip = TRUE,
  elevation = NULL,
  elevation_scale = 1,
  border = "#74ac9080",
  border_width = 1,
  width = NULL,
  height = NULL,
  lng = NULL,
  lat = NULL,
  zoom = NULL,
  globe = FALSE,
  basemap = c("dark", "light", "osm", "satellite"),
  draw_polygon = FALSE,
  aggregate = c("none", "rep_child", "mean"),
  lod_step = 1L
)
```

## Arguments

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

- opacity:

  Numeric scalar, initial layer opacity (0–1). An interactive slider is
  provided in the viewer to adjust at runtime. Default: `0.3`.

- tooltip:

  Logical, or a character vector of column names from `cells`. `TRUE`
  (default) shows the hovered cell's id plus its fill value when `fill`
  is a numeric mapping. A character vector adds those columns to the
  tooltip (requires `aggregate = "none"`). `FALSE` disables the tooltip.
  The tooltip only appears over cells present in the data (see the
  section on hover and click below).

- elevation:

  Column name for 3D extrusion, or `NULL` for flat.

- elevation_scale:

  Numeric scalar, scale factor for elevation.

- border:

  Border (stroke) colour for cell outlines, or `NULL` for no borders. A
  single colour string (e.g. `"#ffffff"`, `"white"`). Default:
  `"#74ac9080"`.

- border_width:

  Numeric scalar, border width in pixels. Default: `1`.

- width, height:

  Widget dimensions. Default: `NULL` (fills container).

- lng, lat, zoom:

  Initial map view. If `NULL` (default), auto-centres on the cell
  centroids.

- globe:

  Logical. Use MapLibre's globe projection instead of the default
  Mercator map. Default: `FALSE`.

- basemap:

  Character vector of basemap styles to make available. Options are
  `"dark"`, `"light"` and `"osm"` (OpenFreeMap vector tiles; no API
  key), `"satellite"` (Esri World Imagery) and `"none"`. The first
  element is shown initially. When more than one is given, an
  interactive selector is shown. Default:
  `c("dark", "light", "osm", "satellite")`.

- draw_polygon:

  Logical. When `TRUE`, adds a "Draw polygon" toggle to the controls
  panel. While draw mode is active, clicks place polygon vertices and a
  double-click closes the polygon. On completion, the polygon is emitted
  to Shiny as a WKT string at `input$<id>_polygon_draw`; the drawn
  outline is held on screen until the user toggles draw mode off,
  presses Escape, or starts a new polygon. While draw mode is on,
  double-click-to-zoom is suppressed and cell-pick click events do not
  fire. The widget itself does not visualise which cells fall inside the
  polygon: resolve the WKT server-side (e.g. with
  [`a5R::a5_polygon_to_cells()`](https://belian-earth.github.io/a5R/reference/a5_polygon_to_cells.html))
  and update the map fills via
  [`a5_view_update()`](https://belian-earth.github.io/a5view/dev/reference/a5_view_update.md).
  The outline is drawn with straight edges on the Web Mercator map, but
  [`a5R::a5_polygon_to_cells()`](https://belian-earth.github.io/a5R/reference/a5_polygon_to_cells.html)
  reads edges as great circles, so along a long edge the selected cells
  can stray from the outline; place more vertices to keep them close.
  Only useful in a Shiny context. Default: `FALSE`.

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

## Value

An htmlwidget.

## Hover, click and Shiny inputs

Hover and click only resolve to cells that are present in the data: the
tooltip, the outline highlight and the Shiny inputs below stay empty
over parts of the map with no cell. With `aggregate = "none"` the
resolved cell is the leaf cell under the cursor. With a pyramid it is
the cell at the level of detail currently on screen, so the id and value
reported are those of the pentagon you can see; zoom in to reach the
leaf cells.

When rendered in Shiny with output id `<id>`, the widget sets:

- `input$<id>_hover`: hex id of the cell under the cursor (or `NULL`).

- `input$<id>_click`: hex id of the clicked cell; clicking it again, or
  clicking empty map, clears it.

- `input$<id>_cursor`, `input$<id>_click_coord`: `list(lng, lat)` of the
  pointer.

- `input$<id>_polygon_draw`: WKT of a completed polygon (see
  `draw_polygon`).
