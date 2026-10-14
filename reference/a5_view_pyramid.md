# View a prebuilt A5 pyramid parquet file

Reads a parquet file produced by
[`a5_build_pyramid()`](https://belian-earth.github.io/a5view/reference/a5_build_pyramid.md)
and renders it through the same lazy row-group decoder as
[`a5_view()`](https://belian-earth.github.io/a5view/reference/a5_view.md)
does for aggregated data. No data preparation or LOD construction
happens at view time. Inside Shiny the file is served to the browser
over HTTP with byte-range support, so only the footer and the row groups
in view are transferred, however large the file; outside Shiny the bytes
are embedded in the widget as base64.

## Usage

``` r
a5_view_pyramid(
  path,
  opacity = 0.3,
  tooltip = TRUE,
  border = NULL,
  border_width = 1,
  width = NULL,
  height = NULL,
  lng = NULL,
  lat = NULL,
  zoom = NULL,
  globe = FALSE,
  basemap = c("dark", "light", "osm", "satellite"),
  draw_polygon = FALSE
)
```

## Arguments

- path:

  Path to a parquet file produced by
  [`a5_build_pyramid()`](https://belian-earth.github.io/a5view/reference/a5_build_pyramid.md).

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
  [`a5_view_update()`](https://belian-earth.github.io/a5view/reference/a5_view_update.md).
  Only useful in a Shiny context. Default: `FALSE`.

## Value

An htmlwidget.

## Details

Colours, the data resolution, and the auto-centred view are all baked
into the file at build time. Visualisation-only options (opacity,
basemap, globe, border width, drawing tools, ...) remain arguments here.
