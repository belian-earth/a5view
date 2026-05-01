
<!-- README.md is generated from README.Rmd. Please edit that file -->

# a5view

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/belian-earth/a5view/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/belian-earth/a5view/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Interactive map viewer for [A5](https://a5geo.org) geospatial cells,
powered by [deck.gl](https://deck.gl). Renders cells directly in the
browser using deck.gl’s native A5Layer — no pre-computation of
boundaries needed. Intentionally minimal API for fast prototyping and
exploration of A5 data.

## Installation

``` r
# install.packages("pak")
pak::pak("belian-earth/a5view")
```

## Usage

``` r
library(a5R)
library(a5view)

# Pick a cell and expand to a disk
cell <- a5_lonlat_to_cell(-3.19, 55.95, resolution = 9)
disk <- a5_grid_disk(cell, k = 10) |>
  a5_uncompact(resolution = 9)

# Colour by distance from centre
a5_view(disk, fill = a5_cell_distance(cell, disk), palette = "Inferno")
```

<img src="man/figures/README-example-1.png" alt="" width="100%" />

Features:

- Colour mapping from numeric vectors or data frame columns with any
  `hcl.colors()` palette
- Interactive basemap selector (dark, light, OSM, satellite)
- Opacity slider
- Tooltip with cell ID on click, fill value on hover
- Cell border styling
- 3D extrusion via `elevation`
- Shiny bindings (`a5_viewOutput` / `renderA5_view`)
- Optional polygon-draw toolbar (`draw_polygon = TRUE`) for selecting
  an area of interest

## Polygon draw mode (Shiny only)

Setting `draw_polygon = TRUE` adds a draw-polygon toggle to the
toolbar. Clicks place vertices, a double-click closes the polygon,
and the result is sent to Shiny as a WKT string at
`input$<id>_polygon_draw`. The widget itself does not visualise which
cells fall inside the polygon — the canonical pattern is to resolve
the WKT server-side and update the map fills via `a5_view_update()`.
Outside Shiny there is no input listener, so the feature has no
effect.

``` r
a5_viewOutput("map")

# server
output$map <- renderA5_view({
  a5_view(my_cells, draw_polygon = TRUE)
})

observeEvent(input$map_polygon_draw, {
  cells_in_poly <- a5R::a5_grid(wk::wkt(input$map_polygon_draw), resolution = 4L)
  # ... use cells_in_poly, e.g. a5_view_update() with selection-driven fills.
})
```

See `inst/examples/polygon-select/app.R` for a complete demo combining
single-cell click selection with polygon-based selection.
