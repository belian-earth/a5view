# A5 cells as a ggplot2 layer

Draws A5 cells from a cell column, with any ggplot2 fill scale. Works
with
[`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html),
[`coord_quickmap()`](https://ggplot2.tidyverse.org/reference/coord_map.html)
and [`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html),
and combines with
[`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html) layers
in the same plot.

## Usage

``` r
geom_a5(
  mapping = NULL,
  data = NULL,
  ...,
  render = c("auto", "vector", "raster"),
  raster_dpi = 300,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Aesthetic mappings. `cell` is required: an `a5_cell` vector or hex
  strings. `fill`, `alpha`, `colour`, `linewidth` and `linetype` are
  understood.

- data, show.legend, inherit.aes, na.rm, ...:

  As for other ggplot2 layers.

- render:

  `"auto"` (default) draws polygons, switching to a raster when cells
  are smaller than a few device pixels. `"vector"` and `"raster"` force
  one mode. Cell borders (`colour`) need vector mode, so `"auto"` stays
  vector when they are set.

- raster_dpi:

  Pixel density for raster mode on vector devices (PDF, SVG), which have
  no pixels of their own. Bitmap devices use their own resolution.

## Value

A list of a ggplot2 layer and a default
[`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html), as
[`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
returns.

## Details

Like [`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html),
the layer comes with a default
[`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html): its
crs is taken from the plot's sf layers, or is EPSG:4326 when there are
none, so the map keeps a fixed geographic aspect ratio. Adding any coord
replaces it.

Large cell sets are drawn as a raster: each device pixel takes the
colour of the cell containing its centre. This is exact at the output
resolution and its cost depends on the number of pixels, not cells.
