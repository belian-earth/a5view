# Package index

## Static plots

Draw cells in ggplot2, alongside
[`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html) layers
and in any projection.

- [`geom_a5()`](https://belian-earth.github.io/a5view/dev/reference/geom_a5.md)
  : A5 cells as a ggplot2 layer

## Interactive maps

Draw cells on a web map with deck.gl and MapLibre.

- [`a5_view()`](https://belian-earth.github.io/a5view/dev/reference/a5_view.md)
  : View A5 cells on an interactive map

## Colour composites

Map several bands or an embedding to per-cell RGB colours.

- [`cells_rgb()`](https://belian-earth.github.io/a5view/dev/reference/cells_rgb.md)
  : Build per-cell packed-integer RGB colours from three numeric
  channels
- [`cells_pca_rgb()`](https://belian-earth.github.io/a5view/dev/reference/cells_pca_rgb.md)
  : Build per-cell RGB colours from the first three principal components
  of an embedding

## Large datasets

Build level-of-detail pyramids and view them without rebuilding.

- [`a5_build_pyramid()`](https://belian-earth.github.io/a5view/dev/reference/a5_build_pyramid.md)
  : Build and save an A5 pyramid parquet file
- [`a5_view_pyramid()`](https://belian-earth.github.io/a5view/dev/reference/a5_view_pyramid.md)
  : View a prebuilt A5 pyramid parquet file

## Shiny

Place maps in apps and update them in place.

- [`a5_viewOutput()`](https://belian-earth.github.io/a5view/dev/reference/a5_viewOutput.md)
  : Shiny output binding for a5_view
- [`renderA5_view()`](https://belian-earth.github.io/a5view/dev/reference/renderA5_view.md)
  : Shiny render function for a5_view
- [`a5_view_update()`](https://belian-earth.github.io/a5view/dev/reference/a5_view_update.md)
  : Update a5_view layer data without full re-render

## Data

Example data.

- [`s2_santiago`](https://belian-earth.github.io/a5view/dev/reference/s2_santiago.md)
  : Sentinel-2 cells over central Santiago, Chile
