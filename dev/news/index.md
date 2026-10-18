# Changelog

## a5view (development version)

- [`geom_a5()`](https://belian-earth.github.io/a5view/dev/reference/geom_a5.md)
  no longer warns “`closed` was renamed `closed_ring`” with a5R 1.0.0:
  it relies on
  [`a5_cell_to_boundary_xy()`](https://belian-earth.github.io/a5R/reference/a5_cell_to_boundary_xy.html)’s
  open-ring default instead of naming the argument.
- The example apps use
  [`a5R::a5_threads()`](https://belian-earth.github.io/a5R/reference/a5_threads.html),
  which replaces
  [`a5_set_threads()`](https://belian-earth.github.io/a5R/reference/a5_set_threads.html)
  in a5R 0.9.9.9006.

## a5view 0.3.0

### Breaking changes

- a5view now draws A5 1.0 cell ids. A5 1.0 gives every cell a new id, so
  the versions do not mix: a5view 0.2.x cannot draw 1.0 ids, and 0.3.0
  cannot draw ids written before 1.0. An old id is still a valid 1.0
  cell, but in a different place, and nothing in the id records which
  version wrote it, so a5view cannot detect old ids and draws them in
  the wrong place without an error. Convert stored ids once with
  [`a5R::a5_migrate()`](https://belian-earth.github.io/a5R/reference/a5_migrate.html),
  or re-index from coordinates.
- Requires a5R \>= 0.9.9.9001 (`Remotes: belian-earth/a5R`): A5 1.0, and
  [`a5_cell_to_boundary_xy()`](https://belian-earth.github.io/a5R/reference/a5_cell_to_boundary_xy.html)
  for
  [`geom_a5()`](https://belian-earth.github.io/a5view/dev/reference/geom_a5.md).
- Pyramid LODs now group leaves by supercell
  ([`a5R::a5_cell_to_supercell()`](https://belian-earth.github.io/a5R/reference/a5_spatial_hierarchy.html),
  the coarse cell containing a leaf’s centre) instead of by index
  parent. The A5 1.0 index hierarchy is not spatially nested, so index
  parents would draw aggregates away from their leaves. Pyramids built
  with
  [`a5_build_pyramid()`](https://belian-earth.github.io/a5view/dev/reference/a5_build_pyramid.md)
  by 0.2.x hold pre-1.0 ids and must be rebuilt.
- Pyramid files name the cell id column `cell` (was `pentagon`) and
  record the A5 index version as `a5_index = "1.0"` in their parquet
  metadata.
  [`a5_view_pyramid()`](https://belian-earth.github.io/a5view/dev/reference/a5_view_pyramid.md)
  refuses files without it.

### New features

- [`a5_view()`](https://belian-earth.github.io/a5view/dev/reference/a5_view.md),
  [`a5_view_update()`](https://belian-earth.github.io/a5view/dev/reference/a5_view_update.md),
  [`a5_build_pyramid()`](https://belian-earth.github.io/a5view/dev/reference/a5_build_pyramid.md)
  and
  [`geom_a5()`](https://belian-earth.github.io/a5view/dev/reference/geom_a5.md)
  accept an `a5_covering`, as returned by
  [`a5_grid_disk()`](https://belian-earth.github.io/a5R/reference/a5_grid_disk.html),
  [`a5_spherical_cap()`](https://belian-earth.github.io/a5R/reference/a5_spherical_cap.html)
  and
  [`a5_polygon_to_cells()`](https://belian-earth.github.io/a5R/reference/a5_polygon_to_cells.html)
  in a5R 1.0, alone or as a data frame column. It is drawn as the cells
  it stores, at their mixed resolutions; uncompact it first with
  [`a5R::a5_uncompact()`](https://belian-earth.github.io/a5R/reference/a5_compaction.html)
  to draw its cells at one resolution. Widget tooltips find cells at
  every resolution present. Pyramids still need cells at one resolution
  and say so.

- New
  [`geom_a5()`](https://belian-earth.github.io/a5view/dev/reference/geom_a5.md)
  draws A5 cells in ggplot2 from a cell column:
  `ggplot(df, aes(cell = cell, fill = value)) + geom_a5()`. It works
  with
  [`coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html),
  [`coord_quickmap()`](https://ggplot2.tidyverse.org/reference/coord_map.html)
  and [`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html),
  so it combines with
  [`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  layers in any projection, sharing scales, limits and graticules. Like
  [`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html), it
  brings a default
  [`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html) (crs
  from the plot’s sf layers, else EPSG:4326), so maps keep a fixed
  geographic aspect ratio unless another coord is added. ggplot2 and sf
  are imported.

- [`geom_a5()`](https://belian-earth.github.io/a5view/dev/reference/geom_a5.md)
  is built for large cell sets. One row per cell goes through ggplot2,
  and outlines are only generated at draw time. With `render = "auto"`
  (the default) cells smaller than about three device pixels are drawn
  as a raster: each pixel takes the colour of the cell containing its
  centre, so the cost depends on the image size, not the number of
  cells. `render = "vector"` and `render = "raster"` force a mode. Cells
  crossing the antimeridian are split at +/-180 degrees.

- [`cells_rgb()`](https://belian-earth.github.io/a5view/dev/reference/cells_rgb.md)
  and
  [`cells_pca_rgb()`](https://belian-earth.github.io/a5view/dev/reference/cells_pca_rgb.md)
  return class `a5_rgb`, an integer vector that prints as hex colours.
  Mapped to `fill` in
  [`geom_a5()`](https://belian-earth.github.io/a5view/dev/reference/geom_a5.md),
  it draws as colours with no scale or legend.
  [`a5_view()`](https://belian-earth.github.io/a5view/dev/reference/a5_view.md)
  treats it as before.

- New dataset `s2_santiago`: 15,626 resolution-18 cells of a cloud-free
  2023 Sentinel-2 mosaic over central Santiago, Chile (blue, green, red
  and near-infrared bands, CC-BY 4.0), for examples of true-colour
  composites and colour mapping. `data-raw/s2_santiago.R` rebuilds it.

- The README presents a5view as the visualisation companion to a5R,
  covering ggplot2 and interactive maps, with rendered examples from
  `s2_santiago`.

### Bug fixes

- On the dark and OSM basemaps, cells drew beneath roads, bridges and
  buildings, because those styles place a label layer low in the stack
  and cells were inserted below the first label layer. Cells now go
  below the style’s closing run of label layers only.

### Internals

- Vendored a5-js is 1.0.0 (`inst/htmlwidgets/lib/a5-js/a5.js`). The
  version is set once, in `bridge.js`, which also feeds the CDN fallback
  used by self-contained HTML and the pyramid tile worker.
- deck.gl 9.4 bundles a5-js 0.10 inside its `A5Layer`. Cells and the
  hover highlight now draw through a small subclass
  (`lib/a5view-tiling/a5-layer.js`) that takes boundaries from the
  vendored a5-js 1.0. It can be dropped once deck.gl moves to a5-js 1.0.
- New tests check the vendored a5-js against a5R: `cellToBoundary()` and
  `lonLatToCell()` agree with
  [`a5_cell_to_boundary()`](https://belian-earth.github.io/a5R/reference/a5_cell_to_boundary.html)
  and
  [`a5_lonlat_to_cell()`](https://belian-earth.github.io/a5R/reference/a5_coordinates.html)
  at resolutions 0 to 18.
- [`geom_a5()`](https://belian-earth.github.io/a5view/dev/reference/geom_a5.md)
  takes cell outlines from
  [`a5R::a5_cell_to_boundary_xy()`](https://belian-earth.github.io/a5R/reference/a5_cell_to_boundary_xy.html),
  which returns every cell’s vertices as flat vectors without building a
  geometry per cell.
