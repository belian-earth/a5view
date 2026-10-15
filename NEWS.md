# a5view 0.3.0

## Breaking changes

* a5view now draws A5 1.0 cell ids. A5 1.0 gives every cell a new id, so
  the versions do not mix: a5view 0.2.x cannot draw 1.0 ids, and 0.3.0
  cannot draw ids written before 1.0. An old id is still a valid 1.0
  cell, but in a different place, and nothing in the id records which
  version wrote it, so a5view cannot detect old ids and draws them in
  the wrong place without an error. Convert stored ids once with
  `a5R::a5_migrate()`, or re-index from coordinates.
* Requires a5R >= 0.9.9.9001 (`Remotes: belian-earth/a5R`): A5 1.0,
  and `a5_cell_to_boundary_xy()` for `geom_a5()`.
* Pyramid LODs now group leaves by supercell (`a5R::a5_cell_to_supercell()`,
  the coarse cell containing a leaf's centre) instead of by index parent.
  The A5 1.0 index hierarchy is not spatially nested, so index parents
  would draw aggregates away from their leaves. Pyramids built with
  `a5_build_pyramid()` by 0.2.x hold pre-1.0 ids and must be rebuilt.
* Pyramid files name the cell id column `cell` (was `pentagon`) and
  record the A5 index version as `a5_index = "1.0"` in their parquet
  metadata. `a5_view_pyramid()` refuses files without it.
* `a5_view()` and `a5_view_update()` refuse an `a5_covering`, as
  returned by `a5_grid_disk()`, `a5_spherical_cap()` and
  `a5_polygon_to_cells()` in a5R 1.0, and point to `a5R::a5_uncompact()`.

## New features

* New `geom_a5()` draws A5 cells in ggplot2 from a cell column:
  `ggplot(df, aes(cell = cell, fill = value)) + geom_a5()`. It works with
  `coord_cartesian()`, `coord_quickmap()` and `coord_sf()`, so it combines
  with `geom_sf()` layers in any projection, sharing scales, limits and
  graticules. Like `geom_sf()`, it brings a default `coord_sf()` (crs
  from the plot's sf layers, else EPSG:4326), so maps keep a fixed
  geographic aspect ratio unless another coord is added. ggplot2 and sf
  are imported.
* `geom_a5()` is built for large cell sets. One row per cell goes through
  ggplot2, and outlines are only generated at draw time. With
  `render = "auto"` (the default) cells smaller than about three device
  pixels are drawn as a raster: each pixel takes the colour of the cell
  containing its centre, so the cost depends on the image size, not the
  number of cells. `render = "vector"` and `render = "raster"` force a
  mode. Cells crossing the antimeridian are split at +/-180 degrees.
* `cells_rgb()` and `cells_pca_rgb()` return class `a5_rgb`, an integer
  vector that prints as hex colours. Mapped to `fill` in `geom_a5()`,
  it draws as colours with no scale or legend. `a5_view()` treats it as
  before.

* New dataset `s2_santiago`: 15,626 resolution-18 cells of a
  cloud-free 2023 Sentinel-2 mosaic over central Santiago, Chile (blue,
  green, red and near-infrared bands, CC-BY 4.0), for examples of
  true-colour composites and colour mapping. `data-raw/s2_santiago.R`
  rebuilds it.
* The README presents a5view as the visualisation companion to a5R,
  covering ggplot2 and interactive maps, with rendered examples from
  `s2_santiago`.

## Bug fixes

* On the dark and OSM basemaps, cells drew beneath roads, bridges and
  buildings, because those styles place a label layer low in the stack
  and cells were inserted below the first label layer. Cells now go
  below the style's closing run of label layers only.

## Internals

* Vendored a5-js is 1.0.0-beta.3 (`inst/htmlwidgets/lib/a5-js/a5.js`). The
  version is set once, in `bridge.js`, which also feeds the CDN fallback
  used by self-contained HTML and the pyramid tile worker.
* deck.gl 9.4 bundles a5-js 0.10 inside its `A5Layer`. Cells and the
  hover highlight now draw through a small subclass
  (`lib/a5view-tiling/a5-layer.js`) that takes boundaries from the vendored
  a5-js 1.0. It can be dropped once deck.gl moves to a5-js 1.0.
* New tests check the vendored a5-js against a5R: `cellToBoundary()` and
  `lonLatToCell()` agree with `a5_cell_to_boundary()` and
  `a5_lonlat_to_cell()` at resolutions 0 to 18.
* `geom_a5()` takes cell outlines from `a5R::a5_cell_to_boundary_xy()`,
  which returns every cell's vertices as flat vectors without building a
  geometry per cell.
