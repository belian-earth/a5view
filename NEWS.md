# a5view 0.3.0

## Breaking changes

* a5view now draws A5 1.0 cell ids. A5 1.0 gives every cell a new id, so
  the versions do not mix: a5view 0.2.x cannot draw 1.0 ids, and 0.3.0
  cannot draw ids written before 1.0. An old id is still a valid 1.0
  cell, but in a different place, and nothing in the id records which
  version wrote it, so a5view cannot detect old ids and draws them in
  the wrong place without an error. Convert stored ids once with
  `a5R::a5_migrate()`, or re-index from coordinates.
* Requires a5R >= 0.9.9.9000 (`Remotes: belian-earth/a5R`), the first
  a5R on A5 1.0.
* Pyramid LODs now group leaves by supercell (`a5R::a5_cell_to_supercell()`,
  the coarse cell containing a leaf's centre) instead of by index parent.
  The A5 1.0 index hierarchy is not spatially nested, so index parents
  would draw aggregates away from their leaves. Pyramids built with
  `a5_build_pyramid()` by 0.2.x hold pre-1.0 ids and must be rebuilt.
* `a5_view()` and `a5_view_update()` refuse an `a5_covering`, as
  returned by `a5_grid_disk()`, `a5_spherical_cap()` and
  `a5_polygon_to_cells()` in a5R 1.0, and point to `a5R::a5_uncompact()`.

## Internals

* Vendored a5-js is 1.0.0-beta.2 (`inst/htmlwidgets/lib/a5-js/a5.js`). The
  version is set once, in `bridge.js`, which also feeds the CDN fallback
  used by self-contained HTML and the pyramid tile worker.
* deck.gl 9.4 bundles a5-js 0.10 inside its `A5Layer`. Cells and the
  hover highlight now draw through a small subclass
  (`lib/a5view-tiling/a5-layer.js`) that takes boundaries from the vendored
  a5-js 1.0. It can be dropped once deck.gl moves to a5-js 1.0.
* New tests check the vendored a5-js against a5R: `cellToBoundary()` and
  `lonLatToCell()` agree with `a5_cell_to_boundary()` and
  `a5_lonlat_to_cell()` at resolutions 0 to 18.
