# Build and save an A5 pyramid parquet file

Performs the same data preparation, fill resolution, and LOD-pyramid
construction as
[`a5_view()`](https://belian-earth.github.io/a5view/reference/a5_view.md)
with `aggregate != "none"`, then writes the result to a parquet file.
The file can be replayed by
[`a5_view_pyramid()`](https://belian-earth.github.io/a5view/reference/a5_view_pyramid.md)
without rebuilding, which is useful for large datasets where the pyramid
takes time to compute.

## Usage

``` r
a5_build_pyramid(
  cells,
  path,
  fill = "#74ac90ff",
  fill_identity = FALSE,
  palette = "Viridis",
  elevation = NULL,
  aggregate = c("rep_child", "mean"),
  lod_step = 1L,
  lng = NULL,
  lat = NULL,
  zoom = NULL
)
```

## Arguments

- cells:

  An
  [a5R::a5_cell](https://belian-earth.github.io/a5R/reference/a5_cell.html)
  vector, or a data frame / tibble containing an `a5_cell` column.

- path:

  Destination path for the parquet file. Required.

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
    [`cells_rgb()`](https://belian-earth.github.io/a5view/reference/cells_rgb.md)
    and
    [`cells_pca_rgb()`](https://belian-earth.github.io/a5view/reference/cells_pca_rgb.md)
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
  [`cells_rgb()`](https://belian-earth.github.io/a5view/reference/cells_rgb.md)
  /
  [`cells_pca_rgb()`](https://belian-earth.github.io/a5view/reference/cells_pca_rgb.md)
  or any vector tagged with `attr(x, "a5_identity") <- TRUE`. Default:
  `FALSE`.

- palette:

  Colour palette used when `fill` is numeric. Either a palette name
  accepted by
  [`grDevices::hcl.colors()`](https://rdrr.io/r/grDevices/palettes.html)
  (e.g. `"viridis"`, `"inferno"`, `"plasma"`, `"turbo"`, `"rocket"`) or
  a character vector of hex colours (at least 2). Default: `"Viridis"`.

- elevation:

  Column name for 3D extrusion, or `NULL` for flat.

- aggregate:

  Aggregation strategy. Same as
  [`a5_view()`](https://belian-earth.github.io/a5view/reference/a5_view.md)
  but `"none"` is rejected here – pyramids only make sense for
  aggregated data. Default: `"rep_child"`.

- lod_step:

  Integer \>= 1, gap between successive precomputed LODs (only used when
  `aggregate != "none"`). `1` (default) precomputes every level from the
  data resolution down to the floor (LOD 2); larger values trade size
  for fewer levels and more visible popping at zoom transitions.

- lng, lat, zoom:

  Initial map view. If `NULL` (default), auto-centres on the cell
  centroids.

## Value

`path`, invisibly.

## Details

Colours are baked into the parquet (as `_fill_r/g/b/a` columns when
`fill` produces per-cell colours, or as a single uniform colour stored
in the file's KV metadata otherwise). One file therefore encodes one
colour scheme; swapping palettes requires rebuilding.
