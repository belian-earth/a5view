# Build per-cell RGB colours from the first three principal components of an embedding

Convenience helper for the common embedding-visualisation workflow:
stack a list-column (or matrix) of equal-length numeric vectors, run a
3-component PCA, percentile-clip the scores per channel to tame
outliers, and pack into integer RGB suitable for
[`a5_view()`](https://belian-earth.github.io/a5view/dev/reference/a5_view.md)
with `fill_identity = TRUE`. Output is handed off to
[`cells_rgb()`](https://belian-earth.github.io/a5view/dev/reference/cells_rgb.md).

## Usage

``` r
cells_pca_rgb(x, clip = c(0.02, 0.98), scale = TRUE, rgb_pcs = c(1, 2, 3))
```

## Arguments

- x:

  A list of numeric vectors of identical length (e.g. an arrow
  list-column of embeddings) or a numeric matrix with rows = cells and
  columns = features. Must not contain `NA`.

- clip:

  Length-2 numeric percentile range applied per principal component to
  clip outliers before colour mapping. Default `c(0.02, 0.98)`. Set to
  `NULL` to skip clipping.

- scale:

  Passed to `irlba::prcomp_irlba(..., scale. = scale)`. Standardises
  features to unit variance before PCA. Default `TRUE`.

- rgb_pcs:

  Length-3 signed integer vector specifying which PC maps to each colour
  channel, in red/green/blue order. The default `c(1, 2, 3)` assigns PC1
  to red, PC2 to green and PC3 to blue. Negative values flip the sign of
  the corresponding PC, useful because PCA components are arbitrary up
  to sign — e.g. `c(1, -2, 3)` uses `-PC2` for green. Must be a signed
  permutation of `1:3`.

## Value

Packed RGB values (class `a5_rgb`, as from
[`cells_rgb()`](https://belian-earth.github.io/a5view/dev/reference/cells_rgb.md)),
the same length as the number of rows in `x`.

## Details

Requires the `irlba` package, which is much faster than a full SVD when
only the top three components are needed.

## Examples

``` r
if (FALSE) { # \dontrun{
df$rgb <- cells_pca_rgb(df$embedding)
a5_view(df, fill = rgb, fill_identity = TRUE)

# Reorder and flip a channel for better contrast:
df$rgb <- cells_pca_rgb(df$embedding, rgb_pcs = c(3, -1, 2))
} # }
```
