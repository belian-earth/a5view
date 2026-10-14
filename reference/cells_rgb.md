# Build per-cell packed-integer RGB colours from three numeric channels

Convenience helper for the common workflow of rescaling three numeric
vectors (e.g. spectral bands) to `0:255` and combining them into the
packed `(R << 16) | (G << 8) | B` integer form expected by
[`a5_view()`](https://belian-earth.github.io/a5view/reference/a5_view.md)
with `fill_identity = TRUE`.

## Usage

``` r
cells_rgb(r, g, b, range = NULL)
```

## Arguments

- r, g, b:

  Numeric vectors of identical length supplying the red, green and blue
  channels.

- range:

  Optional length-2 numeric `c(min, max)` shared across all three
  channels. `NULL` (default) rescales each channel independently.

## Value

Packed RGB values (class `a5_rgb`, an integer vector), the same length
as the inputs. They print as hex colours. In ggplot2, mapping them to
`fill` in
[`geom_a5()`](https://belian-earth.github.io/a5view/reference/geom_a5.md)
draws them as colours with no scale or legend.

## Details

Each channel is rescaled to `0:255` independently using its own min/max,
unless `range` is supplied, in which case the same bounds are applied to
all three channels (useful when the bands are radiometrically
comparable). Values outside `range` are clipped. `NA` inputs propagate
to `NA` in the output. Constant channels (or channels that are entirely
`NA` under per-channel scaling) contribute zero.

## Examples

``` r
n <- 5
df <- data.frame(b1 = runif(n), b2 = runif(n), b3 = runif(n))
df$rgb <- cells_rgb(df$b1, df$b2, df$b3)
```
