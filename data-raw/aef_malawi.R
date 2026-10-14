# aef_malawi: a small crop of the AlphaEarth Foundations (AEF) annual
# satellite embedding over Malawi, aggregated to A5 resolution-15 cells.
#
# Source: the 2021 AEF tile used in the a5px README (UTM zone 36S),
# https://source.coop/tge-labs/aef, CC-BY 4.0. "The AlphaEarth
# Foundations Satellite Embedding dataset is produced by Google and
# Google DeepMind."
#
# Needs network access, a5px and a5R >= 0.9.9.9000 (A5 1.0 cell ids).

library(a5R)

url <- paste0(
  "https://data.source.coop/tge-labs/aef/v1/annual/2021/36S/",
  "xekh5rjs4wg6wb9b4-0000000000-0000000000.tiff"
)

aef <- a5px::a5_read_raster(url, resolution = 15L, bands = 11:13, stat = "mean")

# Crop to a box spanning the colour boundary in the west of the tile,
# by cell centre.
xy <- unclass(a5_cell_to_lonlat(aef$cell))
keep <- xy$x >= 33.20 & xy$x <= 33.45 & xy$y >= -13.05 & xy$y <= -12.80
aef_malawi <- aef[keep, c("cell", "A10", "A11", "A12")]
aef_malawi <- aef_malawi[order(format(aef_malawi$cell)), ]
rownames(aef_malawi) <- NULL

usethis::use_data(aef_malawi, overwrite = TRUE, compress = "xz")
