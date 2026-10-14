# s2_santiago: a small crop of a Sentinel-2 annual cloud-free mosaic over
# central Santiago, Chile, aggregated to A5 resolution-18 cells.
#
# Source: Earth Genome's Sentinel-2 temporal mosaics, tile 19HCC for 2023,
# https://source.coop/earthgenome/sentinel2-temporal-mosaics, CC-BY 4.0.
# Contains modified Copernicus Sentinel data 2023.
#
# Needs network access, a5px and a5R >= 0.9.9.9000 (A5 1.0 cell ids).

library(a5R)

base <- paste0(
  "https://data.source.coop/earthgenome/sentinel2-temporal-mosaics/",
  "19HCC_2023-01-01_2024-01-01/"
)
bands <- c("B02", "B03", "B04", "B08")

# One COG per band. Read a box slightly larger than the crop so cells on
# its edge get all their pixels.
bbox <- c(-70.675, -33.483, -70.639, -33.452)
parts <- lapply(bands, function(b) {
  d <- a5px::a5_read_raster(
    paste0(base, b, ".tif"), resolution = 18L, bbox = bbox, stat = "mean"
  )
  names(d)[2] <- b
  d
})
s2 <- Reduce(function(x, y) merge(x, y, by = "cell"), parts)

# Crop to about 2.8 km square around Parque O'Higgins, by cell centre.
xy <- unclass(a5_cell_to_lonlat(s2$cell))
keep <- xy$x >= -70.672 & xy$x <= -70.642 & xy$y >= -33.480 & xy$y <= -33.455
s2_santiago <- s2[keep, c("cell", bands)]
s2_santiago[bands] <- lapply(s2_santiago[bands], function(v) as.integer(round(v)))
s2_santiago <- s2_santiago[order(format(s2_santiago$cell)), ]
rownames(s2_santiago) <- NULL

usethis::use_data(s2_santiago, overwrite = TRUE, compress = "xz")
