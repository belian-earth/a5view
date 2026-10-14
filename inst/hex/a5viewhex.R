# make a hex logo: the Richat Structure (Mauritania) in AlphaEarth
# Foundations embeddings, drawn with geom_a5()
library(devtools)
library(ggplot2)
load_all()

# ----- read embeddings -----

# A5 resolution 13 (about 700 m cells) over a 44 km box around the
# structure. The box spans two 2024 AEF tiles, found in the index at
# https://data.source.coop/tge-labs/aef/v1/annual/aef_index.parquet.
# AEF: CC-BY 4.0, produced by Google and Google DeepMind.

centre <- c(-11.40, 21.12)
half <- 0.2
hx <- half / cos(centre[2] * pi / 180)
bbox <- c(centre[1] - hx, centre[2] - half, centre[1] + hx, centre[2] + half)

tiles <- paste0(
  "https://data.source.coop/tge-labs/aef/v1/annual/2024/29N/x5svq5kzrn7wigvsu-",
  c("0000000000-0000000000", "0000000000-0000008192"),
  ".tiff"
)

aef <- lapply(tiles, function(u) {
  a5px::a5_read_raster(
    u,
    resolution = 13L,
    bbox = bbox,
    stat = c("mean", "npix"),
    cpu_workers = 4L,
    io_concurrency = 8L
  )
})
aef <- do.call(rbind, aef)

# Cells on the tile edge appear once per tile: combine them, weighted by
# pixel count.
bands <- grep("^A[0-9]{2}$", names(aef), value = TRUE)
key <- format(aef$cell)
emb <- rowsum(as.matrix(aef[bands]) * aef$npix, key) /
  as.vector(rowsum(aef$npix, key))
cells <- a5R::as_a5_cell(rownames(emb))

xy <- unclass(a5R::a5_cell_to_lonlat(cells))
keep <- xy$x >= bbox[1] & xy$x <= bbox[3] &
  xy$y >= bbox[2] & xy$y <= bbox[4] &
  stats::complete.cases(emb)
cells <- cells[keep]
emb <- unname(emb[keep, ])

# ----- colours -----

# PCA of the 64 axes to RGB, then slightly desaturated and darkened so
# the label stays legible. irlba starts from a random vector, which can
# flip a component's sign: the seed fixes the colours.
set.seed(1)
hsv_mat <- rgb2hsv(col2rgb(format(cells_pca_rgb(emb))))
richat <- data.frame(
  cell = cells,
  rgb = hsv(hsv_mat[1, ], pmin(hsv_mat[2, ] * 0.9, 1), hsv_mat[3, ] * 0.8)
)

# ----- make plot -----

ggp <- ggplot(richat, aes(cell = cell, fill = rgb)) +
  scale_fill_identity() +
  geom_a5(colour = "#74ac9071", linewidth = 0.15, render = "vector") +
  annotate(
    "text",
    x = 0,
    y = 0,
    label = "a5view",
    size = 20,
    colour = "#ffffffff",
    family = "Manjari Thin"
  ) +
  coord_sf(
    crs = sprintf("+proj=ortho +lon_0=%f +lat_0=%f", centre[1], centre[2]),
    default_crs = NULL,
    expand = FALSE,
    xlim = c(-21000, 21000),
    ylim = c(-21000, 21000),
    label_axes = "----"
  ) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black", colour = NA))

tf <- fs::file_temp(ext = "png")
ggsave(tf, ggp, width = 100, height = 100, units = "mm", dpi = 300)

t2 <- fs::file_temp(ext = "png")
cropcircles::crop_hex(
  tf,
  to = t2,
  border_size = 10,
  border_colour = "#74ac90ff",
  bg_fill = "#000000ff"
)

usethis::use_logo(t2)
