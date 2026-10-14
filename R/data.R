#' Sentinel-2 cells over central Santiago, Chile
#'
#' A small crop of a cloud-free Sentinel-2 mosaic for 2023, aggregated to
#' A5 resolution-18 cells (about 495 square metres, or 22 m across). It
#' covers lon -70.672 to -70.642, lat -33.480 to -33.455: Parque
#' O'Higgins, the Club Hipódromo Chile racetrack and the surrounding
#' streets, so it shows off true-colour composites and vegetation indices.
#'
#' Values are the mean surface reflectance of the 10 m pixels in each
#' cell, scaled by 10,000 and rounded to integers, as read by
#' `a5px::a5_read_raster(stat = "mean")`; see `data-raw/s2_santiago.R`.
#'
#' @format A data frame with 15,626 rows and 5 columns:
#' \describe{
#'   \item{cell}{A5 cell (`a5_cell`, A5 1.0 id), resolution 18.}
#'   \item{B02, B03, B04}{Blue, green and red reflectance.}
#'   \item{B08}{Near-infrared reflectance.}
#' }
#' @source Earth Genome's Sentinel-2 temporal mosaics on Source
#'   Cooperative, <https://source.coop/earthgenome/sentinel2-temporal-mosaics>,
#'   tile `19HCC_2023-01-01_2024-01-01`. Licensed CC-BY 4.0. Contains
#'   modified Copernicus Sentinel data 2023.
#' @examples
#' a5_view(s2_santiago, fill = cells_rgb(B04, B03, B02, range = c(300, 2800)))
#'
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(s2_santiago, aes(cell = cell, fill = (B08 - B04) / (B08 + B04))) +
#'     geom_a5() +
#'     labs(fill = "NDVI")
#' }
"s2_santiago"
