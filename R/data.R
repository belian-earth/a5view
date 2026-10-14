#' Satellite embedding cells over central Malawi
#'
#' A small crop of the AlphaEarth Foundations (AEF) annual satellite
#' embedding for 2021, aggregated to A5 resolution-15 cells (about
#' 3.2 ha each). It covers lon 33.20 to 33.45, lat -13.05 to -12.80,
#' where the embedding changes sharply across a landscape boundary, so it
#' shows off colour mapping and false-colour composites.
#'
#' Three of the 64 embedding axes are kept. Values are the mean of the
#' raw signed 8-bit pixel values in each cell (not de-quantized), as
#' read by `a5px::a5_read_raster(stat = "mean")`; see
#' `data-raw/aef_malawi.R`.
#'
#' @format A data frame with 23,691 rows and 4 columns:
#' \describe{
#'   \item{cell}{A5 cell (`a5_cell`, A5 1.0 id), resolution 15.}
#'   \item{A10, A11, A12}{Mean embedding values for axes A10 to A12.}
#' }
#' @source The AEF Satellite Embedding COGs on Source Cooperative,
#'   <https://source.coop/tge-labs/aef>, tile
#'   `v1/annual/2021/36S/xekh5rjs4wg6wb9b4-0000000000-0000000000.tiff`.
#'   Licensed CC-BY 4.0: "The AlphaEarth Foundations Satellite Embedding
#'   dataset is produced by Google and Google DeepMind."
#' @examples
#' a5_view(aef_malawi, fill = cells_rgb(A10, A11, A12))
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(aef_malawi, aes(cell = cell, fill = cells_rgb(A10, A11, A12))) +
#'     geom_a5()
#' }
"aef_malawi"
