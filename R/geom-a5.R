# ggplot2 layer for A5 cells
#
# Design (performance first):
#   - The stat keeps one row per cell. It adds only a bounding box per
#     cell (xmin..ymax) so scales and coord_sf limits train correctly;
#     no per-vertex data frame ever goes through ggplot2's pipeline.
#   - The geom returns a single "a5_grob" whose makeContent() runs at
#     draw time, when the panel's size in device pixels is known. It
#     draws either
#       * vector: one polygonGrob(id.lengths =) over all cells, or
#       * raster: for each device pixel, the cell containing its centre
#         (a5_lonlat_to_cell() + match()), drawn as one rasterGrob. Cost
#         scales with pixels, not cells.
#     render = "auto" picks raster once the average cell edge is under
#     A5_RASTER_EDGE_PX device pixels, where polygons would only smear.
#   - coord_sf: cells are lon/lat (EPSG:4326). The stat reports extents
#     like StatSf does, and drawing projects lon/lat to the coord's crs,
#     so geom_a5 and geom_sf layers share scales, limits and graticules.
#
# ggplot2 is in Suggests, so the ggproto objects are built on first use.

A5_RASTER_EDGE_PX <- 3

#' A5 cells as a ggplot2 layer
#'
#' Draws A5 cells from a cell column, with any ggplot2 fill scale. Works
#' with `coord_cartesian()`, `coord_quickmap()` and `coord_sf()`, and
#' combines with `geom_sf()` layers in the same plot.
#'
#' Large cell sets are drawn as a raster: each device pixel takes the
#' colour of the cell containing its centre. This is exact at the output
#' resolution and its cost depends on the number of pixels, not cells.
#'
#' @param mapping Aesthetic mappings. `cell` is required: an `a5_cell`
#'   vector or hex strings. `fill`, `alpha`, `colour`, `linewidth` and
#'   `linetype` are understood.
#' @param data,show.legend,inherit.aes,na.rm,... As for other ggplot2
#'   layers.
#' @param render `"auto"` (default) draws polygons, switching to a
#'   raster when cells are smaller than a few device pixels. `"vector"`
#'   and `"raster"` force one mode. Cell borders (`colour`) need vector
#'   mode, so `"auto"` stays vector when they are set.
#' @param raster_dpi Pixel density for raster mode on vector devices
#'   (PDF, SVG), which have no pixels of their own. Bitmap devices use
#'   their own resolution.
#' @return A ggplot2 layer.
#' @export
geom_a5 <- function(mapping = NULL, data = NULL, ...,
                    render = c("auto", "vector", "raster"),
                    raster_dpi = 300,
                    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  rlang::check_installed(c("ggplot2", "sf", "wk"), reason = "to use `geom_a5()`.")
  render <- rlang::arg_match(render)
  check_number_decimal(raster_dpi, min = 1, arg = "raster_dpi")
  protos <- a5_ggproto()
  # Groups mean nothing for cells, but ggplot2 builds them from every
  # discrete aesthetic: a per-cell colour string (identity fills) costs
  # a sort over every cell. A constant group skips that.
  if (is.null(mapping)) mapping <- ggplot2::aes()
  if (is.null(mapping$group)) mapping$group <- 1L
  ggplot2::layer(
    data = data, mapping = mapping,
    stat = protos$stat, geom = protos$geom, position = "identity",
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(
      render = render, raster_dpi = raster_dpi, na.rm = na.rm, ...
    )
  )
}

a5_gg_cache <- new.env(parent = emptyenv())

a5_ggproto <- function() {
  if (is.null(a5_gg_cache$stat)) {
    a5_gg_cache$stat <- ggplot2::ggproto(
      "StatA5", ggplot2::Stat,
      required_aes = "cell",
      # As StatSf: compute_panel needs the coord to record extents.
      compute_layer = function(self, data, params, layout) {
        params$coord <- layout$coord
        ggplot2::ggproto_parent(ggplot2::Stat, self)$compute_layer(data, params, layout)
      },
      # Cells travel through ggplot2 as hex strings: its missing-value
      # checks split record columns such as a5_cell element by element
      # (about 1.5 s per 200k cells, twice), while hex costs 0.07 s to
      # format and 0.01 s to parse back.
      setup_data = function(data, params) {
        if (a5R::is_a5_cell(data$cell)) data$cell <- format(data$cell)
        data$cell <- as.character(data$cell)
        data
      },
      compute_panel = stat_a5_compute_panel
    )
    a5_gg_cache$geom <- ggplot2::ggproto(
      "GeomA5", ggplot2::Geom,
      required_aes = "cell",
      default_aes = ggplot2::aes(
        fill = "grey35", colour = NA, alpha = NA,
        linewidth = 0.1, linetype = 1
      ),
      draw_key = ggplot2::draw_key_polygon,
      draw_panel = geom_a5_draw_panel
    )
  }
  list(stat = a5_gg_cache$stat, geom = a5_gg_cache$geom)
}

# --- Stat ------------------------------------------------------------------

stat_a5_compute_panel <- function(data, scales, coord) {
  data <- data[!is.na(data$cell), , drop = FALSE]
  if (nrow(data) == 0L) return(data)

  ext <- a5_cell_extent(a5R::a5_cell(data$cell))
  # Centres ride along to the draw step, which culls cells out of view.
  data$a5_lon <- ext$lon
  data$a5_lat <- ext$lat
  if (inherits(coord, "CoordSf")) {
    # As StatSf: record the extent in the coord's crs, and give scales
    # the same extent in default_crs units (crs units when NULL).
    box <- densified_box(min(ext$xmin), max(ext$xmax), min(ext$ymin), max(ext$ymax))
    crs <- coord$params$crs
    in_crs <- project_lonlat(box, crs)
    coord$record_bbox(
      xmin = min(in_crs$x, na.rm = TRUE), xmax = max(in_crs$x, na.rm = TRUE),
      ymin = min(in_crs$y, na.rm = TRUE), ymax = max(in_crs$y, na.rm = TRUE)
    )
    def <- coord$get_default_crs()
    in_def <- if (is.null(def)) in_crs else project_lonlat(box, def)
    data$xmin <- min(in_def$x, na.rm = TRUE)
    data$xmax <- max(in_def$x, na.rm = TRUE)
    data$ymin <- min(in_def$y, na.rm = TRUE)
    data$ymax <- max(in_def$y, na.rm = TRUE)
  } else {
    # Stat output is in transformed scale space (scale_x_reverse() etc).
    xs <- transform_span(scales$x, ext$xmin, ext$xmax)
    ys <- transform_span(scales$y, ext$ymin, ext$ymax)
    data$xmin <- xs$lo
    data$xmax <- xs$hi
    data$ymin <- ys$lo
    data$ymax <- ys$hi
  }
  data
}

# The transformation of a position scale, or NULL for none / identity.
scale_transformation <- function(scale) {
  if (is.null(scale) || is.null(scale$get_transformation)) return(NULL)
  tr <- scale$get_transformation()
  if (is.null(tr) || identical(tr$name, "identity")) NULL else tr
}

transform_span <- function(scale, lo, hi) {
  tr <- scale_transformation(scale)
  if (is.null(tr)) return(list(lo = lo, hi = hi))
  a <- tr$transform(lo)
  b <- tr$transform(hi)
  list(lo = pmin(a, b), hi = pmax(a, b))
}

# Per-cell lon/lat extent. Centre padded by the average edge length (a
# pentagon's circumradius is 0.85 edges) for fine cells; true boundary
# extents for coarse cells, where the padding is a poor fit.
a5_cell_extent <- function(cells) {
  res <- a5R::a5_get_resolution(cells)
  xy <- unclass(a5R::a5_cell_to_lonlat(cells))
  edge_m <- as.numeric(a5R::a5_cell_edge_length_avg(res))
  pad_lat <- edge_m / 111320
  pad_lon <- pad_lat / pmax(cos(xy$y * pi / 180), 0.05)
  out <- list(
    lon = xy$x, lat = xy$y,
    xmin = xy$x - pad_lon, xmax = xy$x + pad_lon,
    ymin = pmax(-90, xy$y - pad_lat), ymax = pmin(90, xy$y + pad_lat)
  )
  coarse <- which(res <= 4L)
  if (length(coarse) > 0L) {
    rings <- a5_rings(cells[coarse])
    id <- factor(rep.int(rings$cell, rings$lengths), levels = seq_along(coarse))
    out$xmin[coarse] <- vapply(split(rings$x, id), min, numeric(1))
    out$xmax[coarse] <- vapply(split(rings$x, id), max, numeric(1))
    out$ymin[coarse] <- vapply(split(rings$y, id), min, numeric(1))
    out$ymax[coarse] <- vapply(split(rings$y, id), max, numeric(1))
  }
  out
}

densified_box <- function(xmin, xmax, ymin, ymax, n = 21L) {
  s <- seq(0, 1, length.out = n)
  xs <- xmin + s * (xmax - xmin)
  ys <- ymin + s * (ymax - ymin)
  list(
    x = c(xs, rep(xmax, n), rev(xs), rep(xmin, n)),
    y = c(rep(ymin, n), ys, rep(ymax, n), rev(ys))
  )
}

# coord_sf keeps crs as given (an EPSG number, string or crs object).
# NULL for none.
as_crs <- function(crs) {
  if (is.null(crs)) return(NULL)
  crs <- sf::st_crs(crs)
  if (is.na(crs)) NULL else crs
}

# lon/lat -> crs (NULL or NA crs: unchanged, i.e. plotted as lon/lat).
project_lonlat <- function(xy, crs) {
  crs <- as_crs(crs)
  if (is.null(crs) || sf::st_is_longlat(crs)) return(xy)
  out <- sf::sf_project(
    sf::st_crs(4326), crs, cbind(xy$x, xy$y),
    keep = TRUE, warn = FALSE, authority_compliant = FALSE
  )
  out[!is.finite(out)] <- NA
  list(x = out[, 1], y = out[, 2])
}

unproject_to_lonlat <- function(xy, crs) {
  crs <- as_crs(crs)
  if (is.null(crs) || sf::st_is_longlat(crs)) return(xy)
  out <- sf::sf_project(
    crs, sf::st_crs(4326), cbind(xy$x, xy$y),
    keep = TRUE, warn = FALSE, authority_compliant = FALSE
  )
  out[!is.finite(out)] <- NA
  list(x = out[, 1], y = out[, 2])
}

# Flat cell rings, open (no repeated first vertex):
# list(x, y, lengths, cell), one entry of `lengths` and `cell` per ring,
# where `cell` indexes the input. Missing cells give no ring; a cell
# crossing the antimeridian gives two (see split_antimeridian()).
a5_rings <- function(cells) {
  if (exists("a5_cell_to_boundary_xy", envir = asNamespace("a5R"), inherits = FALSE)) {
    xy <- a5R::a5_cell_to_boundary_xy(cells, closed = FALSE)
    lengths <- xy$lengths
  } else {
    # a5R before a5_cell_to_boundary_xy(): same vertices, via WKB.
    xy <- wk::wk_coords(a5R::a5_cell_to_boundary(cells, closed = FALSE))
    lengths <- tabulate(xy$feature_id, nbins = length(cells))
  }
  rings <- list(x = xy$x, y = xy$y, lengths = lengths, cell = seq_along(cells))
  drop <- rings$lengths == 0L
  rings$lengths <- rings$lengths[!drop]
  rings$cell <- rings$cell[!drop]
  split_antimeridian(rings)
}

# A5 rings keep longitudes continuous around the cell centre, so a cell
# on the antimeridian has vertices past +/-180. Projections would wrap
# those vertices and smear the ring across the map; instead each such
# ring is clipped into its part inside [-180, 180] and the part beyond,
# shifted by 360 degrees.
split_antimeridian <- function(rings) {
  end <- cumsum(rings$lengths)
  start <- end - rings$lengths + 1L
  ring_id <- rep.int(seq_along(rings$lengths), rings$lengths)
  out_of_range <- unique(ring_id[rings$x > 180 | rings$x < -180])
  if (length(out_of_range) == 0L) return(rings)

  pieces <- list()
  for (i in out_of_range) {
    idx <- start[i]:end[i]
    x <- rings$x[idx]
    y <- rings$y[idx]
    side <- if (max(x) > 180) 180 else -180
    shift <- if (side > 0) -360 else 360
    inside <- clip_halfplane(x, y, side, keep_below = side > 0)
    beyond <- clip_halfplane(x, y, side, keep_below = side < 0)
    beyond$x <- beyond$x + shift
    for (piece in list(inside, beyond)) {
      if (length(piece$x) >= 3L) pieces[[length(pieces) + 1L]] <- c(piece, cell = rings$cell[i])
    }
  }
  keep <- !(ring_id %in% out_of_range)
  kept <- setdiff(seq_along(rings$lengths), out_of_range)
  list(
    x = c(rings$x[keep], unlist(lapply(pieces, `[[`, "x"))),
    y = c(rings$y[keep], unlist(lapply(pieces, `[[`, "y"))),
    lengths = c(rings$lengths[kept], vapply(pieces, function(p) length(p$x), integer(1))),
    cell = c(rings$cell[kept], vapply(pieces, `[[`, integer(1), "cell"))
  )
}

# Sutherland-Hodgman clip of one ring against the vertical line x = x0,
# keeping x <= x0 (keep_below) or x >= x0.
clip_halfplane <- function(x, y, x0, keep_below) {
  inside <- if (keep_below) x <= x0 else x >= x0
  n <- length(x)
  nxt <- c(2:n, 1L)
  ox <- numeric(0)
  oy <- numeric(0)
  for (i in seq_len(n)) {
    j <- nxt[i]
    if (inside[i]) {
      ox <- c(ox, x[i])
      oy <- c(oy, y[i])
    }
    if (inside[i] != inside[j]) {
      t <- (x0 - x[i]) / (x[j] - x[i])
      ox <- c(ox, x0)
      oy <- c(oy, y[i] + t * (y[j] - y[i]))
    }
  }
  list(x = ox, y = oy)
}

# --- Geom ------------------------------------------------------------------

geom_a5_draw_panel <- function(data, panel_params, coord,
                               render = "auto", raster_dpi = 300) {
  if (nrow(data) == 0L) return(grid::nullGrob())
  fill <- data$fill
  if (inherits(fill, "a5_rgb")) fill <- a5_rgb_to_hex(fill)
  fill <- ggplot2::fill_alpha(fill, data$alpha)
  colour <- data$colour
  stroked <- any(!is.na(colour))
  coord_kind <- if (inherits(coord, "CoordSf")) {
    "sf"
  } else if (inherits(coord, "CoordCartesian") && !inherits(coord, "CoordFlip")) {
    "cartesian"
  } else {
    "other"
  }
  grid::gTree(
    cells = a5R::a5_cell(data$cell), fill = fill, colour = colour,
    centres = list(x = data$a5_lon, y = data$a5_lat),
    lwd = data$linewidth * ggplot2::.pt, lty = data$linetype,
    stroked = stroked, render = render, raster_dpi = raster_dpi,
    coord = coord, panel_params = panel_params, coord_kind = coord_kind,
    cl = "a5_grob"
  )
}

#' @exportS3Method grid::makeContent
makeContent.a5_grob <- function(x) {
  view <- a5_view_scale(x)
  child <- if (a5_render_mode(x, view) == "raster") {
    a5_raster_grob(x, view)
  } else {
    a5_vector_grob(x, view)
  }
  grid::setChildren(x, grid::gList(child))
}

# Panel size in device pixels. Vector devices report 72 "pixels" per
# inch; raster mode on them uses raster_dpi instead.
panel_pixels <- function(raster_dpi) {
  w_in <- grid::convertWidth(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  h_in <- grid::convertHeight(grid::unit(1, "npc"), "inches", valueOnly = TRUE)
  ppi <- grDevices::dev.size("px")[1] / grDevices::dev.size("in")[1]
  if (!is.finite(ppi) || ppi < 96) ppi <- raster_dpi
  c(w = max(1L, round(w_in * ppi)), h = max(1L, round(h_in * ppi)))
}

# Pixel grid and ground distance of one pixel at the panel centre (NA
# for coords that cannot be inverted).
a5_view_scale <- function(x) {
  px <- panel_pixels(x$raster_dpi)
  m_per_px <- NA_real_
  if (x$coord_kind != "other") {
    ll <- npc_to_lonlat(x, c(0.5, 0.5 + 1 / px[["w"]]), c(0.5, 0.5))
    if (!anyNA(ll$x) && !anyNA(ll$y)) {
      m_per_px <- haversine_m(ll$x[1], ll$y[1], ll$x[2], ll$y[2])
    }
  }
  list(w = px[["w"]], h = px[["h"]], m_per_px = m_per_px)
}

a5_render_mode <- function(x, view) {
  if (x$render == "vector" || x$coord_kind == "other") return("vector")
  if (x$render == "raster") return("raster")
  if (x$stroked || is.na(view$m_per_px)) return("vector")
  finest <- max(a5R::a5_get_resolution(x$cells))
  edge_px <- as.numeric(a5R::a5_cell_edge_length_avg(finest)) / view$m_per_px
  if (edge_px < A5_RASTER_EDGE_PX) "raster" else "vector"
}

haversine_m <- function(lon1, lat1, lon2, lat2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
    cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371008.8 * asin(sqrt(pmin(1, a)))
}

# Panel npc -> lon/lat, inverting the coord's position transform.
npc_to_lonlat <- function(x, nx, ny) {
  pp <- x$panel_params
  if (x$coord_kind == "sf") {
    xr <- pp$x_range
    yr <- pp$y_range
  } else {
    xr <- pp$x.range
    yr <- pp$y.range
  }
  # coord_*(reverse = ) flips the panel, after any scale transformation.
  reverse <- if (is.null(pp$reverse)) "none" else pp$reverse
  if (reverse %in% c("x", "xy")) xr <- rev(xr)
  if (reverse %in% c("y", "xy")) yr <- rev(yr)
  xy <- list(x = xr[1] + nx * diff(xr), y = yr[1] + ny * diff(yr))
  if (x$coord_kind == "sf") return(unproject_to_lonlat(xy, pp$crs))
  tx <- scale_transformation(pp$x$scale)
  ty <- scale_transformation(pp$y$scale)
  if (!is.null(tx)) xy$x <- tx$inverse(xy$x)
  if (!is.null(ty)) xy$y <- ty$inverse(xy$y)
  xy
}

# lon/lat into the space coord$transform() expects: the position scales'
# transformed space for cartesian coords; lon/lat for coord_sf, whose
# transform() projects it (see a5_vector_grob()).
lonlat_to_scale_space <- function(x, xy) {
  out <- data.frame(x = xy$x, y = xy$y)
  if (x$coord_kind == "sf") return(out)
  pp <- x$panel_params
  tx <- scale_transformation(pp$x$scale)
  ty <- scale_transformation(pp$y$scale)
  if (!is.null(tx)) out$x <- tx$transform(out$x)
  if (!is.null(ty)) out$y <- ty$transform(out$y)
  out
}

a5_vector_grob <- function(x, view) {
  pp <- x$panel_params
  if (x$coord_kind == "sf" && !is.null(as_crs(pp$crs))) {
    # Cells are lon/lat whatever the coord's default_crs says.
    pp$default_crs <- sf::st_crs(4326)
  }
  keep <- a5_cells_in_view(x, pp, view)
  if (length(keep) == 0L) return(grid::nullGrob())
  rings <- a5_rings(x$cells[keep])
  pts <- x$coord$transform(lonlat_to_scale_space(x, rings), pp)
  drawn <- keep[rings$cell]
  stroke <- x$stroked
  grid::polygonGrob(
    pts$x, pts$y, id.lengths = rings$lengths,
    default.units = "native",
    gp = grid::gpar(
      fill = x$fill[drawn],
      col = if (stroke) x$colour[drawn] else NA,
      lwd = if (stroke) x$lwd[drawn] else 0,
      lty = x$lty[drawn]
    )
  )
}

# Indices of cells whose centre lies within the panel, padded by two
# edge lengths of the coarsest cell present so partly visible cells are
# kept. Outlines are only built for these.
a5_cells_in_view <- function(x, pp, view) {
  n <- length(x$cells)
  if (x$coord_kind == "other" || is.na(view$m_per_px)) return(seq_len(n))
  npc <- x$coord$transform(lonlat_to_scale_space(x, x$centres), pp)
  coarsest <- min(a5R::a5_get_resolution(x$cells))
  edge_px <- as.numeric(a5R::a5_cell_edge_length_avg(coarsest)) / view$m_per_px
  pad <- 2 * edge_px / min(view$w, view$h)
  which(npc$x >= -pad & npc$x <= 1 + pad & npc$y >= -pad & npc$y <= 1 + pad)
}

a5_raster_grob <- function(x, view) {
  w <- view$w
  h <- view$h
  cells <- x$cells
  res_all <- a5R::a5_get_resolution(cells)
  resolutions <- sort(unique(res_all), decreasing = TRUE)
  out <- rep(NA_character_, w * h)
  nx <- (seq_len(w) - 0.5) / w
  # Row bands bound memory: about 1e6 pixels at a time.
  band <- max(1L, floor(1e6 / w))
  for (r0 in seq(1L, h, by = band)) {
    rows <- r0:min(h, r0 + band - 1L)
    ny <- 1 - (rows - 0.5) / h            # raster row 1 is the top
    ll <- npc_to_lonlat(x, rep(nx, length(rows)), rep(ny, each = w))
    ok <- which(!is.na(ll$x) & !is.na(ll$y))
    idx <- (rows[1] - 1L) * w + ok
    todo <- rep(TRUE, length(ok))
    for (r in resolutions) {
      sel <- which(todo)
      if (length(sel) == 0L) break
      sub <- which(res_all == r)
      pc <- a5R::a5_lonlat_to_cell(ll$x[ok[sel]], ll$y[ok[sel]], resolution = r)
      hit <- match(pc, cells[sub])
      found <- !is.na(hit)
      out[idx[sel[found]]] <- x$fill[sub[hit[found]]]
      todo[sel[found]] <- FALSE
    }
  }
  grid::rasterGrob(
    matrix(out, nrow = h, ncol = w, byrow = TRUE),
    x = 0, y = 0, width = 1, height = 1, just = c(0, 0),
    default.units = "npc", interpolate = FALSE
  )
}
