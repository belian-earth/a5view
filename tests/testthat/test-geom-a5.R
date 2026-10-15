sample_df <- function() {
  cells <- a5R::a5_uncompact(
    a5R::a5_polygon_to_cells(wk::rct(33.3, -13, 33.4, -12.9), resolution = 12L)
  )
  xy <- unclass(a5R::a5_cell_to_lonlat(cells))
  data.frame(cell = cells, v = xy$x + xy$y)
}

draw_to_null <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  print(p)
  invisible(TRUE)
}

test_that("geom_a5 keeps one row per cell and trains lon/lat scales", {
  df <- sample_df()
  p <- ggplot2::ggplot(df, ggplot2::aes(cell = cell, fill = v)) + geom_a5()
  d <- ggplot2::layer_data(p)
  expect_identical(nrow(d), nrow(df))
  expect_type(d$cell, "character")
  expect_true(all(d$xmin < 33.41 & d$xmax > 33.29))
  expect_true(all(d$ymin < -12.89 & d$ymax > -13.01))
})

test_that("geom_a5 accepts hex ids and drops missing cells", {
  df <- sample_df()
  df$cell <- format(df$cell)
  df$cell[1] <- NA
  p <- ggplot2::ggplot(df, ggplot2::aes(cell = cell)) + geom_a5(na.rm = TRUE)
  expect_identical(nrow(ggplot2::layer_data(p)), nrow(df) - 1L)
})

test_that("geom_a5 draws in every render mode and coord", {
  df <- sample_df()
  base <- ggplot2::ggplot(df, ggplot2::aes(cell = cell, fill = v))
  for (render in c("auto", "vector", "raster")) {
    expect_true(draw_to_null(base + geom_a5(render = render)))
    expect_true(draw_to_null(base + geom_a5(render = render) + ggplot2::coord_quickmap()))
    expect_true(draw_to_null(
      base + geom_a5(render = render) + ggplot2::coord_sf(crs = 32736, default_crs = 4326)
    ))
  }
  expect_true(draw_to_null(base + geom_a5() + ggplot2::coord_polar()))
})

test_that("geom_a5 shares coord_sf limits with geom_sf", {
  df <- sample_df()
  box <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_point(c(33.6, -12.7)), crs = 4326
  ))
  p <- ggplot2::ggplot() +
    geom_a5(data = df, ggplot2::aes(cell = cell)) +
    ggplot2::geom_sf(data = box) +
    ggplot2::coord_sf(crs = 32736)
  b <- ggplot2::ggplot_build(p)
  pp <- b$layout$panel_params[[1]]
  # Limits cover both the cells and the point, in UTM metres.
  corners <- sf::sf_project(
    "EPSG:4326", "EPSG:32736", rbind(c(33.3, -13), c(33.6, -12.7))
  )
  expect_lte(pp$x_range[1], corners[1, 1])
  expect_gte(pp$x_range[2], corners[2, 1])
  expect_lte(pp$y_range[1], corners[1, 2])
  expect_gte(pp$y_range[2], corners[2, 2])
})

test_that("antimeridian rings are split into parts inside [-180, 180]", {
  cell <- a5R::a5_lonlat_to_cell(179.9, 0, resolution = 3L)
  rings <- a5_rings(cell)
  expect_true(all(rings$x >= -180 & rings$x <= 180))
  expect_identical(rings$cell, rep(1L, length(rings$lengths)))
  if (length(rings$lengths) == 2L) {
    # One part on each side.
    id <- rep.int(seq_along(rings$lengths), rings$lengths)
    expect_setequal(sign(tapply(rings$x, id, mean)), c(-1, 1))
  }
})

test_that("polar cells are closed along the pole line", {
  for (res in 0:2) {
    for (lat in c(89.99, -89.99)) {
      rings <- a5_rings(a5R::a5_lonlat_to_cell(10, lat, res))
      expect_true(all(rings$x >= -180 & rings$x <= 180))
      # The fill reaches the pole...
      expect_equal(max(abs(rings$y)), 90)
      # ...with no spike: lon/lat area matches a cell bounded below by
      # its lowest vertex, to within the curvature of its sides.
      id <- rep.int(seq_along(rings$lengths), rings$lengths)
      area <- sum(vapply(split(seq_along(rings$x), id), function(i) {
        x <- rings$x[i]
        y <- rings$y[i]
        abs(sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y)) / 2
      }, numeric(1)))
      span <- diff(range(rings$x))
      expect_gt(area, 0.5 * span * (90 - min(abs(rings$y))))
    }
  }
  # The res-0 polar cell surrounds the pole: its fill spans all longitudes.
  rings <- a5_rings(a5R::a5_lonlat_to_cell(10, 89.99, 0L))
  expect_equal(range(rings$x), c(-180, 180))
})

test_that("cells around a pole fill a polar panel", {
  lon <- seq(-180, 170, 10)
  cells <- unique(a5R::a5_lonlat_to_cell(c(lon, 10), c(rep(70, length(lon)), 89.99), 2L))
  p <- ggplot2::ggplot(data.frame(cell = cells), ggplot2::aes(cell = cell)) +
    geom_a5(render = "vector") +
    ggplot2::coord_sf(crs = "+proj=ortho +lat_0=90 +lon_0=0")
  pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
  # The ring at lat 70 is about 4400 km across.
  expect_gt(diff(pp$x_range), 4e6)
  expect_gt(diff(pp$y_range), 4e6)
  expect_true(draw_to_null(p))
})

test_that("clip_halfplane clips a square at a vertical line", {
  sq <- clip_halfplane(c(0, 2, 2, 0), c(0, 0, 2, 2), 1, keep_below = TRUE)
  expect_equal(range(sq$x), c(0, 1))
  expect_equal(range(sq$y), c(0, 2))
})

test_that("cells_rgb() fills draw as colours with no scale or legend", {
  df <- sample_df()
  df$rgb <- cells_rgb(df$v, rev(df$v), df$v)
  p <- ggplot2::ggplot(df, ggplot2::aes(cell = cell, fill = rgb)) + geom_a5()
  d <- ggplot2::layer_data(p)
  expect_s3_class(d$fill, "a5_rgb")
  expect_identical(format(d$fill), format(df$rgb))
  expect_true(draw_to_null(p))
  expect_length(ggplot2::get_guide_data(p, "fill"), 0L)
})

# Colour at the panel centre of `base + geom_a5(render) + extra`. The
# extras come after the layer: like geom_sf(), geom_a5() brings a
# default coord that replaces any coord added before it.
centre_colour <- function(base, extra, render) {
  f <- withr::local_tempfile(fileext = ".png")
  ragg::agg_png(f, 300, 300)
  print(base + geom_a5(render = render) + extra + ggplot2::theme_void())
  grDevices::dev.off()
  img <- png::readPNG(f)
  img[150, 150, 1:3]
}

test_that("raster and vector agree under reversed and transformed axes", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  df <- sample_df()
  df$rgb <- cells_rgb(seq_len(nrow(df)) %% 7, seq_len(nrow(df)) %% 5, seq_len(nrow(df)) %% 3)
  base <- ggplot2::ggplot(df, ggplot2::aes(cell = cell, fill = rgb))
  variants <- list(
    default_coord = list(),
    cartesian = list(ggplot2::coord_cartesian()),
    scale_reverse = list(
      ggplot2::scale_x_reverse(), ggplot2::scale_y_reverse(), ggplot2::coord_cartesian()
    ),
    coord_reverse = list(ggplot2::coord_cartesian(reverse = "xy", xlim = c(33.31, 33.36))),
    utm = list(ggplot2::coord_sf(crs = 32736))
  )
  # Vector mode anti-aliases cell edges (a few /255 near an edge); a
  # wrong cell differs by at least 1/6 in some channel.
  for (nm in names(variants)) {
    diff <- abs(
      centre_colour(base, variants[[nm]], "raster") -
        centre_colour(base, variants[[nm]], "vector")
    )
    expect_lt(max(diff), 0.06, label = nm)
  }
})

test_that("geom_a5 brings a default lon/lat coord_sf, as geom_sf does", {
  df <- sample_df()
  p <- ggplot2::ggplot(df, ggplot2::aes(cell = cell)) + geom_a5()
  b <- ggplot2::ggplot_build(p)
  expect_s3_class(b$layout$coord, "CoordSf")
  expect_identical(b$layout$panel_params[[1]]$crs, sf::st_crs(4326))
  # Fixed aspect: cos(latitude) correction, as coord_sf(crs = 4326).
  ref <- ggplot2::ggplot_build(p + ggplot2::coord_sf(crs = 4326, default_crs = 4326))
  expect_equal(
    b$layout$coord$aspect(b$layout$panel_params[[1]]),
    ref$layout$coord$aspect(ref$layout$panel_params[[1]])
  )

  # A user coord replaces it silently.
  expect_no_message(
    pc <- ggplot2::ggplot_build(p + ggplot2::coord_cartesian())
  )
  expect_false(inherits(pc$layout$coord, "CoordSf"))

  # With an sf layer, the crs comes from it whichever is added last.
  pt <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(500000, 8570000)), crs = 32736))
  for (q in list(
    p + ggplot2::geom_sf(data = pt, inherit.aes = FALSE),
    ggplot2::ggplot() + ggplot2::geom_sf(data = pt) +
      geom_a5(data = df, ggplot2::aes(cell = cell))
  )) {
    bq <- ggplot2::ggplot_build(q)
    expect_identical(sf::st_crs(bq$layout$panel_params[[1]]$crs), sf::st_crs(32736))
  }
})
