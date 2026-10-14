skip_if_not_installed("ggplot2")
skip_if_not_installed("sf")

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

# Colour of the raster at panel npc (0.5, 0.5) for a plot.
centre_colour <- function(p, render) {
  f <- withr::local_tempfile(fileext = ".png")
  ragg::agg_png(f, 300, 300)
  print(p + geom_a5(render = render) + ggplot2::theme_void())
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
    plain = base,
    scale_reverse = base + ggplot2::scale_x_reverse() + ggplot2::scale_y_reverse(),
    coord_reverse = base + ggplot2::coord_cartesian(reverse = "xy", xlim = c(33.31, 33.36))
  )
  # Vector mode anti-aliases cell edges (a few /255 near an edge); a
  # wrong cell differs by at least 1/6 in some channel.
  for (nm in names(variants)) {
    diff <- abs(centre_colour(variants[[nm]], "raster") - centre_colour(variants[[nm]], "vector"))
    expect_lt(max(diff), 0.06, label = nm)
  }
})
