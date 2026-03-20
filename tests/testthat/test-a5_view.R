# Tests for R/a5_view.R — main function and Shiny bindings

# --- Helper ---
make_cell <- function(lon = 0, lat = 0, res = 5) {
  a5R::a5_lonlat_to_cell(lon, lat, resolution = res)
}

make_cells <- function(n = 3, res = 5) {
  lons <- seq(-1, 1, length.out = n)
  lats <- seq(-1, 1, length.out = n)
  a5R::a5_lonlat_to_cell(lons, lats, resolution = res)
}

# --- Basic widget creation ---

test_that("a5_view returns an htmlwidget with bare cells", {
  cells <- make_cells(5)
  w <- a5_view(cells)
  expect_s3_class(w, "htmlwidget")
  expect_s3_class(w, "a5view")
})

test_that("a5_view returns an htmlwidget with data frame", {
  cells <- make_cells(5)
  df <- data.frame(cell = cells, value = runif(5))
  w <- a5_view(df)
  expect_s3_class(w, "htmlwidget")
})

# --- Argument validation in a5_view ---

test_that("a5_view errors on missing cells", {
  expect_error(a5_view(), "cells")
})

test_that("a5_view errors on invalid cells type", {
  expect_error(a5_view(1:10), "a5_cell")
  expect_error(a5_view("abc"), "a5_cell")
})

test_that("a5_view errors on invalid opacity", {
  cells <- make_cells(3)
  expect_error(a5_view(cells, opacity = -0.1), "between")
  expect_error(a5_view(cells, opacity = 1.5), "between")
  expect_error(a5_view(cells, opacity = "half"), "single number")
})

test_that("a5_view errors on invalid elevation_scale", {
  cells <- make_cells(3)
  expect_error(a5_view(cells, elevation_scale = -1), ">= 0")
})

test_that("a5_view errors on invalid border_width", {
  cells <- make_cells(3)
  expect_error(a5_view(cells, border_width = -1), ">= 0")
})

test_that("a5_view errors on invalid basemap", {
  cells <- make_cells(3)
  expect_error(a5_view(cells, basemap = "mapbox"), "invalid value")
})

test_that("a5_view errors on invalid palette", {
  cells <- make_cells(3)
  expect_error(a5_view(cells, palette = "nope"), "not a recognised")
})

test_that("a5_view errors on invalid border colour", {
  cells <- make_cells(3)
  expect_error(a5_view(cells, border = "notacolour"), "not valid")
})

test_that("a5_view errors on invalid tooltip type", {
  cells <- make_cells(3)
  expect_error(a5_view(cells, tooltip = 42), "logical")
})

# --- Fill argument variants ---

test_that("a5_view works with uniform fill", {
  cells <- make_cells(3)
  w <- a5_view(cells, fill = "#ff0000")
  expect_s3_class(w, "htmlwidget")
  expect_false(w$x$fill_is_column)
  expect_equal(w$x$fill_color, c(255L, 0L, 0L, 255L))
})

test_that("a5_view works with numeric fill vector", {
  cells <- make_cells(5)
  vals <- as.numeric(1:5)
  w <- a5_view(cells, fill = vals)
  expect_true(w$x$fill_is_column)
  expect_true(w$x$has_fill_value)
  expect_equal(w$x$domain, c(1, 5))
})

test_that("a5_view works with colour vector fill", {
  cells <- make_cells(3)
  cols <- c("#ff0000", "#00ff00", "#0000ff")
  w <- a5_view(cells, fill = cols)
  expect_true(w$x$fill_per_cell)
})

test_that("a5_view works with column name fill", {
  cells <- make_cells(3)
  df <- data.frame(cell = cells, score = c(1.0, 2.0, 3.0))
  w <- a5_view(df, fill = score)
  expect_true(w$x$fill_is_column)
  expect_equal(w$x$domain, c(1, 3))
})

test_that("a5_view errors on fill column not found", {
  cells <- make_cells(3)
  df <- data.frame(cell = cells, val = 1:3)
  expect_error(a5_view(df, fill = nonexistent), "not found")
})

test_that("a5_view errors on mismatched fill length", {
  cells <- make_cells(5)
  expect_error(a5_view(cells, fill = 1:3), "length 3")
})

# --- Fill identity ---

test_that("fill_identity works with packed RGB integers", {
  cells <- make_cells(3)
  # Red, green, blue as packed uint32: (R << 16) | (G << 8) | B
  rgb_packed <- c(
    bitwOr(bitwShiftL(255L, 16L), bitwOr(bitwShiftL(0L, 8L), 0L)),   # red
    bitwOr(bitwShiftL(0L, 16L), bitwOr(bitwShiftL(255L, 8L), 0L)),   # green
    bitwOr(bitwShiftL(0L, 16L), bitwOr(bitwShiftL(0L, 8L), 255L))    # blue
  )
  w <- a5_view(cells, fill = rgb_packed, fill_identity = TRUE)
  expect_true(w$x$fill_per_cell)
  expect_false(w$x$fill_is_column)
  rgba <- w$x$columns[["_fill_rgba"]]
  expect_equal(rgba[[1]], c(255L, 0L, 0L, 255L))
  expect_equal(rgba[[2]], c(0L, 255L, 0L, 255L))
  expect_equal(rgba[[3]], c(0L, 0L, 255L, 255L))
})

test_that("fill_identity works with hex colour strings", {
  cells <- make_cells(3)
  hex_cols <- c("#ff0000", "#00ff00", "#0000ff")
  w <- a5_view(cells, fill = hex_cols, fill_identity = TRUE)
  expect_true(w$x$fill_per_cell)
  rgba <- w$x$columns[["_fill_rgba"]]
  expect_equal(rgba[[1]], c(255L, 0L, 0L, 255L))
  expect_equal(rgba[[2]], c(0L, 255L, 0L, 255L))
})

test_that("fill_identity works with column name", {
  cells <- make_cells(3)
  rgb_packed <- c(16711680L, 65280L, 255L)  # red, green, blue
  df <- data.frame(cell = cells, rgb = rgb_packed)
  w <- a5_view(df, fill = rgb, fill_identity = TRUE)
  expect_true(w$x$fill_per_cell)
  rgba <- w$x$columns[["_fill_rgba"]]
  expect_equal(rgba[[1]], c(255L, 0L, 0L, 255L))
})

test_that("fill_identity errors on uniform fill", {
  cells <- make_cells(3)
  expect_error(a5_view(cells, fill = "#ff0000", fill_identity = TRUE), "fill_identity")
})

# --- Palette ---

test_that("a5_view uses custom colour palette", {
  cells <- make_cells(5)
  vals <- as.numeric(1:5)
  w <- a5_view(cells, fill = vals, palette = c("#000000", "#ffffff"))
  expect_length(w$x$palette, 2)
})

test_that("a5_view uses named palette", {
  cells <- make_cells(5)
  vals <- as.numeric(1:5)
  w <- a5_view(cells, fill = vals, palette = "Inferno")
  expect_length(w$x$palette, 8)
})

# --- Border ---

test_that("a5_view sends stroked = TRUE when border is set", {
  cells <- make_cells(3)
  w <- a5_view(cells, border = "white")
  expect_true(w$x$stroked)
  expect_equal(w$x$line_color, c(255L, 255L, 255L, 255L))
})

test_that("a5_view sends stroked = FALSE when border is NULL", {
  cells <- make_cells(3)
  w <- a5_view(cells, border = NULL)
  expect_false(w$x$stroked)
  expect_null(w$x$line_color)
})

test_that("a5_view passes border_width", {
  cells <- make_cells(3)
  w <- a5_view(cells, border = "#000", border_width = 3)
  expect_equal(w$x$line_width, 3)
})

# --- Basemap ---

test_that("a5_view passes basemap list", {
  cells <- make_cells(3)
  w <- a5_view(cells, basemap = c("dark", "osm"))
  expect_equal(w$x$basemaps, list("dark", "osm"))
})

test_that("a5_view accepts single basemap", {
  cells <- make_cells(3)
  w <- a5_view(cells, basemap = "satellite")
  expect_equal(w$x$basemaps, list("satellite"))
})

# --- Tooltip ---

test_that("a5_view sets pickable based on tooltip", {
  cells <- make_cells(3)
  w1 <- a5_view(cells, tooltip = TRUE)
  expect_true(w1$x$pickable)

  w2 <- a5_view(cells, tooltip = FALSE)
  expect_false(w2$x$pickable)
})

# --- View state ---

test_that("a5_view uses provided lng/lat/zoom", {
  cells <- make_cells(3)
  w <- a5_view(cells, lng = 10, lat = 20, zoom = 5)
  expect_equal(w$x$view_state$longitude, 10)
  expect_equal(w$x$view_state$latitude, 20)
  expect_equal(w$x$view_state$zoom, 5)
})

test_that("a5_view auto-centres when no lng/lat/zoom", {
  cells <- make_cells(3)
  w <- a5_view(cells)
  expect_true(is.numeric(w$x$view_state$longitude))
  expect_true(is.numeric(w$x$view_state$latitude))
  expect_true(is.numeric(w$x$view_state$zoom))
})

# --- Sizing policy ---

test_that("a5_view sets sizing policy with zero padding", {
  cells <- make_cells(3)
  w <- a5_view(cells)
  sp <- w$sizingPolicy
  expect_equal(sp$viewer$padding, 0)
  expect_true(sp$viewer$fill)
  expect_equal(sp$browser$padding, 0)
  expect_true(sp$browser$fill)
})

# --- Data payload ---

test_that("a5_view payload uses columnar data", {
  cells <- make_cells(3)
  w <- a5_view(cells)
  expect_true(is.list(w$x$columns))
  expect_true("pentagon" %in% names(w$x$columns))
  expect_length(w$x$columns$pentagon, 3)
})

# --- Shiny bindings ---

test_that("a5_viewOutput returns shiny output", {
  out <- a5_viewOutput("myid")
  expect_true(inherits(out, "shiny.tag") || inherits(out, "shiny.tag.list"))
})

test_that("a5_viewOutput errors on non-string", {
  expect_error(a5_viewOutput(123), "single string")
})
