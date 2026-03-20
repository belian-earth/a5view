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
  # Verify RGBA encoded in Arrow IPC
  tbl <- arrow::read_ipc_stream(base64enc::base64decode(w$x$arrow_ipc))
  expect_equal(as.integer(tbl[["_fill_r"]]), c(255L, 0L, 0L))
  expect_equal(as.integer(tbl[["_fill_g"]]), c(0L, 255L, 0L))
  expect_equal(as.integer(tbl[["_fill_b"]]), c(0L, 0L, 255L))
  expect_equal(as.integer(tbl[["_fill_a"]]), c(255L, 255L, 255L))
})

test_that("fill_identity works with hex colour strings", {
  cells <- make_cells(3)
  hex_cols <- c("#ff0000", "#00ff00", "#0000ff")
  w <- a5_view(cells, fill = hex_cols, fill_identity = TRUE)
  expect_true(w$x$fill_per_cell)
  tbl <- arrow::read_ipc_stream(base64enc::base64decode(w$x$arrow_ipc))
  expect_equal(as.integer(tbl[["_fill_r"]]), c(255L, 0L, 0L))
  expect_equal(as.integer(tbl[["_fill_g"]]), c(0L, 255L, 0L))
})

test_that("fill_identity works with column name", {
  cells <- make_cells(3)
  rgb_packed <- c(16711680L, 65280L, 255L)  # red, green, blue
  df <- data.frame(cell = cells, rgb = rgb_packed)
  w <- a5_view(df, fill = rgb, fill_identity = TRUE)
  expect_true(w$x$fill_per_cell)
  tbl <- arrow::read_ipc_stream(base64enc::base64decode(w$x$arrow_ipc))
  expect_equal(as.integer(tbl[["_fill_r"]]), c(255L, 0L, 0L))
})

test_that("fill_identity errors on uniform fill", {
  cells <- make_cells(3)
  expect_error(a5_view(cells, fill = "#ff0000", fill_identity = TRUE), "fill_identity")
})

# --- Palette ---

test_that("a5_view with custom colour palette pre-computes RGBA", {
  cells <- make_cells(5)
  vals <- as.numeric(1:5)
  w <- a5_view(cells, fill = vals, palette = c("#000000", "#ffffff"))
  # Palette mapping is done R-side; JS receives pre-computed RGBA
  expect_null(w$x$palette)
  expect_true(w$x$fill_per_cell)
})

test_that("a5_view with named palette pre-computes RGBA", {
  cells <- make_cells(5)
  vals <- as.numeric(1:5)
  w <- a5_view(cells, fill = vals, palette = "Inferno")
  expect_null(w$x$palette)
  expect_true(w$x$fill_per_cell)
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

test_that("a5_view payload has inline base64 Arrow IPC", {
  cells <- make_cells(3)
  w <- a5_view(cells)
  # Arrow IPC base64 string present
  expect_true(is.character(w$x$arrow_ipc))
  expect_true(nchar(w$x$arrow_ipc) > 0)
  # Arrow JS dependency attached
  dep_names <- vapply(w$dependencies, function(d) d$name, character(1))
  expect_true("apache-arrow-js" %in% dep_names)
})

# --- Shiny bindings ---

test_that("a5_viewOutput returns shiny output", {
  out <- a5_viewOutput("myid")
  expect_true(inherits(out, "shiny.tag") || inherits(out, "shiny.tag.list"))
})

test_that("a5_viewOutput errors on non-string", {
  expect_error(a5_viewOutput(123), "single string")
})

# --- renderA5_view ---

test_that("renderA5_view returns a shiny render function", {
  fn <- renderA5_view(a5_view(make_cells(3)))
  expect_true(is.function(fn))
})

test_that("renderA5_view works with quoted expression", {
  fn <- renderA5_view(quote(a5_view(make_cells(3))), quoted = TRUE)
  expect_true(is.function(fn))
})

# --- a5_view_update ---

test_that("a5_view_update builds correct message with bare cells", {
  cells <- make_cells(5)
  captured <- NULL
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      captured <<- list(type = type, message = message)
    }
  )
  a5_view_update(mock_session, "map", cells)
  expect_equal(captured$type, "a5view-update-map")
  msg <- captured$message
  expect_true(is.character(msg$arrow_ipc))
  expect_true(nchar(msg$arrow_ipc) > 0)
  expect_false(msg$fill_is_column)
  expect_true(msg$tooltip)
})

test_that("a5_view_update builds correct message with numeric fill", {
  cells <- make_cells(5)
  vals <- as.numeric(1:5)
  captured <- NULL
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      captured <<- list(type = type, message = message)
    }
  )
  a5_view_update(mock_session, "map", cells, fill = vals, palette = "Viridis")
  msg <- captured$message
  expect_true(msg$fill_is_column)
  expect_true(msg$fill_per_cell)
  expect_true(msg$has_fill_value)
  # Arrow IPC should contain RGBA columns
  tbl <- arrow::read_ipc_stream(base64enc::base64decode(msg$arrow_ipc))
  expect_true("_fill_r" %in% names(tbl))
  expect_true("_fill_value" %in% names(tbl))
})

test_that("a5_view_update builds correct message with data frame", {
  cells <- make_cells(3)
  df <- data.frame(cell = cells, score = c(1.0, 2.0, 3.0))
  captured <- NULL
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      captured <<- list(type = type, message = message)
    }
  )
  a5_view_update(mock_session, "map", df, fill = score)
  msg <- captured$message
  expect_true(msg$fill_is_column)
  expect_equal(msg$domain, c(1, 3))
})

test_that("a5_view_update passes tooltip = FALSE", {
  cells <- make_cells(3)
  captured <- NULL
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      captured <<- list(type = type, message = message)
    }
  )
  a5_view_update(mock_session, "map", cells, tooltip = FALSE)
  expect_false(captured$message$tooltip)
})

test_that("a5_view_update validates cells", {
  mock_session <- list(sendCustomMessage = function(...) {})
  expect_error(a5_view_update(mock_session, "map", 1:10), "a5_cell")
})

test_that("a5_view_update returns invisible NULL", {
  cells <- make_cells(3)
  captured <- NULL
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      captured <<- list(type = type, message = message)
    }
  )
  result <- a5_view_update(mock_session, "map", cells)
  expect_null(result)
  expect_false(is.null(captured))
})
