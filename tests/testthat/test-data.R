# Tests for R/data.R — data preparation and view state

# --- Helper: create test cells ---
make_cell <- function(lon = 0, lat = 0, res = 5) {
  a5R::a5_lonlat_to_cell(lon, lat, resolution = res)
}

make_cells <- function(n = 3, res = 5) {
  lons <- seq(-1, 1, length.out = n)
  lats <- seq(-1, 1, length.out = n)
  a5R::a5_lonlat_to_cell(lons, lats, resolution = res)
}

# --- find_cell_column ---

test_that("find_cell_column finds a5_cell column", {
  cell <- make_cell()
  df <- data.frame(my_cell = cell, val = 1)
  expect_equal(find_cell_column(df), "my_cell")
})

test_that("find_cell_column errors when no a5_cell column", {
  df <- data.frame(x = 1, y = 2)
  expect_error(find_cell_column(df), "No.*a5_cell.*column")
})

# --- prepare_data with a5_cell vector ---

test_that("prepare_data handles bare a5_cell vector", {
  cells <- make_cells(3)
  result <- prepare_data(cells)

  expect_s3_class(result$data, "data.frame")
  expect_true("pentagon" %in% names(result$data))
  expect_equal(nrow(result$data), 3)
  expect_true(all(nchar(result$data$pentagon) == 16))
  expect_equal(result$extra, list())
})

test_that("prepare_data filters NA cells", {
  cells <- make_cells(3)
  cells[2] <- a5R::a5_cell(NA_character_)
  result <- prepare_data(cells)

  expect_equal(nrow(result$data), 2)
  expect_equal(sum(result$not_na), 2)
})

# --- prepare_data with data frame ---

test_that("prepare_data handles data frame with a5_cell column", {
  cells <- make_cells(3)
  df <- data.frame(cell = cells, value = c(10, 20, 30))
  result <- prepare_data(df)

  expect_equal(nrow(result$data), 3)
  expect_true("pentagon" %in% names(result$data))
  expect_false("cell" %in% names(result$data))
  expect_equal(result$extra$value, c(10, 20, 30))
})

test_that("prepare_data preserves multiple extra columns", {
  cells <- make_cells(2)
  df <- data.frame(cell = cells, val_a = c(1, 2), val_b = c(3, 4))
  result <- prepare_data(df)

  expect_equal(names(result$extra), c("val_a", "val_b"))
  expect_equal(result$extra$val_a, c(1, 2))
  expect_equal(result$extra$val_b, c(3, 4))
})

test_that("prepare_data filters NAs from data frame and extras", {
  cells <- make_cells(3)
  cells[2] <- a5R::a5_cell(NA_character_)
  df <- data.frame(cell = cells, value = c(10, 20, 30))
  result <- prepare_data(df)

  expect_equal(nrow(result$data), 2)
  expect_equal(result$extra$value, c(10, 30))
})

# --- resolve_elevation_col ---

test_that("resolve_elevation_col returns NULL for non-name expression", {
  cells <- make_cells(2)
  df <- data.frame(cell = cells, elev = c(1, 2))
  expect_null(resolve_elevation_col(df, quote(NULL)))
})

test_that("resolve_elevation_col returns NULL for non-data-frame", {
  cells <- make_cells(2)
  expect_null(resolve_elevation_col(cells, quote(elev)))
})

test_that("resolve_elevation_col finds valid column", {
  cells <- make_cells(2)
  df <- data.frame(cell = cells, elev = c(1, 2))
  expect_equal(resolve_elevation_col(df, quote(elev)), "elev")
})

test_that("resolve_elevation_col errors on missing column", {
  cells <- make_cells(2)
  df <- data.frame(cell = cells, val = c(1, 2))
  expect_error(resolve_elevation_col(df, quote(elev)), "not found")
})

# --- guess_zoom ---

test_that("guess_zoom returns 10 for single point", {
  coords <- data.frame(lon = 0, lat = 0)
  expect_equal(guess_zoom(coords), 10L)
})

test_that("guess_zoom returns reasonable zoom for small area", {
  coords <- data.frame(lon = c(-0.1, 0.1), lat = c(-0.1, 0.1))
  z <- guess_zoom(coords)
  expect_true(z >= 1L && z <= 18L)
  expect_true(z >= 8L)
})

test_that("guess_zoom returns low zoom for large area", {
  coords <- data.frame(lon = c(-180, 180), lat = c(-90, 90))
  z <- guess_zoom(coords)
  expect_equal(z, 1L)
})

test_that("guess_zoom is bounded between 1 and 18", {
  # Very small span
  coords <- data.frame(lon = c(0, 0.0001), lat = c(0, 0.0001))
  z <- guess_zoom(coords)
  expect_true(z <= 18L)

  # Huge span
  coords <- data.frame(lon = c(-180, 180), lat = c(-90, 90))
  z <- guess_zoom(coords)
  expect_true(z >= 1L)
})

# --- auto_view ---

test_that("auto_view uses provided lng/lat/zoom when all given", {
  result <- auto_view("0000000000000000", lng = 10, lat = 20, zoom = 5)
  expect_equal(result$longitude, 10)
  expect_equal(result$latitude, 20)
  expect_equal(result$zoom, 5)
  expect_equal(result$pitch, 0)
  expect_equal(result$bearing, 0)
})

test_that("auto_view computes centroid from hex IDs", {
  cells <- make_cells(5, res = 5)
  hex_ids <- format(cells)
  result <- auto_view(hex_ids)

  expect_true(is.numeric(result$longitude))
  expect_true(is.numeric(result$latitude))
  expect_true(is.numeric(result$zoom))
  expect_equal(result$pitch, 0)
  expect_equal(result$bearing, 0)
})

test_that("auto_view respects partial overrides", {
  cells <- make_cells(5, res = 5)
  hex_ids <- format(cells)

  result <- auto_view(hex_ids, lng = 42)
  expect_equal(result$longitude, 42)
  expect_true(result$latitude != 42)  # computed, not overridden

  result2 <- auto_view(hex_ids, zoom = 3)
  expect_equal(result2$zoom, 3)
})
