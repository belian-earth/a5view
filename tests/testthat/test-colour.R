# Tests for R/colour.R — fill, palette, and colour resolution

# --- hex_to_rgba ---

test_that("hex_to_rgba converts hex to integer RGBA", {
  result <- hex_to_rgba("#ff0000")
  expect_equal(result, c(255L, 0L, 0L, 255L))
})

test_that("hex_to_rgba handles alpha channel", {
  result <- hex_to_rgba("#ff000080")
  expect_equal(result, c(255L, 0L, 0L, 128L))
})

test_that("hex_to_rgba handles named colours", {
  result <- hex_to_rgba("white")
  expect_equal(result, c(255L, 255L, 255L, 255L))
})

# --- resolve_palette ---

test_that("resolve_palette generates colours from named palette", {
  result <- resolve_palette("Viridis", n = 5L)
  expect_length(result, 5)
  expect_true(all(grepl("^#", result)))
})

test_that("resolve_palette passes through custom colours", {
  custom <- c("#ff0000", "#00ff00", "#0000ff")

  result <- resolve_palette(custom, n = 8L)
  expect_identical(result, custom)
})

# --- resolve_fill ---

test_that("resolve_fill detects uniform colour", {
  result <- resolve_fill(NULL, "#3388ff", quote("#3388ff"), 10)
  expect_equal(result$type, "uniform")
  expect_equal(result$value, "#3388ff")
})

test_that("resolve_fill detects numeric vector", {
  vals <- 1:5
  result <- resolve_fill(NULL, vals, quote(vals), 5)
  expect_equal(result$type, "numeric")
  expect_equal(result$values, 1:5)
})

test_that("resolve_fill rejects single number", {
  expect_error(resolve_fill(NULL, 42, quote(42), 10), "single number")
})

test_that("resolve_fill rejects mismatched numeric length", {
  expect_error(resolve_fill(NULL, 1:3, quote(x), 5), "length 3")
})

test_that("resolve_fill rejects all-NA numeric", {
  expect_error(
    resolve_fill(NULL, rep(NA_real_, 3), quote(x), 3),
    "all.*NA"
  )
})

test_that("resolve_fill detects colour vector", {
  cols <- c("#ff0000", "#00ff00", "#0000ff")
  result <- resolve_fill(NULL, cols, quote(cols), 3)
  expect_equal(result$type, "colors")
})

test_that("resolve_fill rejects mismatched colour vector length", {
  cols <- c("#ff0000", "#00ff00")
  expect_error(resolve_fill(NULL, cols, quote(cols), 5), "length 2")
})

test_that("resolve_fill rejects invalid colour in vector", {
  cols <- c("#ff0000", "notacolour", "#0000ff")
  expect_error(resolve_fill(NULL, cols, quote(cols), 3), "Invalid colour")
})

test_that("resolve_fill rejects invalid single colour", {
  expect_error(resolve_fill(NULL, "notacolour", quote("notacolour"), 1), "not valid")
})

test_that("resolve_fill detects column name in data frame", {
  cell <- a5R::a5_lonlat_to_cell(0, 0, resolution = 5)
  df <- data.frame(cell = cell, value = 1.0)
  result <- resolve_fill(df, df$value, quote(value), 1)
  expect_equal(result$type, "column")
  expect_equal(result$col, "value")
})

test_that("resolve_fill errors on missing column name", {
  cell <- a5R::a5_lonlat_to_cell(0, 0, resolution = 5)
  df <- data.frame(cell = cell, value = 1.0)
  expect_error(resolve_fill(df, NULL, quote(missing_col), 1), "not found")
})

test_that("resolve_fill rejects non-colour/non-numeric types", {
  expect_error(resolve_fill(NULL, TRUE, quote(TRUE), 1), "colour string")
})

# --- attach_fill ---

test_that("attach_fill handles uniform fill", {
  df <- data.frame(pentagon = "abc", stringsAsFactors = FALSE)
  prepared <- list(data = df, not_na = TRUE, extra = list())
  fill_resolved <- list(type = "uniform", value = "#ff0000")

  result <- attach_fill(df, fill_resolved, prepared, "Viridis")
  expect_false(result$fill_is_column)
  expect_equal(result$fill_color, c(255L, 0L, 0L, 255L))
  expect_null(result$js_palette)
  expect_null(result$domain)
})

test_that("attach_fill handles numeric fill", {
  df <- data.frame(pentagon = c("a", "b", "c"), stringsAsFactors = FALSE)
  prepared <- list(
    data = df,
    not_na = c(TRUE, TRUE, TRUE),
    extra = list()
  )
  fill_resolved <- list(type = "numeric", values = c(1, 2, 3))

  result <- attach_fill(df, fill_resolved, prepared, "Viridis")
  expect_true(result$fill_is_column)
  expect_equal(result$df[["_fill_value"]], c(1, 2, 3))
  expect_equal(result$domain, c(1, 3))
  expect_length(result$js_palette, 8)
})

test_that("attach_fill handles column fill", {
  df <- data.frame(pentagon = c("a", "b"), stringsAsFactors = FALSE)
  prepared <- list(
    data = df,
    not_na = c(TRUE, TRUE),
    extra = list(value = c(10, 20))
  )
  fill_resolved <- list(type = "column", col = "value")

  result <- attach_fill(df, fill_resolved, prepared, "Viridis")
  expect_true(result$fill_is_column)
  expect_equal(result$df[["_fill_value"]], c(10, 20))
  expect_equal(result$domain, c(10, 20))
})

test_that("attach_fill rejects non-numeric column", {
  df <- data.frame(pentagon = "a", stringsAsFactors = FALSE)
  prepared <- list(
    data = df,
    not_na = TRUE,
    extra = list(label = "foo")
  )
  fill_resolved <- list(type = "column", col = "label")

  expect_error(attach_fill(df, fill_resolved, prepared, "Viridis"), "numeric")
})

test_that("attach_fill errors on missing column", {
  df <- data.frame(pentagon = "a", stringsAsFactors = FALSE)
  prepared <- list(data = df, not_na = TRUE, extra = list())
  fill_resolved <- list(type = "column", col = "missing")

  expect_error(attach_fill(df, fill_resolved, prepared, "Viridis"), "not found")
})
