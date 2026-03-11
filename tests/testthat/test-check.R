# Tests for R/check.R — validation helpers

test_that("check_cells rejects non-cell, non-data-frame input", {
  expect_error(check_cells(1:10), "a5_cell")
  expect_error(check_cells("abc"), "a5_cell")
  expect_error(check_cells(list()), "a5_cell")
})

test_that("check_cells rejects empty a5_cell vector", {
  empty <- a5R::a5_cell(character(0))
  expect_error(check_cells(empty), "length > 0")
})

test_that("check_cells rejects empty data frame", {
  df <- data.frame(x = numeric(0))
  expect_error(check_cells(df), "at least one row")
})

test_that("check_cells accepts valid a5_cell vector", {
  cell <- a5R::a5_lonlat_to_cell(0, 0, resolution = 5)
  expect_no_error(check_cells(cell))
})

test_that("check_cells accepts valid data frame", {
  cell <- a5R::a5_lonlat_to_cell(0, 0, resolution = 5)
  df <- data.frame(cell = cell, val = 1)
  expect_no_error(check_cells(df))
})

test_that("check_number_decimal validates type", {
  expect_error(check_number_decimal("a", arg = "x"), "single number")
  expect_error(check_number_decimal(TRUE, arg = "x"), "single number")
  expect_error(check_number_decimal(c(1, 2), arg = "x"), "single number")
  expect_error(check_number_decimal(NULL, arg = "x"), "single number")
})

test_that("check_number_decimal rejects NA", {
  expect_error(check_number_decimal(NA_real_, arg = "x"), "NA")
})

test_that("check_number_decimal validates range", {
  expect_error(check_number_decimal(-0.1, min = 0, max = 1, arg = "x"), "between")
  expect_error(check_number_decimal(1.5, min = 0, max = 1, arg = "x"), "between")
  expect_error(check_number_decimal(-1, min = 0, arg = "x"), ">= 0")
})

test_that("check_number_decimal accepts valid values", {
  expect_no_error(check_number_decimal(0.5, min = 0, max = 1, arg = "x"))
  expect_no_error(check_number_decimal(0L, min = 0, arg = "x"))
  expect_no_error(check_number_decimal(100, arg = "x"))
})

test_that("check_optional_number allows NULL", {
  expect_no_error(check_optional_number(NULL, "x"))
})

test_that("check_optional_number validates non-NULL", {
  expect_error(check_optional_number("a", "x"), "single number")
  expect_no_error(check_optional_number(3.5, "x"))
})

test_that("check_optional_dimension allows NULL", {
  expect_no_error(check_optional_dimension(NULL, "w"))
})

test_that("check_optional_dimension accepts valid CSS strings", {
  expect_no_error(check_optional_dimension("100%", "w"))
  expect_no_error(check_optional_dimension("400px", "w"))
  expect_no_error(check_optional_dimension("50vh", "w"))
  expect_no_error(check_optional_dimension("10em", "w"))
})

test_that("check_optional_dimension accepts numeric", {
  expect_no_error(check_optional_dimension(400L, "w"))
  expect_no_error(check_optional_dimension(400, "w"))
})

test_that("check_optional_dimension rejects invalid values", {
  expect_error(check_optional_dimension("big", "w"), "CSS string")
  expect_error(check_optional_dimension(TRUE, "w"), "CSS string")
})

test_that("check_border allows NULL", {
  expect_no_error(check_border(NULL))
})

test_that("check_border accepts valid colours", {
  expect_no_error(check_border("white"))
  expect_no_error(check_border("#ff0000"))
  expect_no_error(check_border("#74ac9080"))
})

test_that("check_border rejects non-string", {
  expect_error(check_border(123), "single colour string")
  expect_error(check_border(c("red", "blue")), "single colour string")
})

test_that("check_border rejects invalid colour", {
  expect_error(check_border("notacolour"), "not valid")
})

test_that("check_basemap accepts valid values", {
  expect_no_error(check_basemap("dark"))
  expect_no_error(check_basemap(c("dark", "light")))
  expect_no_error(check_basemap(c("dark", "light", "osm", "satellite")))
  expect_no_error(check_basemap("none"))
})

test_that("check_basemap rejects invalid values", {
  expect_error(check_basemap("mapbox"), "invalid value")
  expect_error(check_basemap(c("dark", "fancy")), "invalid value")
  expect_error(check_basemap(123), "character vector")
})

test_that("check_tooltip accepts valid values", {
  expect_no_error(check_tooltip(TRUE))
  expect_no_error(check_tooltip(FALSE))
  expect_no_error(check_tooltip(c("col1", "col2")))
})

test_that("check_tooltip rejects invalid values", {
  expect_error(check_tooltip(123), "logical")
  expect_error(check_tooltip(NULL), "logical")
  expect_error(check_tooltip(list()), "logical")
})

test_that("check_palette accepts valid palette names", {
  expect_no_error(check_palette("Viridis"))
  expect_no_error(check_palette("Inferno"))
  expect_no_error(check_palette("Plasma"))
  expect_no_error(check_palette("Blues 2"))
})

test_that("check_palette rejects invalid palette name with suggestion", {
  expect_error(check_palette("viridiz"), "Did you mean")
})

test_that("check_palette accepts custom colour vector", {
  expect_no_error(check_palette(c("#ff0000", "#00ff00", "#0000ff")))
  expect_no_error(check_palette(c("red", "blue")))
})

test_that("check_palette rejects invalid custom colours", {
  expect_error(check_palette(c("#ff0000", "notacolour")), "Invalid colour")
})

test_that("check_palette rejects non-character", {
  expect_error(check_palette(42), "palette name")
  expect_error(check_palette(TRUE), "palette name")
})
