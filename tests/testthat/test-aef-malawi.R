test_that("aef_malawi holds A5 1.0 resolution-15 cells inside its crop", {
  d <- aef_malawi
  expect_named(d, c("cell", "A10", "A11", "A12"))
  expect_true(a5R::is_a5_cell(d$cell))
  expect_false(anyNA(d$cell))
  # Pinned ids: catch a change in a5_cell's stored byte layout, or data
  # rebuilt with pre-1.0 ids.
  expect_identical(
    format(d$cell[1:3]),
    c("48a2c002a0000000", "48a2c00a60000000", "48a2c00b20000000")
  )
  expect_true(all(a5R::a5_get_resolution(d$cell) == 15L))
  xy <- unclass(a5R::a5_cell_to_lonlat(d$cell))
  expect_true(all(xy$x >= 33.20 & xy$x <= 33.45))
  expect_true(all(xy$y >= -13.05 & xy$y <= -12.80))
})
