test_that("s2_santiago holds A5 1.0 resolution-18 cells inside its crop", {
  d <- s2_santiago
  expect_named(d, c("cell", "B02", "B03", "B04", "B08"))
  expect_true(a5R::is_a5_cell(d$cell))
  expect_false(anyNA(d$cell))
  # Pinned ids: catch a change in a5_cell's stored byte layout, or data
  # rebuilt with pre-1.0 ids.
  expect_identical(
    format(d$cell[1:3]),
    c("32b5c12040800000", "32b5c12041800000", "32b5c12042800000")
  )
  expect_true(all(a5R::a5_get_resolution(d$cell) == 18L))
  xy <- unclass(a5R::a5_cell_to_lonlat(d$cell))
  expect_true(all(xy$x >= -70.672 & xy$x <= -70.642))
  expect_true(all(xy$y >= -33.480 & xy$y <= -33.455))
})
