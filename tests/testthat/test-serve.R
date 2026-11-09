# Tests for R/serve.R: encoding pyramids for the widget and serving them inside Shiny

make_file <- function(n = 1000L) {
  path <- tempfile(fileext = ".bin")
  writeBin(as.raw(seq_len(n) %% 256L), path)
  path
}

# --- parquet_range_handler ---

test_that("range handler serves the whole file without a Range header", {
  skip_if_not_installed("shiny")
  path <- make_file(1000L)
  res <- parquet_range_handler(path, list())
  expect_equal(res$status, 200L)
  expect_equal(length(res$content), 1000L)
  expect_equal(res$headers[["Accept-Ranges"]], "bytes")
  expect_equal(res$headers[["Content-Length"]], "1000")
})

test_that("range handler serves a closed byte range", {
  skip_if_not_installed("shiny")
  path <- make_file(1000L)
  res <- parquet_range_handler(path, list(HTTP_RANGE = "bytes=10-19"))
  expect_equal(res$status, 206L)
  expect_equal(res$content, as.raw((11:20) %% 256L))
  expect_equal(res$headers[["Content-Range"]], "bytes 10-19/1000")
  expect_equal(res$headers[["Content-Length"]], "10")
})

test_that("range handler serves open-ended and suffix ranges", {
  skip_if_not_installed("shiny")
  path <- make_file(1000L)
  tail_res <- parquet_range_handler(path, list(HTTP_RANGE = "bytes=990-"))
  expect_equal(tail_res$status, 206L)
  expect_equal(length(tail_res$content), 10L)
  expect_equal(tail_res$headers[["Content-Range"]], "bytes 990-999/1000")

  suffix_res <- parquet_range_handler(path, list(HTTP_RANGE = "bytes=-8"))
  expect_equal(suffix_res$status, 206L)
  expect_equal(suffix_res$content, as.raw((993:1000) %% 256L))
  expect_equal(suffix_res$headers[["Content-Range"]], "bytes 992-999/1000")
})

test_that("range handler clamps past-the-end and rejects unsatisfiable ranges", {
  skip_if_not_installed("shiny")
  path <- make_file(100L)
  res <- parquet_range_handler(path, list(HTTP_RANGE = "bytes=90-500"))
  expect_equal(res$status, 206L)
  expect_equal(length(res$content), 10L)

  bad <- parquet_range_handler(path, list(HTTP_RANGE = "bytes=500-600"))
  expect_equal(bad$status, 416L)
  expect_equal(bad$headers[["Content-Range"]], "bytes */100")
})

# --- parquet_payload ---

test_that("parquet_payload inlines base64 outside Shiny", {
  path <- make_file(64L)
  out <- parquet_payload(path)
  expect_identical(out$pyramid$format, "parquet")
  expect_null(out$pyramid$url)
  expect_equal(base64enc::base64decode(out$pyramid$b64), as.raw((1:64) %% 256L))
  expect_true(file.exists(path))
})

test_that("parquet_payload registers a served URL with a Shiny session", {
  path <- make_file(64L)
  s <- mock_session()
  out <- parquet_payload(path, session = s)
  expect_identical(out$pyramid$format, "parquet")
  expect_match(out$pyramid$url, "^session/x/dataobj/a5view-")
  expect_equal(out$pyramid$bytes, 64)
  expect_null(out$pyramid$b64)
  reg <- s$env$registered[[1]]
  expect_equal(reg$data, path)
  expect_identical(reg$filter, parquet_range_handler)
  expect_length(s$env$ended, 0L)
})

test_that("parquet_payload falls back to inline for sessions without registerDataObj", {
  path <- make_file(64L)
  out <- parquet_payload(path, session = list(sendCustomMessage = function(...) NULL))
  expect_true(nzchar(out$pyramid$b64))
})

# --- arrow_pyramid_payload ---

small_cells <- function() {
  unique(a5R::a5_lonlat_to_cell(
    seq(-1, 1, length.out = 20), seq(-1, 1, length.out = 20), resolution = 6
  ))
}

small_pyramid <- function() {
  cells <- small_cells()
  prep <- prepare_view_data(cells, rlang::quo(seq_along(cells)), FALSE, "Viridis")
  pyramid_arrow(prep, "rep_child", 1L)$table
}

tile_request <- function(query) list(QUERY_STRING = query)

test_that("arrow_pyramid_payload inlines one deflated IPC stream outside Shiny", {
  tbl <- small_pyramid()
  out <- arrow_pyramid_payload(tbl)
  expect_identical(out$format, "arrow")
  expect_null(out$url)
  ipc <- memDecompress(base64enc::base64decode(out$b64), "gzip")
  back <- arrow::read_ipc_stream(ipc, as_data_frame = FALSE)
  expect_identical(back$metadata$a5pyramid, tbl$metadata$a5pyramid)
  expect_identical(back$metadata$a5_index, "1.0")
  expect_identical(
    length(arrow::as_record_batch_reader(back)$batches()),
    length(arrow::as_record_batch_reader(tbl)$batches())
  )
})

test_that("arrow_pyramid_payload serves one tile per request inside Shiny", {
  tbl <- small_pyramid()
  batches <- arrow::as_record_batch_reader(tbl)$batches()
  s <- mock_session()
  out <- arrow_pyramid_payload(tbl, session = s, output_id = "map")
  expect_identical(out$format, "arrow")
  expect_match(out$url, "^session/x/dataobj/a5view-tiles-map\\?")
  expect_identical(out$tiles, length(batches))
  expect_identical(out$index, tbl$metadata$a5pyramid)
  expect_identical(out$a5_index, "1.0")
  expect_null(out$b64)

  reg <- s$env$registered[["a5view-tiles-map"]]
  expect_identical(reg$filter, arrow_tile_handler)
  last <- length(batches) - 1L
  res <- reg$filter(reg$data, tile_request(paste0("?w=&nonce=1&tile=", last)))
  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Length"]], as.character(length(res$content)))
  tile <- arrow::read_ipc_stream(memDecompress(res$content, "gzip"), as_data_frame = FALSE)
  # the tile is the last record batch, without the index
  expect_length(tile$metadata, 0L)
  expect_true(tile$Equals(arrow::Table$create(batches[[last + 1L]])$ReplaceSchemaMetadata(
    stats::setNames(list(), character())
  )))
})

test_that("arrow_tile_handler refuses missing and out-of-range tiles", {
  tiles <- list(as.raw(1:3), as.raw(4:6))
  expect_equal(arrow_tile_handler(tiles, tile_request("?tile=1"))$content, as.raw(4:6))
  for (q in c("", "?w=", "?tile=2", "?tile=-1", "?tile=x")) {
    expect_equal(arrow_tile_handler(tiles, tile_request(q))$status, 404L, label = q)
  }
  expect_equal(arrow_tile_handler(tiles, list())$status, 404L)
})

test_that("a later pyramid for an output replaces the earlier one", {
  s <- mock_session()
  arrow_pyramid_payload(small_pyramid(), session = s, output_id = "map")
  arrow_pyramid_payload(small_pyramid(), session = s, output_id = "map")
  expect_named(s$env$registered, "a5view-tiles-map")
  # without an output id, pyramids are named by their content
  arrow_pyramid_payload(small_pyramid(), session = s)
  expect_length(s$env$registered, 2L)
})

# --- integration through the exported functions ---

test_that("a5_view_update serves the pyramid's tiles by URL when the session can", {
  cells <- small_cells()
  captured <- NULL
  s <- mock_session()
  s$sendCustomMessage <- function(type, message) captured <<- message
  a5_view_update(s, "map", cells, fill = seq_along(cells), aggregate = "rep_child")
  expect_identical(captured$pyramid$format, "arrow")
  expect_match(captured$pyramid$url, "dataobj/a5view-tiles-map")
  expect_true(captured$pyramid$tiles > 0)
  expect_null(captured$arrow_ipc)
  expect_true(length(captured$lod_resolutions) > 1L)
})

test_that("a5_view inlines the pyramid outside Shiny", {
  w <- a5_view(small_cells(), aggregate = "mean")
  expect_identical(w$x$pyramid$format, "arrow")
  expect_true(nzchar(w$x$pyramid$b64))
  expect_null(w$x$pyramid$url)
  expect_null(w$x$arrow_ipc)
})

test_that("a5_view_pyramid still reads the Parquet file", {
  path <- withr::local_tempfile(fileext = ".parquet")
  a5_build_pyramid(small_cells(), path, aggregate = "mean")
  w <- a5_view_pyramid(path)
  expect_identical(w$x$pyramid$format, "parquet")
  expect_true(nzchar(w$x$pyramid$b64))
})
