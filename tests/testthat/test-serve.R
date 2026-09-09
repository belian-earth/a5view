# Tests for R/serve.R: serving pyramid parquet files inside Shiny

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

mock_session <- function() {
  env <- new.env()
  env$registered <- list()
  env$ended <- list()
  list(
    registerDataObj = function(name, data, filter) {
      env$registered[[name]] <- list(data = data, filter = filter)
      paste0("session/x/dataobj/", name)
    },
    onSessionEnded = function(fn) env$ended <- c(env$ended, fn),
    env = env
  )
}

test_that("parquet_payload inlines base64 outside Shiny", {
  path <- make_file(64L)
  out <- parquet_payload(path)
  expect_null(out$parquet$url)
  expect_equal(base64enc::base64decode(out$parquet$b64), as.raw((1:64) %% 256L))
  expect_true(file.exists(path))
})

test_that("parquet_payload unlinks temp files after inlining", {
  path <- make_file(64L)
  out <- parquet_payload(path, temp = TRUE)
  expect_true(nzchar(out$parquet$b64))
  expect_false(file.exists(path))
})

test_that("parquet_payload registers a served URL with a Shiny session", {
  path <- make_file(64L)
  s <- mock_session()
  out <- parquet_payload(path, session = s)
  expect_match(out$parquet$url, "^session/x/dataobj/a5view-")
  expect_equal(out$parquet$bytes, 64)
  expect_null(out$parquet$b64)
  reg <- s$env$registered[[1]]
  expect_equal(reg$data, path)
  expect_identical(reg$filter, parquet_range_handler)
  expect_length(s$env$ended, 0L)
})

test_that("parquet_payload defers temp-file cleanup to session end", {
  path <- make_file(64L)
  s <- mock_session()
  parquet_payload(path, session = s, temp = TRUE)
  expect_true(file.exists(path))
  expect_length(s$env$ended, 1L)
  s$env$ended[[1]]()
  expect_false(file.exists(path))
})

test_that("parquet_payload falls back to inline for sessions without registerDataObj", {
  path <- make_file(64L)
  out <- parquet_payload(path, session = list(sendCustomMessage = function(...) NULL))
  expect_true(nzchar(out$parquet$b64))
})

# --- integration through the exported functions ---

test_that("a5_view_update serves the pyramid by URL when the session can", {
  cells <- a5R::a5_lonlat_to_cell(seq(-1, 1, length.out = 20), seq(-1, 1, length.out = 20), resolution = 6)
  captured <- NULL
  s <- mock_session()
  s$sendCustomMessage <- function(type, message) captured <<- message
  a5_view_update(s, "map", cells, fill = seq_len(20), aggregate = "rep_child")
  expect_match(captured$parquet$url, "dataobj")
  expect_true(captured$parquet$bytes > 0)
  expect_null(captured$arrow_ipc)
  expect_true(length(captured$lod_resolutions) > 1L)
})

test_that("a5_view inlines the pyramid outside Shiny", {
  cells <- a5R::a5_lonlat_to_cell(seq(-1, 1, length.out = 20), seq(-1, 1, length.out = 20), resolution = 6)
  w <- a5_view(cells, aggregate = "mean")
  expect_true(nzchar(w$x$parquet$b64))
  expect_null(w$x$parquet$url)
})
