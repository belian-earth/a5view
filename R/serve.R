# Serving pyramids to the browser: Parquet files and Arrow tiles

#' The current Shiny session, or NULL outside Shiny
#' @noRd
shiny_session <- function() {
  if (!rlang::is_installed("shiny")) return(NULL)
  shiny::getDefaultReactiveDomain()
}

#' Serve a parquet file to the browser over HTTP with Range support
#'
#' Registers a per-session data object with Shiny and returns a URL the
#' widget can fetch. Range requests are honoured, so the browser reads
#' only the parquet footer and the row groups the viewport needs, and
#' nothing is base64-encoded into the widget payload. The registration
#' lives with the session and is released when it ends.
#'
#' @param session A Shiny session (anything with a `registerDataObj()`
#'   method).
#' @param path Path to the parquet file. Must outlive the session's use
#'   of it; callers that write temp files should unlink them in
#'   `session$onSessionEnded()`.
#' @return List `list(url, bytes)`.
#' @noRd
serve_parquet_file <- function(session, path) {
  size <- file.info(path)$size
  name <- paste0("a5view-", substr(rlang::hash(c(path, size)), 1, 12))
  url <- session$registerDataObj(name, path, parquet_range_handler)
  list(url = url, bytes = size)
}

#' Shiny data-object handler serving byte ranges of a file
#'
#' @param path The registered data (the file path).
#' @param req Rook request environment. `HTTP_RANGE`, when present, is
#'   `bytes=<start>-<end>` (end inclusive, either bound optional).
#' @return A [shiny::httpResponse()].
#' @noRd
parquet_range_handler <- function(path, req) {
  size <- file.info(path)$size
  base_headers <- list(
    "Accept-Ranges" = "bytes",
    "Cache-Control" = "private, max-age=3600"
  )
  range <- req[["HTTP_RANGE"]]
  if (is.null(range) || !grepl("^bytes=", range)) {
    bytes <- readBin(path, "raw", n = size)
    return(shiny::httpResponse(
      200L, "application/octet-stream", bytes,
      c(base_headers, list("Content-Length" = as.character(size)))
    ))
  }
  spec <- strsplit(sub("^bytes=", "", range), "-", fixed = TRUE)[[1]]
  start <- if (nzchar(spec[1])) as.numeric(spec[1]) else NA
  end <- if (length(spec) > 1L && nzchar(spec[2])) as.numeric(spec[2]) else NA
  if (is.na(start)) {
    # Suffix range: last `end` bytes.
    start <- max(0, size - end)
    end <- size - 1
  } else if (is.na(end)) {
    end <- size - 1
  }
  end <- min(end, size - 1)
  if (start > end || start >= size) {
    return(shiny::httpResponse(
      416L, "text/plain", "Range Not Satisfiable",
      list("Content-Range" = paste0("bytes */", size))
    ))
  }
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  seek(con, where = start)
  bytes <- readBin(con, "raw", n = end - start + 1)
  shiny::httpResponse(
    206L, "application/octet-stream", bytes,
    c(base_headers, list(
      "Content-Range" = sprintf("bytes %.0f-%.0f/%.0f", start, end, size),
      "Content-Length" = as.character(length(bytes))
    ))
  )
}

#' Whether a session can serve data to the browser by URL
#' @noRd
can_serve <- function(session) {
  !is.null(session) && is.function(session$registerDataObj)
}

#' Encode a parquet file for the widget: served by URL inside Shiny,
#' inline base64 otherwise
#'
#' @param path Parquet file path. Inside Shiny it must outlive the
#'   session's use of it.
#' @param session Shiny session or `NULL`.
#' @return List with `pyramid`: `list(format = "parquet", url, bytes)`
#'   or `list(format = "parquet", b64)`.
#' @noRd
parquet_payload <- function(path, session = NULL) {
  if (can_serve(session)) {
    served <- serve_parquet_file(session, path)
    return(list(pyramid = c(list(format = "parquet"), served)))
  }
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  list(pyramid = list(format = "parquet", b64 = base64enc::base64encode(bytes)))
}

#' Encode an a5pyramid Arrow table for the widget
#'
#' Outside Shiny the table is one Arrow IPC stream, deflated and inlined
#' as base64; the browser reads the index from its schema metadata. Inside
#' Shiny each tile is its own deflated IPC stream, without the schema
#' metadata, held with the session and served one tile per request
#' (`&tile=<i>`, from 0); the index travels in the payload. Deflate here
#' is `memCompress(type = "gzip")`, the zlib format (RFC 1950) that the
#' browser's `DecompressionStream("deflate")` reads.
#'
#' The tiles are registered under the output's id, so a later pyramid for
#' the same output replaces them and the old ones are released.
#'
#' @param tbl Table from `pyramid_arrow()`.
#' @param session Shiny session or `NULL`.
#' @param output_id Shiny output id, or `NULL` to look it up.
#' @return `list(format = "arrow", b64)` or `list(format = "arrow", url,
#'   tiles, index, a5_index)`, where `index` is the index JSON.
#' @noRd
arrow_pyramid_payload <- function(tbl, session = NULL, output_id = NULL) {
  if (!can_serve(session)) {
    ipc <- arrow::write_to_raw(tbl, format = "stream")
    return(list(
      format = "arrow",
      b64 = base64enc::base64encode(memCompress(ipc, "gzip"))
    ))
  }
  meta <- tbl$metadata
  bare <- tbl$ReplaceSchemaMetadata(structure(list(), names = character()))
  tiles <- lapply(arrow::as_record_batch_reader(bare)$batches(), function(b) {
    memCompress(arrow::write_to_raw(b, format = "stream"), "gzip")
  })
  if (is.null(output_id)) output_id <- current_output_id(session)
  key <- if (is.null(output_id)) substr(rlang::hash(tiles), 1, 12) else output_id
  url <- session$registerDataObj(paste0("a5view-tiles-", key), tiles, arrow_tile_handler)
  list(
    format = "arrow",
    url = url,
    tiles = length(tiles),
    index = meta$a5pyramid,
    a5_index = meta$a5_index
  )
}

#' The id of the Shiny output being rendered, or `NULL`
#' @noRd
current_output_id <- function(session) {
  if (!rlang::is_installed("shiny")) return(NULL)
  info <- tryCatch(
    shiny::getCurrentOutputInfo(session = session),
    error = function(e) NULL
  )
  info$name
}

#' Shiny data-object handler serving one deflated Arrow tile
#'
#' @param tiles The registered data: a list of raw vectors, tile `i` at
#'   `tiles[[i + 1]]`.
#' @param req Rook request environment; `QUERY_STRING` holds `tile=<i>`.
#' @return A [shiny::httpResponse()].
#' @noRd
arrow_tile_handler <- function(tiles, req) {
  query <- shiny::parseQueryString(paste0("", req[["QUERY_STRING"]]))
  i <- suppressWarnings(as.integer(query$tile))
  if (length(i) != 1L || is.na(i) || i < 0L || i >= length(tiles)) {
    return(shiny::httpResponse(404L, "text/plain", "No such tile"))
  }
  bytes <- tiles[[i + 1L]]
  shiny::httpResponse(
    200L, "application/octet-stream", bytes,
    list(
      "Cache-Control" = "private, max-age=3600",
      "Content-Length" = as.character(length(bytes))
    )
  )
}
