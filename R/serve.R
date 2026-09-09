# Serving pyramid parquet files to the browser inside Shiny

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

#' Encode a parquet file for the widget: served by URL inside Shiny,
#' inline base64 otherwise
#'
#' @param path Parquet file path.
#' @param session Shiny session or `NULL`.
#' @param temp Logical; when `TRUE` the file is a temp file owned by
#'   the widget and is unlinked when the session ends (Shiny) or right
#'   away after encoding (inline).
#' @return List with `parquet` (either `list(url, bytes)` or
#'   `list(b64)`).
#' @noRd
parquet_payload <- function(path, session = NULL, temp = FALSE) {
  if (!is.null(session) && is.function(session$registerDataObj)) {
    if (temp && is.function(session$onSessionEnded)) {
      session$onSessionEnded(function() unlink(path))
    }
    return(list(parquet = serve_parquet_file(session, path)))
  }
  if (temp) on.exit(unlink(path), add = TRUE)
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  list(parquet = list(b64 = base64enc::base64encode(bytes)))
}
