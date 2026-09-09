# Data preparation and view state computation

#' Find the a5_cell column in a data frame
#' @noRd
find_cell_column <- function(df) {
  for (nm in names(df)) {
    if (a5R::is_a5_cell(df[[nm]])) return(nm)
  }
  cli::cli_abort(
    "No {.cls a5_cell} column found in {.arg cells}. Columns present: {.val {names(df)}}."
  )
}

#' Split the input into cells, payload frame and extra columns
#'
#' Cells are carried as an `a5_cell` column named `cell` so every
#' downstream step (grouping, Arrow conversion, view centring) works on
#' the 8-byte ids directly; hex strings are never formed for the bulk
#' data.
#' @return List with `data` (data frame with a `cell` column), `not_na`
#'   (logical), `extra` (named list of non-cell columns) and `a5_cells`
#'   (the non-NA `a5_cell` vector, identical to `data$cell`).
#' @noRd
prepare_data <- function(cells) {
  if (a5R::is_a5_cell(cells)) {
    cell_col <- NULL
    cell_vec <- cells
    other_cols <- character()
  } else {
    cell_col <- find_cell_column(cells)
    cell_vec <- cells[[cell_col]]
    other_cols <- setdiff(names(cells), cell_col)
  }
  not_na <- !is.na(cell_vec)
  keep <- cell_vec[not_na]

  extra <- list()
  if (length(other_cols) > 0L) {
    extra <- lapply(other_cols, function(nm) cells[[nm]][not_na])
    names(extra) <- other_cols
  }

  df <- data.frame(row.names = seq_along(keep))
  df[["cell"]] <- keep
  list(data = df, not_na = not_na, extra = extra, a5_cells = keep)
}

#' Build a pre-aggregated multi-resolution pyramid for a5_view rendering
#'
#' Given the leaf-level cells and their per-cell payload (RGBA, optional
#' fill value, optional elevation), produce a long data frame containing
#' the leaf rows plus aggregated rows at each parent LOD. Each row carries
#' a `_lod` column tagging its resolution. The JS side picks the LOD
#' matching the current zoom and renders that subset, avoiding any
#' per-render aggregation.
#'
#' Aggregation strategies:
#'   - "rep_child": for each parent group, find the leaf child whose
#'     payload is closest (Euclidean) to the parent's mean, and use that
#'     leaf's row verbatim. Preserves real values, no blending.
#'   - "mean": independent mean of every numeric payload column within
#'     each parent group. RGBA channels are averaged then rounded back to
#'     uint8.
#'
#' Grouping uses `vctrs::vec_group_id()` on the parent `a5_cell` vectors
#' (hashed on the raw bytes), which is an order of magnitude cheaper than
#' formatting hex keys.
#'
#' @param leaf_cells `a5_cell` vector at the data resolution.
#' @param df Data frame with a `cell` column plus optional
#'   `_fill_r/g/b/a`, `_fill_value`, `_elevation` columns.
#' @param data_resolution Integer A5 resolution of `leaf_cells`.
#' @param lod_step Integer >= 1, gap between successive LODs.
#' @param aggregate One of `"rep_child"`, `"mean"`.
#' @param min_lod Lowest LOD to precompute (default 2).
#' @return A list with `data` (long data frame with `cell`, `_lod` and
#'   the payload columns) and `lod_resolutions` (sorted ascending
#'   integer vector).
#' @noRd
build_a5_pyramid <- function(leaf_cells, df, data_resolution, lod_step,
                             aggregate, min_lod = 2L) {
  data_resolution <- as.integer(data_resolution)
  lod_step <- as.integer(lod_step)
  min_lod <- as.integer(min_lod)
  aggregate <- match.arg(aggregate, c("rep_child", "mean"))

  agg_cols <- intersect(
    c("_fill_r", "_fill_g", "_fill_b", "_fill_a", "_fill_value", "_elevation"),
    names(df)
  )
  has_rgba <- "_fill_r" %in% agg_cols
  cols_order <- c("cell", "_lod", agg_cols)

  leaf_rows <- df[, intersect(cols_order, names(df)), drop = FALSE]
  leaf_rows[["cell"]] <- leaf_cells
  leaf_rows[["_lod"]] <- data_resolution
  leaf_rows <- leaf_rows[, cols_order, drop = FALSE]

  parent_lods <- integer()
  if (data_resolution > min_lod && lod_step >= 1L) {
    parent_lods <- seq.int(data_resolution - lod_step, min_lod, by = -lod_step)
    parent_lods <- parent_lods[parent_lods >= min_lod]
  }
  if (length(parent_lods) == 0L) {
    return(list(data = leaf_rows, lod_resolutions = data_resolution))
  }

  vals <- NULL
  if (length(agg_cols) > 0L) {
    vals <- as.matrix(df[, agg_cols, drop = FALSE])
    storage.mode(vals) <- "double"
  }

  chunks <- vector("list", length(parent_lods))
  for (k in seq_along(parent_lods)) {
    lod <- parent_lods[[k]]
    parents <- a5R::a5_cell_to_parent(leaf_cells, resolution = lod)
    group <- vctrs::vec_group_id(parents)
    n_groups <- attr(group, "n")

    if (is.null(vals)) {
      # No payload: one row per parent, taken from its first child.
      pick_idx <- match(seq_len(n_groups), group)
      out <- data.frame(row.names = seq_len(n_groups))
    } else {
      sums <- rowsum(vals, group, reorder = FALSE, na.rm = TRUE)
      # rowsum(reorder = FALSE) keeps first-seen order, which matches
      # vec_group_id numbering.
      counts <- tabulate(group, nbins = n_groups)
      means <- sums / counts
      if (aggregate == "mean") {
        pick_idx <- match(seq_len(n_groups), group)
        out <- as.data.frame(means)
        names(out) <- agg_cols
      } else {
        diffs <- vals - means[group, , drop = FALSE]
        sq_dist <- rowSums(diffs * diffs)
        ord <- order(group, sq_dist)
        # First row of each group in (group, distance) order is the
        # child closest to its parent's mean.
        pick_idx <- ord[!duplicated(group[ord])]
        out <- df[pick_idx, agg_cols, drop = FALSE]
        rownames(out) <- NULL
      }
      if (has_rgba) {
        for (ch in c("_fill_r", "_fill_g", "_fill_b", "_fill_a")) {
          out[[ch]] <- as.integer(round(pmin(255, pmax(0, out[[ch]]))))
        }
      }
    }

    out[["cell"]] <- parents[pick_idx]
    out[["_lod"]] <- as.integer(lod)
    chunks[[k]] <- out[, cols_order, drop = FALSE]
  }

  list(
    data = vctrs::vec_rbind(leaf_rows, !!!chunks),
    lod_resolutions = sort(c(data_resolution, parent_lods))
  )
}

#' Serialise a pyramid table to parquet with a spatial row-group index
#'
#' Row groups are planned per LOD: small LODs collapse to one row group,
#' large LODs are bucketed by the parent cell at `lod - pivot_offset` so
#' each row group covers a compact region. A JSON index of per-row-group
#' `{rg, lod_min, lod_max, west, south, east, north, tile_id?}` is stored
#' in the schema KV metadata; the browser reads it up front and decodes
#' only the row groups whose LOD matches and whose bbox overlaps the
#' viewport.
#'
#' Row-group bboxes come from cell centroids padded by the cell edge
#' length at that LOD, which is equivalent to a boundary bbox for
#' culling and far cheaper to compute.
#'
#' @param pdf Pyramid data frame with `cell` (`a5_cell`), `_lod` and the
#'   payload columns.
#' @param has_fill_value,has_rgba_cols,extruded Schema flags.
#' @param row_group_size Integer. Cap on rows per parquet row group.
#' @param pivot_offset Integer. Tile bucketing: rows at `lod` are grouped
#'   by their parent at `lod - pivot_offset`. A5 cells have four
#'   children, so `5` gives up to 1024 rows per row group.
#' @param path Destination path for the parquet file. Encoding for the
#'   browser (inline base64 or a served URL) is `parquet_payload()`'s
#'   job.
#' @param meta Optional named list of additional KV metadata entries
#'   to embed in the parquet schema (one entry per name; values must
#'   be JSON-serialisable). Used by [a5_build_pyramid()] /
#'   [a5_view_pyramid()] to round-trip view-time defaults that are
#'   normally derived from the source data frame.
#' @return `list(n_row_groups, path)`, invisibly.
#' @noRd
serialise_pyramid_to_parquet <- function(pdf, path,
                                         has_fill_value, has_rgba_cols,
                                         extruded,
                                         row_group_size = 10000L,
                                         pivot_offset = 5L,
                                         min_lod = 2L,
                                         meta = NULL) {
  row_group_size <- as.integer(row_group_size)
  pivot_offset <- as.integer(pivot_offset)
  min_lod <- as.integer(min_lod)
  cells <- pdf[["cell"]]
  lods <- pdf[["_lod"]]

  # Plan row groups as lists of row indices into pdf, with the LOD and
  # optional tile id (hex of the bucketing parent) per group.
  chunk_indices <- function(idx, size) {
    if (length(idx) <= size) return(list(idx))
    starts <- seq.int(1L, length(idx), by = size)
    lapply(starts, function(st) idx[st:min(length(idx), st + size - 1L)])
  }
  plan_indices <- list()
  plan_lods <- integer()
  plan_tile_ids <- character()
  for (lod in sort(unique(lods))) {
    lod_idx <- which(lods == lod)
    parent_lod <- max(min_lod, lod - pivot_offset)
    if (length(lod_idx) <= row_group_size || parent_lod >= lod) {
      pieces <- chunk_indices(lod_idx, row_group_size)
      tile_ids <- rep(NA_character_, length(pieces))
    } else {
      parents <- a5R::a5_cell_to_parent(cells[lod_idx], resolution = parent_lod)
      group <- vctrs::vec_group_id(parents)
      by_parent <- split(lod_idx, group)
      parent_hex <- format(parents[match(seq_len(attr(group, "n")), group)])
      pieces <- list()
      tile_ids <- character()
      for (g in seq_along(by_parent)) {
        sub <- chunk_indices(by_parent[[g]], row_group_size)
        pieces <- c(pieces, sub)
        tile_ids <- c(tile_ids, rep(parent_hex[[g]], length(sub)))
      }
    }
    plan_indices <- c(plan_indices, pieces)
    plan_lods <- c(plan_lods, rep(as.integer(lod), length(pieces)))
    plan_tile_ids <- c(plan_tile_ids, tile_ids)
  }

  # Permute so each plan entry is a contiguous slice.
  new_order <- unlist(plan_indices, use.names = FALSE)
  pdf <- pdf[new_order, , drop = FALSE]
  cells <- cells[new_order]
  chunk_lengths <- lengths(plan_indices)
  ends <- cumsum(chunk_lengths)
  starts <- ends - chunk_lengths + 1L

  arrow_tbl <- payload_arrow_table(
    pdf,
    has_lod = TRUE,
    has_fill_value = has_fill_value,
    has_rgba_cols = has_rgba_cols,
    extruded = extruded
  )

  # Row-group bboxes from centroids, padded by the cell edge length at
  # that LOD (converted to degrees at the group's latitude).
  xy <- unclass(a5R::a5_cell_to_lonlat(cells))
  edge_m <- a5R::a5_cell_edge_length_avg(plan_lods)
  rg_index <- vector("list", length(plan_indices))
  for (k in seq_along(plan_indices)) {
    sl <- starts[k]:ends[k]
    lon <- xy$x[sl]
    lat <- xy$y[sl]
    lon_rng <- range(lon)
    lat_rng <- range(lat)
    pad_lat <- as.numeric(edge_m[k]) / 111320
    pad_lon <- pad_lat / max(cos(max(abs(lat_rng)) * pi / 180), 0.05)
    west <- lon_rng[1] - pad_lon
    east <- lon_rng[2] + pad_lon
    if (west < -180 || east > 180 || (east - west) > 180) {
      west <- -180; east <- 180
    }
    entry <- list(
      rg = as.integer(k - 1L),
      lod_min = plan_lods[k],
      lod_max = plan_lods[k],
      west = round(west, 5),
      south = round(max(-90, lat_rng[1] - pad_lat), 5),
      east = round(east, 5),
      north = round(min(90, lat_rng[2] + pad_lat), 5)
    )
    if (!is.na(plan_tile_ids[k])) entry$tile_id <- plan_tile_ids[k]
    rg_index[[k]] <- entry
  }

  schema_kv <- list(
    a5view_row_groups = yyjsonr::write_json_str(rg_index, auto_unbox = TRUE)
  )
  if (!is.null(meta) && length(meta) > 0L) {
    schema_kv$a5view_meta <- yyjsonr::write_json_str(meta, auto_unbox = TRUE)
  }
  arrow_tbl <- arrow_tbl$ReplaceSchemaMetadata(schema_kv)

  sink <- arrow::FileOutputStream$create(path)
  writer <- arrow::ParquetFileWriter$create(
    arrow_tbl$schema, sink,
    properties = arrow::ParquetWriterProperties$create(
      column_names = names(arrow_tbl),
      compression = "snappy",
      # Our KV index replaces parquet column statistics, and dictionary
      # pages cost more than they save on ~1k-row row groups; skipping
      # both trims per-row-group overhead by about a fifth.
      write_statistics = FALSE,
      use_dictionary = FALSE
    )
  )
  for (k in seq_along(plan_indices)) {
    sub <- arrow_tbl$Slice(offset = starts[k] - 1L, length = chunk_lengths[k])
    # chunk_size = nrow forces this slice to land as exactly one row
    # group, so the index above stays 1:1 with the file's row groups.
    writer$WriteTable(sub, chunk_size = sub$num_rows)
  }
  writer$Close()
  sink$close()

  invisible(list(n_row_groups = length(plan_indices), path = path))
}

#' Read the embedded `a5view_meta` KV entry from a pyramid parquet file
#'
#' Returns the parsed JSON object, or `NULL` if the entry is missing.
#' @noRd
read_pyramid_meta <- function(path) {
  reader <- arrow::ParquetFileReader$create(path)
  kv <- reader$GetSchema()$metadata
  json <- kv[["a5view_meta"]]
  if (is.null(json)) return(NULL)
  yyjsonr::read_json_str(json)
}

#' Resolve elevation column name from NSE expression
#' @noRd
resolve_elevation_col <- function(cells, elev_expr) {
  if (!is.name(elev_expr) || !is.data.frame(cells)) {
    return(NULL)
  }
  col <- as.character(elev_expr)
  if (!col %in% names(cells)) {
    cli::cli_abort(
      "Column {.val {col}} not found in {.arg cells}. Available columns: {.val {names(cells)}}."
    )
  }
  col
}

#' Compute initial view state from cell centroids
#'
#' Large inputs are subsampled (evenly, up to `max_cells`) before the
#' centroid pass; the centre and extent are indistinguishable from the
#' full computation for the purpose of picking a starting view.
#' @noRd
auto_view <- function(cells, lng = NULL, lat = NULL, zoom = NULL,
                      max_cells = 100000L) {
  if (!is.null(lng) && !is.null(lat) && !is.null(zoom)) {
    return(list(
      longitude = lng,
      latitude = lat,
      zoom = zoom,
      pitch = 0,
      bearing = 0
    ))
  }

  n <- length(cells)
  if (n > max_cells) {
    cells <- cells[unique(as.integer(round(seq(1, n, length.out = max_cells))))]
  }
  xy <- unclass(a5R::a5_cell_to_lonlat(cells))

  ctr_lng <- if (!is.null(lng)) lng else mean(xy$x, na.rm = TRUE)
  ctr_lat <- if (!is.null(lat)) lat else mean(xy$y, na.rm = TRUE)

  z <- if (!is.null(zoom)) zoom else guess_zoom(xy)

  list(
    longitude = ctr_lng,
    latitude = ctr_lat,
    zoom = z,
    pitch = 0,
    bearing = 0
  )
}

#' Guess a reasonable zoom level from coordinate extent
#' @param xy A list with `x` (longitude) and `y` (latitude) numeric
#'   vectors, e.g. `unclass()` of a `wk::xy()` object.
#' @noRd
guess_zoom <- function(xy) {
  lng_range <- diff(range(xy$x, na.rm = TRUE))
  lat_range <- diff(range(xy$y, na.rm = TRUE))
  span <- max(lng_range, lat_range)
  if (span < 1e-6) {
    return(24L)
  }
  span <- span * 1.3
  z <- log2(360 / span)
  max(1L, min(24L, floor(z)))
}

#' Shared data prep (used by a5_view, a5_view_update, a5_build_pyramid)
#'
#' Resolves fill, prepares the data frame, attaches per-cell RGBA, and
#' attaches elevation. Stops short of pyramid construction so the
#' `aggregate = "none"` path can reuse the same helper.
#' @param fill_quo Quosure of the `fill` argument (see `resolve_fill()`).
#' @param elev_expr Substituted `elevation` argument.
#' @return `NULL` when no non-NA cells remain, else a list with `df`,
#'   `leaf_cells`, `extra`, `fill_color`, `legend`, `has_fill_value`,
#'   `has_rgba_cols`, `extruded` and `data_resolution`.
#' @noRd
prepare_view_data <- function(cells, fill_quo, fill_identity, palette,
                              elev_expr = NULL) {
  n_cells <- if (a5R::is_a5_cell(cells)) length(cells) else nrow(cells)
  fill_resolved <- resolve_fill(cells, fill_quo, n_cells)

  if (!fill_identity && has_identity_tag(cells, fill_resolved)) {
    fill_identity <- TRUE
  }
  if (fill_identity) {
    if (fill_resolved$type == "column") {
      fill_resolved$identity <- TRUE
    } else if (fill_resolved$type == "numeric") {
      fill_resolved$type <- "identity"
    } else if (fill_resolved$type != "colors") {
      cli::cli_abort(
        "{.code fill_identity = TRUE} requires {.arg fill} to be a numeric vector, hex colour vector, or column name."
      )
    }
  }

  elev_col <- resolve_elevation_col(cells, elev_expr)

  prepared <- prepare_data(cells)
  df <- prepared$data

  if (nrow(df) == 0L) {
    return(NULL)
  }

  fill <- attach_fill(df, fill_resolved, prepared, palette)
  df <- fill$df

  extruded <- !is.null(elev_col)
  if (extruded) {
    elev_vals <- prepared$extra[[elev_col]]
    if (!is.numeric(elev_vals)) {
      cli::cli_abort(
        "Elevation column {.val {elev_col}} must be numeric, not {.obj_type_friendly {elev_vals}}."
      )
    }
    df[["_elevation"]] <- as.numeric(elev_vals)
  }

  list(
    df = df,
    leaf_cells = prepared$a5_cells,
    extra = prepared$extra,
    fill_color = fill$fill_color,
    legend = fill$legend,
    has_fill_value = "_fill_value" %in% names(df),
    has_rgba_cols = "_fill_r" %in% names(df),
    extruded = extruded,
    data_resolution = as.integer(a5R::a5_get_resolution(prepared$a5_cells[[1]]))
  )
}

#' Resolve the `tooltip` argument to a vector of extra column names
#'
#' Columns are shipped verbatim to the browser and shown in the hover
#' tooltip. Only atomic columns are allowed (list columns such as
#' embeddings are rejected). Returns `character()` for logical input.
#' @noRd
resolve_tooltip_cols <- function(tooltip, prep, aggregate = "none") {
  if (!is.character(tooltip)) return(character())
  avail <- names(prep$extra)
  bad <- setdiff(tooltip, avail)
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "{.arg tooltip} column{?s} not found: {.val {bad}}.",
      "i" = "Available columns: {.val {avail}}."
    ))
  }
  non_atomic <- tooltip[!vapply(prep$extra[tooltip], is.atomic, logical(1))]
  if (length(non_atomic) > 0L) {
    cli::cli_abort(
      "{.arg tooltip} column{?s} {.val {non_atomic}} must be atomic (not list columns)."
    )
  }
  if (aggregate != "none" && length(tooltip) > 0L) {
    cli::cli_warn(c(
      "{.arg tooltip} columns are only shown when {.code aggregate = \"none\"}.",
      "i" = "Pyramid renders show the cell id and fill value only."
    ))
    return(character())
  }
  unique(tooltip)
}

#' Build the Arrow table shipped to the browser
#'
#' One place defines the wire schema for both the inline IPC path and
#' the parquet pyramid path: `pentagon` (fixed-width hex via
#' `a5R::a5_cell_to_arrow` on the `cell` column), optional `_lod`, `_fill_value`,
#' `_fill_r/g/b/a`, `_elevation`, followed by any extra tooltip columns.
#' @noRd
payload_arrow_table <- function(pdf, has_lod, has_fill_value,
                                has_rgba_cols, extruded, extra = list()) {
  u8 <- function(x) arrow::Array$create(x, type = arrow::uint8())
  cols <- list(pentagon = a5R::a5_cell_to_arrow(pdf[["cell"]]))
  if (has_lod) cols[["_lod"]] <- u8(pdf[["_lod"]])
  if (has_fill_value) {
    # float32 is plenty for a tooltip readout and halves the column.
    cols[["_fill_value"]] <- arrow::Array$create(
      pdf[["_fill_value"]], type = arrow::float32()
    )
  }
  if (has_rgba_cols) {
    cols[["_fill_r"]] <- u8(pdf[["_fill_r"]])
    cols[["_fill_g"]] <- u8(pdf[["_fill_g"]])
    cols[["_fill_b"]] <- u8(pdf[["_fill_b"]])
    cols[["_fill_a"]] <- u8(pdf[["_fill_a"]])
  }
  if (extruded) cols[["_elevation"]] <- pdf[["_elevation"]]
  for (nm in names(extra)) cols[[nm]] <- extra[[nm]]
  do.call(arrow::arrow_table, cols)
}

#' Encode prepared view data for transfer to the browser
#'
#' `aggregate = "none"` ships the leaf rows as base64 Arrow IPC.
#' Otherwise a LOD pyramid is built and serialised to parquet with a
#' row-group index so the browser decodes only what the viewport
#' needs. Inside Shiny the parquet file is served over HTTP with Range
#' support (see `serve_parquet_file()`); elsewhere it is inlined as
#' base64.
#' @param session Shiny session or `NULL`.
#' @return List with `arrow_ipc`, `parquet` (one of which is `NULL`)
#'   and `lod_resolutions`.
#' @noRd
encode_view_data <- function(prep, aggregate, lod_step,
                             tooltip_cols = character(), session = NULL) {
  if (aggregate == "none") {
    tbl <- payload_arrow_table(
      prep$df,
      has_lod = FALSE,
      has_fill_value = prep$has_fill_value,
      has_rgba_cols = prep$has_rgba_cols,
      extruded = prep$extruded,
      extra = prep$extra[tooltip_cols]
    )
    ipc_raw <- arrow::write_to_raw(tbl, format = "stream")
    return(list(
      arrow_ipc = base64enc::base64encode(ipc_raw),
      parquet = NULL,
      lod_resolutions = NULL
    ))
  }

  pyramid <- build_a5_pyramid(
    leaf_cells = prep$leaf_cells,
    df = prep$df,
    data_resolution = prep$data_resolution,
    lod_step = lod_step,
    aggregate = aggregate
  )
  path <- tempfile("a5view-", fileext = ".parquet")
  serialise_pyramid_to_parquet(
    pyramid$data, path,
    has_fill_value = prep$has_fill_value,
    has_rgba_cols = prep$has_rgba_cols,
    extruded = prep$extruded
  )
  c(
    list(arrow_ipc = NULL),
    parquet_payload(path, session = session, temp = TRUE),
    list(lod_resolutions = as.list(as.integer(pyramid$lod_resolutions)))
  )
}
