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

#' Prepare data payload for JS
#' @return List with `data` (data frame with `pentagon` column),
#'   `not_na` (logical), `extra` (named list of non-cell columns),
#'   and `a5_cells` (the non-NA a5_cell vector for Arrow conversion).
#' @noRd
prepare_data <- function(cells) {
  if (a5R::is_a5_cell(cells)) {
    hex <- format(cells)
    not_na <- !is.na(hex)
    df <- data.frame(pentagon = hex[not_na], stringsAsFactors = FALSE)
    return(list(
      data = df, not_na = not_na, extra = list(),
      a5_cells = cells[not_na]
    ))
  }

  cell_col <- find_cell_column(cells)
  hex <- format(cells[[cell_col]])
  not_na <- !is.na(hex)

  other_cols <- setdiff(names(cells), cell_col)
  extra <- lapply(other_cols, function(nm) cells[[nm]][not_na])
  names(extra) <- other_cols

  df <- data.frame(pentagon = hex[not_na], stringsAsFactors = FALSE)
  list(
    data = df, not_na = not_na, extra = extra,
    a5_cells = cells[[cell_col]][not_na]
  )
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
#' @param leaf_cells `a5_cell` vector at the data resolution.
#' @param df Data frame with a `pentagon` (hex string) column plus
#'   optional `_fill_r/g/b/a`, `_fill_value`, `_elevation` columns.
#' @param data_resolution Integer A5 resolution of `leaf_cells`.
#' @param lod_step Integer >= 1, gap between successive LODs.
#' @param aggregate One of `"rep_child"`, `"mean"`.
#' @param min_lod Lowest LOD to precompute (default 2).
#' @return A list with `data` (long data frame including `_lod`), `cells`
#'   (concatenated `a5_cell` vector matching the row order of `data`), and
#'   `lod_resolutions` (sorted ascending integer vector).
#' @noRd
build_a5_pyramid <- function(leaf_cells, df, data_resolution, lod_step,
                             aggregate, min_lod = 2L) {
  data_resolution <- as.integer(data_resolution)
  lod_step <- as.integer(lod_step)
  min_lod <- as.integer(min_lod)
  aggregate <- match.arg(aggregate, c("rep_child", "mean"))

  has_rgba <- "_fill_r" %in% names(df)
  has_fill_value <- "_fill_value" %in% names(df)
  has_elev <- "_elevation" %in% names(df)

  agg_cols <- character()
  if (has_rgba) agg_cols <- c(agg_cols, "_fill_r", "_fill_g", "_fill_b", "_fill_a")
  if (has_fill_value) agg_cols <- c(agg_cols, "_fill_value")
  if (has_elev) agg_cols <- c(agg_cols, "_elevation")

  payload_cols <- agg_cols
  cols_order <- c("pentagon", "_lod", payload_cols)

  leaf_rows <- df[, intersect(cols_order, names(df)), drop = FALSE]
  leaf_rows[["_lod"]] <- data_resolution
  leaf_rows <- leaf_rows[, cols_order, drop = FALSE]

  no_payload <- length(agg_cols) == 0L

  if (data_resolution <= min_lod || lod_step < 1L) {
    return(list(
      data = leaf_rows,
      cells = leaf_cells,
      lod_resolutions = data_resolution
    ))
  }

  parent_lods <- seq.int(data_resolution - lod_step, min_lod, by = -lod_step)
  parent_lods <- parent_lods[parent_lods >= min_lod]
  if (length(parent_lods) == 0L) {
    return(list(
      data = leaf_rows,
      cells = leaf_cells,
      lod_resolutions = data_resolution
    ))
  }

  vals <- if (!no_payload) {
    m <- as.matrix(df[, agg_cols, drop = FALSE])
    storage.mode(m) <- "double"
    m
  } else NULL

  pyramid_chunks <- vector("list", length(parent_lods))
  parent_cell_chunks <- vector("list", length(parent_lods))

  for (k in seq_along(parent_lods)) {
    lod <- parent_lods[[k]]
    parents <- a5R::a5_cell_to_parent(leaf_cells, resolution = lod)
    parent_keys <- format(parents)
    unique_keys <- unique(parent_keys)
    parent_factor <- match(parent_keys, unique_keys)
    n_groups <- length(unique_keys)

    if (no_payload) {
      pick_idx <- match(seq_len(n_groups), parent_factor)
      out <- data.frame(
        pentagon = unique_keys,
        `_lod` = as.integer(lod),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      out <- out[, cols_order, drop = FALSE]
      pyramid_chunks[[k]] <- out
      parent_cell_chunks[[k]] <- parents[pick_idx]
      next
    }

    if (aggregate == "mean") {
      sums <- rowsum(vals, parent_factor, reorder = FALSE, na.rm = TRUE)
      # rowsum(reorder = FALSE) keeps the order in which groups are
      # first seen, which matches our parent_factor numbering.
      counts <- tabulate(parent_factor, nbins = n_groups)
      means <- sums / counts
      out <- as.data.frame(means)
      names(out) <- agg_cols
      pick_idx <- match(seq_len(n_groups), parent_factor)
    } else {
      sums <- rowsum(vals, parent_factor, reorder = FALSE, na.rm = TRUE)
      counts <- tabulate(parent_factor, nbins = n_groups)
      means <- sums / counts
      means_per_leaf <- means[parent_factor, , drop = FALSE]
      diffs <- vals - means_per_leaf
      sq_dist <- rowSums(diffs * diffs)
      ord <- order(parent_factor, sq_dist)
      sorted_factor <- parent_factor[ord]
      pick_idx <- ord[!duplicated(sorted_factor)]
      # pick_idx[i] is the leaf row chosen for parent group i
      out <- df[pick_idx, agg_cols, drop = FALSE]
      rownames(out) <- NULL
    }

    if (has_rgba) {
      out[["_fill_r"]] <- as.integer(round(pmin(255, pmax(0, out[["_fill_r"]]))))
      out[["_fill_g"]] <- as.integer(round(pmin(255, pmax(0, out[["_fill_g"]]))))
      out[["_fill_b"]] <- as.integer(round(pmin(255, pmax(0, out[["_fill_b"]]))))
      out[["_fill_a"]] <- as.integer(round(pmin(255, pmax(0, out[["_fill_a"]]))))
    }

    parent_cells_for_groups <- parents[pick_idx]
    out[["pentagon"]] <- format(parent_cells_for_groups)
    out[["_lod"]] <- as.integer(lod)
    out <- out[, cols_order, drop = FALSE]

    pyramid_chunks[[k]] <- out
    parent_cell_chunks[[k]] <- parent_cells_for_groups
  }

  all_data <- do.call(rbind, c(list(leaf_rows), pyramid_chunks))
  all_cells <- do.call(c, c(list(leaf_cells), parent_cell_chunks))

  list(
    data = all_data,
    cells = all_cells,
    lod_resolutions = sort(c(data_resolution, parent_lods))
  )
}

#' Serialise a pyramid table to base64-encoded parquet bytes
#'
#' Sorts the pyramid by `(_lod ASC, pentagon ASC)`, writes parquet with a
#' fixed `chunk_size` (one row group per chunk), and stuffs a JSON
#' index of per-row-group `{rg, lod_min, lod_max, west, south, east, north}`
#' into the schema's KV metadata. The JS side reads this index up-front
#' and decodes only the row groups whose LOD matches the picked LOD and
#' whose bbox overlaps the viewport.
#'
#' Cell IDs are emitted as zero-padded fixed-width hex via
#' `a5R::a5_cell_to_arrow`, so a lexicographic sort on the hex column
#' matches the underlying uint64 ordering.
#'
#' @param pdf Pyramid data frame (must contain `pentagon`, `_lod`).
#' @param arrow_cells `a5_cell` vector aligned to `pdf` rows.
#' @param has_fill_value,has_rgba_cols,extruded Schema flags.
#' @param row_group_size Integer. Rows per parquet row group.
#' @param path Destination path for the parquet file. When `NULL`
#'   (default), a tempfile is used and unlinked on exit; the bytes are
#'   read back and returned as base64 for inline transfer to the
#'   browser. When supplied, the file is written to that path and
#'   left in place; no base64 is returned.
#' @param meta Optional named list of additional KV metadata entries
#'   to embed in the parquet schema (one entry per name; values must
#'   be JSON-serialisable). Used by [a5_build_pyramid()] /
#'   [a5_view_pyramid()] to round-trip view-time defaults that are
#'   normally derived from the source data frame.
#' @return List `list(b64, n_row_groups, path)`. `b64` is `NULL` when
#'   `path` was supplied.
#' @noRd
serialise_pyramid_to_parquet <- function(pdf, arrow_cells,
                                         has_fill_value, has_rgba_cols,
                                         extruded,
                                         row_group_size = 10000L,
                                         pivot_offset = 4L,
                                         min_lod = 2L,
                                         path = NULL,
                                         meta = NULL) {
  row_group_size <- as.integer(row_group_size)
  pivot_offset <- as.integer(pivot_offset)
  min_lod <- as.integer(min_lod)

  # Plan row groups per LOD. Goals: low LODs collapse to one tiny row
  # group (wide-zoom initial paint = decode <1k rows). High LODs that
  # exceed row_group_size are bucketed by parent at (lod - pivot_offset)
  # so each row group has a tight regional bbox; JS-side viewport
  # pruning then decodes only the parents intersecting the view.
  #
  # We collect a list of "row index" vectors, one per row group, each
  # indexing into the input pdf+arrow_cells. After planning we permute
  # the table so each plan entry maps to a contiguous slice.
  unique_lods <- sort(unique(pdf[["_lod"]]))
  plan_indices <- list()
  plan_lods <- integer(0)
  plan_tile_ids <- character(0)  # NA_character_ for non-tiled row groups
  for (lod in unique_lods) {
    lod_idx <- which(pdf[["_lod"]] == lod)
    n_lod <- length(lod_idx)
    if (n_lod <= row_group_size) {
      plan_indices[[length(plan_indices) + 1L]] <- lod_idx
      plan_lods <- c(plan_lods, lod)
      plan_tile_ids <- c(plan_tile_ids, NA_character_)
      next
    }
    parent_lod <- max(min_lod, lod - pivot_offset)
    if (parent_lod >= lod) {
      # Pivot collapses to current LOD: no spatial bucketing possible,
      # fall back to row_group_size chunks (and no tile_id either).
      starts_in_lod <- seq.int(1L, n_lod, by = row_group_size)
      for (s in starts_in_lod) {
        e <- min(n_lod, s + row_group_size - 1L)
        plan_indices[[length(plan_indices) + 1L]] <- lod_idx[s:e]
        plan_lods <- c(plan_lods, lod)
        plan_tile_ids <- c(plan_tile_ids, NA_character_)
      }
      next
    }
    parents <- a5R::a5_cell_to_parent(arrow_cells[lod_idx], resolution = parent_lod)
    parent_keys <- format(parents)
    by_parent <- split(lod_idx, parent_keys)
    parent_names <- names(by_parent)
    for (i in seq_along(by_parent)) {
      group_rows <- by_parent[[i]]
      tile_id <- parent_names[[i]]
      n <- length(group_rows)
      if (n <= row_group_size) {
        plan_indices[[length(plan_indices) + 1L]] <- group_rows
        plan_lods <- c(plan_lods, lod)
        plan_tile_ids <- c(plan_tile_ids, tile_id)
      } else {
        starts <- seq.int(1L, n, by = row_group_size)
        for (s in starts) {
          e <- min(n, s + row_group_size - 1L)
          plan_indices[[length(plan_indices) + 1L]] <- group_rows[s:e]
          plan_lods <- c(plan_lods, lod)
          plan_tile_ids <- c(plan_tile_ids, tile_id)
        }
      }
    }
  }

  # Permute the table so each plan entry maps to a contiguous slice.
  new_order <- unlist(plan_indices, use.names = FALSE)
  pdf <- pdf[new_order, , drop = FALSE]
  arrow_cells <- arrow_cells[new_order]

  arrow_tbl <- payload_arrow_table(
    pdf, arrow_cells,
    has_lod = TRUE,
    has_fill_value = has_fill_value,
    has_rgba_cols = has_rgba_cols,
    extruded = extruded
  )

  # Now plan_indices' k-th element occupies rows [offsets[k], offsets[k+1]).
  chunk_lengths <- vapply(plan_indices, length, integer(1L))
  offsets <- cumsum(c(1L, chunk_lengths))
  plan <- lapply(seq_along(plan_indices), function(k) {
    list(start = offsets[k], end = offsets[k] + chunk_lengths[k] - 1L,
         lod = plan_lods[k])
  })

  rg_index <- vector("list", length(plan))
  for (k in seq_along(plan)) {
    p <- plan[[k]]
    chunk_cells <- arrow_cells[p$start:p$end]
    bbox <- unclass(wk::wk_bbox(a5R::a5_cell_to_boundary(chunk_cells)))
    if (bbox$xmin < -180 || bbox$xmax > 180) {
      west <- -180; east <- 180
    } else {
      west <- bbox$xmin; east <- bbox$xmax
    }
    tile_id <- plan_tile_ids[[k]]
    entry <- list(
      rg = as.integer(k - 1L),
      lod_min = as.integer(p$lod),
      lod_max = as.integer(p$lod),
      west  = west,
      south = max(-90, bbox$ymin),
      east  = east,
      north = min(90,  bbox$ymax)
    )
    if (!is.na(tile_id)) entry$tile_id <- tile_id
    rg_index[[k]] <- entry
  }
  rg_meta_json <- yyjsonr::write_json_str(rg_index, auto_unbox = TRUE)

  schema_kv <- list(a5view_row_groups = rg_meta_json)
  if (!is.null(meta) && length(meta) > 0L) {
    schema_kv$a5view_meta <- yyjsonr::write_json_str(meta, auto_unbox = TRUE)
  }
  arrow_tbl <- arrow_tbl$ReplaceSchemaMetadata(schema_kv)

  caller_supplied_path <- !is.null(path)
  if (!caller_supplied_path) {
    path <- tempfile(fileext = ".parquet")
    on.exit(unlink(path), add = TRUE)
  }
  sink <- arrow::FileOutputStream$create(path)
  writer <- arrow::ParquetFileWriter$create(
    arrow_tbl$schema, sink,
    properties = arrow::ParquetWriterProperties$create(
      column_names = names(arrow_tbl)
    )
  )
  for (p in plan) {
    sub <- arrow_tbl$Slice(offset = p$start - 1L, length = p$end - p$start + 1L)
    # chunk_size = nrow forces this slice to land as exactly one row group,
    # so the row group index above stays 1:1 with the file's row groups.
    writer$WriteTable(sub, chunk_size = sub$num_rows)
  }
  writer$Close()
  sink$close()

  if (caller_supplied_path) {
    list(b64 = NULL, n_row_groups = length(plan), path = path)
  } else {
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    list(
      b64 = base64enc::base64encode(bytes),
      n_row_groups = length(plan),
      path = path
    )
  }
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
#' @noRd
auto_view <- function(hex_ids, lng = NULL, lat = NULL, zoom = NULL) {
  if (!is.null(lng) && !is.null(lat) && !is.null(zoom)) {
    return(list(
      longitude = lng,
      latitude = lat,
      zoom = zoom,
      pitch = 0,
      bearing = 0
    ))
  }

  cells <- a5R::a5_cell(hex_ids)
  coords <- a5R::a5_cell_to_lonlat(cells)
  xy <- unclass(coords)

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
#' `a5R::a5_cell_to_arrow`), optional `_lod`, `_fill_value`,
#' `_fill_r/g/b/a`, `_elevation`, followed by any extra tooltip columns.
#' @noRd
payload_arrow_table <- function(pdf, cells, has_lod, has_fill_value,
                                has_rgba_cols, extruded, extra = list()) {
  u8 <- function(x) arrow::Array$create(x, type = arrow::uint8())
  cols <- list(pentagon = a5R::a5_cell_to_arrow(cells))
  if (has_lod) cols[["_lod"]] <- u8(pdf[["_lod"]])
  if (has_fill_value) cols[["_fill_value"]] <- pdf[["_fill_value"]]
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
#' needs.
#' @return List with `arrow_ipc`, `parquet_b64` (one of which is
#'   `NULL`) and `lod_resolutions`.
#' @noRd
encode_view_data <- function(prep, aggregate, lod_step,
                             tooltip_cols = character()) {
  if (aggregate == "none") {
    tbl <- payload_arrow_table(
      prep$df, prep$leaf_cells,
      has_lod = FALSE,
      has_fill_value = prep$has_fill_value,
      has_rgba_cols = prep$has_rgba_cols,
      extruded = prep$extruded,
      extra = prep$extra[tooltip_cols]
    )
    ipc_raw <- arrow::write_to_raw(tbl, format = "stream")
    return(list(
      arrow_ipc = base64enc::base64encode(ipc_raw),
      parquet_b64 = NULL,
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
  pq <- serialise_pyramid_to_parquet(
    pyramid$data, pyramid$cells,
    has_fill_value = prep$has_fill_value,
    has_rgba_cols = prep$has_rgba_cols,
    extruded = prep$extruded
  )
  list(
    arrow_ipc = NULL,
    parquet_b64 = pq$b64,
    lod_resolutions = as.list(as.integer(pyramid$lod_resolutions))
  )
}
