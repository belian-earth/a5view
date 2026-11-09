# Data preparation and view state computation

#' Find the a5_cell column in a data frame
#' @noRd
find_cell_column <- function(df) {
  for (nm in names(df)) {
    if (a5R::is_a5_cell(df[[nm]])) return(nm)
  }
  cli::cli_abort(
    "No {.cls a5_cell} or {.cls a5_covering} column found in {.arg cells}. Columns present: {.val {names(df)}}."
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

#' Write prepared view data as an a5pyramid file
#'
#' The pyramid is built and written by [a5pyramid::a5_write_pyramid()]:
#' each coarser LOD groups the leaves by supercell
#' ([a5R::a5_cell_to_supercell()], the coarse cell containing a leaf's
#' centre, since the A5 1.0 index hierarchy is not spatially nested) and
#' reduces the payload columns with `aggregate`. Each level is split into
#' tiles of nearby cells, one row group each, and the file's metadata
#' indexes every row group's LOD and bounding box, so the browser decodes
#' only the row groups in view at the LOD for its zoom. The layout is
#' a5pyramid's (its `FORMAT.md`), which the lazy renderer reads. Column
#' types are set by `pyramid_spec()`.
#'
#' @param prep Output of `prepare_view_data()`, with cells at one
#'   resolution.
#' @param path Destination parquet path.
#' @param aggregate `"rep_child"` or `"mean"`.
#' @param lod_step Integer >= 1, gap between successive LODs.
#' @param meta Optional named list stored as JSON under the `a5view_meta`
#'   key, with the LOD schedule added as `lod_resolutions`. Used by
#'   [a5_build_pyramid()] / [a5_view_pyramid()] to round-trip view-time
#'   defaults.
#' @return The LODs in the file, ascending (the data resolution last).
#' @noRd
write_pyramid <- function(prep, path, aggregate, lod_step, meta = NULL) {
  spec <- pyramid_spec(prep, lod_step, meta)
  with_pyramid_errors(
    a5pyramid::a5_write_pyramid(
      prep$df, path,
      lods = spec$lods,
      aggregate = aggregate,
      types = spec$types,
      metadata = spec$metadata
    )
  )
  spec$all_lods
}

#' Build prepared view data as an a5pyramid Arrow table
#'
#' The pyramid of `write_pyramid()`, from [a5pyramid::a5_pyramid_arrow()]
#' and held in memory rather than written: one record batch per tile, in
#' the order of the file's row groups, with the index in the schema
#' metadata. `a5_view()` and `a5_view_update()` draw pyramids this way, so
#' no temp file is written.
#' @inheritParams write_pyramid
#' @return List with `table` (an Arrow `Table`) and `lods` (its LODs,
#'   ascending).
#' @noRd
pyramid_arrow <- function(prep, aggregate, lod_step) {
  spec <- pyramid_spec(prep, lod_step)
  tbl <- with_pyramid_errors(
    a5pyramid::a5_pyramid_arrow(
      prep$df,
      lods = spec$lods,
      aggregate = aggregate,
      types = spec$types
    )
  )
  list(table = tbl, lods = spec$all_lods)
}

#' The a5pyramid arguments for a view's pyramid
#'
#' Colour channels are stored as `uint8` and the fill value as `float32`,
#' plenty for display and a tooltip readout; elevation stays `float64`.
#' @inheritParams write_pyramid
#' @return List with `lods` (the coarser LODs to build), `all_lods` (every
#'   LOD, ascending), `types` and `metadata`.
#' @noRd
pyramid_spec <- function(prep, lod_step, meta = NULL) {
  data_resolution <- prep$data_resolution
  if (length(data_resolution) > 1L) {
    cli::cli_abort(c(
      "A pyramid needs cells at one resolution, not {sort(data_resolution)}.",
      "i" = "Draw mixed resolutions with {.code aggregate = \"none\"} in {.fn a5_view}."
    ))
  }
  lods <- pyramid_lods(data_resolution, lod_step)
  all_lods <- sort(c(lods, as.integer(data_resolution)))

  cols <- setdiff(names(prep$df), "cell")
  types <- NULL
  if (length(cols) > 0L) {
    types <- ifelse(
      cols == "_fill_value", "float32",
      ifelse(startsWith(cols, "_fill_"), "uint8", "float64")
    )
    names(types) <- cols
  }
  metadata <- NULL
  if (!is.null(meta)) {
    meta$lod_resolutions <- as.list(all_lods)
    metadata <- c(a5view_meta = yyjsonr::write_json_str(meta, auto_unbox = TRUE))
  }
  list(lods = lods, all_lods = all_lods, types = types, metadata = metadata)
}

#' Evaluate an a5pyramid build, explaining duplicated cells in a5view's
#' terms
#' @noRd
with_pyramid_errors <- function(expr) {
  tryCatch(
    expr,
    a5pyramid_error_input = function(e) {
      if (!grepl("more than once", conditionMessage(e), fixed = TRUE)) stop(e)
      cli::cli_abort(c(
        "A pyramid needs each cell once.",
        "i" = "Combine rows with the same cell first, or draw them all with {.code aggregate = \"none\"} in {.fn a5_view}."
      ), parent = e, call = NULL)
    }
  )
}

#' The coarser LODs of a pyramid: every `lod_step`-th resolution below the
#' data resolution, down to `min_lod`. Resolutions 0 and 1 hold the 12 and
#' 60 largest cells, which no map view draws.
#' @noRd
pyramid_lods <- function(data_resolution, lod_step, min_lod = 2L) {
  top <- as.integer(data_resolution) - as.integer(lod_step)
  if (top < min_lod) {
    return(integer())
  }
  seq.int(top, min_lod, by = -as.integer(lod_step))
}

# The A5 index version of the cell ids a pyramid file holds, recorded
# under the `a5_index` KV key. A5 1.0 renumbered every cell, and an id
# does not say which version wrote it.
A5_INDEX_VERSION <- "1.0"

#' Read the `a5view_meta` entry of a pyramid file
#'
#' Returns the parsed JSON object with the file's `a5_index` entry as its
#' `a5_index` element (`NA` when absent) and the file's LODs as
#' `lod_resolutions`, or `NULL` if the file is not an a5pyramid file or
#' has no `a5view_meta` entry (as with pyramids written by a5view 0.3 and
#' earlier, in an older layout).
#' @noRd
read_pyramid_meta <- function(path) {
  info <- tryCatch(
    a5pyramid::a5_pyramid_info(path),
    a5pyramid_error_format = function(e) NULL
  )
  if (is.null(info)) return(NULL)
  json <- unname(info$metadata["a5view_meta"])
  if (is.na(json)) return(NULL)
  meta <- yyjsonr::read_json_str(json)
  meta$a5_index <- info$a5_index
  meta$lod_resolutions <- as.list(info$lods)
  meta
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
#'   `has_rgba_cols`, `extruded` and `data_resolution` (the
#'   resolutions present, finest first).
#' @noRd
prepare_view_data <- function(cells, fill_quo, fill_identity, palette,
                              elev_expr = NULL) {
  cells <- covering_to_cells(cells)
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
    data_resolution = sort(
      unique(as.integer(a5R::a5_get_resolution(prepared$a5_cells))),
      decreasing = TRUE
    )
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
  if ("cell" %in% tooltip) {
    cli::cli_abort(c(
      "{.arg tooltip} cannot show a column named {.val cell}.",
      "i" = "The cell id travels to the browser as {.val cell}, and the tooltip always shows it. Rename the column."
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

#' Build the Arrow table shipped to the browser for `aggregate = "none"`
#'
#' The wire schema of the inline IPC path: `cell` (`uint64` via
#' `a5R::a5_cell_to_arrow()`), optional `_fill_value`, `_fill_r/g/b/a`,
#' `_elevation`, followed by any extra tooltip columns. Pyramids carry the
#' same payload columns, written by `write_pyramid()`.
#' @noRd
payload_arrow_table <- function(pdf, has_fill_value,
                                has_rgba_cols, extruded, extra = list()) {
  u8 <- function(x) arrow::Array$create(x, type = arrow::uint8())
  cols <- list(cell = a5R::a5_cell_to_arrow(pdf[["cell"]]))
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
#' Otherwise a LOD pyramid is built in memory by `pyramid_arrow()`, one
#' record batch per tile, with an index so the browser decodes only the
#' tiles the viewport needs. Inside Shiny the tiles are served over HTTP,
#' one per request; elsewhere they are inlined (see
#' `arrow_pyramid_payload()`). No file is written.
#' @param session Shiny session or `NULL`.
#' @param output_id The Shiny output the widget renders to, or `NULL` to
#'   look it up while rendering. Each output serves one pyramid at a time.
#' @return List with `arrow_ipc`, `pyramid` (one of which is `NULL`)
#'   and `lod_resolutions`.
#' @noRd
encode_view_data <- function(prep, aggregate, lod_step,
                             tooltip_cols = character(), session = NULL,
                             output_id = NULL) {
  if (aggregate == "none") {
    tbl <- payload_arrow_table(
      prep$df,
      has_fill_value = prep$has_fill_value,
      has_rgba_cols = prep$has_rgba_cols,
      extruded = prep$extruded,
      extra = prep$extra[tooltip_cols]
    )
    ipc_raw <- arrow::write_to_raw(tbl, format = "stream")
    return(list(
      arrow_ipc = base64enc::base64encode(ipc_raw),
      pyramid = NULL,
      lod_resolutions = NULL
    ))
  }

  pyr <- pyramid_arrow(prep, aggregate, lod_step)
  list(
    arrow_ipc = NULL,
    pyramid = arrow_pyramid_payload(pyr$table, session, output_id),
    lod_resolutions = as.list(pyr$lods)
  )
}

#' Turn `a5_covering` input into cells
#'
#' A covering is drawn as the cells it stores, at their mixed
#' resolutions; uncompacting is the caller's choice. A data frame column
#' of coverings is used when the frame has no `a5_cell` column, and each
#' row is repeated once per stored cell so its other columns travel with
#' them. Anything else is returned unchanged.
#' @noRd
covering_to_cells <- function(cells) {
  if (a5R::is_a5_covering(cells)) return(a5R::a5_covering_stored(cells))
  if (!is.data.frame(cells)) return(cells)
  if (any(vapply(cells, a5R::is_a5_cell, logical(1)))) return(cells)
  is_cov <- vapply(cells, a5R::is_a5_covering, logical(1))
  if (!any(is_cov)) return(cells)
  expand_covering_column(cells, names(cells)[is_cov][1])
}

#' Replace a data frame's covering column by its stored cells, one row
#' per cell
#' @noRd
expand_covering_column <- function(df, col) {
  n <- lengths(a5R::a5_covering_stored(df[[col]], simplify = FALSE))
  out <- df[rep.int(seq_len(nrow(df)), n), , drop = FALSE]
  out[[col]] <- a5R::a5_covering_stored(df[[col]])
  rownames(out) <- NULL
  out
}
