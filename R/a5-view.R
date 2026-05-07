#' View A5 cells on an interactive map
#'
#' Renders A5 cells using deck.gl's native A5Layer. Cells are drawn as
#' filled pentagons on a basemap.
#'
#' @param cells An [a5R::a5_cell] vector, or a data frame / tibble
#'   containing an `a5_cell` column.
#' @param fill Fill colour specification. One of:
#'   - A single hex colour string (e.g. `"#3388ff"`) for uniform fill.
#'   - A numeric vector (same length as cells) mapped to `palette`.
#'   - A character vector of hex colours (same length as cells) for
#'     per-cell colours.
#'   - An unquoted column name when `cells` is a data frame.
#'   - An expression evaluated against the columns of `cells`, in the
#'     style of `ggplot2::aes()`. This makes the colour helpers
#'     [cells_rgb()] and [cells_pca_rgb()] usable directly inside the
#'     call, e.g. `a5_view(df, fill = cells_rgb(r, g, b))` or
#'     `a5_view(df, fill = cells_pca_rgb(embedding))`. The helpers tag
#'     their output so `fill_identity` is auto-enabled.
#'   Default: `"#3388ff"`.
#' @param fill_identity Logical. When `TRUE`, treat `fill` values as
#'   literal colours rather than mapping through `palette`. Accepts
#'   packed RGB integers (`(R << 16) | (G << 8) | B`) or hex colour
#'   strings. Auto-enabled when `fill` is produced by [cells_rgb()] /
#'   [cells_pca_rgb()] or any vector tagged with
#'   `attr(x, "a5_identity") <- TRUE`. Default: `FALSE`.
#' @param palette Colour palette used when `fill` is numeric. Either a
#'   palette name accepted by [grDevices::hcl.colors()] (e.g.
#'   `"viridis"`, `"inferno"`, `"plasma"`, `"turbo"`, `"rocket"`) or
#'   a character vector of hex colours (at least 2). Default: `"viridis"`.
#' @param opacity Numeric scalar, initial layer opacity (0--1). An
#'   interactive slider is provided in the viewer to adjust at runtime.
#'   Default: `0.6`.
#' @param tooltip Logical or character vector of column names to show
#'   on hover. Default: `TRUE` (show cell hex ID).
#' @param elevation Column name for 3D extrusion, or `NULL` for flat.
#' @param elevation_scale Numeric scalar, scale factor for elevation.
#' @param border Border (stroke) colour for cell outlines, or `NULL`
#'   for no borders. A single colour string (e.g. `"#ffffff"`,
#'   `"white"`). Default: `NULL`.
#' @param border_width Numeric scalar, border width in pixels.
#'   Default: `1`.
#' @param width,height Widget dimensions. Default: `NULL` (fills container).
#' @param lng,lat,zoom Initial map view. If `NULL` (default),
#'   auto-centres on the cell centroids.
#' @param globe Logical. Use a 3D globe projection instead of the
#'   default Mercator map. Default: `FALSE`.
#' @param basemap Character vector of basemap styles to make available.
#'   Options are `"dark"`, `"light"`, `"osm"`, and `"satellite"`. The
#'   first element is shown initially. When multiple basemaps are given,
#'   an interactive selector is shown. Use `"none"` for no basemap.
#'   Default: all four options.
#' @param draw_polygon Logical. When `TRUE`, adds a "Draw polygon"
#'   toggle to the controls panel. While draw mode is active, clicks
#'   place polygon vertices and a double-click closes the polygon.
#'   On completion, the polygon is emitted to Shiny as a WKT string at
#'   `input$<id>_polygon_draw`; the drawn outline is held on screen
#'   until the user toggles draw mode off, presses Escape, or starts a
#'   new polygon. While draw mode is on, deck.gl's
#'   double-click-to-zoom is suppressed and cell-pick click events do
#'   not fire. The widget itself does not visualise which cells fall
#'   inside the polygon — resolve the WKT server-side (e.g. with
#'   [a5R::a5_grid()]) and update the map fills via [a5_view_update()].
#'   Only useful in a Shiny context. Default: `FALSE`.
#' @param aggregate How parent cells are summarised when zooming out.
#'   One of:
#'   - `"none"` (default): no precomputed pyramid. Cells render through
#'     the standard deck.gl `A5Layer`. Cheapest to build, suitable for
#'     small to medium datasets where you don't need LOD switching.
#'   - `"rep_child"`: each parent inherits the row of the child whose
#'     payload (RGBA + optional `fill_value`/`elevation`) is closest in
#'     Euclidean distance to its parent's mean. Preserves real values,
#'     no blending. Use for embedding/RGB visualisations.
#'   - `"mean"`: each numeric payload column is averaged independently
#'     within the parent group. Best for scalar fields; blends colours
#'     in RGB space, which can mute embedding visualisations.
#'   When set to `"rep_child"` or `"mean"`, leaf rows render as-is at
#'   high zoom and aggregated parent rows render at coarser LODs. Above
#'   50k rows per LOD, rendering routes through deck.gl's `TileLayer`
#'   for stable per-tile GPU caching.
#' @param lod_step Integer >= 1, gap between successive precomputed
#'   LODs (only used when `aggregate != "none"`). `1` (default)
#'   precomputes every level from the data resolution down to the
#'   floor (LOD 2); larger values trade size for fewer levels and more
#'   visible popping at zoom transitions.
#' @returns An htmlwidget.
#'
#' @export
a5_view <- function(
  cells,
  fill = "#74ac90ff",
  fill_identity = FALSE,
  palette = "Viridis",
  opacity = 0.3,
  tooltip = TRUE,
  elevation = NULL,
  elevation_scale = 1,
  border = "#74ac9080",
  border_width = 1,
  width = NULL,
  height = NULL,
  lng = NULL,
  lat = NULL,
  zoom = NULL,
  globe = FALSE,
  basemap = c("dark", "light", "osm", "satellite"),
  draw_polygon = FALSE,
  aggregate = c("none", "rep_child", "mean"),
  lod_step = 1L
) {
  aggregate <- match.arg(aggregate)
  # --- Validate all arguments ---
  check_cells(cells)
  check_number_decimal(opacity, min = 0, max = 1, arg = "opacity")
  check_number_decimal(elevation_scale, min = 0, arg = "elevation_scale")
  check_number_decimal(border_width, min = 0, arg = "border_width")
  check_optional_number(lng, "lng")
  check_optional_number(lat, "lat")
  check_optional_number(zoom, "zoom")
  check_optional_dimension(width, "width")
  check_optional_dimension(height, "height")
  check_border(border)
  if (!rlang::is_bool(globe)) {
    cli::cli_abort("{.arg globe} must be {.val TRUE} or {.val FALSE}.")
  }
  check_basemap(basemap)
  check_tooltip(tooltip)
  check_palette(palette)
  check_draw_polygon(draw_polygon)
  if (!rlang::is_bool(fill_identity)) {
    cli::cli_abort("{.arg fill_identity} must be {.val TRUE} or {.val FALSE}.")
  }
  if (!rlang::is_integerish(lod_step, n = 1L) ||
      is.na(lod_step) || lod_step < 1L) {
    cli::cli_abort("{.arg lod_step} must be a positive integer.")
  }
  lod_step <- as.integer(lod_step)

  # --- Resolve fill and elevation (NSE) ---
  fill_quo <- rlang::enquo(fill)
  fill_expr <- rlang::quo_get_expr(fill_quo)
  elev_expr <- substitute(elevation)

  # aes()-style: when fill is a call and cells is a data frame, evaluate
  # the call against the columns of cells as a data mask. Bare names and
  # literal vectors fall through to the existing resolution paths.
  if (is.call(fill_expr) && is.data.frame(cells)) {
    fill <- rlang::eval_tidy(fill_quo, data = cells)
  }

  n_cells <- if (a5R::is_a5_cell(cells)) length(cells) else nrow(cells)
  fill_resolved <- resolve_fill(cells, fill, fill_expr, n_cells)

  # Auto-flip fill_identity when fill is tagged "a5_identity" — this lets
  # cells_rgb() / cells_pca_rgb() (and any column tagged the same way)
  # render as packed RGB without a manual fill_identity = TRUE.
  if (!fill_identity && has_identity_tag(cells, fill_resolved)) {
    fill_identity <- TRUE
  }

  # --- Identity fill: convert column/numeric values to literal colours ---
  if (fill_identity) {
    if (fill_resolved$type == "column") {
      fill_resolved$identity <- TRUE
    } else if (fill_resolved$type == "numeric") {
      fill_resolved$type <- "identity"
    } else if (fill_resolved$type == "colors") {
      # Already hex colour strings — identity is a no-op, pass through
    } else {
      cli::cli_abort(
        "{.code fill_identity = TRUE} requires {.arg fill} to be a numeric vector, hex colour vector, or column name."
      )
    }
  }

  elev_col <- resolve_elevation_col(cells, elev_expr)

  # --- Prepare data ---
  prepared <- prepare_data(cells)
  df <- prepared$data

  if (nrow(df) == 0L) {
    cli::cli_abort("No non-NA cells to display.")
  }

  # --- Validate tooltip columns against available data ---
  if (is.character(tooltip)) {
    avail <- c(names(df), names(prepared$extra))
    bad_tt <- setdiff(tooltip, avail)
    if (length(bad_tt) > 0) {
      cli::cli_abort(
        "{.arg tooltip} column{?s} not found: {.val {bad_tt}}. Available: {.val {avail}}."
      )
    }
  }

  # --- Attach fill, elevation, tooltip data ---
  fill_payload <- attach_fill(df, fill_resolved, prepared, palette)
  df <- normalize_rgba_cols(fill_payload$df)

  extruded <- !is.null(elev_col)
  if (extruded) {
    elev_vals <- prepared$extra[[elev_col]] %||% df[[elev_col]]
    if (!is.numeric(elev_vals)) {
      cli::cli_abort(
        "Elevation column {.val {elev_col}} must be numeric, not {.obj_type_friendly {elev_vals}}."
      )
    }
    df[["_elevation"]] <- as.numeric(elev_vals)
  }

  pickable <- !isFALSE(tooltip)
  has_fill_value <- "_fill_value" %in% names(df)
  has_rgba_cols <- "_fill_r" %in% names(df)

  # --- Auto-center view ---
  view_state <- auto_view(df[["pentagon"]], lng, lat, zoom)

  data_resolution <- as.integer(a5R::a5_get_resolution(prepared$a5_cells[[1]]))

  if (aggregate == "none") {
    # Legacy path: leaf-only payload, rendered as a single A5Layer.
    pdf <- df
    lod_resolutions <- NULL
    arrow_cells <- prepared$a5_cells
  } else {
    pyramid <- build_a5_pyramid(
      leaf_cells = prepared$a5_cells,
      df = df,
      data_resolution = data_resolution,
      lod_step = lod_step,
      aggregate = aggregate
    )
    pdf <- pyramid$data
    lod_resolutions <- as.list(as.integer(pyramid$lod_resolutions))
    arrow_cells <- pyramid$cells
  }

  # --- Build Arrow IPC as base64 for inline transfer ---
  arrow_cols <- list(pentagon = a5R::a5_cell_to_arrow(arrow_cells))
  if (aggregate != "none") {
    arrow_cols[["_lod"]] <- arrow::Array$create(pdf[["_lod"]], type = arrow::uint8())
  }
  if (has_fill_value) {
    arrow_cols[["_fill_value"]] <- pdf[["_fill_value"]]
  }
  if (has_rgba_cols) {
    arrow_cols[["_fill_r"]] <- arrow::Array$create(pdf[["_fill_r"]], type = arrow::uint8())
    arrow_cols[["_fill_g"]] <- arrow::Array$create(pdf[["_fill_g"]], type = arrow::uint8())
    arrow_cols[["_fill_b"]] <- arrow::Array$create(pdf[["_fill_b"]], type = arrow::uint8())
    arrow_cols[["_fill_a"]] <- arrow::Array$create(pdf[["_fill_a"]], type = arrow::uint8())
  }
  if (extruded) {
    arrow_cols[["_elevation"]] <- pdf[["_elevation"]]
  }

  arrow_tbl <- do.call(arrow::arrow_table, arrow_cols)

  parquet_payload <- NULL
  if (aggregate != "none") {
    # Pyramid path: always parquet + JS-side lazy row-group decode.
    # Smaller initial payload, viewport-driven decoding, per-tile GPU
    # caching via the lazy renderer's TileLayer.
    parquet_payload <- serialise_pyramid_to_parquet(
      pdf, arrow_cells,
      has_fill_value = has_fill_value,
      has_rgba_cols = has_rgba_cols,
      extruded = extruded
    )
    arrow_b64 <- NULL
  } else {
    ipc_raw <- arrow::write_to_raw(arrow_tbl, format = "stream")
    arrow_b64 <- base64enc::base64encode(ipc_raw)
  }

  # --- JSON payload: base64 Arrow IPC + metadata ---
  payload <- list(
    arrow_ipc = arrow_b64,
    parquet_b64 = if (!is.null(parquet_payload)) parquet_payload$b64 else NULL,
    parquet_row_groups = if (!is.null(parquet_payload)) parquet_payload$n_row_groups else NULL,
    fill_is_column = fill_payload$fill_is_column,
    fill_color = fill_payload$fill_color,
    fill_per_cell = has_rgba_cols,
    palette = fill_payload$js_palette,
    domain = fill_payload$domain,
    opacity = opacity,
    extruded = extruded,
    elevation_scale = elevation_scale,
    pickable = pickable,
    tooltip = !isFALSE(tooltip),
    has_fill_value = has_fill_value,
    stroked = !is.null(border),
    line_color = if (!is.null(border)) hex_to_rgba(border) else NULL,
    line_width = border_width,
    view_state = view_state,
    globe = globe,
    basemaps = as.list(basemap),
    draw_polygon = draw_polygon,
    data_resolution = data_resolution,
    lod_resolutions = lod_resolutions
  )

  widget <- htmlwidgets::createWidget(
    name = "a5view",
    x = payload,
    width = width,
    height = height,
    package = "a5view",
    sizingPolicy = htmlwidgets::sizingPolicy(
      viewer.padding = 0,
      viewer.fill = TRUE,
      browser.fill = TRUE,
      browser.padding = 0
    )
  )

  # Attach Arrow JS library for decoding
  widget <- geoarrowWidget::attachArrowDependency(widget)
  widget
}

#' Shiny output binding for a5_view
#' @param outputId Output variable name.
#' @param width,height Widget dimensions.
#' @export
a5_viewOutput <- function(outputId, width = "100%", height = "400px") {
  rlang::check_required(outputId)
  if (!rlang::is_string(outputId)) {
    cli::cli_abort("{.arg outputId} must be a single string.")
  }
  htmlwidgets::shinyWidgetOutput(
    outputId,
    "a5view",
    width,
    height,
    package = "a5view"
  )
}

#' Shiny render function for a5_view
#' @param expr An expression that returns an a5_view widget.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression?
#' @export
renderA5_view <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  htmlwidgets::shinyRenderWidget(expr, a5_viewOutput, env, quoted = TRUE)
}

#' Update a5_view layer data without full re-render
#'
#' Sends new cell data to an existing a5_view widget via a Shiny custom
#' message, avoiding the full widget teardown/rebuild cycle. Much faster
#' for interactive updates.
#'
#' @param session The Shiny session object.
#' @param outputId The output ID of the a5_view widget.
#' @param cells An [a5R::a5_cell] vector or data frame.
#' @param fill Fill specification (same as [a5_view()]).
#' @param palette Palette (same as [a5_view()]).
#' @param tooltip Logical, show tooltip.
#' @export
a5_view_update <- function(
  session,
  outputId,
  cells,
  fill = "#74ac90ff",
  palette = "Viridis",
  tooltip = TRUE,
  aggregate = c("none", "rep_child", "mean"),
  lod_step = 1L
) {
  check_cells(cells)
  aggregate <- match.arg(aggregate)
  if (!rlang::is_integerish(lod_step, n = 1L) ||
      is.na(lod_step) || lod_step < 1L) {
    cli::cli_abort("{.arg lod_step} must be a positive integer.")
  }
  lod_step <- as.integer(lod_step)
  # a5_view_update mirrors a5_view's pyramid-always-lazy behaviour.

  fill_quo <- rlang::enquo(fill)
  fill_expr <- rlang::quo_get_expr(fill_quo)
  if (is.call(fill_expr) && is.data.frame(cells)) {
    fill <- rlang::eval_tidy(fill_quo, data = cells)
  }
  n_cells <- if (a5R::is_a5_cell(cells)) length(cells) else nrow(cells)
  fill_resolved <- resolve_fill(cells, fill, fill_expr, n_cells)

  # Auto-flip identity for tagged outputs (e.g. cells_rgb / cells_pca_rgb).
  if (has_identity_tag(cells, fill_resolved)) {
    if (fill_resolved$type == "column") {
      fill_resolved$identity <- TRUE
    } else if (fill_resolved$type == "numeric") {
      fill_resolved$type <- "identity"
    }
  }

  prepared <- prepare_data(cells)
  df <- prepared$data

  if (nrow(df) == 0L) return(invisible(NULL))

  fill_payload <- attach_fill(df, fill_resolved, prepared, palette)
  df <- normalize_rgba_cols(fill_payload$df)
  has_fill_value <- "_fill_value" %in% names(df)
  has_rgba_cols <- "_fill_r" %in% names(df)

  data_resolution <- as.integer(a5R::a5_get_resolution(prepared$a5_cells[[1]]))

  if (aggregate == "none") {
    pdf <- df
    lod_resolutions <- NULL
    arrow_cells <- prepared$a5_cells
  } else {
    pyramid <- build_a5_pyramid(
      leaf_cells = prepared$a5_cells,
      df = df,
      data_resolution = data_resolution,
      lod_step = lod_step,
      aggregate = aggregate
    )
    pdf <- pyramid$data
    lod_resolutions <- as.list(as.integer(pyramid$lod_resolutions))
    arrow_cells <- pyramid$cells
  }

  arrow_cols <- list(pentagon = a5R::a5_cell_to_arrow(arrow_cells))
  if (aggregate != "none") {
    arrow_cols[["_lod"]] <- arrow::Array$create(pdf[["_lod"]], type = arrow::uint8())
  }
  if (has_fill_value) {
    arrow_cols[["_fill_value"]] <- pdf[["_fill_value"]]
  }
  if (has_rgba_cols) {
    arrow_cols[["_fill_r"]] <- arrow::Array$create(pdf[["_fill_r"]], type = arrow::uint8())
    arrow_cols[["_fill_g"]] <- arrow::Array$create(pdf[["_fill_g"]], type = arrow::uint8())
    arrow_cols[["_fill_b"]] <- arrow::Array$create(pdf[["_fill_b"]], type = arrow::uint8())
    arrow_cols[["_fill_a"]] <- arrow::Array$create(pdf[["_fill_a"]], type = arrow::uint8())
  }

  parquet_payload <- NULL
  if (aggregate != "none") {
    parquet_payload <- serialise_pyramid_to_parquet(
      pdf, arrow_cells,
      has_fill_value = has_fill_value,
      has_rgba_cols = has_rgba_cols,
      extruded = FALSE
    )
    arrow_b64 <- NULL
  } else {
    arrow_tbl <- do.call(arrow::arrow_table, arrow_cols)
    ipc_raw <- arrow::write_to_raw(arrow_tbl, format = "stream")
    arrow_b64 <- base64enc::base64encode(ipc_raw)
  }

  msg <- list(
    arrow_ipc = arrow_b64,
    parquet_b64 = if (!is.null(parquet_payload)) parquet_payload$b64 else NULL,
    parquet_row_groups = if (!is.null(parquet_payload)) parquet_payload$n_row_groups else NULL,
    fill_is_column = fill_payload$fill_is_column,
    fill_color = fill_payload$fill_color,
    fill_per_cell = has_rgba_cols,
    palette = fill_payload$js_palette,
    domain = fill_payload$domain,
    has_fill_value = has_fill_value,
    tooltip = !isFALSE(tooltip),
    data_resolution = data_resolution,
    lod_resolutions = lod_resolutions
  )

  session$sendCustomMessage(paste0("a5view-update-", outputId), msg)
  invisible(NULL)
}
