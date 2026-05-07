#' Build and save an A5 pyramid parquet file
#'
#' Performs the same data preparation, fill resolution, and LOD-pyramid
#' construction as [a5_view()] with `aggregate != "none"`, then writes
#' the result to a parquet file. The file can be replayed by
#' [a5_view_pyramid()] without rebuilding, which is useful for large
#' datasets where the pyramid takes time to compute.
#'
#' Colours are baked into the parquet (as `_fill_r/g/b/a` columns when
#' `fill` produces per-cell colours, or as a single uniform colour
#' stored in the file's KV metadata otherwise). One file therefore
#' encodes one colour scheme; swapping palettes requires rebuilding.
#'
#' @inheritParams a5_view
#' @param path Destination path for the parquet file. Required.
#' @param aggregate Aggregation strategy. Same as [a5_view()] but
#'   `"none"` is rejected here -- pyramids only make sense for
#'   aggregated data. Default: `"rep_child"`.
#' @return `path`, invisibly.
#' @export
a5_build_pyramid <- function(
  cells,
  path,
  fill = "#74ac90ff",
  fill_identity = FALSE,
  palette = "Viridis",
  elevation = NULL,
  aggregate = c("rep_child", "mean"),
  lod_step = 1L,
  lng = NULL,
  lat = NULL,
  zoom = NULL
) {
  rlang::check_required(path)
  if (!rlang::is_string(path)) {
    cli::cli_abort("{.arg path} must be a single string.")
  }
  aggregate <- match.arg(aggregate)
  check_cells(cells)
  check_palette(palette)
  check_optional_number(lng, "lng")
  check_optional_number(lat, "lat")
  check_optional_number(zoom, "zoom")
  if (!rlang::is_bool(fill_identity)) {
    cli::cli_abort("{.arg fill_identity} must be {.val TRUE} or {.val FALSE}.")
  }
  if (!rlang::is_integerish(lod_step, n = 1L) ||
      is.na(lod_step) || lod_step < 1L) {
    cli::cli_abort("{.arg lod_step} must be a positive integer.")
  }
  lod_step <- as.integer(lod_step)

  fill_quo <- rlang::enquo(fill)
  fill_expr <- rlang::quo_get_expr(fill_quo)
  elev_expr <- substitute(elevation)
  if (is.call(fill_expr) && is.data.frame(cells)) {
    fill <- rlang::eval_tidy(fill_quo, data = cells)
  }

  prep <- prepare_pyramid_data(
    cells = cells,
    fill = fill,
    fill_expr = fill_expr,
    fill_identity = fill_identity,
    palette = palette,
    elev_expr = elev_expr,
    aggregate = aggregate,
    lod_step = lod_step
  )

  view_state <- auto_view(prep$df[["pentagon"]], lng, lat, zoom)

  meta <- list(
    data_resolution = prep$data_resolution,
    lod_resolutions = as.list(as.integer(prep$lod_resolutions)),
    has_fill_value = prep$has_fill_value,
    fill_per_cell = prep$has_rgba_cols,
    extruded = prep$extruded,
    fill_color = prep$fill_payload$fill_color,
    view_state = view_state
  )

  serialise_pyramid_to_parquet(
    prep$pdf, prep$arrow_cells,
    has_fill_value = prep$has_fill_value,
    has_rgba_cols = prep$has_rgba_cols,
    extruded = prep$extruded,
    path = path,
    meta = meta
  )

  invisible(path)
}

#' View a prebuilt A5 pyramid parquet file
#'
#' Reads a parquet file produced by [a5_build_pyramid()] and renders it
#' through the same lazy row-group decoder as [a5_view()] does for
#' aggregated data. No data preparation or LOD construction happens at
#' view time; only the parquet bytes and a few KV metadata entries are
#' shipped to the browser.
#'
#' Colours, the data resolution, and the auto-centred view are all
#' baked into the file at build time. Visualisation-only options
#' (opacity, basemap, globe, border width, drawing tools, ...) remain
#' arguments here.
#'
#' @param path Path to a parquet file produced by [a5_build_pyramid()].
#' @inheritParams a5_view
#' @return An htmlwidget.
#' @export
a5_view_pyramid <- function(
  path,
  opacity = 0.3,
  tooltip = TRUE,
  border = NULL,
  border_width = 1,
  width = NULL,
  height = NULL,
  lng = NULL,
  lat = NULL,
  zoom = NULL,
  globe = FALSE,
  basemap = c("dark", "light", "osm", "satellite"),
  draw_polygon = FALSE
) {
  rlang::check_required(path)
  if (!rlang::is_string(path) || !file.exists(path)) {
    cli::cli_abort("{.arg path} must point to an existing parquet file.")
  }
  check_number_decimal(opacity, min = 0, max = 1, arg = "opacity")
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
  check_draw_polygon(draw_polygon)

  meta <- read_pyramid_meta(path)
  if (is.null(meta)) {
    cli::cli_abort(c(
      "{.arg path} does not look like an a5view pyramid file.",
      i = "Expected an {.code a5view_meta} KV entry written by {.fn a5_build_pyramid}."
    ))
  }

  bytes <- readBin(path, "raw", n = file.info(path)$size)
  parquet_b64 <- base64enc::base64encode(bytes)

  view_state <- meta$view_state
  if (!is.null(lng)) view_state$longitude <- lng
  if (!is.null(lat)) view_state$latitude <- lat
  if (!is.null(zoom)) view_state$zoom <- zoom

  payload <- list(
    arrow_ipc = NULL,
    parquet_b64 = parquet_b64,
    fill_is_column = FALSE,
    fill_color = meta$fill_color,
    fill_per_cell = isTRUE(meta$fill_per_cell),
    palette = NULL,
    domain = NULL,
    opacity = opacity,
    extruded = isTRUE(meta$extruded),
    elevation_scale = 1,
    pickable = !isFALSE(tooltip),
    tooltip = !isFALSE(tooltip),
    has_fill_value = isTRUE(meta$has_fill_value),
    stroked = !is.null(border),
    line_color = if (!is.null(border)) hex_to_rgba(border) else NULL,
    line_width = border_width,
    view_state = view_state,
    globe = globe,
    basemaps = as.list(basemap),
    draw_polygon = draw_polygon,
    data_resolution = meta$data_resolution,
    lod_resolutions = meta$lod_resolutions
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
  widget <- geoarrowWidget::attachArrowDependency(widget)
  widget
}

#' Shared data prep for the pyramid path
#'
#' Resolves fill + elevation, prepares the data frame, attaches per-cell
#' RGBA, and constructs the LOD pyramid. Callers (a5_view,
#' a5_build_pyramid) handle their own NSE around `fill` and `elevation`
#' and pass the resolved values in.
#' @noRd
prepare_pyramid_data <- function(cells, fill, fill_expr, fill_identity,
                                 palette, elev_expr, aggregate, lod_step) {
  n_cells <- if (a5R::is_a5_cell(cells)) length(cells) else nrow(cells)
  fill_resolved <- resolve_fill(cells, fill, fill_expr, n_cells)

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
    cli::cli_abort("No non-NA cells to display.")
  }

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

  has_fill_value <- "_fill_value" %in% names(df)
  has_rgba_cols <- "_fill_r" %in% names(df)

  data_resolution <- as.integer(a5R::a5_get_resolution(prepared$a5_cells[[1]]))

  pyramid <- build_a5_pyramid(
    leaf_cells = prepared$a5_cells,
    df = df,
    data_resolution = data_resolution,
    lod_step = lod_step,
    aggregate = aggregate
  )

  list(
    pdf = pyramid$data,
    df = df,
    arrow_cells = pyramid$cells,
    fill_payload = fill_payload,
    has_fill_value = has_fill_value,
    has_rgba_cols = has_rgba_cols,
    extruded = extruded,
    data_resolution = data_resolution,
    lod_resolutions = pyramid$lod_resolutions
  )
}
