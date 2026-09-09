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
  check_bool(fill_identity, "fill_identity")
  lod_step <- check_lod_step(lod_step)

  fill_quo <- rlang::enquo(fill)
  prep <- prepare_view_data(
    cells = cells,
    fill_quo = fill_quo,
    fill_identity = fill_identity,
    palette = palette,
    elev_expr = substitute(elevation)
  )
  if (is.null(prep)) {
    cli::cli_abort("No non-NA cells to display.")
  }

  pyramid <- build_a5_pyramid(
    leaf_cells = prep$leaf_cells,
    df = prep$df,
    data_resolution = prep$data_resolution,
    lod_step = lod_step,
    aggregate = aggregate
  )

  legend <- prep$legend
  if (!is.null(legend)) legend$label <- rlang::as_label(fill_quo)

  meta <- list(
    data_resolution = prep$data_resolution,
    lod_resolutions = as.list(as.integer(pyramid$lod_resolutions)),
    has_fill_value = prep$has_fill_value,
    fill_per_cell = prep$has_rgba_cols,
    extruded = prep$extruded,
    fill_color = prep$fill_color,
    legend = legend,
    view_state = auto_view(prep$df[["pentagon"]], lng, lat, zoom)
  )

  serialise_pyramid_to_parquet(
    pyramid$data, pyramid$cells,
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
  check_view_options(
    opacity, border, border_width, width, height, lng, lat, zoom,
    globe, basemap, tooltip, draw_polygon
  )
  if (is.character(tooltip)) {
    cli::cli_abort(c(
      "{.arg tooltip} must be {.val TRUE} or {.val FALSE} for prebuilt pyramids.",
      "i" = "Extra tooltip columns are only available from {.fn a5_view} with {.code aggregate = \"none\"}."
    ))
  }

  meta <- read_pyramid_meta(path)
  if (is.null(meta)) {
    cli::cli_abort(c(
      "{.arg path} does not look like an a5view pyramid file.",
      i = "Expected an {.code a5view_meta} KV entry written by {.fn a5_build_pyramid}."
    ))
  }

  bytes <- readBin(path, "raw", n = file.info(path)$size)

  view_state <- meta$view_state
  if (!is.null(lng)) view_state$longitude <- lng
  if (!is.null(lat)) view_state$latitude <- lat
  if (!is.null(zoom)) view_state$zoom <- zoom

  payload <- c(
    list(
      arrow_ipc = NULL,
      parquet_b64 = base64enc::base64encode(bytes),
      lod_resolutions = meta$lod_resolutions,
      fill_color = meta$fill_color,
      fill_per_cell = isTRUE(meta$fill_per_cell),
      has_fill_value = isTRUE(meta$has_fill_value),
      legend = meta$legend,
      tooltip_cols = list()
    ),
    view_options_payload(
      opacity, tooltip, border, border_width, globe, basemap, draw_polygon
    ),
    list(
      extruded = isTRUE(meta$extruded),
      elevation_scale = 1,
      view_state = view_state,
      data_resolution = meta$data_resolution
    )
  )

  new_a5view_widget(payload, width, height)
}
