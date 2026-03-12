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
#'   Default: `"#3388ff"`.
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
#' @returns An htmlwidget.
#'
#' @export
a5_view <- function(
  cells,
  fill = "#74ac90ff",
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
  basemap = c("dark", "light", "osm", "satellite")
) {
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

  # --- Resolve fill and elevation (NSE) ---
  fill_expr <- substitute(fill)
  elev_expr <- substitute(elevation)

  n_cells <- if (a5R::is_a5_cell(cells)) length(cells) else nrow(cells)
  fill_resolved <- resolve_fill(cells, fill, fill_expr, n_cells)

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
  df <- fill_payload$df

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

  # --- Auto-center view ---
  view_state <- auto_view(df[["pentagon"]], lng, lat, zoom)

  # --- Build JS payload (columnar for compact JSON) ---
  columns <- lapply(df, function(col) {
    if (is.list(col)) col else unname(col)
  })

  has_per_cell_rgba <- "_fill_rgba" %in% names(df)

  payload <- list(
    columns = columns,
    fill_is_column = fill_payload$fill_is_column,
    fill_color = fill_payload$fill_color,
    fill_per_cell = has_per_cell_rgba,
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
    basemaps = as.list(basemap)
  )

  htmlwidgets::createWidget(
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
