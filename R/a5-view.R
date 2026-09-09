#' View A5 cells on an interactive map
#'
#' Renders A5 cells with deck.gl over a MapLibre basemap. Cells are
#' drawn as filled pentagons; the viewer provides a basemap switcher,
#' an opacity slider, a legend for numeric fills, hover tooltips, and
#' an optional polygon-draw tool.
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
#'   Default: `"#74ac90ff"`.
#' @param fill_identity Logical. When `TRUE`, treat `fill` values as
#'   literal colours rather than mapping through `palette`. Accepts
#'   packed RGB integers (`(R << 16) | (G << 8) | B`) or hex colour
#'   strings. Auto-enabled when `fill` is produced by [cells_rgb()] /
#'   [cells_pca_rgb()] or any vector tagged with
#'   `attr(x, "a5_identity") <- TRUE`. Default: `FALSE`.
#' @param palette Colour palette used when `fill` is numeric. Either a
#'   palette name accepted by [grDevices::hcl.colors()] (e.g.
#'   `"viridis"`, `"inferno"`, `"plasma"`, `"turbo"`, `"rocket"`) or
#'   a character vector of hex colours (at least 2). Default: `"Viridis"`.
#' @param opacity Numeric scalar, initial layer opacity (0--1). An
#'   interactive slider is provided in the viewer to adjust at runtime.
#'   Default: `0.3`.
#' @param tooltip Logical, or a character vector of column names from
#'   `cells`. `TRUE` (default) shows the hovered cell's id plus its fill
#'   value when `fill` is a numeric mapping. A character vector adds
#'   those columns to the tooltip (requires `aggregate = "none"`).
#'   `FALSE` disables the tooltip. The tooltip only appears over cells
#'   present in the data (see the section on hover and click below).
#' @param elevation Column name for 3D extrusion, or `NULL` for flat.
#' @param elevation_scale Numeric scalar, scale factor for elevation.
#' @param border Border (stroke) colour for cell outlines, or `NULL`
#'   for no borders. A single colour string (e.g. `"#ffffff"`,
#'   `"white"`). Default: `"#74ac9080"`.
#' @param border_width Numeric scalar, border width in pixels.
#'   Default: `1`.
#' @param width,height Widget dimensions. Default: `NULL` (fills container).
#' @param lng,lat,zoom Initial map view. If `NULL` (default),
#'   auto-centres on the cell centroids.
#' @param globe Logical. Use MapLibre's globe projection instead of
#'   the default Mercator map. Default: `FALSE`.
#' @param basemap Character vector of basemap styles to make available.
#'   Options are `"dark"`, `"light"` and `"osm"` (OpenFreeMap vector
#'   tiles; no API key), `"satellite"` (Esri World Imagery) and
#'   `"none"`. The first element is shown initially. When more than one
#'   is given, an interactive selector is shown.
#'   Default: `c("dark", "light", "osm", "satellite")`.
#' @param draw_polygon Logical. When `TRUE`, adds a "Draw polygon"
#'   toggle to the controls panel. While draw mode is active, clicks
#'   place polygon vertices and a double-click closes the polygon.
#'   On completion, the polygon is emitted to Shiny as a WKT string at
#'   `input$<id>_polygon_draw`; the drawn outline is held on screen
#'   until the user toggles draw mode off, presses Escape, or starts a
#'   new polygon. While draw mode is on, double-click-to-zoom is
#'   suppressed and cell-pick click events do not fire. The widget
#'   itself does not visualise which cells fall inside the polygon:
#'   resolve the WKT server-side (e.g. with
#'   [a5R::a5_polygon_to_cells()]) and update the map fills via
#'   [a5_view_update()]. Only useful in a Shiny context.
#'   Default: `FALSE`.
#' @param aggregate How parent cells are summarised when zooming out.
#'   One of:
#'   - `"none"` (default): no precomputed pyramid. All cells are shipped
#'     as Arrow IPC and rendered through deck.gl's `A5Layer`. Cheapest
#'     to build, suitable for small to medium datasets.
#'   - `"rep_child"`: each parent inherits the row of the child whose
#'     payload (RGBA + optional `fill_value`/`elevation`) is closest in
#'     Euclidean distance to its parent's mean. Preserves real values,
#'     no blending. Use for embedding/RGB visualisations.
#'   - `"mean"`: each numeric payload column is averaged independently
#'     within the parent group. Best for scalar fields; blends colours
#'     in RGB space, which can mute embedding visualisations.
#'   When set to `"rep_child"` or `"mean"`, the pyramid is serialised to
#'   parquet with a spatial row-group index. The browser decodes only
#'   the row groups intersecting the viewport at the level of detail
#'   matching the current zoom. Inside Shiny the parquet file is served
#'   over HTTP with byte-range support, so only the footer and the
#'   visible row groups are ever transferred; elsewhere it is embedded
#'   in the widget as base64.
#' @param lod_step Integer >= 1, gap between successive precomputed
#'   LODs (only used when `aggregate != "none"`). `1` (default)
#'   precomputes every level from the data resolution down to the
#'   floor (LOD 2); larger values trade size for fewer levels and more
#'   visible popping at zoom transitions.
#' @returns An htmlwidget.
#'
#' @section Hover, click and Shiny inputs:
#' Hover and click only resolve to cells that are present in the data:
#' the tooltip, the outline highlight and the Shiny inputs below stay
#' empty over parts of the map with no cell. With `aggregate = "none"`
#' the resolved cell is the leaf cell under the cursor. With a pyramid
#' it is the cell at the level of detail currently on screen, so the id
#' and value reported are those of the pentagon you can see; zoom in
#' to reach the leaf cells.
#'
#' When rendered in Shiny with output id `<id>`, the widget sets:
#' - `input$<id>_hover`: hex id of the cell under the cursor (or `NULL`).
#' - `input$<id>_click`: hex id of the clicked cell; clicking it again,
#'   or clicking empty map, clears it.
#' - `input$<id>_cursor`, `input$<id>_click_coord`: `list(lng, lat)` of
#'   the pointer.
#' - `input$<id>_polygon_draw`: WKT of a completed polygon
#'   (see `draw_polygon`).
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
  check_cells(cells)
  check_view_options(
    opacity, border, border_width, width, height, lng, lat, zoom,
    globe, basemap, tooltip, draw_polygon
  )
  check_number_decimal(elevation_scale, min = 0, arg = "elevation_scale")
  check_palette(palette)
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

  tooltip_cols <- resolve_tooltip_cols(tooltip, prep, aggregate)
  data <- encode_view_data(
    prep, aggregate, lod_step, tooltip_cols,
    session = shiny_session()
  )

  payload <- c(
    data,
    fill_payload(prep, fill_quo, tooltip, tooltip_cols),
    view_options_payload(
      opacity, tooltip, border, border_width, globe, basemap, draw_polygon
    ),
    list(
      extruded = prep$extruded,
      elevation_scale = elevation_scale,
      view_state = auto_view(prep$leaf_cells, lng, lat, zoom),
      data_resolution = prep$data_resolution
    )
  )

  new_a5view_widget(payload, width, height)
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
#' for interactive updates. Visual options (opacity, basemap, borders)
#' are left as they are; only the data, colours, legend and tooltip
#' change.
#'
#' @param session The Shiny session object.
#' @param outputId The output ID of the a5_view widget.
#' @inheritParams a5_view
#' @export
a5_view_update <- function(
  session,
  outputId,
  cells,
  fill = "#74ac90ff",
  fill_identity = FALSE,
  palette = "Viridis",
  tooltip = TRUE,
  aggregate = c("none", "rep_child", "mean"),
  lod_step = 1L
) {
  check_cells(cells)
  aggregate <- match.arg(aggregate)
  check_palette(palette)
  check_tooltip(tooltip)
  check_bool(fill_identity, "fill_identity")
  lod_step <- check_lod_step(lod_step)

  fill_quo <- rlang::enquo(fill)
  prep <- prepare_view_data(
    cells = cells,
    fill_quo = fill_quo,
    fill_identity = fill_identity,
    palette = palette
  )
  if (is.null(prep)) return(invisible(NULL))

  tooltip_cols <- resolve_tooltip_cols(tooltip, prep, aggregate)
  data <- encode_view_data(
    prep, aggregate, lod_step, tooltip_cols,
    session = session
  )

  msg <- c(
    data,
    fill_payload(prep, fill_quo, tooltip, tooltip_cols),
    list(data_resolution = prep$data_resolution)
  )

  session$sendCustomMessage(paste0("a5view-update-", outputId), msg)
  invisible(NULL)
}

#' Payload fields describing colours, legend and tooltip
#' @noRd
fill_payload <- function(prep, fill_quo, tooltip, tooltip_cols) {
  legend <- prep$legend
  if (!is.null(legend)) {
    legend$label <- rlang::as_label(fill_quo)
  }
  list(
    fill_color = prep$fill_color,
    fill_per_cell = prep$has_rgba_cols,
    has_fill_value = prep$has_fill_value,
    legend = legend,
    tooltip = !isFALSE(tooltip),
    tooltip_cols = as.list(tooltip_cols)
  )
}

#' Payload fields for the visual options shared by a5_view() and
#' a5_view_pyramid()
#' @noRd
view_options_payload <- function(opacity, tooltip, border, border_width,
                                 globe, basemap, draw_polygon) {
  list(
    opacity = opacity,
    tooltip = !isFALSE(tooltip),
    stroked = !is.null(border),
    line_color = if (!is.null(border)) hex_to_rgba(border) else NULL,
    line_width = border_width,
    globe = globe,
    basemaps = as.list(basemap),
    draw_polygon = draw_polygon
  )
}

#' Wrap a payload in the a5view htmlwidget with the Arrow JS dependency
#' @noRd
new_a5view_widget <- function(payload, width, height) {
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
  geoarrowWidget::attachArrowDependency(widget)
}
