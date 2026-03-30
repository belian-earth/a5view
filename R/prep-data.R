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
  coords <- a5R::a5_cell_to_lonlat(cells, normalise = FALSE)

  ctr_lng <- if (!is.null(lng)) lng else mean(coords$lon, na.rm = TRUE)
  ctr_lat <- if (!is.null(lat)) lat else mean(coords$lat, na.rm = TRUE)

  z <- if (!is.null(zoom)) zoom else guess_zoom(coords)

  list(
    longitude = ctr_lng,
    latitude = ctr_lat,
    zoom = z,
    pitch = 0,
    bearing = 0
  )
}

#' Guess a reasonable zoom level from coordinate extent
#' @noRd
guess_zoom <- function(coords) {
  lng_range <- diff(range(coords$lon, na.rm = TRUE))
  lat_range <- diff(range(coords$lat, na.rm = TRUE))
  span <- max(lng_range, lat_range)
  if (span < 1e-6) {
    return(24L)
  }
  span <- span * 1.3
  z <- log2(360 / span)
  max(1L, min(24L, floor(z)))
}
