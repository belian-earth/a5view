# Argument validation helpers

#' @noRd
check_cells <- function(cells) {
  if (missing(cells)) {
    cli::cli_abort("{.arg cells} is required.")
  }
  if (!a5R::is_a5_cell(cells) && !is.data.frame(cells)) {
    cli::cli_abort(
      "{.arg cells} must be an {.cls a5_cell} vector or a data frame, not {.obj_type_friendly {cells}}."
    )
  }
  if (a5R::is_a5_cell(cells) && length(cells) == 0L) {
    cli::cli_abort("{.arg cells} must have length > 0.")
  }
  if (is.data.frame(cells) && nrow(cells) == 0L) {
    cli::cli_abort("{.arg cells} data frame must have at least one row.")
  }
}

#' @noRd
check_number_decimal <- function(
  x,
  min = -Inf,
  max = Inf,
  arg = rlang::caller_arg(x)
) {
  if (!rlang::is_scalar_double(x) && !rlang::is_scalar_integer(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number, not {.obj_type_friendly {x}}."
    )
  }
  if (is.na(x)) {
    cli::cli_abort("{.arg {arg}} must not be {.val NA}.")
  }
  if (x < min || x > max) {
    if (is.finite(min) && is.finite(max)) {
      cli::cli_abort("{.arg {arg}} must be between {min} and {max}, not {x}.")
    } else if (is.finite(min)) {
      cli::cli_abort("{.arg {arg}} must be >= {min}, not {x}.")
    }
  }
}

#' @noRd
check_optional_number <- function(x, arg) {
  if (!is.null(x)) {
    check_number_decimal(x, arg = arg)
  }
}

#' @noRd
check_optional_dimension <- function(x, arg) {
  if (!is.null(x)) {
    ok <- rlang::is_scalar_integerish(x) ||
      (rlang::is_string(x) && grepl("^[0-9]+(px|%|em|vh|vw)$", x))
    if (!ok) {
      cli::cli_abort(
        '{.arg {arg}} must be a number, a CSS string (e.g. "100%", "400px"), or NULL.'
      )
    }
  }
}

#' @noRd
check_border <- function(border) {
  if (!is.null(border)) {
    if (!rlang::is_string(border)) {
      cli::cli_abort(
        "{.arg border} must be a single colour string or NULL, not {.obj_type_friendly {border}}."
      )
    }
    tryCatch(
      grDevices::col2rgb(border),
      error = function(e) {
        cli::cli_abort(c(
          "{.arg border} colour {.val {border}} is not valid.",
          "x" = conditionMessage(e)
        ))
      }
    )
  }
}

#' @noRd
check_basemap <- function(basemap) {
  valid_bm <- c("dark", "light", "osm", "satellite", "none")
  if (!is.character(basemap)) {
    cli::cli_abort(
      "{.arg basemap} must be a character vector, not {.obj_type_friendly {basemap}}."
    )
  }
  bad_bm <- setdiff(basemap, valid_bm)
  if (length(bad_bm) > 0) {
    cli::cli_abort(
      "{.arg basemap} contains {cli::qty(length(bad_bm))} invalid value{?s}: {.val {bad_bm}}."
    )
  }
}

#' @noRd
check_tooltip <- function(tooltip) {
  if (!rlang::is_bool(tooltip) && !is.character(tooltip)) {
    cli::cli_abort(
      "{.arg tooltip} must be {.cls logical} or a character vector of column names."
    )
  }
}

#' @noRd
check_palette <- function(palette) {
  if (is.character(palette) && length(palette) == 1L) {
    available <- grDevices::hcl.pals()
    if (!palette %in% available) {
      dists <- utils::adist(tolower(palette), tolower(available))[1, ]
      closest <- available[order(dists)[1:3]]
      cli::cli_abort(c(
        "Palette {.val {palette}} is not a recognised {.fn grDevices::hcl.colors} palette.",
        "i" = "Did you mean {.or {.val {closest}}}?",
        "i" = "Run {.code grDevices::hcl.pals()} to see all available palettes."
      ))
    }
  } else if (is.character(palette) && length(palette) > 1L) {
    tryCatch(
      grDevices::col2rgb(palette),
      error = function(e) {
        cli::cli_abort(c(
          "Invalid colours in {.arg palette}.",
          "x" = conditionMessage(e)
        ))
      }
    )
    if (length(palette) < 2L) {
      cli::cli_abort(
        "{.arg palette} must contain at least 2 colours when providing a custom palette."
      )
    }
  } else {
    cli::cli_abort(
      "{.arg palette} must be a palette name (string) or a character vector of colours, not {.obj_type_friendly {palette}}."
    )
  }
}
