# Fill, palette, and colour resolution

#' Resolve fill argument into a typed result
#' @noRd
resolve_fill <- function(cells, fill, fill_expr, n_cells) {
  # Case 1: unquoted column name in a data frame
  if (is.name(fill_expr) && is.data.frame(cells)) {
    col <- as.character(fill_expr)
    if (col %in% names(cells)) {
      return(list(type = "column", col = col))
    }
    cli::cli_abort(
      "Column {.val {col}} not found in {.arg cells}. Available columns: {.val {names(cells)}}."
    )
  }

  # Case 2: numeric vector — map through palette
  if (is.numeric(fill)) {
    if (length(fill) == 1L) {
      cli::cli_abort(c(
        "{.arg fill} is a single number ({fill}).",
        "i" = "Use a hex colour string (e.g. {.val #3388ff}) for uniform fill,
               or a numeric vector of length {n_cells} for colour mapping."
      ))
    }
    if (length(fill) != n_cells) {
      cli::cli_abort(
        "{.arg fill} has length {length(fill)} but {.arg cells} has {n_cells} element{?s}."
      )
    }
    if (all(is.na(fill))) {
      cli::cli_abort("{.arg fill} must not be all {.val NA}.")
    }
    return(list(type = "numeric", values = fill))
  }

  # Case 3: character vector of colours (length > 1)
  if (is.character(fill) && length(fill) > 1L) {
    if (length(fill) != n_cells) {
      cli::cli_abort(
        "{.arg fill} has length {length(fill)} but {.arg cells} has {n_cells} element{?s}."
      )
    }
    tryCatch(
      grDevices::col2rgb(fill),
      error = function(e) {
        cli::cli_abort(c(
          "Invalid colours in {.arg fill}.",
          "x" = conditionMessage(e)
        ))
      }
    )
    return(list(type = "colors", values = fill))
  }

  # Case 4: single colour string
  if (is.character(fill) && length(fill) == 1L) {
    tryCatch(
      grDevices::col2rgb(fill),
      error = function(e) {
        cli::cli_abort(c(
          "{.arg fill} colour {.val {fill}} is not valid.",
          "x" = conditionMessage(e)
        ))
      }
    )
    return(list(type = "uniform", value = fill))
  }

  cli::cli_abort(
    "{.arg fill} must be a colour string, numeric vector, character vector of colours, or a column name, not {.obj_type_friendly {fill}}."
  )
}

#' Resolve palette to a vector of hex colours
#' @noRd
resolve_palette <- function(palette, n = 8L) {
  if (length(palette) == 1L && is.character(palette)) {
    grDevices::hcl.colors(n, palette = palette)
  } else {
    palette
  }
}

#' Attach fill data to the data frame and return JS payload components
#' @return List with `df`, `fill_is_column`, `fill_color`, `js_palette`, `domain`.
#' @noRd
attach_fill <- function(df, fill_resolved, prepared, palette) {
  if (fill_resolved$type == "column") {
    col_vals <- prepared$extra[[fill_resolved$col]]
    if (is.null(col_vals)) {
      cli::cli_abort(
        "Column {.val {fill_resolved$col}} not found in {.arg cells}."
      )
    }
    if (!is.numeric(col_vals)) {
      cli::cli_abort(
        "Column {.val {fill_resolved$col}} must be numeric for fill mapping, not {.obj_type_friendly {col_vals}}."
      )
    }
    vals <- as.numeric(col_vals)
    domain <- range(vals, na.rm = TRUE)
    rgba <- values_to_rgba(vals, domain, palette)
    df[["_fill_value"]] <- vals
    df[["_fill_r"]] <- rgba$r
    df[["_fill_g"]] <- rgba$g
    df[["_fill_b"]] <- rgba$b
    df[["_fill_a"]] <- rgba$a
    list(
      df = df,
      fill_is_column = TRUE,
      fill_color = NULL,
      js_palette = NULL,
      domain = domain
    )
  } else if (fill_resolved$type == "numeric") {
    vals_sub <- fill_resolved$values[prepared$not_na]
    domain <- range(vals_sub, na.rm = TRUE)
    rgba <- values_to_rgba(vals_sub, domain, palette)
    df[["_fill_value"]] <- vals_sub
    df[["_fill_r"]] <- rgba$r
    df[["_fill_g"]] <- rgba$g
    df[["_fill_b"]] <- rgba$b
    df[["_fill_a"]] <- rgba$a
    list(
      df = df,
      fill_is_column = TRUE,
      fill_color = NULL,
      js_palette = NULL,
      domain = domain
    )
  } else if (fill_resolved$type == "colors") {
    cols_sub <- fill_resolved$values[prepared$not_na]
    df[["_fill_rgba"]] <- lapply(cols_sub, hex_to_rgba)
    list(
      df = df,
      fill_is_column = FALSE,
      fill_color = NULL,
      js_palette = NULL,
      domain = NULL
    )
  } else {
    list(
      df = df,
      fill_is_column = FALSE,
      fill_color = hex_to_rgba(fill_resolved$value),
      js_palette = NULL,
      domain = NULL
    )
  }
}

#' Convert hex colour string to RGBA array
#' @noRd
hex_to_rgba <- function(hex) {
  rgb <- grDevices::col2rgb(hex, alpha = TRUE)
  as.integer(rgb[, 1])
}

#' Map numeric values to RGBA through a palette (vectorised)
#'
#' Returns a list with integer vectors r, g, b, a (each length n).
#' @noRd
values_to_rgba <- function(values, domain, palette) {
  pal_hex <- resolve_palette(palette, 256L)
  rng <- domain[2] - domain[1]
  if (rng == 0) {
    t <- rep(0.5, length(values))
  } else {
    t <- pmin(1, pmax(0, (values - domain[1]) / rng))
  }
  # Map to palette index
  idx <- pmin(length(pal_hex), pmax(1L, as.integer(t * (length(pal_hex) - 1)) + 1L))
  rgba <- grDevices::col2rgb(pal_hex[idx], alpha = TRUE)
  list(
    r = as.integer(rgba[1L, ]),
    g = as.integer(rgba[2L, ]),
    b = as.integer(rgba[3L, ]),
    a = as.integer(rgba[4L, ])
  )
}
