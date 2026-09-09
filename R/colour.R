# Fill, palette, and colour resolution

#' Build per-cell packed-integer RGB colours from three numeric channels
#'
#' Convenience helper for the common workflow of rescaling three numeric
#' vectors (e.g. spectral bands) to `0:255` and combining them into the
#' packed `(R << 16) | (G << 8) | B` integer form expected by
#' [a5_view()] with `fill_identity = TRUE`.
#'
#' Each channel is rescaled to `0:255` independently using its own
#' min/max, unless `range` is supplied, in which case the same bounds
#' are applied to all three channels (useful when the bands are
#' radiometrically comparable). Values outside `range` are clipped.
#' `NA` inputs propagate to `NA` in the output. Constant channels (or
#' channels that are entirely `NA` under per-channel scaling) contribute
#' zero.
#'
#' @param r,g,b Numeric vectors of identical length supplying the red,
#'   green and blue channels.
#' @param range Optional length-2 numeric `c(min, max)` shared across all
#'   three channels. `NULL` (default) rescales each channel independently.
#'
#' @return An integer vector of packed RGB values, the same length as
#'   the inputs.
#'
#' @examples
#' n <- 5
#' df <- data.frame(b1 = runif(n), b2 = runif(n), b3 = runif(n))
#' df$rgb <- cells_rgb(df$b1, df$b2, df$b3)
#'
#' @export
cells_rgb <- function(r, g, b, range = NULL) {
  n <- length(r)
  if (length(g) != n || length(b) != n) {
    cli::cli_abort(
      "{.arg r}, {.arg g} and {.arg b} must be the same length \\
       (got {length(r)}, {length(g)}, {length(b)})."
    )
  }
  if (!is.numeric(r) || !is.numeric(g) || !is.numeric(b)) {
    cli::cli_abort(
      "{.arg r}, {.arg g} and {.arg b} must be numeric vectors."
    )
  }

  if (!is.null(range)) {
    if (!is.numeric(range) || length(range) != 2L || anyNA(range)) {
      cli::cli_abort("{.arg range} must be a length-2 numeric vector.")
    }
    if (range[1] == range[2]) {
      cli::cli_abort("{.arg range} must have distinct min and max.")
    }
    rb <- rescale_byte(r, range[1], range[2])
    gb <- rescale_byte(g, range[1], range[2])
    bb <- rescale_byte(b, range[1], range[2])
  } else {
    rb <- rescale_byte_auto(r)
    gb <- rescale_byte_auto(g)
    bb <- rescale_byte_auto(b)
  }

  out <- bitwOr(bitwOr(bitwShiftL(rb, 16L), bitwShiftL(gb, 8L)), bb)
  attr(out, "a5_identity") <- TRUE
  out
}

#' Build per-cell RGB colours from the first three principal components
#' of an embedding
#'
#' Convenience helper for the common embedding-visualisation workflow:
#' stack a list-column (or matrix) of equal-length numeric vectors,
#' run a 3-component PCA, percentile-clip the scores per channel to
#' tame outliers, and pack into integer RGB suitable for [a5_view()]
#' with `fill_identity = TRUE`. Output is handed off to [cells_rgb()].
#'
#' Requires the `irlba` package, which is much faster than a full SVD
#' when only the top three components are needed.
#'
#' @param x A list of numeric vectors of identical length (e.g. an arrow
#'   list-column of embeddings) or a numeric matrix with rows = cells
#'   and columns = features. Must not contain `NA`.
#' @param clip Length-2 numeric percentile range applied per principal
#'   component to clip outliers before colour mapping. Default
#'   `c(0.02, 0.98)`. Set to `NULL` to skip clipping.
#' @param scale Passed to `irlba::prcomp_irlba(..., scale. = scale)`.
#'   Standardises features to unit variance before PCA. Default `TRUE`.
#' @param rgb_pcs Length-3 signed integer vector specifying which PC
#'   maps to each colour channel, in red/green/blue order. The default
#'   `c(1, 2, 3)` assigns PC1 to red, PC2 to green and PC3 to blue.
#'   Negative values flip the sign of the corresponding PC, useful
#'   because PCA components are arbitrary up to sign — e.g.
#'   `c(1, -2, 3)` uses `-PC2` for green. Must be a signed permutation
#'   of `1:3`.
#'
#' @return An integer vector of packed RGB values, the same length as
#'   the number of rows in `x`.
#'
#' @examples
#' \dontrun{
#' df$rgb <- cells_pca_rgb(df$embedding)
#' a5_view(df, fill = rgb, fill_identity = TRUE)
#'
#' # Reorder and flip a channel for better contrast:
#' df$rgb <- cells_pca_rgb(df$embedding, rgb_pcs = c(3, -1, 2))
#' }
#'
#' @export
cells_pca_rgb <- function(x, clip = c(0.02, 0.98), scale = TRUE,
                          rgb_pcs = c(1, 2, 3)) {
  if (!rlang::is_installed("irlba")) {
    cli::cli_abort(c(
      "{.fn cells_pca_rgb} requires the {.pkg irlba} package.",
      "i" = "Install with {.code install.packages(\"irlba\")}."
    ))
  }
  if (!is.null(clip)) {
    if (!is.numeric(clip) || length(clip) != 2L || anyNA(clip)) {
      cli::cli_abort("{.arg clip} must be a length-2 numeric vector.")
    }
    if (clip[1] < 0 || clip[2] > 1 || clip[1] >= clip[2]) {
      cli::cli_abort("{.arg clip} must be increasing within [0, 1].")
    }
  }
  if (!is.numeric(rgb_pcs) || length(rgb_pcs) != 3L || anyNA(rgb_pcs) ||
      any(rgb_pcs != as.integer(rgb_pcs)) || any(rgb_pcs == 0L) ||
      !setequal(abs(rgb_pcs), 1:3)) {
    cli::cli_abort(
      "{.arg rgb_pcs} must be a signed permutation of {.code 1:3} \\
       (e.g. {.code c(1, 2, 3)} or {.code c(1, -2, 3)})."
    )
  }

  mat <- as_embedding_matrix(x)
  if (nrow(mat) < 4L) {
    cli::cli_abort(
      "{.arg x} has {nrow(mat)} row{?s}; need at least 4 for a 3-component PCA."
    )
  }
  if (ncol(mat) < 3L) {
    cli::cli_abort(
      "{.arg x} has {ncol(mat)} feature{?s}; need at least 3 for a 3-component PCA."
    )
  }

  fit <- irlba::prcomp_irlba(mat, n = 3L, scale. = scale)
  scores <- fit$x

  if (!is.null(clip)) {
    scores[, 1L] <- clip_quantile(scores[, 1L], clip)
    scores[, 2L] <- clip_quantile(scores[, 2L], clip)
    scores[, 3L] <- clip_quantile(scores[, 3L], clip)
  }

  idx <- abs(rgb_pcs)
  sgn <- sign(rgb_pcs)
  cells_rgb(
    scores[, idx[1L]] * sgn[1L],
    scores[, idx[2L]] * sgn[2L],
    scores[, idx[3L]] * sgn[3L]
  )
}

#' Coerce a list-of-vectors or matrix into a numeric matrix (rows = cells)
#' @noRd
as_embedding_matrix <- function(x) {
  if (is.matrix(x) && is.numeric(x)) {
    if (anyNA(x)) {
      cli::cli_abort("{.arg x} must not contain {.val NA} values.")
    }
    return(x)
  }
  if (is.list(x) && !is.data.frame(x)) {
    lens <- lengths(x)
    if (length(x) == 0L || any(lens == 0L)) {
      cli::cli_abort("All elements of {.arg x} must be non-empty.")
    }
    if (length(unique(lens)) != 1L) {
      cli::cli_abort(
        "All elements of {.arg x} must have the same length \\
         (got lengths {.val {sort(unique(lens))}})."
      )
    }
    flat <- unlist(x, use.names = FALSE)
    if (!is.numeric(flat)) {
      cli::cli_abort("All elements of {.arg x} must be numeric.")
    }
    if (anyNA(flat)) {
      cli::cli_abort("{.arg x} must not contain {.val NA} values.")
    }
    return(matrix(flat, nrow = length(x), ncol = lens[1], byrow = TRUE))
  }
  cli::cli_abort(
    "{.arg x} must be a numeric matrix or a list of numeric vectors, \\
     not {.obj_type_friendly {x}}."
  )
}

#' Clip a numeric vector to a quantile range
#' @noRd
clip_quantile <- function(x, range) {
  q <- stats::quantile(x, range, na.rm = TRUE, names = FALSE)
  pmin(pmax(x, q[1]), q[2])
}

#' Rescale a numeric vector to the integer range 0:255 given bounds
#' @noRd
rescale_byte <- function(x, lo, hi) {
  if (!is.finite(lo) || !is.finite(hi)) {
    return(rep(NA_integer_, length(x)))
  }
  if (hi == lo) {
    out <- rep(0L, length(x))
    out[is.na(x)] <- NA_integer_
    return(out)
  }
  as.integer(round(pmin(pmax((x - lo) / (hi - lo), 0), 1) * 255))
}

#' Rescale using a vector's own min/max, propagating all-NA as NA
#' @noRd
rescale_byte_auto <- function(x) {
  if (all(is.na(x))) {
    return(rep(NA_integer_, length(x)))
  }
  rescale_byte(x, min(x, na.rm = TRUE), max(x, na.rm = TRUE))
}

#' Detect whether a resolved fill carries the `a5_identity` attribute
#'
#' Set by [cells_rgb()] / [cells_pca_rgb()] (or any user code) on packed-
#' integer RGB outputs so [a5_view()] can auto-flip `fill_identity`.
#' @noRd
has_identity_tag <- function(cells, fill_resolved) {
  tagged_value <- switch(
    fill_resolved$type,
    "numeric" = fill_resolved$values,
    "column"  = if (is.data.frame(cells)) cells[[fill_resolved$col]] else NULL,
    NULL
  )
  !is.null(tagged_value) && isTRUE(attr(tagged_value, "a5_identity"))
}

#' Resolve the `fill` argument into a typed result
#'
#' `fill_quo` is the quosure captured by the exported function. A bare
#' symbol naming a column of `cells` resolves to that column without
#' being evaluated; anything else is evaluated with the columns of
#' `cells` (when a data frame) as a data mask, so helpers such as
#' [cells_rgb()] can reference columns directly and ordinary variables
#' from the caller's environment still work.
#' @noRd
resolve_fill <- function(cells, fill_quo, n_cells) {
  expr <- rlang::quo_get_expr(fill_quo)
  if (is.name(expr) && is.data.frame(cells) &&
      as.character(expr) %in% names(cells)) {
    return(list(type = "column", col = as.character(expr)))
  }

  mask <- if (is.data.frame(cells)) cells else NULL
  fill <- rlang::eval_tidy(fill_quo, data = mask)

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

  if (is.character(fill) && length(fill) > 1L) {
    if (length(fill) != n_cells) {
      cli::cli_abort(
        "{.arg fill} has length {length(fill)} but {.arg cells} has {n_cells} element{?s}."
      )
    }
    check_colours(fill, "fill")
    return(list(type = "colors", values = fill))
  }

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

#' Attach per-cell fill columns to the data frame
#'
#' Per-cell colours land in `_fill_r/g/b/a` integer columns; numeric
#' mappings additionally keep the raw value in `_fill_value`.
#' @return List with `df`, `fill_color` (RGBA integer vector for a
#'   uniform fill, else `NULL`) and `legend` (`list(colors, domain)`
#'   for numeric mappings, else `NULL`).
#' @noRd
attach_fill <- function(df, fill_resolved, prepared, palette) {
  type <- fill_resolved$type
  fill_color <- NULL
  legend <- NULL

  if (type == "uniform") {
    fill_color <- hex_to_rgba(fill_resolved$value)
  } else {
    if (type == "column") {
      vals <- prepared$extra[[fill_resolved$col]]
      if (is.null(vals)) {
        cli::cli_abort(
          "Column {.val {fill_resolved$col}} not found in {.arg cells}."
        )
      }
      if (isTRUE(fill_resolved$identity)) {
        type <- "identity"
      } else if (!is.numeric(vals)) {
        cli::cli_abort(
          "Column {.val {fill_resolved$col}} must be numeric for fill mapping, not {.obj_type_friendly {vals}}."
        )
      } else {
        type <- "numeric"
      }
    } else {
      vals <- fill_resolved$values[prepared$not_na]
    }

    if (type == "numeric") {
      vals <- as.numeric(vals)
      domain <- range(vals, na.rm = TRUE)
      pal_hex <- resolve_palette(palette, 256L)
      rgba <- values_to_rgba(vals, domain, pal_hex)
      df[["_fill_value"]] <- vals
      legend <- list(colors = pal_hex, domain = domain)
    } else if (type == "identity") {
      rgba <- identity_to_rgba(vals)
    } else {
      rgba <- colours_to_rgba(vals)
    }
    df[["_fill_r"]] <- rgba$r
    df[["_fill_g"]] <- rgba$g
    df[["_fill_b"]] <- rgba$b
    df[["_fill_a"]] <- rgba$a
  }

  list(df = df, fill_color = fill_color, legend = legend)
}

#' Convert a single colour string to an RGBA integer vector
#' @noRd
hex_to_rgba <- function(hex) {
  rgb <- grDevices::col2rgb(hex, alpha = TRUE)
  as.integer(rgb[, 1])
}

#' Convert a vector of colour strings to RGBA channel vectors
#' @return A list of integer vectors `r`, `g`, `b`, `a`.
#' @noRd
colours_to_rgba <- function(cols) {
  m <- grDevices::col2rgb(cols, alpha = TRUE)
  list(
    r = as.integer(m[1L, ]),
    g = as.integer(m[2L, ]),
    b = as.integer(m[3L, ]),
    a = as.integer(m[4L, ])
  )
}

#' Convert identity fill values to RGBA channel vectors
#'
#' Handles packed RGB integers `(R << 16) | (G << 8) | B` or colour
#' strings.
#' @param values Numeric (packed RGB) or character (colour) vector.
#' @return A list of integer vectors `r`, `g`, `b`, `a`.
#' @noRd
identity_to_rgba <- function(values) {
  if (is.numeric(values)) {
    vals <- as.integer(values)
    list(
      r = bitwAnd(bitwShiftR(vals, 16L), 0xFFL),
      g = bitwAnd(bitwShiftR(vals, 8L), 0xFFL),
      b = bitwAnd(vals, 0xFFL),
      a = rep(255L, length(vals))
    )
  } else if (is.character(values)) {
    colours_to_rgba(values)
  } else {
    cli::cli_abort(
      "{.arg fill} with {.code fill_identity = TRUE} must be numeric (packed RGB) or character (hex colours), not {.obj_type_friendly {values}}."
    )
  }
}

#' Map numeric values to RGBA through a palette (vectorised)
#' @param pal_hex Character vector of hex colours (the resolved palette).
#' @return A list of integer vectors `r`, `g`, `b`, `a`.
#' @noRd
values_to_rgba <- function(values, domain, pal_hex) {
  rng <- domain[2] - domain[1]
  if (rng == 0) {
    t <- rep(0.5, length(values))
  } else {
    t <- pmin(1, pmax(0, (values - domain[1]) / rng))
  }
  idx <- pmin(
    length(pal_hex),
    pmax(1L, as.integer(t * (length(pal_hex) - 1)) + 1L)
  )
  colours_to_rgba(pal_hex[idx])
}
