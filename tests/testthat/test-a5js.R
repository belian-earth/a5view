# The browser draws cells with the vendored a5-js; R builds them with
# a5R. Both must agree on A5 1.0 ids and geometry, or cells land in the
# wrong place. These tests run the vendored bundle under Node.

a5js_path <- function() {
  system.file("htmlwidgets", "lib", "a5-js", "a5.js", package = "a5view")
}

# Run the vendored a5-js on `points` (lon/lat matrix) at each of `res`,
# and on the hex ids `hex`. Returns list(cells = <hex per point x res>,
# boundaries = <list of n x 2 matrices, one per hex>).
run_a5js <- function(points, res, hex) {
  dir <- withr::local_tempdir()
  # Node reads .js as CommonJS; the bundle is ESM.
  file.copy(a5js_path(), file.path(dir, "a5.mjs"))
  num <- function(v) sprintf("%.17g", v)
  input <- sprintf(
    '{"points":[%s],"res":[%s],"hex":[%s]}',
    paste0("[", num(points[, 1]), ",", num(points[, 2]), "]", collapse = ","),
    paste(res, collapse = ","),
    paste0('"', hex, '"', collapse = ",")
  )
  writeLines(input, file.path(dir, "input.json"))
  writeLines(c(
    'import * as A5 from "./a5.mjs";',
    'import { readFileSync } from "node:fs";',
    'const x = JSON.parse(readFileSync(process.argv[2], "utf8"));',
    "const hex = (c) => c.toString(16).padStart(16, '0');",
    "const cells = [];",
    "for (const r of x.res) for (const p of x.points) cells.push(hex(A5.lonLatToCell(p, r)));",
    "const boundaries = x.hex.map((h) =>",
    "  A5.cellToBoundary(A5.hexToU64(h), { closedRing: true, segments: 'auto' }));",
    "console.log(JSON.stringify({ cells, boundaries }));"
  ), file.path(dir, "run.mjs"))
  out <- system2(
    "node", c(file.path(dir, "run.mjs"), file.path(dir, "input.json")),
    stdout = TRUE
  )
  yyjsonr::read_json_str(paste(out, collapse = ""))
}

# Points spread over faces, poles, the antimeridian and the AEF example
# patch in Malawi (lon 34, lat -13).
sample_points <- function() {
  withr::with_seed(42, {
    n <- 40
    rnd <- cbind(
      runif(n, -180, 180),
      asin(runif(n, -1, 1)) * 180 / pi
    )
  })
  rbind(
    c(34, -13), c(34.05, -12.95), c(-3.19, 55.95), c(0, 0),
    c(179.999, 10), c(-179.999, -10), c(12, 89.9), c(-60, -89.9),
    rnd
  )
}

test_that("vendored a5-js matches a5R: lonLatToCell and cellToBoundary", {
  skip_if(Sys.which("node") == "", "node not available")
  skip_if_not(nzchar(a5js_path()))

  pts <- sample_points()
  res <- c(0L, 1L, 5L, 10L, 14L, 18L)

  r_cells <- do.call(c, lapply(res, function(r) {
    a5R::a5_lonlat_to_cell(pts[, 1], pts[, 2], resolution = r)
  }))
  r_hex <- format(r_cells)

  js <- run_a5js(pts, res, r_hex)

  # lonLatToCell: same id for every point and resolution.
  expect_identical(js$cells, r_hex)

  # cellToBoundary: same ring, vertex for vertex, within 1e-9 degrees.
  r_rings <- wk::wk_coords(a5R::a5_cell_to_boundary(r_cells))
  worst <- 0
  for (i in seq_along(r_hex)) {
    r_xy <- as.matrix(r_rings[r_rings$feature_id == i, c("x", "y")])
    js_xy <- js$boundaries[[i]]
    expect_identical(nrow(js_xy), nrow(r_xy), label = paste("vertex count of", r_hex[[i]]))
    if (nrow(js_xy) == nrow(r_xy)) worst <- max(worst, abs(js_xy - r_xy))
  }
  expect_lt(worst, 1e-9)
})

test_that("a5-js version is declared once and matches the yaml", {
  bridge <- readLines(system.file(
    "htmlwidgets", "lib", "a5-js", "bridge.js", package = "a5view"
  ))
  ver <- sub('.*A5_JS_VERSION = "([^"]+)".*', "\\1",
             grep("var A5_JS_VERSION =", bridge, value = TRUE))
  expect_length(ver, 1L)

  # htmltools takes numeric versions only, so the yaml writes
  # "1.0.0-beta.3" as "1.0.0-3".
  yaml <- readLines(system.file("htmlwidgets", "a5view.yaml", package = "a5view"))
  i <- grep("name: a5-js", yaml, fixed = TRUE)
  expect_identical(
    trimws(sub("version:", "", yaml[i + 1L])),
    sub("-[a-z]+\\.", "-", ver)
  )

  # No other script hard-codes an a5-js version.
  js_files <- list.files(
    system.file("htmlwidgets", package = "a5view"),
    pattern = "\\.js$", recursive = TRUE, full.names = TRUE
  )
  js_files <- setdiff(js_files, c(a5js_path(), grep("bridge\\.js$", js_files, value = TRUE)))
  hits <- unlist(lapply(js_files, function(f) grep("a5-js@", readLines(f, warn = FALSE), value = TRUE)))
  expect_length(hits, 0L)
})
