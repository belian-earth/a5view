# The lazy renderer reads a pyramid from four sources: Parquet inlined or
# served with Range requests (a5_view_pyramid()), and Arrow inlined or
# served one tile per request (a5_view(), a5_view_update()). These tests
# run its tile core under Node on one pyramid from every source, and check
# that each decodes to the same index and the same packed geometry.

lib_file <- function(...) {
  system.file("htmlwidgets", "lib", ..., package = "a5view")
}

arrow_js_path <- function() {
  system.file(
    "htmlwidgets", "lib", "apache-arrow", "Arrow.es2015.min.js",
    package = "geoarrowWidget"
  )
}

# Decode every tile of every source with the renderer's tile core and
# compare each source with inline Parquet. Returns the parsed summary.
run_tile_core <- function(sources, files) {
  dir <- withr::local_tempdir()
  # Node reads .js as CommonJS, and the ES module bundles need .mjs.
  file.copy(lib_file("a5-js", "a5.js"), file.path(dir, "a5.mjs"))
  file.copy(lib_file("hyparquet", "hyparquet.esm.js"), file.path(dir, "hyparquet.mjs"))
  file.copy(arrow_js_path(), file.path(dir, "arrow.cjs"))
  file.copy(lib_file("a5view-tiling", "lazy-renderer.js"), file.path(dir, "lazy-renderer.cjs"))
  for (nm in names(files)) writeBin(files[[nm]], file.path(dir, nm))
  writeLines(yyjsonr::write_json_str(sources, auto_unbox = TRUE), file.path(dir, "sources.json"))
  writeLines(c(
    'import { readFileSync } from "node:fs";',
    'import { createRequire } from "node:module";',
    'import * as A5 from "./a5.mjs";',
    'import * as hp from "./hyparquet.mjs";',
    "const require = createRequire(import.meta.url);",
    'const Arrow = require("./arrow.cjs");',
    "globalThis.window = globalThis;",
    "window.A5View = { log() {} };",
    'require("./lazy-renderer.cjs");',
    "const dir = process.argv[2];",
    "const file = (name) => readFileSync(dir + '/' + name);",
    "const pq = file('pyramid.parquet');",
    "const requests = [];",
    "globalThis.fetch = async (url, opts) => {",
    "  const u = new URL(url);",
    "  requests.push(u.pathname + u.search);",
    "  if (u.pathname === '/tiles') return new Response(file('tile-' + u.searchParams.get('tile') + '.bin'));",
    "  const [s, e] = opts.headers.Range.slice(6).split('-').map(Number);",
    "  return new Response(pq.subarray(s, e + 1), { status: 206 });",
    "};",
    "const sources = JSON.parse(readFileSync(dir + '/sources.json', 'utf8'));",
    "const decoded = {};",
    "for (const [name, src] of Object.entries(sources)) {",
    "  const core = window.A5View.lazy._tileCore(A5, hp, Arrow);",
    "  const index = await core.open(src);",
    "  const last = index.levels.at(-1);",
    "  const n = (last.tiles || last.row_groups)[1];",
    "  const tiles = [];",
    "  for (let i = 0; i < n; i++) tiles.push(await core.decode(i));",
    "  decoded[name] = { index, tiles };",
    "}",
    "const ref = decoded.parquet_inline;",
    "const same = (a, b) => (a == null && b == null) ||",
    "  (a != null && b != null && a.length === b.length && a.every((v, i) => v === b[i]));",
    "const KEYS = ['cells', 'starts', 'positions', 'colors', 'values', 'elevation'];",
    "const out = { rows: ref.tiles.map((t) => t.n), present: KEYS.filter((k) => ref.tiles[0][k]), sources: {} };",
    "for (const [name, d] of Object.entries(decoded)) {",
    "  const bad = [];",
    "  d.tiles.forEach((t, i) => KEYS.forEach((k) => { if (!same(t[k], ref.tiles[i][k])) bad.push(i + ':' + k); }));",
    "  out.sources[name] = { tiles: d.tiles.length, index: d.index, mismatches: bad };",
    "}",
    "out.requests = requests.filter((r) => r.startsWith('/tiles')).length;",
    "console.log(JSON.stringify(out));"
  ), file.path(dir, "run.mjs"))
  out <- system2("node", c(file.path(dir, "run.mjs"), dir), stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) stop(paste(out, collapse = "\n"))
  yyjsonr::read_json_str(paste(out, collapse = ""))
}

test_that("every pyramid source decodes to the same tiles", {
  skip_if(Sys.which("node") == "", "node not available")
  skip_if_not(nzchar(arrow_js_path()), "geoarrowWidget's Arrow JS not found")

  centre <- a5R::a5_lonlat_to_cell(-3.19, 55.95, resolution = 9)
  cells <- a5R::a5_uncompact(a5R::a5_spherical_cap(centre, 3000), resolution = 12)
  df <- withr::with_seed(1, data.frame(
    cell = cells, value = stats::runif(length(cells)), elev = stats::runif(length(cells), 0, 50)
  ))
  df$value[1:3] <- NA
  prep <- prepare_view_data(df, rlang::quo(value), FALSE, "Viridis", elev_expr = quote(elev))

  path <- withr::local_tempfile(fileext = ".parquet")
  write_pyramid(prep, path, "mean", 1L)
  tbl <- pyramid_arrow(prep, "mean", 1L)$table

  s <- mock_session()
  served <- arrow_pyramid_payload(tbl, session = s, output_id = "map")
  tiles <- s$env$registered[[1]]$data
  files <- c(
    list(pyramid.parquet = readBin(path, "raw", file.info(path)$size)),
    stats::setNames(tiles, sprintf("tile-%d.bin", seq_along(tiles) - 1L))
  )

  # the index as writers before 2026-11-09 named it
  old <- yyjsonr::read_json_str(served$index)
  names(old)[names(old) == "tile_bbox"] <- "row_group_bbox"
  old$levels$row_groups <- old$levels$tiles
  old$levels$tiles <- NULL

  url <- function(src) modifyList(src, list(url = "http://a5view.test/tiles?w=&nonce=1"))
  sources <- list(
    parquet_inline = parquet_payload(path)$pyramid,
    parquet_served = list(format = "parquet", url = "http://a5view.test/pyramid.parquet",
                          bytes = file.info(path)$size),
    arrow_inline = arrow_pyramid_payload(tbl),
    arrow_served = url(served),
    arrow_served_old_names = url(modifyList(
      served, list(index = yyjsonr::write_json_str(old, auto_unbox = TRUE))
    ))
  )
  out <- run_tile_core(sources, files)

  n_tiles <- length(tiles)
  info <- a5pyramid::a5_pyramid_info(path)
  expect_equal(sum(out$rows), sum(info$levels$rows))
  expect_setequal(out$present, c("cells", "starts", "positions", "colors", "values", "elevation"))
  for (nm in names(sources)) {
    res <- out$sources[[nm]]
    expect_identical(res$tiles, n_tiles, label = paste(nm, "tile count"))
    expect_length(res$mismatches, 0L)
  }
  # Parquet and Arrow carry the same index
  expect_identical(out$sources$arrow_inline$index, out$sources$parquet_inline$index)
  # served Arrow fetched each tile once, for both sources that use it
  expect_identical(out$requests, 2L * n_tiles)
})
