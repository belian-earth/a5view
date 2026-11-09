// =====================================================================
// a5view lazy pyramid renderer
// =====================================================================
// Reads the tiles of an a5pyramid pyramid on demand, so initial paint
// only decodes what the current viewport + LOD need. Subsequent
// pans/zooms decode any newly-required tiles; everything else is served
// from a per-tile cache of packed geometry.
//
// Wire format: an a5pyramid pyramid (specified in a5pyramid's FORMAT.md),
// in one of its two forms:
//   - Parquet, a file written by a5pyramid::a5_write_pyramid() through
//     R/prep-data.R::write_pyramid(); tile i is row group i. Inlined as
//     base64 or served with Range support (a5_view_pyramid()).
//   - Arrow, a table from a5pyramid::a5_pyramid_arrow() through
//     R/prep-data.R::pyramid_arrow(); tile i is record batch i. Inlined as
//     one deflated IPC stream, or served one deflated IPC stream per
//     tile (a5_view() and a5_view_update(), see R/serve.R).
// In both:
//   - Tiles follow the levels in ascending LOD order; each holds one
//     level, a run of consecutive cell ids.
//   - Metadata key `a5pyramid` (Parquet KV metadata, Arrow schema
//     metadata) holds the JSON index: `levels` (each `{lod, rows, tiles:
//     [start, end)}`) and `tile_bbox`, `[west, south, east, north]` per
//     tile, flat. `west > east` means the box crosses the antimeridian.
//     Files written before 2026-11-09 name these `row_groups` and
//     `row_group_bbox`; both are read.
//   - Key `a5_index` records the A5 index version of the cell ids.
//   - Columns `cell` (uint64), `lod`, `ncell`, then the payload columns
//     `_fill_r/g/b/a`, `_fill_value`, `_elevation` as present.
//   Every pyramid tile is one tile of the TileLayer for its level. Tiles
//   of levels with at most TILING.EAGER_MAX_ROWS rows are decoded as soon
//   as the pyramid opens. (The renderer's own names, `rg`, `rgIndex`,
//   count tiles.)
//
// Work split:
//   - A Web Worker owns the source, reads tiles (with hyparquet, or Arrow
//     JS after inflating with DecompressionStream) and turns each into
//     packed typed arrays: pentagon boundaries from a5-js, RGBA, fill
//     values. Only transferable buffers cross back.
//   - The main thread builds deck.gl layers from those buffers using
//     binary attributes (no per-cell objects, no accessor calls, no
//     polygon normalisation), so a tile's first paint costs a few
//     sub-layer instantiations rather than tens of milliseconds.
//   - If workers are unavailable (or the worker fails to load its
//     libraries) the same packing code runs on the main thread.
//
// Render strategy:
//   - Every LOD renders through a deck.gl TileLayer + an A5-aware
//     Tileset2D whose tiles are the level's row groups; each tile is one
//     SolidPolygonLayer, plus a PathLayer of outlines when stroked and
//     cells are big enough on screen to show a border (see
//     buildGeomLayer). A change of stroke starts a fresh TileLayer, so
//     tiles are rebuilt with or without outlines.
//
// Dependencies:
//   - window.A5ViewHyparquetReady / window.A5ViewHyparquetUrl (set by
//     lib/hyparquet/hyparquet-bootstrap.js)
//   - window.Arrow (Arrow JS, attached by geoarrowWidget)
//   - window.A5Ready / window.A5ViewA5Url (set by lib/a5-js/bridge.js)
//   - window.A5View.tiling: getViewportBbox, bboxKey, getA5Resolution,
//     pickLod, bigintToHex, REBUILD_THROTTLE_MS, MIN_HOLD_MS.
//
// ctx (provided by a5view.js):
//   getOpacity()    current layer opacity
//   getBeforeId()   MapLibre layer id to insert cell layers beneath
//   onDataReady()   called whenever a tile's geometry lands
// =====================================================================
(function () {
  var T = window.A5View = window.A5View || {};
  var TILING = T.tiling = T.tiling || {};
  var LAZY = T.lazy = T.lazy || {};
  var log = function () { T.log.apply(null, arguments); };

  // Below this projected edge length (CSS px) cell borders are skipped:
  // they would only smear the fill and double the per-tile layer count.
  TILING.MIN_STROKE_PX = 3;

  // Row groups of levels with at most this many rows are decoded as soon
  // as the file opens: they cover the wide zooms, and are small.
  TILING.EAGER_MAX_ROWS = 10000;

  // ───────────────────────────────────────────────────────────────────
  // Tile core: source handling + tile decode + geometry packing.
  //
  // Written as a self-contained function of its library modules so
  // the very same code runs inside a Worker (stringified into a Blob)
  // or on the main thread as a fallback. Nothing in here may reference
  // closure variables from this file.
  // ───────────────────────────────────────────────────────────────────
  function tileCore(A5, hp, Arrow) {
    var file = null;      // AsyncBuffer
    var metadata = null;
    var rowOffsets = null;

    function base64ToBytes(b64) {
      var binary = atob(b64);
      var n = binary.length;
      var out = new Uint8Array(n);
      for (var i = 0; i < n; i++) out[i] = binary.charCodeAt(i);
      return out;
    }

    function bytesBuffer(bytes) {
      return {
        byteLength: bytes.byteLength,
        slice: function (start, end) {
          var s = start | 0;
          var e = (end == null) ? bytes.byteLength : (end | 0);
          return bytes.buffer.slice(bytes.byteOffset + s, bytes.byteOffset + e);
        }
      };
    }

    // AsyncBuffer over an HTTP URL with Range support. Slices are served
    // from previously fetched ranges when possible; `prefetch` pulls a
    // whole row group in one request.
    function remoteBuffer(url, byteLength) {
      var ranges = [];
      function cached(start, end) {
        for (var i = 0; i < ranges.length; i++) {
          var r = ranges[i];
          if (start >= r.start && end <= r.end) {
            return r.bytes.buffer.slice(
              r.bytes.byteOffset + (start - r.start),
              r.bytes.byteOffset + (end - r.start)
            );
          }
        }
        return null;
      }
      function fetchRange(start, end) {
        return fetch(url, { headers: { Range: "bytes=" + start + "-" + (end - 1) } })
          .then(function (res) {
            if (!res.ok) throw new Error("range request failed: " + res.status);
            return res.arrayBuffer();
          })
          .then(function (buf) {
            var bytes = new Uint8Array(buf);
            if (bytes.byteLength !== end - start) {
              if (bytes.byteLength === byteLength) {
                // Server ignored the Range header: keep the whole file.
                ranges = [{ start: 0, end: byteLength, bytes: bytes }];
                return cached(start, end);
              }
              throw new Error("unexpected range length " + bytes.byteLength);
            }
            ranges.push({ start: start, end: end, bytes: bytes });
            if (ranges.length > 4096) ranges.splice(0, ranges.length >> 1);
            return buf;
          });
      }
      return {
        byteLength: byteLength,
        slice: function (start, end) {
          var s = start | 0;
          var e = (end == null) ? byteLength : (end | 0);
          var hit = cached(s, e);
          return hit ? Promise.resolve(hit) : fetchRange(s, e);
        },
        prefetch: function (start, end) {
          return cached(start, end) ? Promise.resolve() : fetchRange(start, end).then(function () {});
        }
      };
    }

    // Byte extent [start, end) of a row group: its column chunks are
    // written contiguously.
    function rowGroupByteRange(rg) {
      var start = Infinity, end = 0;
      for (var i = 0; i < rg.columns.length; i++) {
        var md = rg.columns[i].meta_data;
        if (!md) continue;
        var off = Number(md.data_page_offset);
        if (md.dictionary_page_offset != null && Number(md.dictionary_page_offset) < off) {
          off = Number(md.dictionary_page_offset);
        }
        var stop = off + Number(md.total_compressed_size);
        if (off < start) start = off;
        if (stop > end) end = stop;
      }
      return { start: start, end: end };
    }

    function toBigIntCell(v) {
      if (typeof v === "bigint") return v;
      if (typeof v === "number") return BigInt(v);
      if (v && typeof v.byteLength === "number" && v.byteLength === 8) {
        var dv = new DataView(v.buffer || v, v.byteOffset || 0, 8);
        return dv.getBigUint64(0, false);
      }
      if (typeof v === "string") return BigInt("0x" + v);
      return BigInt(v);
    }

    // Inflate zlib-format bytes (R's memCompress(type = "gzip")).
    function inflate(bytes) {
      if (typeof DecompressionStream === "undefined") {
        return Promise.reject(new Error("this browser cannot inflate the pyramid (no DecompressionStream)"));
      }
      var stream = new Blob([bytes]).stream().pipeThrough(new DecompressionStream("deflate"));
      return new Response(stream).arrayBuffer().then(function (buf) { return new Uint8Array(buf); });
    }

    // Check and parse the `a5pyramid` index against the `a5_index` entry.
    function readIndex(json, a5Index) {
      if (!json) {
        throw new Error("not an a5pyramid pyramid (no `a5pyramid` metadata); rebuild it with a5_build_pyramid()");
      }
      var index = typeof json === "string" ? JSON.parse(json) : json;
      if (!(index.format_version <= 1)) {
        throw new Error("a5pyramid format version " + index.format_version + " is newer than this viewer reads");
      }
      if (a5Index !== "1.0") {
        throw new Error("the pyramid's cells are not A5 1.0 ids (a5_index " + a5Index + ")");
      }
      return index;
    }

    // Tiles the index lists: the end of the last level's range.
    function indexTileCount(index) {
      var last = index.levels[index.levels.length - 1];
      var range = last ? (last.tiles || last.row_groups) : [0, 0];
      return range[1];
    }

    // A tile's columns, the form decode() packs:
    //   { n, cell, fill_r, fill_g, fill_b, fill_a, fill_value, elevation }
    // `cell` holds BigInt ids, the rest numbers (null where missing);
    // each is an array or typed array, or null when the column is absent.
    var PAYLOAD = { fill_r: "_fill_r", fill_g: "_fill_g", fill_b: "_fill_b",
                    fill_a: "_fill_a", fill_value: "_fill_value", elevation: "_elevation" };

    function rowsToColumns(rows) {
      var n = rows.length;
      var first = rows[0] || {};
      var out = { n: n, cell: new Array(n) };
      for (var i = 0; i < n; i++) out.cell[i] = toBigIntCell(rows[i].cell);
      Object.keys(PAYLOAD).forEach(function (k) {
        var name = PAYLOAD[k];
        if (n === 0 || first[name] === undefined) { out[k] = null; return; }
        var col = new Array(n);
        for (var j = 0; j < n; j++) col[j] = rows[j][name];
        out[k] = col;
      });
      return out;
    }

    function batchToColumns(batch) {
      var n = batch.numRows;
      function column(name) {
        var v = batch.getChild(name);
        if (!v) return null;
        // toArray() is a view of the values, which ignores validity
        if (v.nullCount === 0) return v.toArray();
        var out = new Array(n);
        for (var i = 0; i < n; i++) out[i] = v.get(i);
        return out;
      }
      var out = { n: n, cell: column("cell") };
      Object.keys(PAYLOAD).forEach(function (k) { out[k] = column(PAYLOAD[k]); });
      return out;
    }

    // Sources. Each open* resolves to the index and sets readTile(i), a
    // Promise of tile i's columns.
    var readTile = null;

    // Parquet, inline ({ b64 }) or served with Range support ({ url,
    // bytes }): tile i is row group i.
    function openParquet(source) {
      var p;
      if (source.url) {
        file = remoteBuffer(source.url, Number(source.bytes));
        p = hp.parquetMetadataAsync(file);
      } else {
        var bytes = base64ToBytes(source.b64);
        file = bytesBuffer(bytes);
        p = Promise.resolve(hp.parquetMetadata(
          bytes.buffer.slice(bytes.byteOffset, bytes.byteOffset + bytes.byteLength)
        ));
      }
      return p.then(function (md) {
        metadata = md;
        var off = 0;
        rowOffsets = md.row_groups.map(function (rg) {
          var n = Number(rg.num_rows);
          var o = { start: off, end: off + n };
          off += n;
          return o;
        });
        var kv = {};
        (md.key_value_metadata || []).forEach(function (e) { kv[e.key] = e.value; });
        var index = readIndex(kv.a5pyramid, kv.a5_index);
        readTile = function (i) {
          var off = rowOffsets[i];
          var ready = Promise.resolve();
          if (file.prefetch) {
            var range = rowGroupByteRange(metadata.row_groups[i]);
            ready = file.prefetch(range.start, range.end);
          }
          return ready.then(function () {
            return hp.parquetReadObjects({
              file: file, metadata: metadata, rowStart: off.start, rowEnd: off.end
            });
          }).then(rowsToColumns);
        };
        return index;
      });
    }

    // Arrow, inline: { b64 }, one deflated IPC stream of every tile, the
    // index in its schema metadata. Tile i is record batch i.
    function openArrowInline(source) {
      return inflate(base64ToBytes(source.b64)).then(function (bytes) {
        var table = Arrow.tableFromIPC(bytes);
        var meta = table.schema.metadata;
        var index = readIndex(meta.get("a5pyramid"), meta.get("a5_index"));
        var batches = table.batches;
        if (batches.length !== indexTileCount(index)) {
          throw new Error("the pyramid has " + batches.length + " record batches but its index lists " +
                          indexTileCount(index) + " tiles");
        }
        readTile = function (i) { return Promise.resolve(batchToColumns(batches[i])); };
        return index;
      });
    }

    // Arrow, served: { url, tiles, index, a5_index }. Tile i is a deflated
    // IPC stream of one record batch at url + "&tile=" + i.
    function openArrowServed(source) {
      var index = readIndex(source.index, source.a5_index);
      if (Number(source.tiles) !== indexTileCount(index)) {
        throw new Error("the server has " + source.tiles + " tiles but the index lists " +
                        indexTileCount(index));
      }
      var sep = source.url.indexOf("?") === -1 ? "?" : "&";
      readTile = function (i) {
        return fetch(source.url + sep + "tile=" + i).then(function (res) {
          if (!res.ok) throw new Error("tile request failed: " + res.status);
          return res.arrayBuffer();
        }).then(function (buf) {
          return inflate(new Uint8Array(buf));
        }).then(function (bytes) {
          var batches = Arrow.tableFromIPC(bytes).batches;
          if (batches.length !== 1) throw new Error("tile " + i + " holds " + batches.length + " record batches");
          return batchToColumns(batches[0]);
        });
      };
      return Promise.resolve(index);
    }

    // source: { format: "parquet" | "arrow", ... } (see the open*
    // functions). Resolves to the a5pyramid index.
    function open(source) {
      return Promise.resolve().then(function () {
        if (source.format === "arrow") {
          if (!Arrow) throw new Error("Arrow JS is not loaded");
          return source.url ? openArrowServed(source) : openArrowInline(source);
        }
        if (!hp) throw new Error("hyparquet is not loaded");
        return openParquet(source);
      });
    }

    // Decode one tile and pack it:
    //   cells      BigUint64Array(n)       cell ids
    //   starts     Uint32Array(n + 1)      vertex start index per cell
    //   positions  Float64Array(nv * 2)    [lon, lat, ...]
    //   colors     Uint8Array(nv * 4)      RGBA per vertex, when present
    //   values     Float32Array(n)         `_fill_value` per cell, when present
    //   elevation  Float64Array(nv)        `_elevation` per vertex, when present
    // deck.gl's polygon attributes are per vertex, so colour and
    // elevation are expanded here rather than per cell.
    function decode(rgIdx) {
      return readTile(rgIdx).then(function (cols) { return pack(rgIdx, cols); });
    }

    function pack(rgIdx, cols) {
      var n = cols.n;
      var cells = new BigUint64Array(n);
      if (cols.cell instanceof BigUint64Array) {
        cells.set(cols.cell);
      } else {
        for (var c0 = 0; c0 < n; c0++) cells[c0] = toBigIntCell(cols.cell[c0]);
      }
      var rings = new Array(n);
      var nv = 0;
      for (var i = 0; i < n; i++) {
        var ring = A5.cellToBoundary(cells[i], { closedRing: false });
        rings[i] = ring;
        nv += ring.length;
      }
      var starts = new Uint32Array(n + 1);
      var positions = new Float64Array(nv * 2);
      var p = 0;
      for (var j = 0; j < n; j++) {
        starts[j] = p / 2;
        var r = rings[j];
        for (var k = 0; k < r.length; k++) {
          positions[p++] = r[k][0];
          positions[p++] = r[k][1];
        }
      }
      starts[n] = nv;
      var out = { rg: rgIdx, n: n, cells: cells, starts: starts, positions: positions };
      var transfer = [cells.buffer, starts.buffer, positions.buffer];
      if (cols.fill_r) {
        var colors = new Uint8Array(nv * 4);
        var fr = cols.fill_r, fg = cols.fill_g, fb = cols.fill_b, fa = cols.fill_a;
        for (var c = 0; c < n; c++) {
          for (var cv = starts[c]; cv < starts[c + 1]; cv++) {
            colors[cv * 4] = fr[c]; colors[cv * 4 + 1] = fg[c];
            colors[cv * 4 + 2] = fb[c]; colors[cv * 4 + 3] = fa[c];
          }
        }
        out.colors = colors; transfer.push(colors.buffer);
      }
      if (cols.fill_value) {
        var values = new Float32Array(n);
        for (var v = 0; v < n; v++) values[v] = cols.fill_value[v];
        out.values = values; transfer.push(values.buffer);
      }
      if (cols.elevation) {
        var elev = new Float64Array(nv);
        for (var e = 0; e < n; e++) {
          var ev = cols.elevation[e] || 0;
          for (var ei = starts[e]; ei < starts[e + 1]; ei++) elev[ei] = ev;
        }
        out.elevation = elev; transfer.push(elev.buffer);
      }
      out.transfer = transfer;
      return out;
    }

    return { open: open, decode: decode };
  }

  // For tests (tests/testthat/test-lazy-renderer.R), which run it under Node.
  LAZY._tileCore = tileCore;

  // Worker entry: load the libraries the source needs (a5-js and
  // hyparquet as ES modules, Arrow JS as a classic script), then serve
  // open / decode requests over postMessage. Runs from a Blob URL so no
  // extra file has to be shipped; it inherits the page's origin.
  function workerMain() {
    var core = null;
    var loaded = {};
    function tryImport(list) {
      return import(list[0]).catch(function (e) {
        if (list.length > 1) return tryImport(list.slice(1));
        throw e;
      });
    }
    function loadArrow(list) {
      var err = new Error("Arrow JS not found");
      for (var i = 0; i < list.length && !self.Arrow; i++) {
        try { importScripts(list[i]); } catch (e) { err = e; }
      }
      return self.Arrow ? Promise.resolve(self.Arrow) : Promise.reject(err);
    }
    function lib(name, urls) {
      if (!loaded[name]) loaded[name] = name === "arrow" ? loadArrow(urls.arrow) : tryImport(urls[name]);
      return loaded[name];
    }
    function errorMessage(e) { return String(e && e.message || e); }
    self.onmessage = function (ev) {
      var msg = ev.data;
      if (msg.type === "open") {
        var arrow = msg.source.format === "arrow";
        Promise.all([
          lib("a5", msg.libs),
          arrow ? null : lib("hyparquet", msg.libs),
          arrow ? lib("arrow", msg.libs) : null
        ]).then(function (mods) {
          core = tileCore(mods[0], mods[1], mods[2]);
          return core.open(msg.source).then(function (index) {
            self.postMessage({ type: "opened", id: msg.id, index: index });
          }, function (e) {
            self.postMessage({ type: "error", id: msg.id, message: errorMessage(e) });
          });
        }, function (e) {
          self.postMessage({ type: "error", id: msg.id, libs: true, message: errorMessage(e) });
        });
      } else if (msg.type === "decode") {
        core.decode(msg.rg).then(function (out) {
          var transfer = out.transfer; delete out.transfer;
          out.type = "decoded"; out.id = msg.id;
          self.postMessage(out, transfer);
        }).catch(function (e) {
          self.postMessage({ type: "error", id: msg.id, rg: msg.rg, message: errorMessage(e) });
        });
      }
    };
  }

  function makeWorker() {
    if (typeof Worker === "undefined" || typeof Blob === "undefined") return null;
    try {
      var src = tileCore.toString() + "\n(" + workerMain.toString() + ")();";
      var url = URL.createObjectURL(new Blob([src], { type: "text/javascript" }));
      var w = new Worker(url);
      URL.revokeObjectURL(url);
      return w;
    } catch (e) {
      log("[a5view] worker unavailable, decoding on main thread:", e);
      return null;
    }
  }

  // The core on the main thread, with the page's libraries.
  function inThreadDecoder() {
    var core = null;
    return {
      isWorker: false,
      open: function (source) {
        var arrow = source.format === "arrow";
        return Promise.all([
          TILING.ensureA5(),
          arrow ? null : window.A5ViewHyparquetReady
        ]).then(function (mods) {
          core = tileCore(mods[0], mods[1], arrow ? window.Arrow : null);
          return core.open(source);
        });
      },
      decode: function (rg) { return core.decode(rg); },
      close: function () {}
    };
  }

  // Uniform promise-based front over either a worker or the in-thread
  // core: { open(source) -> Promise<a5pyramid index>, decode(rg) -> Promise<packed>,
  // close(), isWorker }. A worker that cannot load its libraries hands
  // over to the main thread.
  function makeDecoder(libUrls) {
    var worker = makeWorker();
    if (!worker) return inThreadDecoder();
    var pending = new Map();
    var nextId = 1;
    worker.onmessage = function (ev) {
      var msg = ev.data;
      var p = pending.get(msg.id);
      if (!p) return;
      pending.delete(msg.id);
      if (msg.type === "error") {
        var err = new Error(msg.message);
        err.libs = !!msg.libs;
        p.reject(err);
      } else {
        p.resolve(msg);
      }
    };
    worker.onerror = function (e) {
      pending.forEach(function (p) { p.reject(new Error("worker error: " + (e.message || e))); });
      pending.clear();
    };
    function send(msg) {
      return new Promise(function (resolve, reject) {
        msg.id = nextId++;
        pending.set(msg.id, { resolve: resolve, reject: reject });
        worker.postMessage(msg);
      });
    }
    var front = {
      isWorker: true,
      open: function (source) {
        return send({ type: "open", source: source, libs: libUrls }).then(function (m) {
          return m.index;
        }, function (e) {
          if (!e.libs) throw e;
          log("[a5view] worker could not load its libraries (" + e.message + "); decoding on main thread");
          front.close();
          var local = inThreadDecoder();
          front.isWorker = false;
          front.decode = local.decode;
          front.close = local.close;
          return local.open(source);
        });
      },
      decode: function (rg) { return send({ type: "decode", rg: rg }); },
      close: function () { worker.terminate(); pending.clear(); }
    };
    return front;
  }

  // Arrow JS is on the page from geoarrowWidget; the worker loads the
  // same script, falling back to the CDN build of the same version.
  var ARROW_CDN = "https://cdn.jsdelivr.net/npm/apache-arrow@21.2.0/Arrow.es2015.min.js";

  function libUrls() {
    var a5 = [];
    if (window.A5ViewA5Url) a5.push(window.A5ViewA5Url);
    if (window.A5ViewA5Cdn) a5.push(window.A5ViewA5Cdn);
    var hyp = [];
    if (window.A5ViewHyparquetUrl) hyp.push(window.A5ViewHyparquetUrl);
    hyp.push("https://cdn.jsdelivr.net/npm/hyparquet@1.25.6/+esm");
    var arrow = [];
    var scripts = document.getElementsByTagName("script");
    for (var i = 0; i < scripts.length; i++) {
      var src = scripts[i].src || "";
      if (/Arrow\.es\d+\.min\.js/.test(src)) arrow.push(absoluteUrl(src));
    }
    arrow.push(ARROW_CDN);
    return { a5: a5, hyparquet: hyp, arrow: arrow };
  }

  function absoluteUrl(url) {
    try { return new URL(url, window.location.href).href; } catch (_) { return url; }
  }

  // Concatenate packed row groups into one geometry block.
  function concatPacked(parts) {
    if (parts.length === 1) return parts[0];
    var n = 0, nv = 0, hasColor = true, hasValue = true, hasElev = true;
    parts.forEach(function (p) {
      n += p.n; nv += p.starts[p.n];
      hasColor = hasColor && !!p.colors; hasValue = hasValue && !!p.values; hasElev = hasElev && !!p.elevation;
    });
    var out = {
      n: n,
      cells: new BigUint64Array(n),
      starts: new Uint32Array(n + 1),
      positions: new Float64Array(nv * 2),
      colors: hasColor ? new Uint8Array(nv * 4) : null,
      values: hasValue ? new Float32Array(n) : null,
      elevation: hasElev ? new Float64Array(nv) : null
    };
    var ci = 0, vi = 0;
    parts.forEach(function (p) {
      out.cells.set(p.cells, ci);
      out.positions.set(p.positions, vi * 2);
      for (var i = 0; i < p.n; i++) out.starts[ci + i] = p.starts[i] + vi;
      if (hasColor) out.colors.set(p.colors, vi * 4);
      if (hasValue) out.values.set(p.values, ci);
      if (hasElev) out.elevation.set(p.elevation, vi);
      ci += p.n; vi += p.starts[p.n];
    });
    out.starts[n] = vi;
    return out;
  }

  // Longitude range west..east as one or two intervals within -180..180
  // (two when it crosses the antimeridian, west > east).
  function lonIntervals(west, east) {
    return west <= east ? [[west, east]] : [[west, 180], [-180, east]];
  }

  // Test whether a [w, s, e, n] box overlaps a query box; either may
  // cross the antimeridian (west > east).
  function bboxOverlapXY(b, q) {
    if (b[3] < q.south || b[1] > q.north) return false;
    var bi = lonIntervals(b[0], b[2]);
    var qi = lonIntervals(q.west, q.east);
    for (var i = 0; i < bi.length; i++) {
      for (var j = 0; j < qi.length; j++) {
        if (bi[i][0] <= qi[j][1] && qi[j][0] <= bi[i][1]) return true;
      }
    }
    return false;
  }

  var EMPTY_DATA = [];

  LAZY.createRenderer = function (ctx) {
    var decoder = null;
    var rgIndex = null;             // one entry per row group, from the index
    var decodedCache = new Map();   // rg -> packed geometry
    var pendingDecodes = new Map(); // rg -> Promise
    var loadVersion = 0;
    var ready = false;
    var sourceFormat = null;        // "parquet" or "arrow"

    // Per-LOD lookup tables built from rgIndex.
    var tilesByLod = new Map();     // lod -> Map<tileHex, [rgIdx, ...]>
    var bboxByLod = new Map();      // lod -> Map<tileHex, [w, s, e, n]>
    var tileGeomCache = new Map();  // lod -> Map<tileHex, { count, geom }>
    var tilesetClassesByLod = new Map();
    var layerMemo = new Map();      // layerId -> { key, layer }
    var currentTilesId = new Map(); // lod -> id of its current TileLayer
    var dirtyTiles = new Map();     // lod -> Set<tileHex>
    // Hover lookup: lod -> { index: Map<hex, [rg, i]>, pending: [rg, ...] }
    var rowsByLod = new Map();

    var MAX_CONCURRENT_DECODES = 8;
    var decodeQueue = [];

    var lastBuiltLod = null;
    var prevWarmLod = null;
    var lodChangedAt = 0;
    var holdTimer = null;

    var nowMs = (typeof performance !== "undefined" && performance.now)
      ? function () { return performance.now(); }
      : function () { return Date.now(); };

    function reset() {
      if (decoder) decoder.close();
      decoder = null;
      rgIndex = null;
      decodedCache = new Map();
      pendingDecodes = new Map();
      decodeQueue = [];
      tilesByLod = new Map();
      bboxByLod = new Map();
      tileGeomCache = new Map();
      tilesetClassesByLod = new Map();
      layerMemo = new Map();
      currentTilesId = new Map();
      dirtyTiles = new Map();
      rowsByLod = new Map();
      lastBuiltLod = null;
      prevWarmLod = null;
      lodChangedAt = 0;
      if (holdTimer) { clearTimeout(holdTimer); holdTimer = null; }
      if (retryTimer) { clearTimeout(retryTimer); retryTimer = null; }
      ready = false;
      loadVersion++;
    }

    // source: { format: "parquet", b64 | url, bytes } or
    // { format: "arrow", b64 | url, tiles, index, a5_index } (see
    // tileCore's open*). Resolves once the index is known; tiles decode on
    // demand from then on.
    function init(source) {
      reset();
      var thisLoad = loadVersion;
      var t0 = nowMs();
      if (source.url) source = Object.assign({}, source, { url: absoluteUrl(source.url) });
      sourceFormat = source.format || "parquet";
      log("[a5view] lazy.init: " + sourceFormat + " " +
          (source.url ? "url " + source.url : "inline b64=" + (source.b64 && source.b64.length)));
      decoder = makeDecoder(libUrls());
      return decoder.open(source).then(function (index) {
        if (thisLoad !== loadVersion) return;
        // One entry per row group, as the renderer indexes them:
        // { rg, lod_min, lod_max, west, south, east, north, tile_id, eager }.
        // Each row group is its own tile.
        rgIndex = [];
        // a5pyramid renamed `row_groups` / `row_group_bbox` to `tiles` /
        // `tile_bbox` on 2026-11-09; read either.
        var boxes = index.tile_bbox || index.row_group_bbox;
        index.levels.forEach(function (level) {
          var eager = level.rows <= TILING.EAGER_MAX_ROWS;
          var range = level.tiles || level.row_groups;
          for (var rg = range[0]; rg < range[1]; rg++) {
            rgIndex.push({
              rg: rg, lod_min: level.lod, lod_max: level.lod,
              west: boxes[4 * rg], south: boxes[4 * rg + 1],
              east: boxes[4 * rg + 2], north: boxes[4 * rg + 3],
              tile_id: "rg" + rg, eager: eager
            });
          }
        });
        for (var k = 0; k < rgIndex.length; k++) {
          var entry = rgIndex[k];
          var lod = entry.lod_min;
          var byTile = tilesByLod.get(lod);
          if (!byTile) { byTile = new Map(); tilesByLod.set(lod, byTile); }
          byTile.set(entry.tile_id, [entry.rg]);
          var bboxByTile = bboxByLod.get(lod);
          if (!bboxByTile) { bboxByTile = new Map(); bboxByLod.set(lod, bboxByTile); }
          bboxByTile.set(entry.tile_id, [entry.west, entry.south, entry.east, entry.north]);
        }
        ready = true;
        log("[a5view] init: " + (nowMs() - t0).toFixed(0) + "ms, rg=" + rgIndex.length +
            " lods=" + tilesByLod.size +
            (decoder.isWorker ? " (worker)" : " (main thread)"));
        // The small LODs cover wide zooms and are almost always needed
        // first; fetch them eagerly.
        for (var p = 0; p < rgIndex.length; p++) {
          if (rgIndex[p].eager) decodeRowGroup(rgIndex[p].rg);
        }
      });
    }

    function pumpDecodeQueue() {
      while (pendingDecodes.size < MAX_CONCURRENT_DECODES && decodeQueue.length > 0) {
        var next = decodeQueue.shift();
        if (decodedCache.has(next) || pendingDecodes.has(next)) continue;
        startDecode(next);
      }
    }

    function startDecode(rgIdx) {
      var thisLoad = loadVersion;
      var pending = pendingDecodes; // reset() starts a new map
      var t0 = nowMs();
      var p = decoder.decode(rgIdx).then(function (packed) {
        pending.delete(rgIdx);
        if (thisLoad !== loadVersion) return null;
        decodedCache.set(rgIdx, packed);
        var entry = rgIndex[rgIdx];
        var byLod = rowsByLod.get(entry.lod_min);
        if (!byLod) { byLod = { index: new Map(), pending: [] }; rowsByLod.set(entry.lod_min, byLod); }
        byLod.pending.push(rgIdx);
        markTileDirty(entry);
        var dt = nowMs() - t0;
        if (dt > 50) log("[a5view] rg" + rgIdx + " (" + packed.n + " cells) in " + dt.toFixed(0) + "ms");
        if (ctx.onDataReady) ctx.onDataReady();
        pumpDecodeQueue();
        return packed;
      }).catch(function (err) {
        pending.delete(rgIdx);
        // a decode the next pyramid superseded fails quietly
        if (thisLoad !== loadVersion) return null;
        console.error("[a5view] tile", rgIdx, "decode failed:", err);
        pumpDecodeQueue();
        return null;
      });
      pendingDecodes.set(rgIdx, p);
      return p;
    }

    function decodeRowGroup(rgIdx) {
      if (decodedCache.has(rgIdx)) return Promise.resolve(decodedCache.get(rgIdx));
      var pending = pendingDecodes.get(rgIdx);
      if (pending) return pending;
      if (pendingDecodes.size >= MAX_CONCURRENT_DECODES) {
        if (decodeQueue.indexOf(rgIdx) === -1) decodeQueue.push(rgIdx);
        return Promise.resolve(null);
      }
      return startDecode(rgIdx);
    }

    // ── Layers ──────────────────────────────────────────────────────

    // Layer instances are memoised on everything they depend on, so a
    // hover or pan redraw hands deck.gl the same objects.
    function memoLayer(layerId, key, make) {
      var m = layerMemo.get(layerId);
      if (m && m.key === key) return m.layer;
      var layer = make();
      layerMemo.set(layerId, { key: key, layer: layer });
      return layer;
    }

    // Projected edge length of a cell at `lod` (CSS px) at the current
    // view; decides whether borders are worth drawing.
    function cellEdgePx(lod, viewport) {
      if (!window.A5 || !window.A5.cellEdgeLengthAvg) return Infinity;
      var edgeM = window.A5.cellEdgeLengthAvg(lod);
      var lat = (viewport.latitude || 0) * Math.PI / 180;
      var mPerPx = 40075016.686 * Math.cos(lat) / (512 * Math.pow(2, viewport.zoom || 0));
      return edgeM / mPerPx;
    }

    function styleKey(x, stroked) {
      return [ctx.getOpacity(), ctx.getBeforeId(), stroked, x.line_width,
              x.extruded, x.elevation_scale, x.fill_per_cell].join("|");
    }

    // A SolidPolygonLayer over a packed geometry block, via binary
    // attributes, plus a PathLayer of the cell outlines when stroked.
    // PolygonLayer is not used: it derives its outlines through the
    // `getPolygon` accessor, which binary data does not supply, finds no
    // paths, and then draws neither its outlines nor its fill. Each class
    // keeps its own id, so deck.gl never carries one layer's state into
    // another class.
    function buildGeomLayer(x, geom, layerId, stroked, extra) {
      var data = {
        length: geom.n,
        startIndices: geom.starts,
        attributes: {
          getPolygon: { value: geom.positions, size: 2 }
        }
      };
      var props = {
        id: layerId,
        data: data,
        _normalize: false,
        _windingOrder: "CCW",
        positionFormat: "XY",
        opacity: ctx.getOpacity(),
        pickable: false,
        extruded: x.extruded,
        elevationScale: x.elevation_scale
      };
      if (x.fill_per_cell && geom.colors) {
        data.attributes.getFillColor = { value: geom.colors, size: 4 };
      } else {
        props.getFillColor = x.fill_color || [116, 172, 144, 255];
      }
      if (x.extruded && geom.elevation) {
        data.attributes.getElevation = { value: geom.elevation, size: 1 };
      }
      for (var k in extra) props[k] = extra[k];
      var fill = new window.deck.SolidPolygonLayer(props);
      // as PolygonLayer, no ground outlines under extruded cells
      if (!stroked || x.extruded) return fill;
      var lineProps = {
        id: layerId + "-outline",
        data: {
          length: geom.n,
          startIndices: geom.starts,
          attributes: { getPath: { value: geom.positions, size: 2 } }
        },
        _pathType: "loop",
        positionFormat: "XY",
        opacity: ctx.getOpacity(),
        pickable: false,
        getColor: x.line_color || [0, 0, 0, 0],
        getWidth: x.line_width || 1,
        widthUnits: "pixels"
      };
      for (var e in extra) lineProps[e] = extra[e];
      return [fill, new window.deck.PathLayer(lineProps)];
    }

    // Geometry for one tile, cached until more of its row groups land.
    function tileGeometry(lod, hex, rgs) {
      var parts = [];
      for (var i = 0; i < rgs.length; i++) {
        var g = decodedCache.get(rgs[i]);
        if (g) parts.push(g);
      }
      if (parts.length === 0) return null;
      var perLod = tileGeomCache.get(lod);
      if (!perLod) { perLod = new Map(); tileGeomCache.set(lod, perLod); }
      var cached = perLod.get(hex);
      if (cached && cached.count === parts.length) return cached.geom;
      cached = { count: parts.length, geom: concatPacked(parts) };
      perLod.set(hex, cached);
      return cached.geom;
    }

    // deck.gl only regenerates a tile's sub-layers when `tile.layers` is
    // null. TileLayer declares getTileData / renderSubLayers with
    // `compare: false`, so a fresh layer instance after a decode lands
    // does nothing by itself; we null the affected tiles explicitly.
    function markTileDirty(entry) {
      if (!entry || !entry.tile_id) return;
      var set = dirtyTiles.get(entry.lod_min);
      if (!set) { set = new Set(); dirtyTiles.set(entry.lod_min, set); }
      set.add(entry.tile_id);
      flushDirtyTiles(entry.lod_min);
    }
    // Sub-layer instantiation is spread over frames: a flush releases at
    // most MAX_TILES_PER_FLUSH dirty tiles, and renderSubLayers builds
    // at most MAX_NEW_TILES_PER_PASS tiles per synchronous pass (deck
    // instantiates them after the callbacks return, at roughly 10 ms
    // per 4k-cell tile), deferring the rest to a retry a frame later.
    // Three keeps a fill-in frame around 35 ms; a screenful of tiles
    // completes over a handful of frames rather than one long one.
    var MAX_TILES_PER_FLUSH = 6;
    var MAX_NEW_TILES_PER_PASS = 3;
    var buildPassCount = null;
    function withinBuildBudget() {
      if (buildPassCount == null) {
        // First call of a synchronous pass; the timeout fires once the
        // pass (and the frame it ran in) has finished.
        buildPassCount = 0;
        setTimeout(function () { buildPassCount = null; }, 0);
      }
      buildPassCount++;
      return buildPassCount <= MAX_NEW_TILES_PER_PASS;
    }
    function deferTile(lod, hex) {
      var set = dirtyTiles.get(lod);
      if (!set) { set = new Set(); dirtyTiles.set(lod, set); }
      set.add(hex);
      if (ctx.onDataReady) ctx.onDataReady();
    }
    // Retries always go through a timer: a flush can be requested from
    // inside deck's own render pass (renderSubLayers), and re-entering
    // the widget's redraw from there leaves tiles half-processed.
    var retryTimer = null;
    function scheduleRetry(delay) {
      if (retryTimer) return;
      retryTimer = setTimeout(function () {
        retryTimer = null;
        if (ctx.onDataReady) ctx.onDataReady();
      }, delay);
    }
    function flushDirtyTiles(lod) {
      var set = dirtyTiles.get(lod);
      if (!set || set.size === 0) return;
      var m = layerMemo.get(currentTilesId.get(lod));
      var tileset = m && m.layer.state && m.layer.state.tileset;
      if (!tileset) {
        // deck hasn't applied the layer yet (state moves to the new
        // instance on its next update); try again shortly.
        scheduleRetry(50);
        return;
      }
      var released = 0;
      var tiles = tileset.tiles || tileset._tiles || [];
      var present = new Set();
      for (var i = 0; i < tiles.length; i++) {
        var t = tiles[i];
        if (!t.index || !set.has(t.index.i)) continue;
        present.add(t.index.i);
        if (released < MAX_TILES_PER_FLUSH) {
          t.layers = null;
          set.delete(t.index.i);
          released++;
        }
      }
      // Tiles no longer in the tileset were evicted; they get a fresh
      // renderSubLayers call if they come back.
      set.forEach(function (hex) { if (!present.has(hex)) set.delete(hex); });
      if (released > 0 && typeof m.layer.setNeedsUpdate === "function") m.layer.setNeedsUpdate();
      if (set.size > 0) scheduleRetry(16);
    }
    function flushAllDirtyTiles() {
      dirtyTiles.forEach(function (set, lod) { if (set.size > 0) flushDirtyTiles(lod); });
    }
    function deferTile(lod, hex) {
      var set = dirtyTiles.get(lod);
      if (!set) { set = new Set(); dirtyTiles.set(lod, set); }
      set.add(hex);
      scheduleRetry(0);
    }
    function flushDirtyTiles(lod) {
      var set = dirtyTiles.get(lod);
      if (!set || set.size === 0) return;
      var m = layerMemo.get(currentTilesId.get(lod));
      var tileset = m && m.layer.state && m.layer.state.tileset;
      if (!tileset) return; // deck hasn't applied the layer yet; retried on next build
      var released = 0;
      var tiles = tileset.tiles || tileset._tiles || [];
      for (var i = 0; i < tiles.length && released < MAX_TILES_PER_FLUSH; i++) {
        var t = tiles[i];
        if (t.index && set.has(t.index.i)) {
          t.layers = null;
          set.delete(t.index.i);
          released++;
        }
      }
      if (released > 0 && typeof m.layer.setNeedsUpdate === "function") m.layer.setNeedsUpdate();
      if (set.size > 0 && ctx.onDataReady) ctx.onDataReady();
    }

    // Tileset2D over the data-bearing tiles of one LOD: atomic tiles,
    // no parent walk (A5 is not the quadtree deck.gl assumes).
    function makeTilesetClass(lod, bboxByTile) {
      var Base = window.deck && (window.deck._Tileset2D || window.deck.Tileset2D);
      if (!Base) throw new Error("deck.Tileset2D not found");
      return class extends Base {
        constructor(opts) {
          super(opts);
          this._lastKey = null;
          this._lastIndices = null;
        }
        getTileIndices(opts) {
          var qbbox = TILING.getViewportBbox(opts.viewport);
          var key = TILING.bboxKey(qbbox);
          if (this._lastKey === key && this._lastIndices) return this._lastIndices;
          var matches = [];
          bboxByTile.forEach(function (b, hex) {
            if (bboxOverlapXY(b, qbbox)) matches.push({ i: hex });
          });
          this._lastKey = key;
          this._lastIndices = matches;
          return matches;
        }
        getTileId(index) { return index ? index.i : null; }
        getTileMetadata(index) {
          if (!index) return null;
          var b = bboxByTile.get(index.i) || [-180, -85.05, 180, 85.05];
          // deck.gl expects west <= east; widen a tile across the
          // antimeridian to every longitude (getTileIndices culls exactly)
          if (b[0] > b[2]) b = [-180, b[1], 180, b[3]];
          return { bbox: { west: b[0], south: b[1], east: b[2], north: b[3] } };
        }
        getTileZoom() { return lod; }
        getParentIndex() { return null; }
        _getNearestAncestor() { return null; }
      };
    }

    function buildTiledLayer(x, lod, byTile, stroked) {
      var ver = loadVersion;
      var entry = tilesetClassesByLod.get(lod);
      var TilesetClass;
      if (entry && entry.ver === ver) {
        TilesetClass = entry.cls;
      } else {
        TilesetClass = makeTilesetClass(lod, bboxByLod.get(lod));
        tilesetClassesByLod.set(lod, { cls: TilesetClass, ver: ver });
      }
      // stroked and unstroked tiles are different layer classes (see
      // buildGeomLayer), so a change of stroke starts a fresh TileLayer
      var layerId = "a5-lazy-tiles-lod" + lod + (stroked ? "-stroked" : "") + "-v" + ver;
      currentTilesId.set(lod, layerId);
      return memoLayer(layerId, styleKey(x, stroked), function () {
        return new window.deck.TileLayer({
          id: layerId,
          data: EMPTY_DATA,
          TilesetClass: TilesetClass,
          extent: [-180, -85.05, 180, 85.05],
          opacity: ctx.getOpacity(),
          pickable: false,
          beforeId: ctx.getBeforeId(),
          getTileData: function (props) {
            var hex = props.index ? props.index.i : null;
            var rgs = (hex && byTile.get(hex)) || null;
            if (!rgs || rgs.length === 0) return null;
            for (var i = 0; i < rgs.length; i++) {
              if (!decodedCache.has(rgs[i])) decodeRowGroup(rgs[i]);
            }
            return rgs;
          },
          renderSubLayers: function (props) {
            var rgs = props.data;
            if (!rgs || rgs.length === 0) return null;
            var hex = props.tile.index.i;
            var geom = tileGeometry(lod, hex, rgs);
            if (!geom || geom.n === 0) return null;
            if (!withinBuildBudget()) { deferTile(lod, hex); return null; }
            return buildGeomLayer(x, geom, "a5-lazy-lod" + lod + "-tile-" + hex, stroked, {});
          }
        });
      });
    }

    function buildLayerAtLod(x, lod, viewport) {
      var stroked = !!x.stroked && cellEdgePx(lod, viewport) >= TILING.MIN_STROKE_PX;
      var byTile = tilesByLod.get(lod);
      if (byTile && byTile.size > 0) return buildTiledLayer(x, lod, byTile, stroked);
      return null;
    }

    function lodHasDecodedData(lod) {
      var byTile = tilesByLod.get(lod);
      if (!byTile) return false;
      var found = false;
      byTile.forEach(function (rgs) {
        if (found) return;
        for (var j = 0; j < rgs.length; j++) {
          if (decodedCache.has(rgs[j])) { found = true; return; }
        }
      });
      return found;
    }

    function getMinHoldMs() {
      var v = TILING.MIN_HOLD_MS;
      return (typeof v === "number" && v >= 0) ? v : 200;
    }

    // Layer(s) for the current viewport. While the LOD just switched to
    // is still cold, the previous warm LOD is stacked underneath so the
    // crossover is visible rather than a blank flash.
    //
    // deck.gl finalises every layer missing from the list it is given,
    // and a finalised instance cannot be initialised again ("assertion
    // failed"). So memoised layers live only while they are drawn: an
    // LOD left and later returned to gets a fresh TileLayer.
    function buildLodLayer(x, viewport) {
      var out = buildLodLayers(x, viewport);
      var drawn = new Set([].concat(out || []).map(function (l) { return l.id; }));
      layerMemo.forEach(function (m, id) { if (!drawn.has(id)) layerMemo.delete(id); });
      return out;
    }

    function buildLodLayers(x, viewport) {
      if (!ready || !rgIndex || !window.A5) return null;
      var schedule = x.lod_resolutions || null;
      if (!schedule || schedule.length === 0) return null;
      flushAllDirtyTiles();

      var R = TILING.getA5Resolution(viewport);
      var lod = TILING.pickLod(R, schedule);
      if (lod == null) return null;
      if (lod !== lastBuiltLod) {
        log("[a5view] lod=" + lod + " (R=" + R + ", zoom=" + (viewport.zoom || 0).toFixed(2) + ")");
        lastBuiltLod = lod;
        lodChangedAt = nowMs();
        if (holdTimer) clearTimeout(holdTimer);
        holdTimer = setTimeout(function () {
          holdTimer = null;
          if (ctx.onDataReady) ctx.onDataReady();
        }, getMinHoldMs() + 32);
      }

      var current = buildLayerAtLod(x, lod, viewport);
      if (current == null) return null;

      var holdElapsed = (nowMs() - lodChangedAt) >= getMinHoldMs();
      if (lodHasDecodedData(lod) && holdElapsed) {
        prevWarmLod = lod;
        return current;
      }
      // Skip the stacked hold at non-solid opacity: two LODs composited
      // read visibly denser than the single layer the user expects.
      if (ctx.getOpacity() < 1 - 1e-6) return current;
      if (prevWarmLod != null && prevWarmLod !== lod) {
        var prev = buildLayerAtLod(x, prevWarmLod, viewport);
        if (prev) return [prev, current];
      }
      return current;
    }

    // Decoded row at a LOD for a hex cell id, or null. The per-LOD hex
    // index is filled lazily from row groups as they are first needed.
    function findRow(lod, hex) {
      var byLod = rowsByLod.get(lod);
      if (!byLod) return null;
      while (byLod.pending.length > 0) {
        var rg = byLod.pending.pop();
        var g = decodedCache.get(rg);
        if (!g) continue;
        for (var i = 0; i < g.n; i++) {
          byLod.index.set(TILING.bigintToHex(g.cells[i]), [rg, i]);
        }
      }
      var hit = byLod.index.get(hex);
      if (!hit) return null;
      var geom = decodedCache.get(hit[0]);
      if (!geom) return null;
      var row = { cell: geom.cells[hit[1]] };
      if (geom.values) row._fill_value = geom.values[hit[1]];
      if (geom.elevation) row._elevation = geom.elevation[hit[1]];
      return row;
    }

    return {
      init: init,
      reset: reset,
      buildLodLayer: buildLodLayer,
      findRow: findRow,
      currentLod: function () { return lastBuiltLod; },
      isReady: function () { return ready; },
      stats: function () {
        return {
          loadVersion: loadVersion,
          format: sourceFormat,
          rowGroups: rgIndex ? rgIndex.length : 0,
          decoded: decodedCache.size,
          pending: pendingDecodes.size,
          queued: decodeQueue.length,
          tiledLods: tilesByLod.size,
          indexedLods: Array.from(rowsByLod.keys()),
          dirtyTiles: Array.from(dirtyTiles.values()).reduce(function (a, s) { return a + s.size; }, 0),
          retryPending: !!retryTimer,
          worker: !!(decoder && decoder.isWorker)
        };
      }
    };
  };
})();
