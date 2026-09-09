// =====================================================================
// a5view lazy parquet renderer
// =====================================================================
// Reads row groups on demand from a pyramid parquet file so initial
// paint only decodes what the current viewport + LOD need. Subsequent
// pans/zooms decode any newly-required row groups; everything else is
// served from a per-row-group cache of packed geometry.
//
// Wire format (set by R/prep-data.R::serialise_pyramid_to_parquet):
//   - Parquet sorted by `_lod` ASC; each row group holds a single LOD.
//     Large LODs are bucketed by parent cell at `lod - pivot_offset`.
//     Each row group's KV entry carries `{rg, lod_min, lod_max, west,
//     south, east, north, tile_id?}` where `tile_id` is the parent
//     cell's hex id (absent for the small, non-tiled LODs).
//   - Schema KV metadata key `a5view_row_groups` holds that JSON array.
//
// Work split:
//   - A Web Worker owns the parquet source (inline bytes or an HTTP URL
//     read with Range requests), decodes row groups with hyparquet and
//     turns each into packed typed arrays: pentagon boundaries from
//     a5-js, RGBA, fill values. Only transferable buffers cross back.
//   - The main thread builds deck.gl layers from those buffers using
//     binary attributes (no per-cell objects, no accessor calls, no
//     polygon normalisation), so a tile's first paint costs a few
//     sub-layer instantiations rather than tens of milliseconds.
//   - If workers are unavailable (or the worker fails to load its
//     libraries) the same packing code runs on the main thread.
//
// Render strategy:
//   - LODs whose row groups carry `tile_id`s render through a deck.gl
//     TileLayer + an A5-aware Tileset2D; each tile is one
//     SolidPolygonLayer (PolygonLayer when stroked and cells are big
//     enough on screen to show a border).
//   - Small LODs (no tile_id) render as one SolidPolygonLayer over the
//     concatenation of their row groups.
//
// Dependencies:
//   - window.A5ViewHyparquetReady / window.A5ViewHyparquetUrl (set by
//     lib/hyparquet/hyparquet-bootstrap.js)
//   - window.A5Ready / window.A5ViewA5Url (set by lib/a5-js/bridge.js)
//   - window.A5View.tiling: getViewportBbox, bboxKey, getA5Resolution,
//     pickLod, bigintToHex, REBUILD_THROTTLE_MS, MIN_HOLD_MS.
//
// ctx (provided by a5view.js):
//   getOpacity()    current layer opacity
//   getBeforeId()   MapLibre layer id to insert cell layers beneath
//   onDataReady()   called whenever a row group's geometry lands
// =====================================================================
(function () {
  var T = window.A5View = window.A5View || {};
  var TILING = T.tiling = T.tiling || {};
  var LAZY = T.lazy = T.lazy || {};
  var log = function () { T.log.apply(null, arguments); };

  // Below this projected edge length (CSS px) cell borders are skipped:
  // they would only smear the fill and double the per-tile layer count.
  TILING.MIN_STROKE_PX = 3;

  // ───────────────────────────────────────────────────────────────────
  // Tile core: source handling + row-group decode + geometry packing.
  //
  // Written as a self-contained function of its two library modules so
  // the very same code runs inside a Worker (stringified into a Blob)
  // or on the main thread as a fallback. Nothing in here may reference
  // closure variables from this file.
  // ───────────────────────────────────────────────────────────────────
  function tileCore(A5, hp) {
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

    // source: { b64 } or { url, bytes }. Resolves to the KV row-group
    // index (array) once the footer has been read.
    function open(source) {
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
        var kv = (md.key_value_metadata || []).filter(function (e) {
          return e.key === "a5view_row_groups";
        })[0];
        if (!kv) throw new Error("parquet missing a5view_row_groups KV");
        return JSON.parse(kv.value);
      });
    }

    // Decode one row group and pack it:
    //   cells      BigUint64Array(n)       cell ids
    //   starts     Uint32Array(n + 1)      vertex start index per cell
    //   positions  Float64Array(nv * 2)    [lon, lat, ...]
    //   colors     Uint8Array(nv * 4)      RGBA per vertex, when present
    //   values     Float32Array(n)         `_fill_value` per cell, when present
    //   elevation  Float64Array(nv)        `_elevation` per vertex, when present
    // deck.gl's polygon attributes are per vertex, so colour and
    // elevation are expanded here rather than per cell.
    function decode(rgIdx) {
      var off = rowOffsets[rgIdx];
      var ready = Promise.resolve();
      if (file.prefetch) {
        var range = rowGroupByteRange(metadata.row_groups[rgIdx]);
        ready = file.prefetch(range.start, range.end);
      }
      return ready.then(function () {
        return hp.parquetReadObjects({
          file: file, metadata: metadata, rowStart: off.start, rowEnd: off.end
        });
      }).then(function (rows) {
        var n = rows.length;
        var hasColor = n > 0 && rows[0]._fill_r !== undefined;
        var hasValue = n > 0 && rows[0]._fill_value !== undefined;
        var hasElev = n > 0 && rows[0]._elevation !== undefined;
        var cells = new BigUint64Array(n);
        var rings = new Array(n);
        var nv = 0;
        for (var i = 0; i < n; i++) {
          var cell = toBigIntCell(rows[i].pentagon);
          cells[i] = cell;
          var ring = A5.cellToBoundary(cell, { closedRing: false });
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
        if (hasColor) {
          var colors = new Uint8Array(nv * 4);
          for (var c = 0; c < n; c++) {
            var row = rows[c];
            for (var cv = starts[c]; cv < starts[c + 1]; cv++) {
              colors[cv * 4] = row._fill_r; colors[cv * 4 + 1] = row._fill_g;
              colors[cv * 4 + 2] = row._fill_b; colors[cv * 4 + 3] = row._fill_a;
            }
          }
          out.colors = colors; transfer.push(colors.buffer);
        }
        if (hasValue) {
          var values = new Float32Array(n);
          for (var v = 0; v < n; v++) values[v] = rows[v]._fill_value;
          out.values = values; transfer.push(values.buffer);
        }
        if (hasElev) {
          var elev = new Float64Array(nv);
          for (var e = 0; e < n; e++) {
            var ev = rows[e]._elevation || 0;
            for (var ei = starts[e]; ei < starts[e + 1]; ei++) elev[ei] = ev;
          }
          out.elevation = elev; transfer.push(elev.buffer);
        }
        out.transfer = transfer;
        return out;
      });
    }

    return { open: open, decode: decode };
  }

  // Worker entry: import the two libraries, then serve open / decode
  // requests over postMessage. Runs from a Blob URL so no extra file
  // has to be shipped; it inherits the page's origin.
  function workerMain() {
    var core = null;
    var libs = null;
    function loadLibs(urls) {
      if (libs) return libs;
      function tryImport(list) {
        return import(list[0]).catch(function (e) {
          if (list.length > 1) return tryImport(list.slice(1));
          throw e;
        });
      }
      libs = Promise.all([tryImport(urls.a5), tryImport(urls.hyparquet)]);
      return libs;
    }
    self.onmessage = function (ev) {
      var msg = ev.data;
      if (msg.type === "open") {
        loadLibs(msg.libs).then(function (mods) {
          core = tileCore(mods[0], mods[1]);
          return core.open(msg.source);
        }).then(function (rgIndex) {
          self.postMessage({ type: "opened", id: msg.id, rgIndex: rgIndex });
        }).catch(function (e) {
          self.postMessage({ type: "error", id: msg.id, message: String(e && e.message || e) });
        });
      } else if (msg.type === "decode") {
        core.decode(msg.rg).then(function (out) {
          var transfer = out.transfer; delete out.transfer;
          out.type = "decoded"; out.id = msg.id;
          self.postMessage(out, transfer);
        }).catch(function (e) {
          self.postMessage({ type: "error", id: msg.id, rg: msg.rg, message: String(e && e.message || e) });
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

  // Uniform promise-based front over either a worker or the in-thread
  // core: { open(source) -> Promise<rgIndex>, decode(rg) -> Promise<packed>,
  // close(), isWorker }
  function makeDecoder(libUrls) {
    var worker = makeWorker();
    if (worker) {
      var pending = new Map();
      var nextId = 1;
      worker.onmessage = function (ev) {
        var msg = ev.data;
        var p = pending.get(msg.id);
        if (!p) return;
        pending.delete(msg.id);
        if (msg.type === "error") p.reject(new Error(msg.message));
        else p.resolve(msg);
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
      return {
        isWorker: true,
        open: function (source) {
          return send({ type: "open", source: source, libs: libUrls }).then(function (m) { return m.rgIndex; });
        },
        decode: function (rg) { return send({ type: "decode", rg: rg }); },
        close: function () { worker.terminate(); pending.clear(); }
      };
    }
    var core = null;
    return {
      isWorker: false,
      open: function (source) {
        return Promise.all([TILING.ensureA5(), window.A5ViewHyparquetReady]).then(function (mods) {
          core = tileCore(mods[0], mods[1]);
          return core.open(source);
        });
      },
      decode: function (rg) { return core.decode(rg); },
      close: function () {}
    };
  }

  function libUrls() {
    var a5 = [];
    if (window.A5ViewA5Url) a5.push(window.A5ViewA5Url);
    a5.push("https://cdn.jsdelivr.net/npm/a5-js@0.10.0/+esm");
    var hyp = [];
    if (window.A5ViewHyparquetUrl) hyp.push(window.A5ViewHyparquetUrl);
    hyp.push("https://cdn.jsdelivr.net/npm/hyparquet@1.25.6/+esm");
    return { a5: a5, hyparquet: hyp };
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

  // Test whether a [w, s, e, n] bbox overlaps a query bbox that may
  // straddle the antimeridian (west > east).
  function bboxOverlapXY(b, q) {
    if (q.west <= q.east) {
      return !(b[2] < q.west || b[0] > q.east || b[3] < q.south || b[1] > q.north);
    }
    return (!(b[2] < q.west || b[0] > 180 || b[3] < q.south || b[1] > q.north)) ||
           (!(b[2] < -180 || b[0] > q.east || b[3] < q.south || b[1] > q.north));
  }

  var EMPTY_DATA = [];

  LAZY.createRenderer = function (ctx) {
    var decoder = null;
    var rgIndex = null;             // KV entries, one per row group
    var decodedCache = new Map();   // rg -> packed geometry
    var pendingDecodes = new Map(); // rg -> Promise
    var loadVersion = 0;
    var ready = false;

    // Per-LOD lookup tables built from rgIndex.
    var tilesByLod = new Map();     // lod -> Map<tileHex, [rgIdx, ...]>
    var bboxByLod = new Map();      // lod -> Map<tileHex, [w, s, e, n]>
    var flatByLod = new Map();      // lod -> [rgIdx, ...]
    var flatGeomCache = new Map();  // lod -> { count, geom }
    var tileGeomCache = new Map();  // lod -> Map<tileHex, { count, geom }>
    var tilesetClassesByLod = new Map();
    var layerMemo = new Map();      // layerId -> { key, layer }
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
      flatByLod = new Map();
      flatGeomCache = new Map();
      tileGeomCache = new Map();
      tilesetClassesByLod = new Map();
      layerMemo = new Map();
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

    // source: { b64 } (inline) or { url, bytes } (served with Range
    // support). Resolves once the row-group index is known; row groups
    // decode on demand from then on.
    function init(source) {
      reset();
      var thisLoad = loadVersion;
      var t0 = nowMs();
      if (source.url) source = { url: absoluteUrl(source.url), bytes: source.bytes };
      log("[a5view] lazy.init: " + (source.url ? "url " + source.url + " bytes=" + source.bytes
                                               : "inline b64=" + (source.b64 && source.b64.length)));
      decoder = makeDecoder(libUrls());
      return decoder.open(source).then(function (index) {
        if (thisLoad !== loadVersion) return;
        rgIndex = index;
        for (var k = 0; k < rgIndex.length; k++) {
          var entry = rgIndex[k];
          var lod = entry.lod_min;
          if (entry.tile_id) {
            var byTile = tilesByLod.get(lod);
            if (!byTile) { byTile = new Map(); tilesByLod.set(lod, byTile); }
            var lst = byTile.get(entry.tile_id);
            if (!lst) { lst = []; byTile.set(entry.tile_id, lst); }
            lst.push(entry.rg);
            var bboxByTile = bboxByLod.get(lod);
            if (!bboxByTile) { bboxByTile = new Map(); bboxByLod.set(lod, bboxByTile); }
            var bb = bboxByTile.get(entry.tile_id);
            if (bb) {
              if (entry.west < bb[0]) bb[0] = entry.west;
              if (entry.south < bb[1]) bb[1] = entry.south;
              if (entry.east > bb[2]) bb[2] = entry.east;
              if (entry.north > bb[3]) bb[3] = entry.north;
            } else {
              bboxByTile.set(entry.tile_id, [entry.west, entry.south, entry.east, entry.north]);
            }
          } else {
            var flat = flatByLod.get(lod);
            if (!flat) { flat = []; flatByLod.set(lod, flat); }
            flat.push(entry.rg);
          }
        }
        ready = true;
        log("[a5view] init: " + (nowMs() - t0).toFixed(0) + "ms, rg=" + rgIndex.length +
            " tiledLods=" + tilesByLod.size + " flatLods=" + flatByLod.size +
            (decoder.isWorker ? " (worker)" : " (main thread)"));
        // The small, non-tiled LODs cover wide zooms and are almost
        // always needed first; fetch them eagerly.
        for (var p = 0; p < rgIndex.length; p++) {
          if (!rgIndex[p].tile_id) decodeRowGroup(rgIndex[p].rg);
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
      var t0 = nowMs();
      var p = decoder.decode(rgIdx).then(function (packed) {
        pendingDecodes.delete(rgIdx);
        if (thisLoad !== loadVersion) { pumpDecodeQueue(); return null; }
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
        pendingDecodes.delete(rgIdx);
        console.error("[a5view] row-group", rgIdx, "decode failed:", err);
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

    // One SolidPolygonLayer (or PolygonLayer with stroke) over a packed
    // geometry block, via binary attributes.
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
      if (stroked) {
        props.stroked = true;
        props.filled = true;
        props.getLineColor = x.line_color || [0, 0, 0, 0];
        props.getLineWidth = x.line_width || 1;
        props.lineWidthUnits = "pixels";
        return new window.deck.PolygonLayer(props);
      }
      return new window.deck.SolidPolygonLayer(props);
    }

    // Flat path: one layer over every decoded row group of a small LOD.
    function buildFlatLayer(x, lod, rgs, stroked) {
      var count = 0;
      var parts = [];
      for (var i = 0; i < rgs.length; i++) {
        var g = decodedCache.get(rgs[i]);
        if (g) { parts.push(g); count++; }
        else decodeRowGroup(rgs[i]);
      }
      if (parts.length === 0) return null;
      var cached = flatGeomCache.get(lod);
      if (!cached || cached.count !== count) {
        cached = { count: count, geom: concatPacked(parts) };
        flatGeomCache.set(lod, cached);
      }
      var layerId = "a5-lazy-flat-lod" + lod + "-v" + loadVersion;
      return memoLayer(layerId, count + "|" + styleKey(x, stroked), function () {
        return buildGeomLayer(x, cached.geom, layerId, stroked, { beforeId: ctx.getBeforeId() });
      });
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
      var m = layerMemo.get("a5-lazy-tiles-lod" + lod + "-v" + loadVersion);
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
      var m = layerMemo.get("a5-lazy-tiles-lod" + lod + "-v" + loadVersion);
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
      var layerId = "a5-lazy-tiles-lod" + lod + "-v" + ver;
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
      var flat = flatByLod.get(lod);
      if (flat && flat.length > 0) return buildFlatLayer(x, lod, flat, stroked);
      return null;
    }

    function lodHasDecodedData(lod) {
      var flat = flatByLod.get(lod);
      if (flat) {
        for (var i = 0; i < flat.length; i++) if (decodedCache.has(flat[i])) return true;
      }
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
    function buildLodLayer(x, viewport) {
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
      var row = { pentagon: geom.cells[hit[1]] };
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
          rowGroups: rgIndex ? rgIndex.length : 0,
          decoded: decodedCache.size,
          pending: pendingDecodes.size,
          queued: decodeQueue.length,
          tiledLods: tilesByLod.size,
          flatLods: flatByLod.size,
          indexedLods: Array.from(rowsByLod.keys()),
          dirtyTiles: Array.from(dirtyTiles.values()).reduce(function (a, s) { return a + s.size; }, 0),
          retryPending: !!retryTimer,
          worker: !!(decoder && decoder.isWorker)
        };
      }
    };
  };
})();
