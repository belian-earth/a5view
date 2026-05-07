// =====================================================================
// a5view lazy parquet renderer
// =====================================================================
// Reads row groups on demand from an inline parquet payload so initial
// paint only decodes what the current viewport+LOD need. Subsequent
// pans/zooms decode any newly-required row groups; everything else is
// served from a Map<rg, decodedRows> cache.
//
// Wire format (set by R/prep-data.R::serialise_pyramid_to_parquet):
//   - Parquet sorted by `_lod` ASC, with row groups bucketed by parent
//     cell at `lod - pivot_offset`. Each row group's KV entry carries
//     `{rg, lod_min, lod_max, west, south, east, north, tile_id?}` where
//     `tile_id` is the parent cell's hex id (absent for non-tiled LODs).
//   - Schema KV metadata key `a5view_row_groups` holds that JSON array.
//
// Render strategy:
//   - For LODs whose row groups carry `tile_id`s, render through a
//     deck.gl TileLayer + an A5Tileset2D class. Each tile's
//     `renderSubLayers` instantiates an A5Layer over the rows decoded
//     from that tile's row group(s). Per-tile sub-layers stay on the
//     GPU across pans → smooth zoom/pan.
//   - For low-LOD single-row-group LODs (no tile_id), build one flat
//     A5Layer over all the rows — there's only one row group anyway.
//
// Dependencies:
//   - window.A5ViewHyparquetReady : Promise<hyparquet module>
//     (set by lib/hyparquet/hyparquet-bootstrap.js)
//   - window.A5 : a5-js bridge (cellToBoundary used by Tileset2D)
//   - window.A5View.tiling : shares helpers (boundary cache, pickLod,
//     getViewportBbox, getA5Resolution, makeA5Tileset2DClass).
// =====================================================================
(function () {
  var T = window.A5View = window.A5View || {};
  var TILING = T.tiling = T.tiling || {};
  var LAZY = T.lazy = T.lazy || {};

  function base64ToBytes(b64) {
    var binary = atob(b64);
    var n = binary.length;
    var out = new Uint8Array(n);
    for (var i = 0; i < n; i++) out[i] = binary.charCodeAt(i);
    return out;
  }

  // hyparquet wants an AsyncBuffer: { byteLength, slice(start, end) }.
  function makeAsyncBuffer(bytes) {
    return {
      byteLength: bytes.byteLength,
      slice: function (start, end) {
        var s = start | 0;
        var e = (end == null) ? bytes.byteLength : (end | 0);
        return bytes.buffer.slice(bytes.byteOffset + s, bytes.byteOffset + e);
      }
    };
  }

  // Normalise a pentagon cell value coming back from hyparquet into a
  // BigInt. a5R writes A5 ids as INT64, hyparquet decodes as bigint
  // already; defensive paths cover other shapes (FIXED_LEN_BYTE_ARRAY,
  // hex string).
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

  // Test whether a row-group bbox overlaps a query bbox. The query bbox
  // may straddle the antimeridian (west > east); split if so.
  function bboxOverlap(rg, q) {
    if (q.west <= q.east) {
      return !(rg.east < q.west || rg.west > q.east ||
               rg.north < q.south || rg.south > q.north);
    }
    return bboxOverlap(rg, { west: q.west, east: 180, south: q.south, north: q.north }) ||
           bboxOverlap(rg, { west: -180, east: q.east, south: q.south, north: q.north });
  }

  LAZY.createRenderer = function (ctx) {
    var fileBytes = null;
    var asyncBuffer = null;
    var metadata = null;
    var rgIndex = null;             // Array of KV entries (one per row group)
    var rgRowOffsets = null;        // Array<{start, end, num_rows}>
    var decodedCache = new Map();   // rg -> Array<rowObj>
    var pendingDecodes = new Map(); // rg -> Promise
    var loadVersion = 0;
    var ready = false;

    // Per-LOD lookup tables built from rgIndex.
    //   tilesByLod : Map<lod, Map<tileIdHex, [rgIdx, ...]>>
    //   bboxByLod  : Map<lod, Map<tileIdHex, [w, s, e, n]>>  (data-extent bbox)
    //   flatByLod  : Map<lod, [rgIdx, ...]>           (low LODs without tile_id)
    var tilesByLod = new Map();
    var bboxByLod  = new Map();
    var flatByLod  = new Map();
    // Cached row arrays for the flat path: stable references so deck.gl
    // sees the same `data` on rebuilds and skips GPU re-upload.
    var flatRowsCache = new Map(); // (loadVersion + lod) -> {rows, complete}

    // Cap concurrent parquetReadObjects calls. Bounded mainly to keep
    // 1000s-of-tiles bursts from runaway-queueing on the microtask
    // queue; for typical viewport sizes this is large enough to drain
    // in one continuous burst, minimising blank-tile time on zoom.
    var MAX_CONCURRENT_DECODES = 32;
    var decodeQueue = []; // FIFO of pending rgIdx requests

    // ctx.onDataReady → scheduleRebuild in the host, which already
    // throttles at REBUILD_THROTTLE_MS (32 ms). No extra debounce
    // needed — fast rebuilds are exactly what we want for incremental
    // tile fade-in.
    function scheduleDataReady() {
      if (ctx.onDataReady) ctx.onDataReady();
    }


    function reset() {
      fileBytes = null;
      asyncBuffer = null;
      metadata = null;
      rgIndex = null;
      rgRowOffsets = null;
      decodedCache = new Map();
      pendingDecodes = new Map();
      decodeQueue = [];
      tilesByLod = new Map();
      bboxByLod = new Map();
      flatByLod = new Map();
      flatRowsCache = new Map();
      tileRowsCacheByLod = new Map();
      tilesetClassesByLod = new Map();
      ready = false;
      loadVersion++;
    }

    // Prefetch the low-LOD (no tile_id) row groups eagerly. Those are
    // the wide-zoom entries — small and almost always needed by the
    // first render. We DON'T prefetch tile-bucketed row groups: with
    // ~256 rows each they'd individually pass any sensible row-count
    // threshold, and there can be thousands of them.

    var nowMs = (typeof performance !== "undefined" && performance.now)
      ? function () { return performance.now(); }
      : function () { return Date.now(); };

    function init(parquetB64) {
      reset();
      console.log("[a5view] lazy.init: bytes b64=" + (parquetB64 && parquetB64.length));
      var thisLoad = loadVersion;
      var t0 = nowMs();
      fileBytes = base64ToBytes(parquetB64);
      var tBase64 = nowMs();
      asyncBuffer = makeAsyncBuffer(fileBytes);

      return window.A5ViewHyparquetReady.then(function (hp) {
        if (thisLoad !== loadVersion) return;
        var ab = fileBytes.buffer.slice(
          fileBytes.byteOffset,
          fileBytes.byteOffset + fileBytes.byteLength
        );
        var md = hp.parquetMetadata(ab);
        var tMeta = nowMs();
        metadata = md;

        var kvs = md.key_value_metadata || [];
        for (var i = 0; i < kvs.length; i++) {
          if (kvs[i].key === "a5view_row_groups") {
            try { rgIndex = JSON.parse(kvs[i].value); }
            catch (e) { console.error("[a5view] bad row-group KV metadata:", e); }
            break;
          }
        }
        if (!rgIndex) {
          console.error("[a5view] parquet missing a5view_row_groups KV");
          return;
        }

        rgRowOffsets = new Array(md.row_groups.length);
        var off = 0;
        for (var j = 0; j < md.row_groups.length; j++) {
          var rg = md.row_groups[j];
          var n = Number(rg.num_rows);
          rgRowOffsets[j] = { start: off, end: off + n, num_rows: n };
          off += n;
        }

        // Index row groups by LOD. tile_id present → tile bucket (also
        // build the per-tile bbox by union'ing each contributing rg's
        // bbox so we never call cellToBoundary at runtime); absent →
        // flat (low-LOD single row group).
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
              if (entry.west  < bb[0]) bb[0] = entry.west;
              if (entry.south < bb[1]) bb[1] = entry.south;
              if (entry.east  > bb[2]) bb[2] = entry.east;
              if (entry.north > bb[3]) bb[3] = entry.north;
            } else {
              bboxByTile.set(entry.tile_id,
                [entry.west, entry.south, entry.east, entry.north]);
            }
          } else {
            var flat = flatByLod.get(lod);
            if (!flat) { flat = []; flatByLod.set(lod, flat); }
            flat.push(entry.rg);
          }
        }

        ready = true;
        var tReady = nowMs();
        console.log(
          "[a5view] init: base64 " + (tBase64 - t0).toFixed(0) + "ms, " +
          "metadata " + (tMeta - tBase64).toFixed(0) + "ms, " +
          "kv+ready " + (tReady - tMeta).toFixed(0) + "ms, " +
          "rg=" + rgIndex.length + " tiledLods=" + tilesByLod.size +
          " flatLods=" + flatByLod.size
        );
        // Sanity dump: stats on tile bboxes per tiled LOD so we can see
        // whether they're tight regional or pathologically global.
        bboxByLod.forEach(function (byTile, lodKey) {
          var n = byTile.size;
          var globalCount = 0;
          var first = null;
          var lonSpans = [], latSpans = [];
          byTile.forEach(function (b) {
            if (!first) first = b;
            if (b[0] <= -180 && b[2] >= 180) globalCount++;
            lonSpans.push(b[2] - b[0]);
            latSpans.push(b[3] - b[1]);
          });
          var meanLon = lonSpans.reduce(function (a,c){return a+c;}, 0) / Math.max(1, n);
          var meanLat = latSpans.reduce(function (a,c){return a+c;}, 0) / Math.max(1, n);
          console.log("[a5view] lod " + lodKey + " tiles=" + n +
                      " globalBbox=" + globalCount +
                      " meanLonSpan=" + meanLon.toFixed(2) +
                      " meanLatSpan=" + meanLat.toFixed(2) +
                      " first=" + JSON.stringify(first));
        });

        // Prefetch only the low-LOD flat row groups (no tile_id). These
        // are typically a few entries with up to a few thousand rows,
        // covering wide-zoom paint. Tile-bucketed row groups stay lazy.
        for (var p = 0; p < rgIndex.length; p++) {
          if (!rgIndex[p].tile_id) decodeRowGroup(rgIndex[p].rg);
        }
      });
    }

    // Drain the decode queue while we have headroom. Called on completion
    // of every in-flight decode.
    function pumpDecodeQueue() {
      while (pendingDecodes.size < MAX_CONCURRENT_DECODES && decodeQueue.length > 0) {
        var next = decodeQueue.shift();
        if (decodedCache.has(next) || pendingDecodes.has(next)) continue;
        startDecode(next);
      }
    }

    function startDecode(rgIdx) {
      var thisLoad = loadVersion;
      var off = rgRowOffsets[rgIdx];
      var t0 = nowMs();
      var p = window.A5ViewHyparquetReady.then(function (hp) {
        return hp.parquetReadObjects({
          file: asyncBuffer,
          metadata: metadata,
          rowStart: off.start,
          rowEnd: off.end
        });
      }).then(function (rows) {
        pendingDecodes.delete(rgIdx);
        if (thisLoad !== loadVersion) { pumpDecodeQueue(); return []; }
        decodedCache.set(rgIdx, rows);
        var dt = nowMs() - t0;
        if (dt > 5) {
          console.log("[a5view] decoded rg" + rgIdx + " (" + rows.length +
                      " rows) in " + dt.toFixed(0) + "ms");
        }
        scheduleDataReady();
        pumpDecodeQueue();
        return rows;
      }).catch(function (err) {
        pendingDecodes.delete(rgIdx);
        console.error("[a5view] row-group", rgIdx, "decode failed:", err);
        pumpDecodeQueue();
        return [];
      });
      pendingDecodes.set(rgIdx, p);
      return p;
    }

    function decodeRowGroup(rgIdx) {
      if (decodedCache.has(rgIdx)) return Promise.resolve(decodedCache.get(rgIdx));
      var pending = pendingDecodes.get(rgIdx);
      if (pending) return pending;
      // At capacity: queue and return a placeholder. Fire-and-forget
      // callers (getTileData, buildFlatLayer/buildTileRows) don't use
      // the returned promise; they re-check decodedCache on the next
      // ctx.onDataReady-driven rebuild.
      if (pendingDecodes.size >= MAX_CONCURRENT_DECODES) {
        if (decodeQueue.indexOf(rgIdx) === -1) decodeQueue.push(rgIdx);
        return Promise.resolve([]);
      }
      return startDecode(rgIdx);
    }

    function buildA5LayerProps(x, data, layerId, updateKey) {
      var hasFill = x.fill_per_cell;
      var uniformFill = x.fill_color || [116, 172, 144, 255];
      var getFillColor = hasFill
        ? function (d) { return [d._fill_r, d._fill_g, d._fill_b, d._fill_a]; }
        : uniformFill;

      var props = {
        id: layerId,
        data: data,
        getPentagon: function (d) {
          // hyparquet may yield bigint, number, or bytes; A5Layer wants
          // BigInt. Cache on the row to avoid repeating the cast.
          return d.__pent || (d.__pent = toBigIntCell(d.pentagon));
        },
        getFillColor: getFillColor,
        opacity: ctx.getOpacity(),
        pickable: x.pickable && !ctx.getDrawMode(),
        autoHighlight: false,
        extruded: x.extruded,
        elevationScale: x.elevation_scale,
        stroked: x.stroked,
        getLineColor: x.line_color || [0, 0, 0, 0],
        getLineWidth: x.line_width || 1,
        lineWidthUnits: "pixels",
        updateTriggers: {
          getFillColor: updateKey,
          getElevation: updateKey
        }
      };
      if (x.extruded) {
        props.getElevation = function (d) { return d._elevation || 0; };
      }
      if (ctx.getGlobe()) {
        props.parameters = { depthCompare: "always", cullMode: "back" };
      }
      return props;
    }

    function buildA5Layer(x, data, layerId, updateKey) {
      return new window.deck.A5Layer(buildA5LayerProps(x, data, layerId, updateKey));
    }

    // Flat path: low LODs without tile bucketing. One A5Layer over the
    // concatenated rows of every row group at this LOD. Decodes on
    // demand; rebuild fires when row groups land. Rows array is cached
    // by (load, lod) so deck.gl gets the same data reference across
    // rebuilds and skips re-uploading buffers.
    function buildFlatLayer(x, lod, rgs) {
      var cacheKey = loadVersion + "|" + lod;
      var cached = flatRowsCache.get(cacheKey);
      var rows, complete;
      if (cached && cached.complete) {
        rows = cached.rows;
        complete = true;
      } else {
        rows = [];
        complete = true;
        for (var i = 0; i < rgs.length; i++) {
          var d = decodedCache.get(rgs[i]);
          if (d) {
            for (var j = 0; j < d.length; j++) {
              if (d[j]._lod === lod) rows.push(d[j]);
            }
          } else {
            complete = false;
            decodeRowGroup(rgs[i]);
          }
        }
        flatRowsCache.set(cacheKey, { rows: rows, complete: complete });
      }
      if (rows.length === 0) return null;
      var key = "flat|" + cacheKey + "|" + rows.length + "|" + (complete ? 1 : 0);
      return buildA5Layer(x, rows, "a5-lazy-flat-" + lod, key);
    }

    // Tiled path: deck.gl TileLayer with a custom A5 Tileset2D over
    // our row-group tiles. The Tileset2D iterates bboxByTile (the data
    // extents we computed in R) for visible-tile selection, and
    // disables deck.gl's parent-chain walk by returning null from
    // getParentIndex — A5 cells form a hierarchy but it's not the
    // quadtree shape deck.gl assumes, and walking it creates phantom
    // tiles for every coarser level that froze the page last time we
    // tried this. The flat base layer (rendered separately by
    // buildLodLayer) covers the fade-in role that the parent walk
    // would have played.
    //
    // Per-(lod, tileId) cache of materialised row arrays so deck.gl
    // sub-layers see the same `data` reference between rebuilds and
    // skip GPU buffer re-uploads.
    var tileRowsCacheByLod = new Map(); // lod -> Map<tileId, { key, rows }>
    // Tileset2D class cache, keyed by (lod, loadVersion). Rebuilt only
    // on data swap.
    var tilesetClassesByLod = new Map();

    function bboxOverlapXY(b, q) {
      if (q.west <= q.east) {
        return !(b[2] < q.west || b[0] > q.east ||
                 b[3] < q.south || b[1] > q.north);
      }
      return (!(b[2] < q.west || b[0] > 180     || b[3] < q.south || b[1] > q.north)) ||
             (!(b[2] < -180   || b[0] > q.east  || b[3] < q.south || b[1] > q.north));
    }

    // Materialise polygon entries for one tile, cached per (lod, tileId)
    // so deck.gl sees the same data reference until the tile's decoded
    // set grows. We pre-resolve cell boundaries here so the per-tile
    // sub-layer can be a flat SolidPolygonLayer / PolygonLayer rather
    // than the A5Layer CompositeLayer (CompositeLayer-inside-TileLayer
    // produces a 4-level layer tree per tile and freezes the page at
    // ~75 visible tiles).
    function buildTilePolys(lod, hex, rgs) {
      var perLod = tileRowsCacheByLod.get(lod);
      if (!perLod) { perLod = new Map(); tileRowsCacheByLod.set(lod, perLod); }
      var decodedCount = 0;
      for (var i = 0; i < rgs.length; i++) {
        if (decodedCache.has(rgs[i])) decodedCount++;
      }
      if (decodedCount === 0) return null;
      var key = loadVersion + "|" + decodedCount;
      var cached = perLod.get(hex);
      if (cached && cached.key === key) return cached.entries;
      var A5 = window.A5;
      var entries = [];
      for (var j = 0; j < rgs.length; j++) {
        var d = decodedCache.get(rgs[j]);
        if (!d) continue;
        for (var k = 0; k < d.length; k++) {
          var r = d[k];
          if (r._lod !== lod) continue;
          var cell = r.__pent || (r.__pent = toBigIntCell(r.pentagon));
          var entry = {
            polygon: TILING.cachedBoundary(A5, cell),
            pentagon: cell,
            r: r._fill_r, g: r._fill_g, b: r._fill_b, a: r._fill_a,
            elevation: r._elevation
          };
          entries.push(entry);
        }
      }
      perLod.set(hex, { key: key, entries: entries });
      return entries;
    }

    // Build a SolidPolygonLayer (or PolygonLayer when stroke is on)
    // from per-tile polygon entries. Flat layer tree — no
    // CompositeLayer nesting.
    function buildPolygonSubLayer(x, entries, layerId, updateKey) {
      var hasFill = x.fill_per_cell;
      var uniformFill = x.fill_color || [116, 172, 144, 255];
      var getFillColor = hasFill
        ? function (d) { return [d.r, d.g, d.b, d.a]; }
        : uniformFill;

      var props = {
        id: layerId,
        data: entries,
        getPolygon: function (d) { return d.polygon; },
        getFillColor: getFillColor,
        opacity: ctx.getOpacity(),
        pickable: x.pickable && !ctx.getDrawMode(),
        autoHighlight: false,
        extruded: x.extruded,
        elevationScale: x.elevation_scale,
        updateTriggers: {
          getFillColor: updateKey,
          getElevation: updateKey
        }
      };
      if (x.extruded) {
        props.getElevation = function (d) { return d.elevation || 0; };
      }
      if (ctx.getGlobe()) {
        props.parameters = { depthCompare: "always", cullMode: "back" };
      }
      if (x.stroked) {
        props.stroked = true;
        props.filled = true;
        props.getLineColor = x.line_color || [0, 0, 0, 0];
        props.getLineWidth = x.line_width || 1;
        props.lineWidthUnits = "pixels";
        return new window.deck.PolygonLayer(props);
      }
      return new window.deck.SolidPolygonLayer(props);
    }

    // Custom Tileset2D: atomic A5 tiles, no parent walks.
    function makeLazyA5TilesetClass(lod, bboxByTile) {
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
          var key = TILING._bboxKey(qbbox);
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
        // Bypass deck.gl's parent walk. _rebuildTree calls this for every
        // cache tile; the default impl loops `while (getTileZoom(index) >
        // _minZoom)` and our constant getTileZoom never trips that exit.
        _getNearestAncestor() { return null; }
      };
    }

    function buildTiledLayer(x, lod, byTile) {
      var ver = loadVersion;
      var bboxByTile = bboxByLod.get(lod);
      var entry = tilesetClassesByLod.get(lod);
      var TilesetClass;
      if (entry && entry.ver === ver) {
        TilesetClass = entry.cls;
      } else {
        TilesetClass = makeLazyA5TilesetClass(lod, bboxByTile);
        tilesetClassesByLod.set(lod, { cls: TilesetClass, ver: ver });
      }

      // Pass opacity up to TileLayer so deck.gl detects the change and
      // nulls tile.layers, forcing renderSubLayers to re-run with the
      // latest ctx.getOpacity(). Without this, deck.gl's prop diff sees
      // nothing changed and skips sublayer regen, so the slider's
      // effect never propagates.
      return new window.deck.TileLayer({
        id: "a5-lazy-tiles-lod" + lod + "-v" + ver,
        data: [],
        TilesetClass: TilesetClass,
        extent: [-180, -85.05, 180, 85.05],
        opacity: ctx.getOpacity(),
        pickable: x.pickable && !ctx.getDrawMode(),
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
          var entries = buildTilePolys(lod, hex, rgs);
          if (!entries || entries.length === 0) return null;
          var key = ver + "|" + lod + "|" + hex + "|" + entries.length;
          return buildPolygonSubLayer(
            x, entries, "a5-lazy-lod" + lod + "-tile-" + hex, key
          );
        }
      });
    }

    var lastBuiltLod = null;
    function buildLodLayer(x, viewport) {
      if (!ready || !rgIndex || !window.A5) return null;
      var schedule = x.lod_resolutions || null;
      if (!schedule || schedule.length === 0) return null;

      var R = TILING.getA5Resolution(viewport);
      var lod = TILING.pickLod(R, schedule);
      if (lod == null) return null;
      if (lod !== lastBuiltLod) {
        console.log("[a5view] buildLodLayer lod=" + lod +
                    " (R=" + R + ", zoom=" + (viewport.zoom || 0).toFixed(2) +
                    ") tiled=" + tilesByLod.has(lod) + " flat=" + flatByLod.has(lod));
        lastBuiltLod = lod;
      }

      var byTile = tilesByLod.get(lod);
      if (byTile && byTile.size > 0) {
        return buildTiledLayer(x, lod, byTile);
      }
      var flat = flatByLod.get(lod);
      if (flat && flat.length > 0) {
        return buildFlatLayer(x, lod, flat);
      }
      return null;
    }

    return {
      init: init,
      reset: reset,
      buildLodLayer: buildLodLayer,
      isReady: function () { return ready; },
      stats: function () {
        return {
          loadVersion: loadVersion,
          rowGroups: rgIndex ? rgIndex.length : 0,
          decoded: decodedCache.size,
          pending: pendingDecodes.size,
          tiledLods: tilesByLod.size,
          flatLods: flatByLod.size
        };
      }
    };
  };
})();
