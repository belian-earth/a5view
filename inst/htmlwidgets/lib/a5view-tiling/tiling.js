// =====================================================================
// a5view LOD-picker module
// =====================================================================
// The R side ships a single Arrow IPC payload containing leaf rows plus
// pre-aggregated parent rows at each LOD, tagged with a `_lod` column.
// At render time we:
//
//   1. Pick the LOD matching the current viewport zoom (and latitude
//      compensation, if enabled) by interpolating into ZOOM_TO_RES and
//      snapping to the nearest available LOD in the data's schedule.
//   2. Viewport-cull that LOD's row indices using cached per-cell
//      bbox bounds.
//   3. Render the survivors as a SolidPolygonLayer (or PolygonLayer
//      when stroke is requested), using cached cell boundaries as the
//      polygon geometry.
//
// All tunable knobs live on  window.A5View.tiling.*
//
// ─── LEVERS ──────────────────────────────────────────────────────────
//   ZOOM_TO_RES          : (zoom, R) anchor table; zoom -> A5 resolution
//                          is linearly interpolated.
//   USE_LAT_COMPENSATION : high latitudes get a small zoom boost
//                          (Mercator pixels cover less ground there).
//   VIEWPORT_BUFFER      : query-bbox inflation around the visible area.
//   REBUILD_THROTTLE_MS  : floor on time between layer rebuilds during
//                          continuous pan/zoom.
//   BBOX_QUANTUM_DIVISOR : memoisation grid for viewport keys.
//
// ─── CACHES ──────────────────────────────────────────────────────────
//   cellToBoundary       : per cell BigInt id; pentagon geometry never
//                          changes so memoised page-lifetime.
//   per-LOD row groups   : row indices grouped by `_lod` column,
//                          rebuilt on data swap.
//   per-cell bbox bounds : Float32Array(4n) of [minLon, minLat, maxLon,
//                          maxLat] computed lazily from cached
//                          boundaries; trivially fast viewport culling.
//   visible-set          : last assembled polygon array, keyed by
//                          (lod, bboxKey, dataVersion); fine pans
//                          within a quantum reuse it.
// =====================================================================
(function () {
  var T = window.A5View = window.A5View || {};
  var TILING = T.tiling = T.tiling || {};

  // ───────────────────────────────────────────────────────────────────
  // LEVERS
  // ───────────────────────────────────────────────────────────────────

  TILING.ZOOM_TO_RES = [
    [0, 7],
    [3, 10],
    [7, 14],
    [10, 15],
    [16, 22]];

  TILING.USE_LAT_COMPENSATION = true;

  TILING.VIEWPORT_BUFFER = 0.1;

  TILING.REBUILD_THROTTLE_MS = 32;

  TILING.BBOX_QUANTUM_DIVISOR = 32;

  // Above this row count, an LOD renders through a deck.gl TileLayer
  // (per-tile sub-layers cached across renders) instead of one big
  // SolidPolygonLayer. Below: single layer is cheaper than the tile
  // bookkeeping. Tune from the console if needed.
  TILING.TILE_THRESHOLD = 50000;

  // Tile bucketing offset: each tile groups rows whose LOD parent at
  // (lod - TILE_OFFSET) is the tile id. Larger offset = fewer, bigger
  // tiles. Pentagons subdivide ~3.5x per level so OFFSET 4 ≈ 150 cells
  // per tile.
  TILING.TILE_OFFSET = 4;

  // ───────────────────────────────────────────────────────────────────
  // Profiler (opt-in)
  // ───────────────────────────────────────────────────────────────────
  // From the browser console:
  //   A5View.tiling.profile.enable()
  //   ...interact with the map...
  //   A5View.tiling.profile.report()
  (function () {
    var enabled = false;
    var stats = Object.create(null);
    var now = (typeof performance !== "undefined" && performance.now)
      ? function () { return performance.now(); }
      : function () { return Date.now(); };

    function time(name, fn) {
      if (!enabled) return fn();
      var t0 = now();
      var out = fn();
      var dt = now() - t0;
      var s = stats[name] || (stats[name] = { ms: 0, n: 0 });
      s.ms += dt; s.n += 1;
      return out;
    }
    function bump(name, ms) {
      if (!enabled) return;
      var s = stats[name] || (stats[name] = { ms: 0, n: 0 });
      s.ms += (ms || 0); s.n += 1;
    }
    function report() {
      var rows = Object.keys(stats).sort().map(function (k) {
        var s = stats[k];
        return {
          op: k, n: s.n,
          ms_total: +s.ms.toFixed(2),
          ms_avg: +(s.ms / s.n).toFixed(3)
        };
      });
      if (typeof console.table === "function") console.table(rows);
      else console.log(rows);
      return rows;
    }

    TILING.profile = {
      enable: function () { enabled = true; },
      disable: function () { enabled = false; },
      reset: function () { stats = Object.create(null); },
      report: report,
      time: time,
      bump: bump,
      isEnabled: function () { return enabled; }
    };
  })();
  var profile = TILING.profile;

  // ───────────────────────────────────────────────────────────────────
  // a5-js bridge
  // ───────────────────────────────────────────────────────────────────

  TILING.ensureA5 = function () {
    return window.A5Ready || Promise.reject(
      new Error("a5-js bridge not loaded — check inst/htmlwidgets/lib/a5-js")
    );
  };

  // ───────────────────────────────────────────────────────────────────
  // Pure helpers
  // ───────────────────────────────────────────────────────────────────

  TILING.bigintToHex = function (b) {
    return b.toString(16).padStart(16, "0");
  };

  // Boundary cache: cellToBoundary is the heaviest call we make. The
  // result is a function of cell id alone, so memoise page-lifetime.
  // Soft cap with FIFO drop of the oldest half on overflow.
  var BOUNDARY_CACHE = new Map();
  var BOUNDARY_CACHE_LIMIT = 20000;

  function cachedBoundary(A5, cellBigInt) {
    var b = BOUNDARY_CACHE.get(cellBigInt);
    if (b) return b;
    var t0 = profile.isEnabled() ? performance.now() : 0;
    b = A5.cellToBoundary(cellBigInt, { closedRing: false });
    if (profile.isEnabled()) profile.bump("cellToBoundary", performance.now() - t0);
    if (BOUNDARY_CACHE.size >= BOUNDARY_CACHE_LIMIT) {
      var i = 0, drop = BOUNDARY_CACHE_LIMIT >> 1;
      var it = BOUNDARY_CACHE.keys();
      var step = it.next();
      while (!step.done && i < drop) {
        BOUNDARY_CACHE.delete(step.value);
        step = it.next(); i++;
      }
    }
    BOUNDARY_CACHE.set(cellBigInt, b);
    return b;
  }
  TILING.cachedBoundary = cachedBoundary;
  TILING.boundaryCacheSize = function () { return BOUNDARY_CACHE.size; };

  // Compute the bbox of an array of [lon, lat] points and write it into
  // out[0..3] = [minLon, minLat, maxLon, maxLat].
  function boundaryToBboxInto(boundary, out, off) {
    var minLon = Infinity, maxLon = -Infinity;
    var minLat = Infinity, maxLat = -Infinity;
    for (var i = 0; i < boundary.length; i++) {
      var p = boundary[i];
      if (p[0] < minLon) minLon = p[0];
      if (p[0] > maxLon) maxLon = p[0];
      if (p[1] < minLat) minLat = p[1];
      if (p[1] > maxLat) maxLat = p[1];
    }
    out[off]     = minLon;
    out[off + 1] = minLat;
    out[off + 2] = maxLon;
    out[off + 3] = maxLat;
  }

  // Sanitise a viewport into a {west, south, east, north} bbox,
  // inflated by VIEWPORT_BUFFER on each side. Handles missing / NaN
  // bounds (globe view at low zoom) and Mercator zoom-0 wrap past
  // [-180, 180]. May legitimately return west > east (antimeridian).
  TILING.getViewportBbox = function (viewport) {
    var bounds = null;
    try {
      if (viewport && typeof viewport.getBounds === "function") {
        bounds = viewport.getBounds();
      }
    } catch (_) { bounds = null; }
    var ok = bounds && bounds.length === 4 &&
             Number.isFinite(bounds[0]) && Number.isFinite(bounds[1]) &&
             Number.isFinite(bounds[2]) && Number.isFinite(bounds[3]);
    var bbox;
    if (!ok) {
      bbox = { west: -180, south: -90, east: 180, north: 90 };
    } else if (bounds[2] - bounds[0] >= 360) {
      bbox = {
        west: -180, east: 180,
        south: Math.max(-90, bounds[1]),
        north: Math.min(90, bounds[3])
      };
    } else {
      bbox = {
        west: bounds[0], east: bounds[2],
        south: Math.max(-90, bounds[1]),
        north: Math.min(90, bounds[3])
      };
    }
    var lonSpan = (bbox.east >= bbox.west)
      ? (bbox.east - bbox.west)
      : (360 - (bbox.west - bbox.east));
    var lonBuf = lonSpan * TILING.VIEWPORT_BUFFER;
    var latBuf = (bbox.north - bbox.south) * TILING.VIEWPORT_BUFFER;
    var bSouth = Math.max(-90, bbox.south - latBuf);
    var bNorth = Math.min(90, bbox.north + latBuf);
    if ((lonSpan + 2 * lonBuf) >= 360) {
      return { west: -180, east: 180, south: bSouth, north: bNorth };
    }
    var bWest = bbox.west - lonBuf;
    var bEast = bbox.east + lonBuf;
    if (bWest < -180) bWest += 360;
    if (bEast > 180) bEast -= 360;
    return { west: bWest, east: bEast, south: bSouth, north: bNorth };
  };

  // Snap a bbox to a coarse grid so close-but-not-equal bboxes hash the
  // same. Quantum scales with span: a 5° pan over a 200°-wide view
  // hits the same cache cell, but a 5° pan over a 30°-wide view busts.
  function bboxKey(bbox) {
    var lonSpan = (bbox.east >= bbox.west)
      ? (bbox.east - bbox.west)
      : (360 - (bbox.west - bbox.east));
    var latSpan = bbox.north - bbox.south;
    var span = Math.max(lonSpan, latSpan, 1e-3);
    var q = span / TILING.BBOX_QUANTUM_DIVISOR;
    function snap(x) { return Math.round(x / q); }
    return snap(bbox.west) + "/" + snap(bbox.south) + "/" +
           snap(bbox.east) + "/" + snap(bbox.north) + "@" + q.toFixed(6);
  }
  TILING._bboxKey = bboxKey;

  // Test whether a per-cell bbox stored at out[off..off+3] intersects a
  // {west, south, east, north} query bbox. Antimeridian-crossing query
  // bboxes (west > east) are handled by splitting upstream.
  function bboxIntersects(bounds, off, q) {
    return !(bounds[off + 2] < q.west || bounds[off]     > q.east ||
             bounds[off + 3] < q.south || bounds[off + 1] > q.north);
  }

  // Map deck.gl viewport zoom (+ optional latitude compensation) to an
  // A5 LOD resolution by interpolating into TILING.ZOOM_TO_RES.
  TILING.getA5Resolution = function (viewport) {
    var z = viewport.zoom || 0;
    if (TILING.USE_LAT_COMPENSATION) {
      var lat = viewport.latitude || 0;
      z += Math.log(1 / Math.max(0.05, Math.cos(lat * Math.PI / 180)));
    }
    var t = TILING.ZOOM_TO_RES;
    if (!t || t.length === 0) return 0;
    if (t.length === 1) return Math.max(0, Math.floor(t[0][1]));
    if (z <= t[0][0]) {
      var slopeL = (t[1][1] - t[0][1]) / (t[1][0] - t[0][0] || 1);
      return Math.max(0, Math.floor(t[0][1] + slopeL * (z - t[0][0])));
    }
    for (var i = 1; i < t.length; i++) {
      if (z <= t[i][0]) {
        var z0 = t[i - 1][0], r0 = t[i - 1][1];
        var z1 = t[i][0],     r1 = t[i][1];
        var f = (z1 === z0) ? 0 : (z - z0) / (z1 - z0);
        return Math.max(0, Math.floor(r0 + f * (r1 - r0)));
      }
    }
    var n = t.length;
    var slopeR = (t[n - 1][1] - t[n - 2][1]) / (t[n - 1][0] - t[n - 2][0] || 1);
    return Math.max(0, Math.floor(t[n - 1][1] + slopeR * (z - t[n - 1][0])));
  };

  // Hierarchical descent from the 12 res-0 A5 cells: at each level,
  // keep cells whose lon/lat-extent intersects the query bbox and
  // expand them via cellToChildren until reaching `targetRes`.
  // Antimeridian: a query bbox with west > east is split around 180°
  // and the results unioned. Cells whose own boundary straddles the
  // antimeridian over-include slightly (extra tiles, not wrong tiles).
  TILING.bboxToCells = function (A5, bbox, targetRes) {
    return profile.time("bboxToCells", function () {
      if (bbox.west > bbox.east) {
        var leftHalf = TILING.bboxToCells(
          A5,
          { west: bbox.west, south: bbox.south, east: 180, north: bbox.north },
          targetRes
        );
        var rightHalf = TILING.bboxToCells(
          A5,
          { west: -180, south: bbox.south, east: bbox.east, north: bbox.north },
          targetRes
        );
        var seen = new Set();
        var out = [];
        for (var i = 0; i < leftHalf.length; i++) {
          var k = leftHalf[i];
          if (!seen.has(k)) { seen.add(k); out.push(k); }
        }
        for (var j = 0; j < rightHalf.length; j++) {
          var k2 = rightHalf[j];
          if (!seen.has(k2)) { seen.add(k2); out.push(k2); }
        }
        return out;
      }

      function intersects(boundary) {
        var minLon = Infinity, maxLon = -Infinity;
        var minLat = Infinity, maxLat = -Infinity;
        for (var i = 0; i < boundary.length; i++) {
          var p = boundary[i];
          if (p[0] < minLon) minLon = p[0];
          if (p[0] > maxLon) maxLon = p[0];
          if (p[1] < minLat) minLat = p[1];
          if (p[1] > maxLat) maxLat = p[1];
        }
        return !(maxLon < bbox.west || minLon > bbox.east ||
                 maxLat < bbox.south || minLat > bbox.north);
      }

      var frontier = Array.from(A5.getRes0Cells());
      var result = [];
      while (frontier.length > 0) {
        var next = [];
        for (var fi = 0; fi < frontier.length; fi++) {
          var cell = frontier[fi];
          var res = A5.getResolution(cell);
          var boundary = cachedBoundary(A5, cell);
          if (!intersects(boundary)) continue;
          if (res >= targetRes) {
            result.push(cell);
          } else {
            var children = A5.cellToChildren(cell);
            for (var ci = 0; ci < children.length; ci++) next.push(children[ci]);
          }
        }
        frontier = next;
      }
      return result;
    });
  };

  // Tileset2D subclass: deck.gl TileLayer queries it for which tiles
  // intersect the viewport. We drive the index list directly from the
  // tile-groups map (the keys are exactly the data-bearing tile ids).
  // This sidesteps the bboxToCells descent — at intermediate levels
  // the descent can prune a parent whose sampled-boundary bbox under-
  // covers its true extent, losing every leaf below it. Iterating the
  // known data tiles eliminates that failure mode.
  //
  // getTileGroups() is captured at construction so live updates land
  // automatically when the renderer rebuilds the class on version change.
  TILING.makeA5Tileset2DClass = function (A5, pinnedRes, getTileGroups) {
    var Base = (window.deck && (window.deck._Tileset2D || window.deck.Tileset2D));
    if (!Base) {
      throw new Error("deck.Tileset2D not found — required by A5Tileset2D");
    }
    var bboxByTile = new Map();
    function tileBbox(hex) {
      var b = bboxByTile.get(hex);
      if (b) return b;
      var cell = BigInt("0x" + hex);
      var boundary = cachedBoundary(A5, cell);
      var minLon = Infinity, maxLon = -Infinity;
      var minLat = Infinity, maxLat = -Infinity;
      for (var i = 0; i < boundary.length; i++) {
        var p = boundary[i];
        if (p[0] < minLon) minLon = p[0];
        if (p[0] > maxLon) maxLon = p[0];
        if (p[1] < minLat) minLat = p[1];
        if (p[1] > maxLat) maxLat = p[1];
      }
      b = [minLon, minLat, maxLon, maxLat];
      bboxByTile.set(hex, b);
      return b;
    }
    function intersectsHalf(b, w, e, s, n) {
      return !(b[2] < w || b[0] > e || b[3] < s || b[1] > n);
    }
    return class extends Base {
      constructor(opts) {
        super(opts);
        this._lastIndicesKey = null;
        this._lastIndices = null;
      }
      getTileIndices(opts) {
        var bbox = TILING.getViewportBbox(opts.viewport);
        var key = bboxKey(bbox);
        if (this._lastIndicesKey === key && this._lastIndices) {
          return this._lastIndices;
        }
        var groups = getTileGroups();
        var out = [];
        if (!groups || groups.size === 0) {
          this._lastIndicesKey = key;
          this._lastIndices = out;
          return out;
        }
        var crossDateline = bbox.west > bbox.east;
        groups.forEach(function (_indices, hex) {
          var b = tileBbox(hex);
          var hit;
          if (crossDateline) {
            hit = intersectsHalf(b, bbox.west, 180, bbox.south, bbox.north) ||
                  intersectsHalf(b, -180, bbox.east, bbox.south, bbox.north);
          } else {
            hit = intersectsHalf(b, bbox.west, bbox.east, bbox.south, bbox.north);
          }
          if (hit) out.push({ i: hex });
        });
        this._lastIndicesKey = key;
        this._lastIndices = out;
        return out;
      }
      getTileId(index) { return index.i; }
      getTileMetadata(index) {
        var b = tileBbox(index.i);
        return { bbox: { west: b[0], south: b[1], east: b[2], north: b[3] } };
      }
      getTileZoom(index) {
        return A5.getResolution(BigInt("0x" + index.i));
      }
      getParentIndex(index) {
        var parent = A5.cellToParent(BigInt("0x" + index.i));
        return { i: TILING.bigintToHex(parent) };
      }
    };
  };

  // Pick the LOD from a sorted-ascending schedule that best matches a
  // target resolution R. We pick the largest available LOD <= R, so
  // zooming past the data's leaf level keeps showing leaves rather than
  // jumping back to a coarser tier.
  TILING.pickLod = function (R, schedule) {
    if (!schedule || schedule.length === 0) return null;
    var lo = schedule[0];
    var hi = schedule[schedule.length - 1];
    if (R >= hi) return hi;
    if (R <= lo) return lo;
    var picked = lo;
    for (var i = 0; i < schedule.length; i++) {
      if (schedule[i] <= R) picked = schedule[i];
      else break;
    }
    return picked;
  };

  // ───────────────────────────────────────────────────────────────────
  // Stateful renderer factory
  // ───────────────────────────────────────────────────────────────────
  //
  // ctx (provided by a5view.js) exposes widget state and side effects:
  //
  //   getDataVersion()         bumps when widget data swaps
  //   getElId()                widget container id (for Shiny inputs)
  //   getOpacity()             current layer opacity
  //   getGlobe()               whether GlobeView is active
  //   getDrawMode()            polygon-draw mode active
  //   getPickable(x)           x.pickable && !drawMode
  //   onHover(pentagon|null)   widget-side hover state diff + Shiny
  //   onClick(pentagon)        widget-side click toggle + Shiny + redraw
  //
  TILING.createRenderer = function (ctx) {
    var lodGroups = null;        // Map<lod, Uint32Array of row indices>
    var lodGroupsVersion = -1;
    var bboxBounds = null;       // Float32Array(4n) per row
    var bboxBoundsVersion = -1;
    var lastVisibleKey = null;
    var lastVisible = null;
    // Tile groups: Map<lod, Map<tileIdHex, Uint32Array of row indices>>.
    // Built on demand for any LOD whose row count exceeds TILE_THRESHOLD.
    var tileGroupsByLod = new Map();
    var tileGroupsVersion = -1;
    // Cached per-tile polygon assembly: Map<lod-tileId, {data, ver}>.
    // Reused across re-renders of the same tile so deck.gl's TileLayer
    // gets stable sub-layer props.
    var tileSubLayerCache = new Map();
    // A5Tileset2D classes cached per LOD. The class closes over a
    // tile-groups getter, so on data swap we must rebuild — store
    // alongside the dataVersion to detect.
    var TilesetClassesByLod = new Map();

    // Partition row indices by `_lod` column. Returns Map<lod, indices>.
    function ensureLodGroups(x) {
      var ver = ctx.getDataVersion();
      if (lodGroupsVersion === ver && lodGroups) return lodGroups;
      return profile.time("ensureLodGroups", function () {
        var cols = x.data;
        var out = new Map();
        if (cols && cols.lods) {
          var lodCol = cols.lods;
          var lodVals = lodCol.data && lodCol.data[0] && lodCol.data[0].values;
          var n = cols.length;
          var counts = new Map();
          var i;
          if (lodVals) {
            for (i = 0; i < n; i++) {
              var v = lodVals[i];
              counts.set(v, (counts.get(v) || 0) + 1);
            }
          } else {
            for (i = 0; i < n; i++) {
              var v2 = lodCol.get(i);
              counts.set(v2, (counts.get(v2) || 0) + 1);
            }
          }
          var arrays = new Map();
          var cursors = new Map();
          counts.forEach(function (c, lod) {
            arrays.set(lod, new Uint32Array(c));
            cursors.set(lod, 0);
          });
          if (lodVals) {
            for (i = 0; i < n; i++) {
              var v3 = lodVals[i];
              var arr = arrays.get(v3);
              var p = cursors.get(v3);
              arr[p] = i;
              cursors.set(v3, p + 1);
            }
          } else {
            for (i = 0; i < n; i++) {
              var v4 = lodCol.get(i);
              var arr2 = arrays.get(v4);
              var p2 = cursors.get(v4);
              arr2[p2] = i;
              cursors.set(v4, p2 + 1);
            }
          }
          out = arrays;
        }
        lodGroups = out;
        lodGroupsVersion = ver;
        // All version-bound caches depend on this; clear.
        lastVisibleKey = null;
        lastVisible = null;
        tileGroupsByLod = new Map();
        tileSubLayerCache = new Map();
        return out;
      });
    }

    // For each LOD whose row count exceeds TILE_THRESHOLD, group its
    // row indices by parent A5 cell at (lod - TILE_OFFSET). Returns a
    // Map<tileIdHex, Uint32Array> for the requested LOD, or null if
    // the LOD is below threshold.
    function ensureTileGroupsForLod(x, lod) {
      var ver = ctx.getDataVersion();
      if (tileGroupsVersion !== ver) {
        tileGroupsByLod = new Map();
        tileGroupsVersion = ver;
      }
      var existing = tileGroupsByLod.get(lod);
      if (existing) return existing;
      var groups = ensureLodGroups(x);
      var indices = groups.get(lod);
      if (!indices || indices.length <= TILING.TILE_THRESHOLD) {
        tileGroupsByLod.set(lod, null);
        return null;
      }
      var A5 = window.A5;
      if (!A5) return null;
      var pinnedRes = lod - TILING.TILE_OFFSET;
      if (pinnedRes < 0) {
        tileGroupsByLod.set(lod, null);
        return null;
      }
      return profile.time("ensureTileGroups", function () {
        var pcol = x.data.pentagons;
        var counts = new Map();
        var keys = new Array(indices.length);
        var i;
        for (i = 0; i < indices.length; i++) {
          var rowIdx = indices[i];
          var cell = pcol.get(rowIdx);
          var parent = A5.cellToParent(cell, pinnedRes);
          var key = TILING.bigintToHex(parent);
          keys[i] = key;
          counts.set(key, (counts.get(key) || 0) + 1);
        }
        var arrays = new Map();
        var cursors = new Map();
        counts.forEach(function (c, k) {
          arrays.set(k, new Uint32Array(c));
          cursors.set(k, 0);
        });
        for (i = 0; i < indices.length; i++) {
          var k2 = keys[i];
          var arr = arrays.get(k2);
          var p = cursors.get(k2);
          arr[p] = indices[i];
          cursors.set(k2, p + 1);
        }
        tileGroupsByLod.set(lod, arrays);
        return arrays;
      });
    }

    // Compute (lazily) per-row bbox bounds. We pay the boundary cost
    // once per cell and store [minLon,minLat,maxLon,maxLat] in a flat
    // Float32Array. Subsequent viewport culls do four float compares
    // per row, no Map lookups, no boundary work.
    //
    // NOTE: this iterates all rows up front. For huge datasets (millions
    // of cells) we could compute lazily per visible cell, but the
    // boundary cache + Float32 storage is small and fast enough that
    // the upfront pass typically beats lazy fill for any view that
    // touches more than a handful of cells.
    function ensureBboxBounds(x) {
      var ver = ctx.getDataVersion();
      if (bboxBoundsVersion === ver && bboxBounds) return bboxBounds;
      var A5 = window.A5;
      if (!A5) return null;
      return profile.time("ensureBboxBounds", function () {
        var cols = x.data;
        var n = cols.length;
        var out = new Float32Array(n * 4);
        var pcol = cols.pentagons;
        for (var i = 0; i < n; i++) {
          var cell = pcol.get(i);
          var b = cachedBoundary(A5, cell);
          boundaryToBboxInto(b, out, i * 4);
        }
        bboxBounds = out;
        bboxBoundsVersion = ver;
        return out;
      });
    }

    // Walk a row-index array, fetch per-row bbox, push survivors. The
    // query bbox may straddle the antimeridian (west > east); split.
    function cullToVisible(indices, bounds, qBbox) {
      var visible;
      if (qBbox.west <= qBbox.east) {
        visible = [];
        for (var i = 0; i < indices.length; i++) {
          var idx = indices[i];
          if (bboxIntersects(bounds, idx * 4, qBbox)) visible.push(idx);
        }
        return visible;
      }
      var left  = { west: qBbox.west, east: 180,        south: qBbox.south, north: qBbox.north };
      var right = { west: -180,       east: qBbox.east, south: qBbox.south, north: qBbox.north };
      visible = [];
      for (var j = 0; j < indices.length; j++) {
        var idx2 = indices[j];
        var off = idx2 * 4;
        if (bboxIntersects(bounds, off, left) ||
            bboxIntersects(bounds, off, right)) visible.push(idx2);
      }
      return visible;
    }

    // Assemble per-row polygon entries from a row-index array, pulling
    // boundary geometry from the cache and fill/elevation from the
    // arrow columns. The returned shape is consumed by both the single
    // and tiled layer builders.
    function buildPolygonsForIndices(x, rowIndices) {
      var A5 = window.A5;
      var cols = x.data;
      var pcol = cols.pentagons;
      var fr = cols.fillR, fg = cols.fillG, fb = cols.fillB, fa = cols.fillA;
      var fillVals = (fr && fr.data && fr.data[0]) ? {
        r: fr.data[0].values,
        g: fg.data[0].values,
        b: fb.data[0].values,
        a: fa.data[0].values
      } : null;
      var elevCol = cols.elevation || null;
      var rows = new Array(rowIndices.length);
      for (var i = 0; i < rowIndices.length; i++) {
        var rowIdx = rowIndices[i];
        var cell = pcol.get(rowIdx);
        var entry = { polygon: cachedBoundary(A5, cell), pentagon: cell };
        if (fillVals) {
          entry.r = fillVals.r[rowIdx];
          entry.g = fillVals.g[rowIdx];
          entry.b = fillVals.b[rowIdx];
          entry.a = fillVals.a[rowIdx];
        } else if (fr) {
          entry.r = fr.get(rowIdx);
          entry.g = fg.get(rowIdx);
          entry.b = fb.get(rowIdx);
          entry.a = fa.get(rowIdx);
        }
        if (elevCol) entry.elevation = elevCol.get(rowIdx) || 0;
        rows[i] = entry;
      }
      return rows;
    }

    // Construct a SolidPolygonLayer (or PolygonLayer when stroke is
    // requested) from a row-entry array. updateKey pins
    // updateTriggers so deck.gl skips re-running accessors on no-op
    // rebuilds.
    function buildPolygonLayerFromRows(x, rows, layerId, updateKey) {
      var hasFill = x.fill_per_cell;
      var uniformFill = x.fill_color || [116, 172, 144, 255];
      var getFillColor = hasFill
        ? function (d) { return [d.r, d.g, d.b, d.a]; }
        : uniformFill;

      var props = {
        id: layerId,
        data: rows,
        getPolygon: function (d) { return d.polygon; },
        getFillColor: getFillColor,
        opacity: ctx.getOpacity(),
        pickable: ctx.getPickable(x),
        autoHighlight: false,
        extruded: x.extruded,
        elevationScale: x.elevation_scale,
        onHover: function (info) {
          ctx.onHover((info && info.object) ? info.object.pentagon : null);
        },
        onClick: function (info) {
          if (info && info.object) ctx.onClick(info.object.pentagon);
        },
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

    // Single-layer path for small LODs: viewport-cull the LOD's row
    // indices via cached bboxes, assemble polygons, return one layer.
    // Memoises (lod, bboxKey, dataVersion) so fine pans within a
    // quantum reuse the assembled polygons array.
    function buildSingleLayer(x, lod, indices, viewport) {
      var bounds = ensureBboxBounds(x);
      if (!bounds) return null;
      var bbox = TILING.getViewportBbox(viewport);
      var ver = ctx.getDataVersion();
      var key = lod + "|" + bboxKey(bbox) + "|" + ver;

      var layerData;
      if (lastVisibleKey === key && lastVisible) {
        layerData = lastVisible;
      } else {
        layerData = profile.time("buildLodLayerData", function () {
          var visible = cullToVisible(indices, bounds, bbox);
          return buildPolygonsForIndices(x, visible);
        });
        lastVisibleKey = key;
        lastVisible = layerData;
      }
      if (layerData.length === 0) return null;
      return buildPolygonLayerFromRows(x, layerData, "a5-lod", key);
    }

    // Tiled path for big LODs: route through deck.gl TileLayer with
    // an A5-aware Tileset2D. deck.gl caches per-tile sub-layers across
    // viewport changes, so panning within already-loaded tiles
    // doesn't rebuild any GPU buffers.
    function buildTiledLayer(x, lod, tileGroups) {
      var pinnedRes = lod - TILING.TILE_OFFSET;
      var ver = ctx.getDataVersion();
      var entry = TilesetClassesByLod.get(lod);
      var TilesetClass;
      if (entry && entry.ver === ver) {
        TilesetClass = entry.cls;
      } else {
        TilesetClass = TILING.makeA5Tileset2DClass(
          window.A5,
          pinnedRes,
          function () { return tileGroups; }
        );
        TilesetClassesByLod.set(lod, { cls: TilesetClass, ver: ver });
      }
      return new window.deck.TileLayer({
        // Bake dataVersion into the id so a data swap forces a fresh
        // TileLayer (re-fetches all tiles via getTileData).
        id: "a5-tiles-lod" + lod + "-v" + ver,
        data: [],
        TilesetClass: TilesetClass,
        // World extent so deck.gl doesn't frustum-cull tiles whose
        // bbox sits at the edge of the viewport.
        extent: [-180, -85.05, 180, 85.05],
        getTileData: function (props) {
          var key = props.index ? props.index.i : null;
          var tileIndices = (key && tileGroups.get(key)) || null;
          return (tileIndices && tileIndices.length > 0) ? tileIndices : null;
        },
        renderSubLayers: function (props) {
          var tileIndices = props.data;
          if (!tileIndices || tileIndices.length === 0) return null;
          var tileId = props.tile.index.i;
          var cacheKey = lod + "|" + tileId;
          var cached = tileSubLayerCache.get(cacheKey);
          var rows;
          if (cached && cached.ver === ver && cached.indices === tileIndices) {
            rows = cached.rows;
          } else {
            rows = profile.time("buildTilePolys", function () {
              return buildPolygonsForIndices(x, tileIndices);
            });
            tileSubLayerCache.set(cacheKey, {
              rows: rows, ver: ver, indices: tileIndices
            });
          }
          return buildPolygonLayerFromRows(x, rows, "a5-tile-" + tileId, ver);
        }
      });
    }

    // Pick the LOD for the current viewport, then dispatch to the
    // tiled path (big LOD with sub-layer caching) or single-layer
    // path (small LOD, one polygon layer).
    function buildLodLayer(x, viewport) {
      if (!x.data || !window.A5) return null;
      var schedule = x.lod_resolutions || null;
      if (!schedule || schedule.length === 0) return null;
      var R = TILING.getA5Resolution(viewport);
      var lod = TILING.pickLod(R, schedule);
      if (lod == null) return null;
      var groups = ensureLodGroups(x);
      var indices = groups.get(lod);
      if (!indices || indices.length === 0) return null;

      var tileGroups = ensureTileGroupsForLod(x, lod);
      if (tileGroups) {
        return buildTiledLayer(x, lod, tileGroups);
      }
      return buildSingleLayer(x, lod, indices, viewport);
    }

    return { buildLodLayer: buildLodLayer };
  };
})();
