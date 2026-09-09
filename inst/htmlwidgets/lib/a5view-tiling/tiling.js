// =====================================================================
// a5view tiling helpers
// =====================================================================
// Shared, stateless helpers used by the lazy parquet renderer
// (lazy-renderer.js) and the widget (a5view.js):
//
//   - zoom -> A5 resolution mapping (ZOOM_TO_RES) and LOD snapping
//   - viewport -> lon/lat bbox sanitisation (antimeridian aware)
//   - a page-lifetime cache of cell boundaries (cellToBoundary is the
//     heaviest call we make, and the geometry never changes)
//
// Tunable knobs live on window.A5View.tiling.* so they can be tweaked
// from the browser console. Set window.A5View.DEBUG = true to enable
// diagnostic logging and the zoom/resolution readout.
// =====================================================================
(function () {
  var T = window.A5View = window.A5View || {};
  var TILING = T.tiling = T.tiling || {};

  T.DEBUG = !!T.DEBUG;
  T.log = function () {
    if (T.DEBUG && typeof console !== "undefined") {
      console.log.apply(console, arguments);
    }
  };

  // ───────────────────────────────────────────────────────────────────
  // LEVERS
  // ───────────────────────────────────────────────────────────────────

  // (zoom, R) anchor table; zoom -> A5 resolution is linearly
  // interpolated between anchors and extrapolated beyond them.
  TILING.ZOOM_TO_RES = [
    [0, 7],
    [3, 10],
    [7, 14],
    [10, 15],
    [16, 22]];

  // High latitudes get a small zoom boost (Mercator pixels cover less
  // ground there).
  TILING.USE_LAT_COMPENSATION = true;

  // Query-bbox inflation around the visible area (fraction per side).
  TILING.VIEWPORT_BUFFER = 0.1;

  // Floor on time between layer rebuilds during continuous pan/zoom.
  TILING.REBUILD_THROTTLE_MS = 32;

  // Memoisation grid for viewport keys.
  TILING.BBOX_QUANTUM_DIVISOR = 32;

  // Minimum time (ms) the previous LOD's layer stays in the scene
  // after a LOD change. Forces a visible crossover even when decodes
  // are fast. <100 is invisible; >300 feels laggy.
  TILING.MIN_HOLD_MS = 1000;

  // ───────────────────────────────────────────────────────────────────
  // a5-js bridge
  // ───────────────────────────────────────────────────────────────────

  TILING.ensureA5 = function () {
    return window.A5Ready || Promise.reject(
      new Error("a5-js bridge not loaded: check inst/htmlwidgets/lib/a5-js")
    );
  };

  // ───────────────────────────────────────────────────────────────────
  // Pure helpers
  // ───────────────────────────────────────────────────────────────────

  TILING.bigintToHex = function (b) {
    return b.toString(16).padStart(16, "0");
  };

  // Boundary cache keyed by cell BigInt. Soft cap with FIFO drop of
  // the oldest half on overflow.
  var BOUNDARY_CACHE = new Map();
  var BOUNDARY_CACHE_LIMIT = 20000;

  TILING.cachedBoundary = function (A5, cellBigInt) {
    var b = BOUNDARY_CACHE.get(cellBigInt);
    if (b) return b;
    b = A5.cellToBoundary(cellBigInt, { closedRing: false });
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
  };
  TILING.boundaryCacheSize = function () { return BOUNDARY_CACHE.size; };

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
  TILING.bboxKey = function (bbox) {
    var lonSpan = (bbox.east >= bbox.west)
      ? (bbox.east - bbox.west)
      : (360 - (bbox.west - bbox.east));
    var latSpan = bbox.north - bbox.south;
    var span = Math.max(lonSpan, latSpan, 1e-3);
    var q = span / TILING.BBOX_QUANTUM_DIVISOR;
    function snap(x) { return Math.round(x / q); }
    return snap(bbox.west) + "/" + snap(bbox.south) + "/" +
           snap(bbox.east) + "/" + snap(bbox.north) + "@" + q.toFixed(6);
  };

  // Map viewport zoom (+ optional latitude compensation) to an A5
  // resolution by interpolating into TILING.ZOOM_TO_RES.
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

  // Pick the LOD from a sorted-ascending schedule that best matches a
  // target resolution R: the largest available LOD <= R, so zooming
  // past the data's leaf level keeps showing leaves.
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
})();
