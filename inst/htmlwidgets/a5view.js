HTMLWidgets.widget({
  name: "a5view",
  type: "output",

  factory: function(el, width, height) {
    var T = window.A5View;
    var TILING = T.tiling;
    var ACCENT = "#74ac90";
    var FONT = "'Inter',system-ui,-apple-system,sans-serif";

    // --- Widget state -------------------------------------------------
    var map = null;           // maplibregl.Map
    var overlay = null;       // deck.MapboxOverlay
    var lastPayload = null;   // most recent x (mutated in place on update)
    var currentBasemap = null;
    var currentOpacity = 0.6;
    var currentGlobe = false;
    var labelLayerId = null;  // first symbol layer of the active style
    var hovered = null;          // { cell, hex, row } under the cursor
    var hoveredPentagon = null;  // hovered.cell (BigInt) or null
    var clickedPentagon = null;
    var legendEl = null;
    var debugEl = null;
    var tipEl = null;

    // Polygon-draw mode state. The widget only handles the drawing UX
    // and emits WKT to Shiny on completion; visualising the resulting
    // cell selection is the R caller's responsibility.
    var draw = {
      enabled: false,     // feature switched on by the R side
      mode: false,        // drawing currently active
      vertices: [],       // [[lon, lat], ...]
      cursor: null,       // [lon, lat] live preview
      clickTimer: null,   // pending single-click vertex add
      committed: false,   // last polygon completed; held on screen
      btn: null
    };
    var DRAW_DBLCLICK_MS = 280;

    // --- Basemaps -----------------------------------------------------
    // Light / dark / OSM are OpenFreeMap vector styles (no API key, no
    // usage limits, OpenMapTiles schema over OpenStreetMap data).
    // Satellite is Esri World Imagery as a raster style.
    function rasterStyle(tiles, attribution) {
      return {
        version: 8,
        sources: {
          basemap: {
            type: "raster", tiles: tiles, tileSize: 256, maxzoom: 19,
            attribution: attribution
          }
        },
        layers: [{ id: "basemap", type: "raster", source: "basemap" }]
      };
    }
    var BASEMAPS = {
      dark: {
        label: "Dark", swatch: "#2c2c2c", bg: "#1b1b1b",
        style: "https://tiles.openfreemap.org/styles/dark"
      },
      light: {
        label: "Light", swatch: "#e8e8e8", bg: "#f0f0f0",
        style: "https://tiles.openfreemap.org/styles/positron"
      },
      osm: {
        label: "OSM", swatch: "#d4cfc5", bg: "#e8e0d8",
        style: "https://tiles.openfreemap.org/styles/liberty"
      },
      satellite: {
        label: "Satellite", swatch: "#2a4a2e", bg: "#1a2e1a",
        style: rasterStyle(
          ["https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"],
          "© Esri"
        )
      },
      none: {
        label: "None", swatch: "#111111", bg: "#111111",
        style: { version: 8, sources: {}, layers: [] }
      }
    };

    // --- Icons --------------------------------------------------------
    var LAYERS_SVG =
      '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmlns="http://www.w3.org/2000/svg">' +
        '<path d="M8 1L1 5.5L8 10L15 5.5L8 1Z" fill="currentColor" opacity="0.9"/>' +
        '<path d="M1 8L8 12.5L15 8" stroke="currentColor" stroke-width="1.3" fill="none" opacity="0.6"/>' +
        '<path d="M1 10.5L8 15L15 10.5" stroke="currentColor" stroke-width="1.3" fill="none" opacity="0.35"/>' +
      '</svg>';
    var OPACITY_SVG =
      '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmlns="http://www.w3.org/2000/svg">' +
        '<circle cx="8" cy="8" r="6.5" stroke="currentColor" stroke-width="1.3" opacity="0.9"/>' +
        '<path d="M8 1.5A6.5 6.5 0 0 0 8 14.5Z" fill="currentColor" opacity="0.5"/>' +
      '</svg>';
    var DRAW_SVG =
      '<svg width="16" height="16" viewBox="0 0 16 16" fill="none" xmlns="http://www.w3.org/2000/svg">' +
        '<path d="M3 12L8 3L13 12L3 12Z" stroke="currentColor" stroke-width="1.3" fill="none" stroke-linejoin="round"/>' +
        '<circle cx="3" cy="12" r="1.6" fill="currentColor"/>' +
        '<circle cx="8" cy="3" r="1.6" fill="currentColor"/>' +
        '<circle cx="13" cy="12" r="1.6" fill="currentColor"/>' +
      '</svg>';

    // --- Styles -------------------------------------------------------
    function injectStyles() {
      if (document.querySelector("style[data-a5view]")) return;
      var style = document.createElement("style");
      style.setAttribute("data-a5view", "");
      style.textContent =
        ".a5v-panel{" +
          "background:rgba(20,20,20,0.72);" +
          "backdrop-filter:blur(12px);-webkit-backdrop-filter:blur(12px);" +
          "border-radius:10px;box-shadow:0 4px 20px rgba(0,0,0,0.4);" +
          "font-family:" + FONT + ";font-size:11px;color:#bbb;" +
          "user-select:none;overflow:hidden;" +
        "}" +
        ".a5v-toolbar{" +
          "position:absolute;top:12px;left:12px;z-index:2;" +
          "display:flex;gap:6px;align-items:flex-start;" +
        "}" +
        ".a5v-ctrl{position:relative;}" +
        ".a5v-toggle{" +
          "width:32px;height:32px;border-radius:8px;border:none;cursor:pointer;" +
          "background:rgba(20,20,20,0.72);color:#ccc;padding:0;" +
          "backdrop-filter:blur(8px);-webkit-backdrop-filter:blur(8px);" +
          "display:flex;align-items:center;justify-content:center;" +
          "transition:background 0.2s,color 0.2s;" +
          "box-shadow:0 2px 8px rgba(0,0,0,0.3);" +
        "}" +
        ".a5v-toggle:hover{background:rgba(30,30,30,0.85);color:" + ACCENT + ";}" +
        ".a5v-toggle.open{color:" + ACCENT + ";background:rgba(20,20,20,0.85);}" +
        ".a5v-drop{" +
          "position:absolute;top:40px;left:0;" +
          "opacity:0;transform:translateY(-8px) scale(0.95);" +
          "transition:opacity 0.2s ease,transform 0.2s ease;" +
          "pointer-events:none;" +
        "}" +
        ".a5v-drop.open{opacity:1;transform:translateY(0) scale(1);pointer-events:auto;}" +
        ".a5v-opt{" +
          "display:flex;align-items:center;gap:8px;" +
          "padding:8px 14px 8px 10px;cursor:pointer;border:none;width:100%;" +
          "background:transparent;color:#aaa;text-align:left;" +
          "transition:background 0.15s,color 0.15s;" +
          "position:relative;font-size:11px;font-family:inherit;" +
          "letter-spacing:0.3px;white-space:nowrap;" +
        "}" +
        ".a5v-opt:hover{background:rgba(255,255,255,0.06);color:#ddd;}" +
        ".a5v-opt.active{color:#fff;}" +
        ".a5v-opt.active::before{" +
          "content:'';position:absolute;left:0;top:4px;bottom:4px;width:3px;" +
          "border-radius:0 2px 2px 0;background:" + ACCENT + ";" +
        "}" +
        ".a5v-swatch{" +
          "width:14px;height:14px;border-radius:4px;flex-shrink:0;" +
          "border:1.5px solid rgba(255,255,255,0.15);" +
        "}" +
        ".a5v-opt.active .a5v-swatch{border-color:" + ACCENT + ";}" +
        ".a5v-slider-panel{padding:10px 14px;min-width:140px;}" +
        ".a5v-slider-label{" +
          "display:flex;justify-content:space-between;align-items:center;" +
          "color:#aaa;font-size:10px;letter-spacing:0.3px;margin-bottom:8px;" +
        "}" +
        ".a5v-slider-val{color:" + ACCENT + ";font-variant-numeric:tabular-nums;}" +
        ".a5v-range{" +
          "-webkit-appearance:none;appearance:none;width:100%;height:4px;" +
          "border-radius:2px;outline:none;cursor:pointer;margin:0;" +
          "background:linear-gradient(to right," + ACCENT + " var(--pct),rgba(255,255,255,0.15) var(--pct));" +
        "}" +
        ".a5v-range::-webkit-slider-thumb{" +
          "-webkit-appearance:none;width:14px;height:14px;border-radius:50%;" +
          "background:" + ACCENT + ";border:2px solid rgba(20,20,20,0.8);" +
          "box-shadow:0 1px 4px rgba(0,0,0,0.3);transition:transform 0.15s;" +
        "}" +
        ".a5v-range::-webkit-slider-thumb:hover{transform:scale(1.2);}" +
        ".a5v-range::-moz-range-thumb{" +
          "width:14px;height:14px;border-radius:50%;" +
          "background:" + ACCENT + ";border:2px solid rgba(20,20,20,0.8);" +
          "box-shadow:0 1px 4px rgba(0,0,0,0.3);" +
        "}" +
        ".a5v-range::-moz-range-track{height:4px;border-radius:2px;border:none;background:rgba(255,255,255,0.15);}" +
        ".a5v-range::-moz-range-progress{height:4px;border-radius:2px;background:" + ACCENT + ";}" +
        // Legend (bottom-left)
        ".a5v-legend{" +
          "position:absolute;bottom:12px;left:12px;z-index:2;" +
          "padding:9px 12px 8px;min-width:150px;max-width:240px;" +
        "}" +
        ".a5v-legend-title{" +
          "color:#ddd;font-size:10.5px;letter-spacing:0.3px;margin-bottom:7px;" +
          "white-space:nowrap;overflow:hidden;text-overflow:ellipsis;" +
        "}" +
        ".a5v-legend-bar{height:8px;border-radius:4px;border:1px solid rgba(255,255,255,0.12);}" +
        ".a5v-legend-ticks{" +
          "display:flex;justify-content:space-between;margin-top:5px;" +
          "font-size:10px;color:#aaa;font-variant-numeric:tabular-nums;" +
        "}" +
        // Debug readout (opt-in via A5View.DEBUG)
        ".a5v-debug{" +
          "position:absolute;top:12px;right:52px;z-index:2;padding:5px 9px;" +
          "color:#bbb;font-family:" + FONT + ";font-size:10px;letter-spacing:0.3px;" +
          "font-variant-numeric:tabular-nums;pointer-events:none;" +
        "}" +
        // Tooltip (positioned from MapLibre pointer events)
        ".a5v-tip{" +
          "position:absolute;z-index:3;pointer-events:none;" +
          "background:rgba(20,20,20,0.85);backdrop-filter:blur(8px);-webkit-backdrop-filter:blur(8px);" +
          "color:#ddd;padding:6px 10px;border-radius:8px;box-shadow:0 4px 16px rgba(0,0,0,0.4);" +
          "font-family:" + FONT + ";font-size:11px;line-height:1.5;white-space:nowrap;" +
        "}" +
        ".a5v-tip-id{font-family:ui-monospace,Menlo,monospace;color:#fff;letter-spacing:0.3px;}" +
        ".a5v-tip-row{display:flex;gap:10px;justify-content:space-between;color:#ccc;}" +
        ".a5v-tip-row span:first-child{color:#8a8a8a;}" +
        ".a5v-tip-row span:last-child{color:" + ACCENT + ";font-variant-numeric:tabular-nums;}" +
        // MapLibre controls, glass style to match the panels
        ".a5view .maplibregl-ctrl-group{" +
          "background:rgba(20,20,20,0.72) !important;" +
          "backdrop-filter:blur(12px);-webkit-backdrop-filter:blur(12px);" +
          "border-radius:8px !important;box-shadow:0 2px 8px rgba(0,0,0,0.3) !important;" +
          "border:none !important;overflow:hidden;" +
        "}" +
        ".a5view .maplibregl-ctrl-group button{" +
          "width:32px !important;height:32px !important;background:transparent !important;" +
          "border:none !important;border-bottom:1px solid rgba(255,255,255,0.06) !important;" +
          "cursor:pointer;transition:background 0.15s;" +
        "}" +
        ".a5view .maplibregl-ctrl-group button:last-child{border-bottom:none !important;}" +
        ".a5view .maplibregl-ctrl-group button:hover{background:rgba(255,255,255,0.08) !important;}" +
        ".a5view .maplibregl-ctrl-group button .maplibregl-ctrl-icon{filter:invert(0.7);}" +
        ".a5view .maplibregl-ctrl-group button:hover .maplibregl-ctrl-icon{" +
          "filter:invert(0.55) sepia(1) saturate(2) hue-rotate(100deg) brightness(1.1);" +
        "}" +
        ".a5view .maplibregl-ctrl-scale{" +
          "background:rgba(20,20,20,0.5) !important;color:#ccc !important;" +
          "border-color:rgba(255,255,255,0.35) !important;font-family:" + FONT + ";" +
          "font-size:10px !important;" +
        "}" +
        ".a5view .maplibregl-ctrl-attrib{" +
          "background:rgba(0,0,0,0.4) !important;backdrop-filter:blur(8px);" +
          "border-radius:4px;font-size:9px !important;font-family:" + FONT + ";" +
        "}" +
        ".a5view .maplibregl-ctrl-attrib.maplibregl-compact-show{" +
          "padding:4px 28px 4px 8px !important;background:rgba(20,20,20,0.85) !important;color:#aaa !important;" +
        "}" +
        ".a5view .maplibregl-ctrl-attrib a{color:#ddd !important;}" +
        ".a5view .maplibregl-ctrl-attrib-button{filter:invert(0.7);}";
      document.head.appendChild(style);
    }

    // --- Small helpers ------------------------------------------------
    function shinyInput(suffix, value, isEvent) {
      if (typeof Shiny === "undefined" || !Shiny.setInputValue) return;
      Shiny.setInputValue(el.id + suffix, value, isEvent ? { priority: "event" } : undefined);
    }

    function pentToHex(p) {
      if (p == null) return null;
      if (typeof p === "bigint") return TILING.bigintToHex(p);
      return String(p);
    }

    // Resolve a [lon, lat] coordinate to the cell under the cursor,
    // but only if that cell is in the data: the in-memory path looks
    // the leaf cell up in the Arrow table, the pyramid path looks the
    // cell at the currently rendered LOD up among decoded rows. So
    // hovering empty map gives nothing, and what you hover is what
    // you see. Returns { cell, hex, row } or null.
    function resolveHover(coord) {
      if (!coord || !window.A5 || !lastPayload) return null;
      var x = lastPayload;
      var res = x.parquet_b64 ? lazyRenderer.currentLod() : x.data_resolution;
      if (res == null) return null;
      var cell = window.A5.lonLatToCell([coord[0], coord[1]], res);
      var hex = pentToHex(cell);
      var row = x.parquet_b64 ? lazyRenderer.findRow(res, hex) : findLegacyRow(hex);
      return row ? { cell: cell, hex: hex, row: row } : null;
    }

    function fmtNumber(v) {
      if (typeof v !== "number") return String(v);
      if (!Number.isFinite(v)) return String(v);
      var a = Math.abs(v);
      if (a !== 0 && (a >= 1e6 || a < 1e-3)) return v.toExponential(2);
      return v.toLocaleString(undefined, { maximumSignificantDigits: 5 });
    }

    function fmtValue(v) {
      if (v == null) return "NA";
      if (typeof v === "number") return fmtNumber(v);
      if (typeof v === "bigint") return v.toString();
      return String(v);
    }

    function escapeHtml(s) {
      return String(s).replace(/[&<>"]/g, function (c) {
        return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c];
      });
    }

    // --- Redraw plumbing ----------------------------------------------
    // Throttled (leading + trailing) so continuous pan/zoom rebuilds at
    // most once per REBUILD_THROTTLE_MS but the first change is instant.
    var rebuildTimer = null;
    var lastRebuildAt = 0;
    function now() {
      return (typeof performance !== "undefined") ? performance.now() : Date.now();
    }
    function redraw() {
      lastRebuildAt = now();
      if (overlay && lastPayload) {
        overlay.setProps({ layers: buildLayers(lastPayload) });
      }
    }
    function scheduleRedraw() {
      var throttle = TILING.REBUILD_THROTTLE_MS;
      var since = now() - lastRebuildAt;
      if (since >= throttle) {
        if (rebuildTimer) { clearTimeout(rebuildTimer); rebuildTimer = null; }
        redraw();
      } else if (!rebuildTimer) {
        rebuildTimer = setTimeout(function () {
          rebuildTimer = null;
          redraw();
        }, throttle - since);
      }
    }

    // --- Controls -----------------------------------------------------
    var openDropdowns = [];
    function closeDropdowns() {
      openDropdowns.forEach(function (d) { d.close(); });
    }
    document.addEventListener("click", closeDropdowns);

    function makeDropdown(iconSvg, title) {
      var ctrl = document.createElement("div");
      ctrl.className = "a5v-ctrl";
      var toggle = document.createElement("button");
      toggle.className = "a5v-toggle";
      toggle.innerHTML = iconSvg;
      toggle.title = title;
      var drop = document.createElement("div");
      drop.className = "a5v-drop a5v-panel";
      var isOpen = false;
      function close() {
        if (!isOpen) return;
        isOpen = false;
        drop.classList.remove("open");
        toggle.classList.remove("open");
      }
      toggle.addEventListener("click", function (e) {
        e.stopPropagation();
        var wasOpen = isOpen;
        closeDropdowns();
        isOpen = !wasOpen;
        drop.classList.toggle("open", isOpen);
        toggle.classList.toggle("open", isOpen);
      });
      ctrl.appendChild(toggle);
      ctrl.appendChild(drop);
      var api = { el: ctrl, drop: drop, close: close };
      openDropdowns.push(api);
      return api;
    }

    function buildControls(basemaps) {
      var existing = el.querySelector(".a5v-toolbar");
      if (existing) existing.remove();
      openDropdowns = [];

      var toolbar = document.createElement("div");
      toolbar.className = "a5v-toolbar";

      // Basemap selector
      if (basemaps.length > 1) {
        var bm = makeDropdown(LAYERS_SVG, "Basemap");
        basemaps.forEach(function (key) {
          var info = BASEMAPS[key];
          if (!info) return;
          var opt = document.createElement("button");
          opt.className = "a5v-opt" + (key === currentBasemap ? " active" : "");
          opt.dataset.basemap = key;
          var swatch = document.createElement("span");
          swatch.className = "a5v-swatch";
          swatch.style.background = info.swatch;
          var label = document.createElement("span");
          label.textContent = info.label;
          opt.appendChild(swatch);
          opt.appendChild(label);
          opt.addEventListener("click", function (e) {
            e.stopPropagation();
            setBasemap(key);
            bm.close();
          });
          bm.drop.appendChild(opt);
        });
        toolbar.appendChild(bm.el);
      }

      // Opacity slider
      var op = makeDropdown(OPACITY_SVG, "Opacity");
      var panel = document.createElement("div");
      panel.className = "a5v-slider-panel";
      var labelRow = document.createElement("div");
      labelRow.className = "a5v-slider-label";
      var labelText = document.createElement("span");
      labelText.textContent = "Opacity";
      var labelVal = document.createElement("span");
      labelVal.className = "a5v-slider-val";
      labelVal.textContent = Math.round(currentOpacity * 100) + "%";
      labelRow.appendChild(labelText);
      labelRow.appendChild(labelVal);
      var slider = document.createElement("input");
      slider.type = "range";
      slider.className = "a5v-range";
      slider.min = "0";
      slider.max = "100";
      slider.value = String(Math.round(currentOpacity * 100));
      slider.style.setProperty("--pct", slider.value + "%");
      slider.addEventListener("input", function (e) {
        e.stopPropagation();
        var val = parseInt(slider.value, 10);
        currentOpacity = val / 100;
        labelVal.textContent = val + "%";
        slider.style.setProperty("--pct", val + "%");
        scheduleRedraw();
      });
      panel.appendChild(labelRow);
      panel.appendChild(slider);
      op.drop.appendChild(panel);
      toolbar.appendChild(op.el);

      // Draw polygon toggle
      if (draw.enabled) {
        var drawCtrl = document.createElement("div");
        drawCtrl.className = "a5v-ctrl";
        var drawBtn = document.createElement("button");
        drawBtn.className = "a5v-toggle" + (draw.mode ? " open" : "");
        drawBtn.innerHTML = DRAW_SVG;
        drawBtn.title = "Draw polygon (click to place vertices, double-click to finish, Esc to cancel)";
        drawBtn.addEventListener("click", function (e) {
          e.stopPropagation();
          setDrawMode(!draw.mode);
        });
        drawCtrl.appendChild(drawBtn);
        toolbar.appendChild(drawCtrl);
        draw.btn = drawBtn;
      }

      // Keep pointer events on the toolbar from reaching the map
      ["mousedown", "touchstart", "dblclick", "wheel"].forEach(function (evt) {
        toolbar.addEventListener(evt, function (e) { e.stopPropagation(); });
      });

      el.appendChild(toolbar);
    }

    // Legend for numeric fills: palette bar + domain ticks + label.
    function updateLegend(x) {
      var legend = x.legend;
      if (!legend || !legend.colors || !legend.domain) {
        if (legendEl) { legendEl.remove(); legendEl = null; }
        return;
      }
      if (!legendEl) {
        legendEl = document.createElement("div");
        legendEl.className = "a5v-legend a5v-panel";
        el.appendChild(legendEl);
      }
      var colors = legend.colors;
      var stops = [];
      var n = Math.min(colors.length, 32);
      for (var i = 0; i < n; i++) {
        var c = colors[Math.round(i * (colors.length - 1) / Math.max(1, n - 1))];
        stops.push(c.length === 9 ? c.slice(0, 7) : c);
      }
      var lo = legend.domain[0], hi = legend.domain[1];
      legendEl.innerHTML =
        '<div class="a5v-legend-title" title="' + escapeHtml(legend.label || "") + '">' +
          escapeHtml(legend.label || "value") + "</div>" +
        '<div class="a5v-legend-bar" style="background:linear-gradient(to right,' + stops.join(",") + ')"></div>' +
        '<div class="a5v-legend-ticks"><span>' + fmtNumber(lo) + "</span><span>" +
          fmtNumber(hi) + "</span></div>";
    }

    // Zoom / A5 resolution readout, only when A5View.DEBUG is set.
    function updateDebug() {
      if (!T.DEBUG || !map) {
        if (debugEl) { debugEl.remove(); debugEl = null; }
        return;
      }
      if (!debugEl) {
        debugEl = document.createElement("div");
        debugEl.className = "a5v-debug";
        el.appendChild(debugEl);
      }
      var vp = getCurrentViewport();
      debugEl.textContent = "zoom " + vp.zoom.toFixed(2) +
        "  R " + TILING.getA5Resolution(vp) + "  lat " + vp.latitude.toFixed(1) + "°";
    }

    // --- Basemap ------------------------------------------------------
    function setBasemap(key) {
      if (!BASEMAPS[key]) key = "none";
      currentBasemap = key;
      el.style.background = BASEMAPS[key].bg;
      el.querySelectorAll(".a5v-opt").forEach(function (opt) {
        opt.classList.toggle("active", opt.dataset.basemap === key);
      });
      if (map) {
        labelLayerId = null;
        map.setStyle(BASEMAPS[key].style);
      }
    }

    function applyProjection() {
      if (!map || !map.setProjection) return;
      try {
        map.setProjection({ type: currentGlobe ? "globe" : "mercator" });
      } catch (e) {
        console.warn("[a5view] setProjection failed:", e);
      }
    }

    // Runs on every style load (initial + each basemap switch). Finds
    // the first label layer so cells can be inserted beneath it, and
    // re-applies projection + deck layers, which don't survive a
    // style swap.
    function onStyleLoad() {
      var style = map.getStyle();
      labelLayerId = null;
      if (style && style.layers) {
        for (var i = 0; i < style.layers.length; i++) {
          if (style.layers[i].type === "symbol") {
            labelLayerId = style.layers[i].id;
            break;
          }
        }
      }
      applyProjection();
      redraw();
    }

    // --- Draw mode ----------------------------------------------------
    function resetDrawGeometry() {
      draw.vertices = [];
      draw.cursor = null;
      draw.committed = false;
      if (draw.clickTimer) { clearTimeout(draw.clickTimer); draw.clickTimer = null; }
    }

    function setDrawMode(on) {
      draw.mode = !!on;
      if (draw.btn) draw.btn.classList.toggle("open", draw.mode);
      if (map) {
        map.getCanvas().style.cursor = draw.mode ? "crosshair" : "";
        // Double-click closes the polygon while drawing; don't zoom.
        if (draw.mode) map.doubleClickZoom.disable(); else map.doubleClickZoom.enable();
        // Focus so the Esc keydown listener fires without a prior click.
        if (draw.mode) {
          try { map.getCanvas().focus({ preventScroll: true }); } catch (_) {}
        }
      }
      resetDrawGeometry();
      if (draw.mode) {
        hovered = null;
        hoveredPentagon = null;
        clickedPentagon = null;
        updateTooltip(null);
      }
      redraw();
    }

    function completeDrawnPolygon() {
      if (draw.vertices.length < 3) {
        resetDrawGeometry();
        scheduleRedraw();
        return;
      }
      var ring = draw.vertices.slice();
      var first = ring[0], last = ring[ring.length - 1];
      if (first[0] !== last[0] || first[1] !== last[1]) ring.push([first[0], first[1]]);
      var wkt = "POLYGON((" + ring.map(function (p) { return p[0] + " " + p[1]; }).join(", ") + "))";
      shinyInput("_polygon_draw", wkt, true);
      // Hold the completed polygon on screen until the user toggles
      // draw mode off, hits Esc, or starts a new polygon.
      draw.cursor = null;
      draw.committed = true;
      scheduleRedraw();
    }

    function handleDrawClick(coordinate) {
      // First click of a potential double-click schedules add-vertex;
      // a second click within DRAW_DBLCLICK_MS completes the polygon.
      if (draw.clickTimer) {
        clearTimeout(draw.clickTimer);
        draw.clickTimer = null;
        completeDrawnPolygon();
        return;
      }
      var coord = [coordinate[0], coordinate[1]];
      draw.clickTimer = setTimeout(function () {
        draw.clickTimer = null;
        if (draw.committed) {
          draw.vertices = [];
          draw.committed = false;
        }
        draw.vertices.push(coord);
        scheduleRedraw();
      }, DRAW_DBLCLICK_MS);
    }

    function buildDrawLayers() {
      if (!draw.mode) return [];
      var pts = draw.vertices.slice();
      var preview = (draw.cursor && pts.length > 0 && !draw.committed)
        ? pts.concat([draw.cursor]) : pts;
      var layers = [];
      if (preview.length >= 3) {
        layers.push(new deck.SolidPolygonLayer({
          id: "a5-draw-fill",
          data: [{ polygon: preview }],
          getPolygon: function (d) { return d.polygon; },
          getFillColor: [116, 172, 144, 60],
          pickable: false,
          parameters: { depthTest: false }
        }));
      }
      if (preview.length >= 2) {
        var path = preview.slice();
        if (preview.length >= 3) path.push(preview[0]);
        layers.push(new deck.PathLayer({
          id: "a5-draw-path",
          data: [{ path: path }],
          getPath: function (d) { return d.path; },
          getColor: [116, 172, 144, 230],
          getWidth: 2,
          widthUnits: "pixels",
          pickable: false,
          parameters: { depthTest: false }
        }));
      }
      if (draw.vertices.length > 0) {
        layers.push(new deck.ScatterplotLayer({
          id: "a5-draw-vertices",
          data: draw.vertices.map(function (p) { return { position: p }; }),
          getPosition: function (d) { return d.position; },
          getFillColor: [255, 255, 255, 255],
          getLineColor: [116, 172, 144, 255],
          stroked: true,
          getRadius: 5,
          radiusUnits: "pixels",
          getLineWidth: 2,
          lineWidthUnits: "pixels",
          pickable: false,
          parameters: { depthTest: false }
        }));
      }
      return layers;
    }

    // --- Data (aggregate = "none": in-memory Arrow table) --------------
    var dataVersion = 0;
    var cachedFill = null;      // Uint8ClampedArray [r,g,b,a,...]
    var cachedFillVersion = -1;
    var rowIndex = null;        // Map<hex, rowIdx> for tooltip lookups
    var rowIndexVersion = -1;

    function decodeArrowData(b64) {
      var binary = atob(b64);
      var bytes = new Uint8Array(binary.length);
      for (var i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
      var table = Arrow.tableFromIPC(bytes.buffer);
      return {
        table: table,
        length: table.numRows,
        pentagons: table.getChild("pentagon"),
        fillValues: table.getChild("_fill_value"),
        fillR: table.getChild("_fill_r"),
        fillG: table.getChild("_fill_g"),
        fillB: table.getChild("_fill_b"),
        fillA: table.getChild("_fill_a"),
        elevation: table.getChild("_elevation")
      };
    }

    // Interleaved [r,g,b,a, ...] for the A5Layer accessor. Built once
    // per data version; a uniform fill is expanded too so the accessor
    // signature is the same in both cases.
    function ensureFillArray(x) {
      if (cachedFill && cachedFillVersion === dataVersion) return cachedFill;
      var cols = x.data;
      var n = cols.length;
      var arr = new Uint8ClampedArray(n * 4);
      var i, off;
      if (x.fill_per_cell && cols.fillR) {
        var r = cols.fillR.toArray(), g = cols.fillG.toArray();
        var b = cols.fillB.toArray(), a = cols.fillA.toArray();
        for (i = 0; i < n; i++) {
          off = i * 4;
          arr[off] = r[i]; arr[off + 1] = g[i]; arr[off + 2] = b[i]; arr[off + 3] = a[i];
        }
      } else {
        var c = x.fill_color || [116, 172, 144, 255];
        for (i = 0; i < n; i++) {
          off = i * 4;
          arr[off] = c[0]; arr[off + 1] = c[1]; arr[off + 2] = c[2];
          arr[off + 3] = (c[3] !== undefined) ? c[3] : 255;
        }
      }
      cachedFill = arr;
      cachedFillVersion = dataVersion;
      return arr;
    }

    // Row lookup by hex cell id (legacy path), built on first hover.
    function findLegacyRow(hex) {
      var cols = lastPayload && lastPayload.data;
      if (!cols) return null;
      if (!rowIndex || rowIndexVersion !== dataVersion) {
        rowIndex = new Map();
        var n = cols.length;
        for (var i = 0; i < n; i++) rowIndex.set(pentToHex(cols.pentagons.get(i)), i);
        rowIndexVersion = dataVersion;
      }
      var idx = rowIndex.get(hex);
      if (idx == null) return null;
      var row = {};
      if (cols.fillValues) row._fill_value = cols.fillValues.get(idx);
      (lastPayload.tooltip_cols || []).forEach(function (nm) {
        var col = cols.table.getChild(nm);
        if (col) row[nm] = col.get(idx);
      });
      return row;
    }

    // The A5Layer is rebuilt only when something it depends on changes.
    // deck.gl diffs `data` by reference: a fresh object per redraw would
    // make it recompute every cell boundary on each hover.
    var legacyLayer = null;
    var legacyLayerKey = null;
    function buildA5Layer(x) {
      if (!x.data) return null;
      var key = [dataVersion, currentOpacity, labelLayerId, x.stroked,
                 x.line_width, x.extruded, x.elevation_scale].join("|");
      if (legacyLayer && legacyLayerKey === key) return legacyLayer;
      var fillArr = ensureFillArray(x);
      var cols = x.data;
      if (!cols.deckData) cols.deckData = { length: cols.length };
      var props = {
        id: "a5-layer",
        data: cols.deckData,
        getPentagon: function (_d, info) { return cols.pentagons.get(info.index); },
        getFillColor: function (_d, info) {
          var off = info.index * 4;
          return [fillArr[off], fillArr[off + 1], fillArr[off + 2], fillArr[off + 3]];
        },
        opacity: currentOpacity,
        extruded: x.extruded,
        elevationScale: x.elevation_scale,
        pickable: false,
        stroked: x.stroked,
        getLineColor: x.line_color || [0, 0, 0, 0],
        getLineWidth: x.line_width || 1,
        lineWidthUnits: "pixels",
        beforeId: labelLayerId || undefined,
        updateTriggers: { getFillColor: dataVersion, getPentagon: dataVersion }
      };
      if (x.extruded && cols.elevation) {
        props.getElevation = function (_d, info) { return cols.elevation.get(info.index) || 0; };
      }
      legacyLayer = new deck.A5Layer(props);
      legacyLayerKey = key;
      return legacyLayer;
    }

    // --- Data (pyramid: lazy parquet renderer) --------------------------
    var lazyRenderer = T.lazy.createRenderer({
      getOpacity: function () { return currentOpacity; },
      getBeforeId: function () { return labelLayerId || undefined; },
      onDataReady: function () { scheduleRedraw(); }
    });

    // Minimal viewport for LOD selection. deck's own viewport (with
    // getBounds) is what the TileLayer sees; here we only need zoom and
    // latitude, which MapLibre shares with deck 1:1.
    function getCurrentViewport() {
      if (map) return { zoom: map.getZoom(), latitude: map.getCenter().lat };
      var vs = (lastPayload && lastPayload.view_state) || {};
      return { zoom: vs.zoom || 0, latitude: vs.latitude || 0 };
    }

    // --- Layers -------------------------------------------------------
    function buildHighlightLayer() {
      var target = clickedPentagon || hoveredPentagon;
      if (!target) return null;
      return new deck.A5Layer({
        id: "a5-highlight",
        data: [{ pentagon: target }],
        getPentagon: function (d) { return d.pentagon; },
        getFillColor: [0, 0, 0, 0],
        getLineColor: clickedPentagon ? [255, 255, 255, 255] : [255, 255, 255, 220],
        getLineWidth: clickedPentagon ? 2.5 : 2,
        lineWidthUnits: "pixels",
        stroked: true,
        pickable: false,
        updateTriggers: { getLineColor: !!clickedPentagon, getLineWidth: !!clickedPentagon }
      });
    }

    function buildLayers(x) {
      var layers = [];
      if (x.parquet_b64) {
        var lz = lazyRenderer.buildLodLayer(x, getCurrentViewport());
        if (Array.isArray(lz)) layers = layers.concat(lz);
        else if (lz) layers.push(lz);
      } else {
        var a5l = buildA5Layer(x);
        if (a5l) layers.push(a5l);
      }
      var hl = buildHighlightLayer();
      if (hl) layers.push(hl);
      return layers.concat(buildDrawLayers());
    }

    // --- Tooltip ------------------------------------------------------
    function tooltipHtml() {
      var x = lastPayload;
      var html = '<div class="a5v-tip-id">' + hovered.hex + "</div>";
      var row = hovered.row;
      if (!row) return html;
      if (x.has_fill_value && row._fill_value !== undefined) {
        var label = (x.legend && x.legend.label) || "value";
        html += '<div class="a5v-tip-row"><span>' + escapeHtml(label) + "</span><span>" +
          escapeHtml(fmtValue(row._fill_value)) + "</span></div>";
      }
      (x.tooltip_cols || []).forEach(function (nm) {
        if (row[nm] === undefined) return;
        html += '<div class="a5v-tip-row"><span>' + escapeHtml(nm) + "</span><span>" +
          escapeHtml(fmtValue(row[nm])) + "</span></div>";
      });
      return html;
    }

    function updateTooltip(point) {
      var show = hovered && lastPayload && lastPayload.tooltip && !draw.mode && point;
      if (!show) {
        if (tipEl) tipEl.style.display = "none";
        return;
      }
      if (!tipEl) {
        tipEl = document.createElement("div");
        tipEl.className = "a5v-tip";
        el.appendChild(tipEl);
      }
      tipEl.innerHTML = tooltipHtml();
      tipEl.style.display = "block";
      // Keep the box inside the widget: flip to the left / above when
      // it would overflow the right / bottom edge.
      var w = tipEl.offsetWidth, h = tipEl.offsetHeight;
      var left = point.x + 14, top = point.y + 14;
      if (left + w > el.clientWidth - 8) left = point.x - w - 14;
      if (top + h > el.clientHeight - 8) top = point.y - h - 14;
      tipEl.style.left = Math.max(0, left) + "px";
      tipEl.style.top = Math.max(0, top) + "px";
    }

    // --- Pointer events (from MapLibre) -------------------------------
    // MapLibre's events carry exact lngLat + CSS pixel positions; deck's
    // picking pipeline isn't involved (no layer is pickable).
    function onHover(e) {
      var coord = [e.lngLat.lng, e.lngLat.lat];
      if (draw.mode) {
        if (draw.committed) return;
        draw.cursor = coord;
        if (draw.vertices.length > 0) scheduleRedraw();
        return;
      }
      shinyInput("_cursor", { lng: coord[0], lat: coord[1] });
      var hit = resolveHover(coord);
      var hex = hit ? hit.hex : null;
      if (hex !== (hovered ? hovered.hex : null)) {
        hovered = hit;
        hoveredPentagon = hit ? hit.cell : null;
        scheduleRedraw();
        shinyInput("_hover", hex, true);
      }
      updateTooltip(e.point);
    }

    function onLeave() {
      if (hovered) {
        hovered = null;
        hoveredPentagon = null;
        scheduleRedraw();
        shinyInput("_hover", null, true);
      }
      updateTooltip(null);
    }

    function onClick(e) {
      var coord = [e.lngLat.lng, e.lngLat.lat];
      if (draw.mode) {
        handleDrawClick(coord);
        return;
      }
      shinyInput("_click_coord", { lng: coord[0], lat: coord[1] }, true);
      var hit = resolveHover(coord);
      var cell = hit ? hit.cell : null;
      clickedPentagon = (cell != null && clickedPentagon !== cell) ? cell : null;
      redraw();
      shinyInput("_click", pentToHex(clickedPentagon), true);
    }

    // --- Map creation -------------------------------------------------
    function createMap(x) {
      var vs = x.view_state || {};
      map = new maplibregl.Map({
        container: el,
        style: BASEMAPS[currentBasemap].style,
        center: [vs.longitude || 0, vs.latitude || 0],
        zoom: vs.zoom || 1,
        pitch: vs.pitch || 0,
        bearing: vs.bearing || 0,
        attributionControl: false,
        canvasContextAttributes: { antialias: true }
      });
      overlay = new deck.MapboxOverlay({ interleaved: true, layers: [] });
      map.addControl(overlay);
      // Debug / advanced handle: document.getElementById(id)._a5view
      el._a5view = { map: map, overlay: overlay, lazy: lazyRenderer, redraw: redraw };
      map.addControl(new maplibregl.NavigationControl({ visualizePitch: true }), "top-right");
      map.addControl(new maplibregl.ScaleControl({ unit: "metric" }), "bottom-right");
      map.addControl(new maplibregl.AttributionControl({ compact: true }), "bottom-right");

      map.on("style.load", onStyleLoad);
      map.on("move", function () {
        updateDebug();
        // The pyramid path is viewport-driven; the in-memory path
        // renders every row so needs no rebuild on move.
        if (lastPayload && lastPayload.parquet_b64) scheduleRedraw();
      });
      map.on("mousemove", onHover);
      map.on("mouseout", onLeave);
      map.on("click", onClick);
      map.once("load", function () {
        var attrib = el.querySelector(".maplibregl-ctrl-attrib");
        if (attrib) attrib.classList.remove("maplibregl-compact-show");
      });

      // Esc cancels an in-progress draw and clears any committed polygon.
      el.addEventListener("keydown", function (e) {
        if (e.key !== "Escape" || !draw.mode) return;
        resetDrawGeometry();
        scheduleRedraw();
      });
    }

    // Swap in a new data payload (initial render or Shiny update).
    function loadData(x) {
      dataVersion++;
      hovered = null;
      hoveredPentagon = null;
      clickedPentagon = null;
      updateTooltip(null);
      if (x.parquet_b64) {
        x.data = null;
        lazyRenderer.init(x.parquet_b64).then(redraw).catch(function (e) {
          console.error("[a5view] pyramid init failed:", e);
        });
      } else if (x.arrow_ipc && typeof Arrow !== "undefined") {
        lazyRenderer.reset();
        x.data = decodeArrowData(x.arrow_ipc);
      }
      redraw();
      // Highlight + pyramid tiles need a5-js (lonLatToCell,
      // cellToBoundary); rebuild once it's resolved.
      TILING.ensureA5().then(redraw).catch(function (e) {
        console.error("[a5view] a5-js failed to load:", e);
      });
    }

    // --- Shiny proxy: update data in place ---------------------------------
    if (typeof Shiny !== "undefined") {
      Shiny.addCustomMessageHandler("a5view-update-" + el.id, function (msg) {
        if (!lastPayload || !overlay) return;
        ["fill_color", "fill_per_cell", "has_fill_value", "legend", "tooltip",
         "tooltip_cols", "data_resolution", "lod_resolutions"].forEach(function (k) {
          if (msg[k] !== undefined) lastPayload[k] = msg[k];
        });
        lastPayload.parquet_b64 = msg.parquet_b64 || null;
        lastPayload.arrow_ipc = msg.arrow_ipc || null;
        updateLegend(lastPayload);
        loadData(lastPayload);
      });
    }

    return {
      renderValue: function (x) {
        injectStyles();
        el.classList.add("a5view");
        lastPayload = x;
        currentOpacity = x.opacity;
        draw.enabled = !!x.draw_polygon;
        if (!draw.enabled) draw.mode = false;
        resetDrawGeometry();

        var basemaps = x.basemaps || ["dark"];
        var wantGlobe = !!x.globe;
        if (!map) {
          currentBasemap = BASEMAPS[basemaps[0]] ? basemaps[0] : "none";
          currentGlobe = wantGlobe;
          el.style.background = BASEMAPS[currentBasemap].bg;
          createMap(x);
        } else {
          if (currentGlobe !== wantGlobe) {
            currentGlobe = wantGlobe;
            applyProjection();
          }
          if (basemaps.indexOf(currentBasemap) === -1) setBasemap(basemaps[0]);
        }

        buildControls(basemaps);
        updateLegend(x);
        updateDebug();
        // Defer decode one frame so the basemap paints first.
        setTimeout(function () { loadData(x); }, 0);
      },

      resize: function () {
        if (map) map.resize();
      }
    };
  }
});
