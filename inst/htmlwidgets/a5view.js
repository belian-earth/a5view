HTMLWidgets.widget({
  name: "a5view",
  type: "output",

  factory: function(el, width, height) {
    var deckgl = null;
    var currentBasemap = null;
    var currentOpacity = 0.6;
    var currentGlobe = false;
    var currentViewState = null;
    var ACCENT = "#74ac90";

    // Polygon-draw mode state (per-widget). The widget only handles the
    // drawing UX and emits WKT to Shiny on completion — visualising the
    // resulting cell selection is the R caller's responsibility.
    var drawEnabled = false;       // is the feature enabled at all?
    var drawMode = false;          // is drawing currently active?
    var drawVertices = [];         // [[lon, lat], ...]
    var drawCursor = null;         // [lon, lat] live preview
    var drawClickTimer = null;     // pending single-click vertex add
    var drawToggleBtn = null;      // toolbar button for the toggle
    var polygonCommitted = false;  // last polygon completed; held on screen
    var DRAW_DBLCLICK_MS = 280;

    // RAF coalescing for redraws driven by mouse movement
    var rafScheduled = false;

    var BASEMAP_TILES = {
      dark: {
        label: "Dark",
        swatch: "#2c2c2c",
        url: "https://basemaps.cartocdn.com/dark_all/{z}/{x}/{y}@2x.png",
        bg: "#1b1b1b"
      },
      light: {
        label: "Light",
        swatch: "#e8e8e8",
        url: "https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}@2x.png",
        bg: "#f0f0f0"
      },
      osm: {
        label: "OSM",
        swatch: "#d4cfc5",
        url: "https://tile.openstreetmap.org/{z}/{x}/{y}.png",
        bg: "#e8e0d8"
      },
      satellite: {
        label: "Satellite",
        swatch: "#2a4a2e",
        url: "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        bg: "#1a2e1a"
      }
    };

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


    function makeTileLayer(basemapKey) {
      if (!basemapKey || basemapKey === "none") return null;
      var info = BASEMAP_TILES[basemapKey];
      if (!info) return null;

      return new deck.TileLayer({
        id: "basemap-tiles",
        data: info.url,
        minZoom: 0,
        maxZoom: 19,
        tileSize: 256,
        renderSubLayers: function(props) {
          var bounds = props.tile.boundingBox;
          return new deck.BitmapLayer(props, {
            data: null,
            image: props.data,
            bounds: [bounds[0][0], bounds[0][1], bounds[1][0], bounds[1][1]]
          });
        }
      });
    }

    function injectStyles(container) {
      if (container.querySelector("style[data-a5view]")) return;
      var style = document.createElement("style");
      style.setAttribute("data-a5view", "");
      style.textContent =
        // Shared panel glass
        ".a5v-panel{" +
          "background:rgba(20,20,20,0.7);" +
          "backdrop-filter:blur(12px);-webkit-backdrop-filter:blur(12px);" +
          "border-radius:10px;box-shadow:0 4px 20px rgba(0,0,0,0.4);" +
          "font-family:'Inter',system-ui,-apple-system,sans-serif;font-size:11px;" +
          "user-select:none;overflow:hidden;" +
        "}" +
        // Control row (top-left toolbar)
        ".a5v-toolbar{" +
          "position:absolute;top:12px;left:12px;z-index:1;" +
          "display:flex;gap:6px;align-items:flex-start;" +
        "}" +
        // Debug zoom/resolution label (bottom-right)
        ".a5v-debug{" +
          "position:absolute;bottom:12px;right:12px;z-index:1;" +
          "padding:5px 9px;color:#bbb;" +
          "font-family:'Inter',system-ui,-apple-system,sans-serif;" +
          "font-size:10px;letter-spacing:0.3px;" +
          "font-variant-numeric:tabular-nums;" +
          "pointer-events:none;" +
        "}" +
        // Each control wrapper
        ".a5v-ctrl{position:relative;}" +
        // Toggle buttons
        ".a5v-toggle{" +
          "width:32px;height:32px;border-radius:8px;border:none;cursor:pointer;" +
          "background:rgba(20,20,20,0.7);color:#ccc;" +
          "backdrop-filter:blur(8px);-webkit-backdrop-filter:blur(8px);" +
          "display:flex;align-items:center;justify-content:center;" +
          "transition:background 0.2s,color 0.2s,box-shadow 0.2s;" +
          "box-shadow:0 2px 8px rgba(0,0,0,0.3);" +
        "}" +
        ".a5v-toggle:hover{background:rgba(30,30,30,0.85);color:" + ACCENT + ";}" +
        ".a5v-toggle.open{color:" + ACCENT + ";background:rgba(20,20,20,0.85);}" +
        // Dropdown menus
        ".a5v-drop{" +
          "position:absolute;top:40px;left:0;" +
          "opacity:0;transform:translateY(-8px) scale(0.95);" +
          "transition:opacity 0.2s ease,transform 0.2s ease;" +
          "pointer-events:none;" +
        "}" +
        ".a5v-drop.open{opacity:1;transform:translateY(0) scale(1);pointer-events:auto;}" +
        // Basemap options
        ".a5v-opt{" +
          "display:flex;align-items:center;gap:8px;" +
          "padding:8px 14px 8px 10px;cursor:pointer;border:none;width:100%;" +
          "background:transparent;color:#aaa;text-align:left;" +
          "transition:background 0.15s,color 0.15s;" +
          "position:relative;font-size:11px;font-family:inherit;" +
          "letter-spacing:0.3px;" +
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
        // Opacity slider panel
        ".a5v-slider-panel{" +
          "padding:10px 14px;min-width:140px;" +
        "}" +
        ".a5v-slider-label{" +
          "display:flex;justify-content:space-between;align-items:center;" +
          "color:#aaa;font-size:10px;letter-spacing:0.3px;margin-bottom:8px;" +
          "font-family:inherit;" +
        "}" +
        ".a5v-slider-val{color:" + ACCENT + ";font-variant-numeric:tabular-nums;}" +
        // Custom range slider
        ".a5v-range{" +
          "-webkit-appearance:none;appearance:none;width:100%;height:4px;" +
          "border-radius:2px;outline:none;cursor:pointer;" +
          "background:linear-gradient(to right," + ACCENT + " var(--pct),rgba(255,255,255,0.15) var(--pct));" +
        "}" +
        ".a5v-range::-webkit-slider-thumb{" +
          "-webkit-appearance:none;width:14px;height:14px;border-radius:50%;" +
          "background:" + ACCENT + ";border:2px solid rgba(20,20,20,0.8);" +
          "box-shadow:0 1px 4px rgba(0,0,0,0.3);" +
          "transition:transform 0.15s;" +
        "}" +
        ".a5v-range::-webkit-slider-thumb:hover{transform:scale(1.2);}" +
        ".a5v-range::-moz-range-thumb{" +
          "width:14px;height:14px;border-radius:50%;" +
          "background:" + ACCENT + ";border:2px solid rgba(20,20,20,0.8);" +
          "box-shadow:0 1px 4px rgba(0,0,0,0.3);" +
        "}" +
        ".a5v-range::-moz-range-track{" +
          "height:4px;border-radius:2px;border:none;" +
          "background:rgba(255,255,255,0.15);" +
        "}" +
        ".a5v-range::-moz-range-progress{" +
          "height:4px;border-radius:2px;" +
          "background:" + ACCENT + ";" +
        "}";
      container.appendChild(style);
    }

    // --- Generic dropdown toggle logic ---
    // Returns { toggle, drop, close } so callers can wire up content
    function makeDropdown(container, iconSvg, title) {
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

      toggle.addEventListener("click", function(e) {
        e.stopPropagation();
        isOpen = !isOpen;
        drop.classList.toggle("open", isOpen);
        toggle.classList.toggle("open", isOpen);
      });

      document.addEventListener("click", function() { close(); });

      ctrl.appendChild(toggle);
      ctrl.appendChild(drop);

      return { el: ctrl, drop: drop, close: close };
    }

    function buildControls(basemaps, container) {
      // Remove old toolbar
      var existing = container.querySelector(".a5v-toolbar");
      if (existing) existing.remove();

      injectStyles(container);

      var toolbar = document.createElement("div");
      toolbar.className = "a5v-toolbar";

      // --- Basemap selector ---
      if (basemaps.length > 1) {
        var bm = makeDropdown(container, LAYERS_SVG, "Basemap");

        basemaps.forEach(function(key) {
          var info = BASEMAP_TILES[key];
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

          opt.addEventListener("click", function(e) {
            e.stopPropagation();
            setBasemap(key, container);
            bm.close();
          });

          bm.drop.appendChild(opt);
        });

        toolbar.appendChild(bm.el);
      }

      // --- Opacity slider ---
      var op = makeDropdown(container, OPACITY_SVG, "Opacity");
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

      slider.addEventListener("input", function(e) {
        e.stopPropagation();
        var val = parseInt(slider.value, 10);
        currentOpacity = val / 100;
        labelVal.textContent = val + "%";
        slider.style.setProperty("--pct", val + "%");
        scheduleRedraw();
      });

      // Prevent map interaction while dragging slider
      slider.addEventListener("mousedown", function(e) { e.stopPropagation(); });
      slider.addEventListener("touchstart", function(e) { e.stopPropagation(); });

      panel.appendChild(labelRow);
      panel.appendChild(slider);
      op.drop.appendChild(panel);
      toolbar.appendChild(op.el);

      // --- Draw polygon toggle ---
      if (drawEnabled) {
        var drawCtrl = document.createElement("div");
        drawCtrl.className = "a5v-ctrl";

        var drawBtn = document.createElement("button");
        drawBtn.className = "a5v-toggle" + (drawMode ? " open" : "");
        drawBtn.innerHTML = DRAW_SVG;
        drawBtn.title = "Draw polygon (click to place vertices, double-click to finish)";

        drawBtn.addEventListener("click", function(e) {
          e.stopPropagation();
          setDrawMode(!drawMode, container);
        });

        drawCtrl.appendChild(drawBtn);
        toolbar.appendChild(drawCtrl);
        drawToggleBtn = drawBtn;
      }

      container.appendChild(toolbar);
    }

    function setDrawMode(on, container) {
      drawMode = !!on;
      if (drawToggleBtn) drawToggleBtn.classList.toggle("open", drawMode);
      if (container) container.style.cursor = drawMode ? "crosshair" : "";

      // Pull focus into the widget so the ESC keydown listener on el
      // fires without the user first clicking the map.
      if (drawMode && container && container.focus) {
        try { container.focus({ preventScroll: true }); } catch (_) { container.focus(); }
      }

      // Cancel any pending vertex add
      if (drawClickTimer) {
        clearTimeout(drawClickTimer);
        drawClickTimer = null;
      }

      // Clear in-progress vertices when toggling off
      if (!drawMode) {
        drawVertices = [];
        drawCursor = null;
        polygonCommitted = false;
      } else {
        // Drop any cell highlight so it doesn't render over the draw overlay
        hoveredPentagon = null;
        clickedPentagon = null;
      }

      // Suppress deck.gl's built-in doubleClickZoom while drawing — we use
      // double-click to close the polygon and don't want the map to zoom.
      if (deckgl) {
        deckgl.setProps({
          controller: drawMode ? { doubleClickZoom: false } : true
        });
      }

      if (deckgl && lastPayload) {
        deckgl.setProps({ layers: buildLayers(lastPayload) });
      }
    }

    function completeDrawnPolygon() {
      if (drawVertices.length < 3) {
        // Not a valid polygon — discard and reset
        drawVertices = [];
        drawCursor = null;
        polygonCommitted = false;
        scheduleRedraw();
        return;
      }

      // Build WKT POLYGON((lon lat, lon lat, ..., lon lat)) — closed ring
      var ring = drawVertices.slice();
      var first = ring[0];
      var last = ring[ring.length - 1];
      if (first[0] !== last[0] || first[1] !== last[1]) {
        ring.push([first[0], first[1]]);
      }
      var coords = ring.map(function(p) {
        return p[0] + " " + p[1];
      }).join(", ");
      var wkt = "POLYGON((" + coords + "))";

      if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
        Shiny.setInputValue(el.id + "_polygon_draw", wkt, {priority: "event"});
      }

      // Hold the completed polygon and its cell highlight on screen until the
      // user toggles draw mode off, hits ESC, or starts a new polygon.
      drawCursor = null;
      polygonCommitted = true;
      scheduleRedraw();
    }

    function scheduleRedraw() {
      if (rafScheduled) return;
      rafScheduled = true;
      requestAnimationFrame(function() {
        rafScheduled = false;
        if (deckgl && lastPayload) {
          deckgl.setProps({ layers: buildLayers(lastPayload) });
        }
      });
    }

    // Debug label (bottom-right): zoom + computed A5 resolution + lat.
    // Useful for dialing the BIAS / SLOPE levers in tiling.js.
    var debugLabel = null;
    function ensureDebugLabel() {
      if (debugLabel) return debugLabel;
      debugLabel = document.createElement("div");
      debugLabel.className = "a5v-debug";
      el.appendChild(debugLabel);
      return debugLabel;
    }
    function updateDebugLabel(viewState) {
      if (!viewState) return;
      ensureDebugLabel();
      var zoom = (viewState.zoom != null) ? viewState.zoom.toFixed(2) : "?";
      var lat = (viewState.latitude != null) ? viewState.latitude.toFixed(1) : "?";
      var R = (window.A5View && window.A5View.tiling)
        ? window.A5View.tiling.getA5Resolution(viewState)
        : "?";
      debugLabel.textContent = "zoom " + zoom + "  R " + R + "  lat " + lat + "°";
    }

    // Throttled rebuild for tile-mode viewport changes. Pure debounce
    // means the first zoom waits the full timer before anything updates
    // — feels sluggish. Throttle (leading + trailing) rebuilds the
    // first viewport change immediately, throttles subsequent changes
    // to one rebuild per REBUILD_THROTTLE_MS window, and fires a final
    // trailing rebuild when motion stops. Threshold lives in tiling.js.
    var rebuildTimer = null;
    var lastRebuildAt = 0;
    function doRebuild() {
      lastRebuildAt = (typeof performance !== "undefined")
        ? performance.now() : Date.now();
      if (deckgl && lastPayload) {
        deckgl.setProps({ layers: buildLayers(lastPayload) });
      }
    }
    function scheduleRebuild() {
      var THROTTLE = (window.A5View && window.A5View.tiling &&
                      typeof window.A5View.tiling.REBUILD_THROTTLE_MS === "number")
        ? window.A5View.tiling.REBUILD_THROTTLE_MS : 80;
      var now = (typeof performance !== "undefined")
        ? performance.now() : Date.now();
      var since = now - lastRebuildAt;
      if (since >= THROTTLE) {
        // Leading edge: rebuild now, drop any pending trailing call.
        if (rebuildTimer) { clearTimeout(rebuildTimer); rebuildTimer = null; }
        doRebuild();
      } else if (!rebuildTimer) {
        // Trailing edge: schedule one rebuild at the end of the window.
        rebuildTimer = setTimeout(function () {
          rebuildTimer = null;
          doRebuild();
        }, THROTTLE - since);
      }
      // else: trailing rebuild already pending, do nothing.
    }

    function handleDrawClick(coordinate) {
      // First click of a potential double-click: schedule add-vertex.
      // Second click within DRAW_DBLCLICK_MS: cancel and complete polygon.
      if (drawClickTimer) {
        clearTimeout(drawClickTimer);
        drawClickTimer = null;
        completeDrawnPolygon();
        return;
      }
      var coord = [coordinate[0], coordinate[1]];
      drawClickTimer = setTimeout(function() {
        drawClickTimer = null;
        // First vertex after a committed polygon clears the previous shape.
        if (polygonCommitted) {
          drawVertices = [];
          polygonCommitted = false;
        }
        drawVertices.push(coord);
        scheduleRedraw();
      }, DRAW_DBLCLICK_MS);
    }

    var lastPayload = null;

    function setBasemap(key, container) {
      currentBasemap = key;
      var info = BASEMAP_TILES[key];
      if (info) container.style.background = info.bg;

      var opts = container.querySelectorAll(".a5v-opt");
      opts.forEach(function(opt) {
        opt.classList.toggle("active", opt.dataset.basemap === key);
      });

      if (deckgl && lastPayload) {
        deckgl.setProps({ layers: buildLayers(lastPayload) });
      }
    }

    var hoveredPentagon = null;
    var clickedPentagon = null;

    // Convert pentagon ID (BigInt or string) to hex string for Shiny/display
    function pentToHex(p) {
      if (p == null) return null;
      if (typeof p === "bigint") return p.toString(16).padStart(16, "0");
      return String(p);
    }

    // Resolve a [lon, lat] coordinate to the A5 cell BigInt at the
    // dataset's native resolution. Highlight + tooltip use this so
    // they're independent of which layers are actually rendered.
    function coordToCell(coord) {
      if (!coord || !window.A5 || !lastPayload) return null;
      var res = lastPayload.data_resolution;
      if (res == null) return null;
      return window.A5.lonLatToCell([coord[0], coord[1]], res);
    }

    // Bumped on every data swap. The LOD renderer's per-version caches
    // (group partition, bbox bounds, visible-set) read this to detect
    // staleness.
    var dataVersion = 0;
    // Cached fill / picking arrays for the legacy (aggregate = "none")
    // A5Layer path. Rebuilt only when data version changes.
    var cachedFillColorArray = null;
    var cachedPickingData = null;
    var cachedDataLength = -1;
    function invalidateCache() {
      cachedDataLength = -1;
      dataVersion++;
    }

    // Build interleaved [r,g,b,a, ...] Uint8ClampedArray for the
    // legacy A5Layer accessor. When fill is uniform we still produce
    // an array so the accessor signature is uniform.
    function buildFillColorArray(x) {
      var cols = x.data;
      var n = cols.length;
      var arr = new Uint8ClampedArray(n * 4);
      if (x.fill_per_cell && cols.fillR) {
        var rData = cols.fillR.data && cols.fillR.data[0] && cols.fillR.data[0].values;
        if (rData) {
          var gData = cols.fillG.data[0].values;
          var bData = cols.fillB.data[0].values;
          var aData = cols.fillA.data[0].values;
          for (var i = 0; i < n; i++) {
            var off = i * 4;
            arr[off] = rData[i]; arr[off + 1] = gData[i];
            arr[off + 2] = bData[i]; arr[off + 3] = aData[i];
          }
        } else {
          for (var i = 0; i < n; i++) {
            var off = i * 4;
            arr[off] = cols.fillR.get(i); arr[off + 1] = cols.fillG.get(i);
            arr[off + 2] = cols.fillB.get(i); arr[off + 3] = cols.fillA.get(i);
          }
        }
      } else {
        var c = x.fill_color || [116, 172, 144, 255];
        for (var i = 0; i < n; i++) {
          var off = i * 4;
          arr[off] = c[0]; arr[off + 1] = c[1];
          arr[off + 2] = c[2]; arr[off + 3] = (c[3] !== undefined) ? c[3] : 255;
        }
      }
      return arr;
    }

    // Minimal row array A5Layer needs for picking (one object per cell).
    function buildPickingArray(cols) {
      var n = cols.length;
      var rows = new Array(n);
      for (var i = 0; i < n; i++) rows[i] = { pentagon: cols.pentagons.get(i) };
      return rows;
    }

    function ensureCachedArrays(x) {
      var n = x.data.length;
      if (n !== cachedDataLength) {
        cachedFillColorArray = buildFillColorArray(x);
        cachedPickingData = buildPickingArray(x.data);
        cachedDataLength = n;
      }
    }

    // Legacy single-layer rendering: one A5Layer over all rows.
    // Used when aggregate = "none" (no LOD pyramid in the payload).
    function buildA5Layer(x) {
      if (!x.data) return null;
      ensureCachedArrays(x);
      var fillArr = cachedFillColorArray;
      var pickingData = cachedPickingData;
      var cols = x.data;

      var props = {
        id: "a5-layer",
        data: pickingData,
        getPentagon: function (d) { return d.pentagon; },
        getFillColor: function (_d, info) {
          var off = info.index * 4;
          return [fillArr[off], fillArr[off + 1], fillArr[off + 2], fillArr[off + 3]];
        },
        opacity: currentOpacity,
        extruded: x.extruded,
        elevationScale: x.elevation_scale,
        pickable: x.pickable && !drawMode,
        autoHighlight: false,
        stroked: x.stroked,
        getLineColor: x.line_color || [0, 0, 0, 0],
        getLineWidth: x.line_width || 1,
        lineWidthUnits: "pixels",
        updateTriggers: {
          getFillColor: [x.fill_is_column, x.fill_color, x.fill_per_cell, dataVersion]
        }
      };
      if (x.extruded && cols.elevation) {
        props.getElevation = function (_d, info) {
          return (info.index >= 0) ? (cols.elevation.get(info.index) || 0) : 0;
        };
      }
      if (currentGlobe) {
        props.parameters = { depthCompare: "always", cullMode: "back" };
      }
      return new deck.A5Layer(props);
    }

    // Shared widget-state hooks both renderers consume.
    var rendererCtx = {
      getDataVersion: function () { return dataVersion; },
      getElId: function () { return el.id; },
      getOpacity: function () { return currentOpacity; },
      getGlobe: function () { return currentGlobe; },
      getDrawMode: function () { return drawMode; },
      // Called by the lazy renderer when a row-group decode lands.
      onDataReady: function () { scheduleRebuild(); }
    };

    // LOD-picker renderer (in-memory Arrow IPC path). All tunable knobs
    // (ZOOM_TO_RES, VIEWPORT_BUFFER, REBUILD_THROTTLE_MS, ...) live in
    // inst/htmlwidgets/lib/a5view-tiling/tiling.js.
    var lodRenderer = window.A5View.tiling.createRenderer(rendererCtx);

    // Lazy renderer (parquet + hyparquet path). Decodes row groups on
    // demand, scheduling rebuilds as new data lands.
    var lazyRenderer = window.A5View.lazy.createRenderer(rendererCtx);

    function buildHighlightLayer() {
      var target = clickedPentagon || hoveredPentagon;
      if (!target) return null;

      return new deck.A5Layer({
        id: "a5-highlight",
        data: [{ pentagon: target }],
        getPentagon: function(d) { return d.pentagon; },
        getFillColor: [0, 0, 0, 0],
        getLineColor: clickedPentagon
          ? [255, 255, 255, 255]
          : [255, 255, 255, 220],
        getLineWidth: clickedPentagon ? 2.5 : 2,
        lineWidthUnits: "pixels",
        stroked: true,
        pickable: false
      });
    }

    function buildDrawLayers() {
      if (!drawMode) return [];

      // Vertices, plus cursor as a transient last point for the live preview.
      // Once the polygon is committed, the cursor no longer extends the ring.
      var pts = drawVertices.slice();
      var preview = (drawCursor && pts.length > 0 && !polygonCommitted)
        ? pts.concat([drawCursor])
        : pts;

      var layers = [];

      // Filled polygon preview (>=3 points including cursor)
      if (preview.length >= 3) {
        layers.push(new deck.SolidPolygonLayer({
          id: "a5-draw-fill",
          data: [{ polygon: preview }],
          getPolygon: function(d) { return d.polygon; },
          getFillColor: [116, 172, 144, 60],
          pickable: false,
          parameters: { depthTest: false }
        }));
      }

      // Outline as a path. Closed ring if >=3 points; open polyline otherwise.
      if (preview.length >= 2) {
        var pathPts = preview.slice();
        if (preview.length >= 3) {
          pathPts.push(preview[0]);
        }
        layers.push(new deck.PathLayer({
          id: "a5-draw-path",
          data: [{ path: pathPts }],
          getPath: function(d) { return d.path; },
          getColor: [116, 172, 144, 230],
          getWidth: 2,
          widthUnits: "pixels",
          pickable: false,
          parameters: { depthTest: false }
        }));
      }

      // Vertex markers (only the placed vertices, not the cursor)
      if (drawVertices.length > 0) {
        layers.push(new deck.ScatterplotLayer({
          id: "a5-draw-vertices",
          data: drawVertices.map(function(p) { return { position: p }; }),
          getPosition: function(d) { return d.position; },
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

    // Pull the active deck.gl viewport, or synthesise a minimal one
    // from view state when deck.gl hasn't initialised yet (first
    // render). The returned object exposes whatever properties our
    // helpers need (zoom, latitude, optionally getBounds()).
    function getCurrentViewport() {
      if (deckgl && deckgl.viewManager) {
        var vps = deckgl.viewManager.getViewports();
        if (vps && vps.length > 0) return vps[0];
      }
      var vs = currentViewState || (lastPayload && lastPayload.view_state) || {};
      return {
        zoom: vs.zoom || 0,
        latitude: vs.latitude || 0,
        longitude: vs.longitude || 0
        // No getBounds — getViewportBbox falls back to whole-world.
      };
    }

    function getStableLayers(x) {
      var layers = [];
      var tileLayer = makeTileLayer(currentBasemap);
      if (tileLayer) layers.push(tileLayer);
      // Two render paths:
      //   1. Lazy parquet (x.parquet_b64): TileLayer + per-tile A5Layer,
      //      hyparquet-backed row-group decode on demand.
      //   2. Legacy single A5Layer: aggregate = "none", no LOD pyramid.
      if (x.parquet_b64) {
        var lz = lazyRenderer.buildLodLayer(x, getCurrentViewport());
        if (lz) layers.push(lz);
      } else {
        var a5l = buildA5Layer(x);
        if (a5l) layers.push(a5l);
      }
      var hl = buildHighlightLayer();
      if (hl) layers.push(hl);
      return layers;
    }

    function buildLayers(x) {
      var layers = getStableLayers(x).slice();
      var drawLayers = buildDrawLayers();
      for (var i = 0; i < drawLayers.length; i++) layers.push(drawLayers[i]);
      return layers;
    }

    // Synchronous Arrow IPC base64 decode (used for initial standalone render)
    function decodeArrowData(b64) {
      var binary = atob(b64);
      var bytes = new Uint8Array(binary.length);
      for (var i = 0; i < binary.length; i++) {
        bytes[i] = binary.charCodeAt(i);
      }
      var table = Arrow.tableFromIPC(bytes.buffer);
      return arrowTableToColumnar(table);
    }

    function arrowTableToColumnar(table) {
      return {
        length: table.numRows,
        pentagons: table.getChild("pentagon"),
        lods: table.getChild("_lod"),
        fillValues: table.getChild("_fill_value"),
        fillR: table.getChild("_fill_r"),
        fillG: table.getChild("_fill_g"),
        fillB: table.getChild("_fill_b"),
        fillA: table.getChild("_fill_a"),
        elevation: table.getChild("_elevation")
      };
    }

    // The LOD renderer needs the a5-js bridge resolved so it can call
    // cellToBoundary. No-op once loaded.
    function ensureA5Prep() {
      if (window.A5) return Promise.resolve();
      return window.A5View.tiling.ensureA5();
    }

    var widgetObj = {
      renderValue: function(x) {
        invalidateCache();
        // First paint: deck.gl + basemap immediately. Layer builders
        // bail with null while data isn't ready, so this unblocks the
        // canvas from any synchronous decode + a5-js load.
        renderDeck(x);

        // Defer the heavy lifting one frame so the basemap paints
        // before we tie up the main thread.
        setTimeout(function () {
          console.log("[a5view] renderValue dispatch: parquet_b64=" +
                      !!x.parquet_b64 + " arrow_ipc=" + !!x.arrow_ipc +
                      " lazy.exists=" + !!(window.A5View && window.A5View.lazy));
          if (x.parquet_b64) {
            // Lazy path: parse parquet metadata only. Row groups are
            // decoded later on demand, with rebuilds triggered by
            // ctx.onDataReady().
            lazyRenderer.init(x.parquet_b64).then(function () {
              if (deckgl && lastPayload) {
                deckgl.setProps({ layers: buildLayers(lastPayload) });
              }
            }).catch(function (e) {
              console.error("[a5view] lazy init failed:", e);
            });
          } else if (x.arrow_ipc && typeof Arrow !== "undefined") {
            x.data = decodeArrowData(x.arrow_ipc);
            invalidateCache();
          }
          if (deckgl && lastPayload) {
            deckgl.setProps({ layers: buildLayers(lastPayload) });
          }
          // Rebuild once a5-js is ready (cellToBoundary becomes
          // available); the first build returned null for the LOD layer.
          ensureA5Prep().then(function () {
            if (deckgl && lastPayload) {
              deckgl.setProps({ layers: buildLayers(lastPayload) });
            }
          }).catch(function (e) {
            console.error("[a5view] a5-js prep failed:", e);
          });
        }, 0);
      },

      resize: function(width, height) {}
    };

    // Shiny proxy: update data in-place without full widget re-render
    if (typeof Shiny !== "undefined") {
      Shiny.addCustomMessageHandler("a5view-update-" + el.id, function(msg) {
        if (!lastPayload || !deckgl) return;

        // Clear stale highlight — data has changed
        clickedPentagon = null;
        hoveredPentagon = null;

        // Update metadata
        if (msg.fill_is_column !== undefined) lastPayload.fill_is_column = msg.fill_is_column;
        if (msg.fill_color !== undefined) lastPayload.fill_color = msg.fill_color;
        if (msg.fill_per_cell !== undefined) lastPayload.fill_per_cell = msg.fill_per_cell;
        if (msg.palette !== undefined) lastPayload.palette = msg.palette;
        if (msg.domain !== undefined) lastPayload.domain = msg.domain;
        if (msg.has_fill_value !== undefined) lastPayload.has_fill_value = msg.has_fill_value;
        if (msg.data_resolution !== undefined) lastPayload.data_resolution = msg.data_resolution;
        if (msg.lod_resolutions !== undefined) lastPayload.lod_resolutions = msg.lod_resolutions;
        if (msg.tooltip !== undefined) {
          lastPayload.tooltip = msg.tooltip;
          lastPayload.pickable = true;
        }

        if (msg.parquet_b64) {
          lastPayload.parquet_b64 = msg.parquet_b64;
          lastPayload.parquet_row_groups = msg.parquet_row_groups;
          lastPayload.arrow_ipc = null;
          lastPayload.data = null;
          invalidateCache();
          lazyRenderer.init(msg.parquet_b64).then(function () {
            if (deckgl && lastPayload) {
              deckgl.setProps({ layers: buildLayers(lastPayload) });
            }
          }).catch(function (e) {
            console.error("[a5view] lazy update init failed:", e);
          });
        } else if (msg.arrow_ipc && typeof Arrow !== "undefined") {
          lastPayload.parquet_b64 = null;
          lastPayload.data = decodeArrowData(msg.arrow_ipc);
          invalidateCache();
        }

        deckgl.setProps({ layers: buildLayers(lastPayload) });
      });
    }

    function renderDeck(x) {
        lastPayload = x;
        currentOpacity = x.opacity;
        drawEnabled = !!x.draw_polygon;
        if (!drawEnabled) {
          drawMode = false;
          drawVertices = [];
          drawCursor = null;
        }

        var basemaps = x.basemaps || ["dark"];
        currentBasemap = basemaps[0];

        var info = BASEMAP_TILES[currentBasemap];
        el.style.background = info ? info.bg : "#000";

        if (getComputedStyle(el).position === "static") {
          el.style.position = "relative";
        }

        var tooltipFn = function(info) {
              if (!hoveredPentagon || !lastPayload || !lastPayload.tooltip) return null;
              if (drawMode) return null;
              return { text: pentToHex(hoveredPentagon) };
            };

        var layers = buildLayers(x);

        // Recreate deck if globe mode changed
        var wantGlobe = !!(x.globe && deck._GlobeView);
        if (deckgl && wantGlobe !== currentGlobe) {
          deckgl.finalize();
          while (el.lastChild) el.removeChild(el.lastChild);
          deckgl = null;
        }

        if (deckgl) {
          deckgl.setProps({ layers: layers, getTooltip: tooltipFn });
        } else {
          var deckProps = {
            container: el,
            initialViewState: currentViewState || x.view_state,
            controller: true,
            layers: layers,
            getTooltip: tooltipFn,
            onViewStateChange: function(e) {
              currentViewState = e.viewState;
              updateDebugLabel(e.viewState);
              // The lazy path is viewport-driven via TileLayer + the
              // row-group decode-on-demand renderer; the legacy A5Layer
              // path renders all rows so doesn't need viewport rebuilds.
              if (lastPayload && lastPayload.parquet_b64) {
                scheduleRebuild();
              }
            },
            onHover: function(info) {
              if (drawMode && info && info.coordinate) {
                if (polygonCommitted) return;
                drawCursor = [info.coordinate[0], info.coordinate[1]];
                if (drawVertices.length > 0) {
                  scheduleRedraw();
                }
                return;
              }
              if (typeof Shiny !== "undefined" && Shiny.setInputValue && info && info.coordinate) {
                Shiny.setInputValue(el.id + "_cursor", {
                  lng: info.coordinate[0],
                  lat: info.coordinate[1]
                });
              }
              // Coord-driven cell highlight: independent of which layers
              // are rendered, so it works the same in both legacy and
              // pyramid modes.
              var cell = coordToCell(info && info.coordinate);
              if (cell !== hoveredPentagon) {
                hoveredPentagon = cell;
                scheduleRedraw();
                if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
                  Shiny.setInputValue(el.id + "_hover", pentToHex(cell),
                    { priority: "event" });
                }
              }
            },
            onClick: function(info) {
              if (drawMode && info && info.coordinate) {
                handleDrawClick(info.coordinate);
                return;
              }
              if (typeof Shiny !== "undefined" && Shiny.setInputValue && info && info.coordinate) {
                Shiny.setInputValue(el.id + "_click_coord", {
                  lng: info.coordinate[0],
                  lat: info.coordinate[1]
                }, {priority: "event"});
              }
              // Coord-driven cell click: same path for both modes.
              var cell = coordToCell(info && info.coordinate);
              clickedPentagon = (clickedPentagon === cell) ? null : cell;
              if (lastPayload) {
                deckgl.setProps({ layers: buildLayers(lastPayload) });
              }
              if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
                Shiny.setInputValue(el.id + "_click", pentToHex(clickedPentagon),
                  { priority: "event" });
              }
            }
          };

          if (wantGlobe) {
            deckProps.views = new deck._GlobeView({ resolution: 10 });
          }

          currentGlobe = wantGlobe;
          deckgl = new deck.DeckGL(deckProps);

          // ESC cancels an in-progress draw and clears any committed
          // polygon. Listener is scoped to el (no cross-widget leak); el
          // is made focusable below and gets focused on entering draw
          // mode so the keydown reliably fires.
          el.addEventListener("keydown", function(e) {
            if (e.key !== "Escape" || !drawMode) return;
            if (drawClickTimer) { clearTimeout(drawClickTimer); drawClickTimer = null; }
            drawVertices = [];
            drawCursor = null;
            polygonCommitted = false;
            scheduleRedraw();
          });
          if (el.tabIndex < 0) el.tabIndex = 0;
        }

        buildControls(basemaps, el);
        updateDebugLabel(currentViewState || x.view_state);

        // Keep cursor styling consistent when controls are rebuilt
        if (drawMode) el.style.cursor = "crosshair";
    }

    return widgetObj;
  }
});
