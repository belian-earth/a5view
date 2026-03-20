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
        if (deckgl && lastPayload) {
          deckgl.setProps({ layers: buildLayers(lastPayload) });
        }
      });

      // Prevent map interaction while dragging slider
      slider.addEventListener("mousedown", function(e) { e.stopPropagation(); });
      slider.addEventListener("touchstart", function(e) { e.stopPropagation(); });

      panel.appendChild(labelRow);
      panel.appendChild(slider);
      op.drop.appendChild(panel);
      toolbar.appendChild(op.el);

      container.appendChild(toolbar);
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

    // Build fill color Uint8ClampedArray for deck.gl binary attribute.
    // RGBA is pre-computed on R side and arrives in Arrow columns.
    function buildFillColorArray(x) {
      var cols = x.data;
      var n = cols.length;
      var arr = new Uint8ClampedArray(n * 4);

      if (x.fill_per_cell && cols.fillR) {
        // RGBA pre-computed R-side — interleave into [r,g,b,a, r,g,b,a, ...]
        var rData = cols.fillR.data && cols.fillR.data[0] && cols.fillR.data[0].values;
        if (rData) {
          // Fast path: direct typed array access from Arrow chunks
          var gData = cols.fillG.data[0].values;
          var bData = cols.fillB.data[0].values;
          var aData = cols.fillA.data[0].values;
          for (var i = 0; i < n; i++) {
            var off = i * 4;
            arr[off] = rData[i];
            arr[off + 1] = gData[i];
            arr[off + 2] = bData[i];
            arr[off + 3] = aData[i];
          }
        } else {
          // Fallback: use .get() accessor
          for (var i = 0; i < n; i++) {
            var off = i * 4;
            arr[off] = cols.fillR.get(i);
            arr[off + 1] = cols.fillG.get(i);
            arr[off + 2] = cols.fillB.get(i);
            arr[off + 3] = cols.fillA.get(i);
          }
        }
      } else {
        // Uniform color
        var c = x.fill_color || [116, 172, 144, 255];
        for (var i = 0; i < n; i++) {
          var off = i * 4;
          arr[off] = c[0];
          arr[off + 1] = c[1];
          arr[off + 2] = c[2];
          arr[off + 3] = c[3] !== undefined ? c[3] : 255;
        }
      }
      return arr;
    }

    // Build minimal row array for A5Layer (needs array data for picking)
    function buildPickingArray(cols) {
      var n = cols.length;
      var rows = new Array(n);
      for (var i = 0; i < n; i++) {
        rows[i] = { pentagon: cols.pentagons.get(i) };
      }
      return rows;
    }

    // Cached arrays — rebuilt only when data changes
    var cachedFillColorArray = null;
    var cachedPickingData = null;
    var cachedDataLength = -1;

    function ensureCachedArrays(x) {
      var n = x.data.length;
      if (n !== cachedDataLength) {
        cachedFillColorArray = buildFillColorArray(x);
        cachedPickingData = buildPickingArray(x.data);
        cachedDataLength = n;
      }
    }

    function invalidateCache() {
      cachedDataLength = -1;
    }

    function buildA5Layer(x) {
      var cols = x.data;
      ensureCachedArrays(x);
      var fillColorArray = cachedFillColorArray;
      var pickingData = cachedPickingData;

      var layerProps = {
        id: "a5-layer",
        data: pickingData,
        getPentagon: function(d) { return d.pentagon; },
        getFillColor: function(d, info) {
          var off = info.index * 4;
          return [fillColorArray[off], fillColorArray[off + 1], fillColorArray[off + 2], fillColorArray[off + 3]];
        },
        opacity: currentOpacity,
        extruded: x.extruded,
        elevationScale: x.elevation_scale,
        pickable: x.pickable,
        autoHighlight: false,
        onHover: function(info) {
          var newHover = (info && info.object) ? info.object.pentagon : null;
          if (newHover !== hoveredPentagon) {
            hoveredPentagon = newHover;
            if (deckgl && lastPayload) {
              deckgl.setProps({ layers: buildLayers(lastPayload) });
            }
            if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
              Shiny.setInputValue(el.id + "_hover", pentToHex(newHover), {priority: "event"});
            }
          }
        },
        onClick: function(info) {
          if (info && info.object) {
            var id = info.object.pentagon;
            clickedPentagon = (clickedPentagon === id) ? null : id;
            if (deckgl && lastPayload) {
              deckgl.setProps({ layers: buildLayers(lastPayload) });
            }
            if (typeof Shiny !== "undefined" && Shiny.setInputValue) {
              Shiny.setInputValue(el.id + "_click", pentToHex(clickedPentagon), {priority: "event"});
            }
          }
        },
        stroked: x.stroked,
        getLineColor: x.line_color || [0, 0, 0, 0],
        getLineWidth: x.line_width || 1,
        lineWidthUnits: "pixels",
        updateTriggers: {
          getFillColor: [x.fill_is_column, x.fill_color, x.fill_per_cell, pickingData.length]
        }
      };

      if (x.extruded && cols.elevation) {
        layerProps.getElevation = function(d, info) {
          return (cols.elevation && info.index >= 0) ? (cols.elevation.get(info.index) || 0) : 0;
        };
      }

      if (currentGlobe) {
        layerProps.parameters = { depthTest: false };
      }

      return new deck.A5Layer(layerProps);
    }

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

    function buildLayers(x) {
      var layers = [];
      var tileLayer = makeTileLayer(currentBasemap);
      if (tileLayer) layers.push(tileLayer);
      layers.push(buildA5Layer(x));
      var highlight = buildHighlightLayer();
      if (highlight) layers.push(highlight);
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
        fillValues: table.getChild("_fill_value"),
        fillR: table.getChild("_fill_r"),
        fillG: table.getChild("_fill_g"),
        fillB: table.getChild("_fill_b"),
        fillA: table.getChild("_fill_a"),
        elevation: table.getChild("_elevation")
      };
    }

    var widgetObj = {
      renderValue: function(x) {
        if (x.arrow_ipc && typeof Arrow !== "undefined") {
          x.data = decodeArrowData(x.arrow_ipc);
        }
        invalidateCache();
        renderDeck(x);
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
        if (msg.tooltip !== undefined) {
          lastPayload.tooltip = msg.tooltip;
          lastPayload.pickable = true;
        }

        if (msg.arrow_ipc && typeof Arrow !== "undefined") {
          lastPayload.data = decodeArrowData(msg.arrow_ipc);
          invalidateCache();
        }

        deckgl.setProps({ layers: buildLayers(lastPayload) });
      });
    }

    function renderDeck(x) {
        lastPayload = x;
        currentOpacity = x.opacity;

        var basemaps = x.basemaps || ["dark"];
        currentBasemap = basemaps[0];

        var info = BASEMAP_TILES[currentBasemap];
        el.style.background = info ? info.bg : "#000";

        if (getComputedStyle(el).position === "static") {
          el.style.position = "relative";
        }

        var tooltipFn = function(info) {
              if (!info || !info.object || !lastPayload || !lastPayload.tooltip) return null;
              var id = info.object.pentagon;

              var idStr = (typeof id === "bigint")
                ? id.toString(16).padStart(16, "0")
                : String(id);
              if (clickedPentagon === id) {
                return { text: idStr };
              }

              var cols = lastPayload.data;
              if (lastPayload.has_fill_value && cols && cols.fillValues && info.index >= 0) {
                var val = cols.fillValues.get(info.index);
                if (val != null) {
                  var display = (typeof val === "number")
                    ? (Number.isInteger(val) ? val.toString() : val.toPrecision(4))
                    : String(val);
                  return { text: display };
                }
              }

              return { text: idStr };
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
            },
            onHover: function(info) {
              if (typeof Shiny !== "undefined" && Shiny.setInputValue && info && info.coordinate) {
                Shiny.setInputValue(el.id + "_cursor", {
                  lng: info.coordinate[0],
                  lat: info.coordinate[1]
                });
              }
            },
            onClick: function(info) {
              if (typeof Shiny !== "undefined" && Shiny.setInputValue && info && info.coordinate) {
                Shiny.setInputValue(el.id + "_click_coord", {
                  lng: info.coordinate[0],
                  lat: info.coordinate[1]
                }, {priority: "event"});
              }
              if (!info || !info.object) {
                if (clickedPentagon) {
                  clickedPentagon = null;
                  if (lastPayload) {
                    deckgl.setProps({ layers: buildLayers(lastPayload) });
                  }
                }
              }
            }
          };

          if (wantGlobe) {
            deckProps.views = new deck._GlobeView({ resolution: 10 });
          }

          currentGlobe = wantGlobe;
          deckgl = new deck.DeckGL(deckProps);
        }

        buildControls(basemaps, el);
    }

    return widgetObj;
  }
});
