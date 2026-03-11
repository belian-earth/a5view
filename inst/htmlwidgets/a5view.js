HTMLWidgets.widget({
  name: "a5view",
  type: "output",

  factory: function(el, width, height) {
    var deckgl = null;
    var currentBasemap = null;
    var currentOpacity = 0.6;
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

    function colorScale(value, domain, palette) {
      if (domain[0] === domain[1]) return palette[0];
      var t = Math.max(0, Math.min(1,
        (value - domain[0]) / (domain[1] - domain[0])
      ));
      var i = Math.min(
        Math.floor(t * (palette.length - 1)),
        palette.length - 2
      );
      var f = t * (palette.length - 1) - i;
      return palette[i].map(function(c, j) {
        return Math.round(c + (palette[i + 1][j] - c) * f);
      });
    }

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

    function buildA5Layer(x) {
      var getFillColor;
      if (x.fill_is_column) {
        var domain = x.domain;
        var palette = x.palette;
        getFillColor = function(d) {
          return colorScale(d._fill_value, domain, palette);
        };
      } else if (x.fill_per_cell) {
        getFillColor = function(d) {
          return d._fill_rgba;
        };
      } else {
        getFillColor = x.fill_color;
      }

      var layerProps = {
        id: "a5-layer",
        data: x.data,
        getPentagon: function(d) { return d.pentagon; },
        getFillColor: getFillColor,
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
          }
        },
        onClick: function(info) {
          if (info && info.object) {
            // Toggle: click same cell again to dismiss
            var id = info.object.pentagon;
            clickedPentagon = (clickedPentagon === id) ? null : id;
            if (deckgl && lastPayload) {
              deckgl.setProps({ layers: buildLayers(lastPayload) });
            }
          }
        },
        stroked: x.stroked,
        getLineColor: x.line_color || [0, 0, 0, 0],
        getLineWidth: x.line_width || 1,
        lineWidthUnits: "pixels",
        updateTriggers: {
          getFillColor: [x.fill_is_column, x.fill_color]
        }
      };

      if (x.extruded) {
        layerProps.getElevation = function(d) {
          return d._elevation || 0;
        };
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

    return {
      renderValue: function(x) {
        lastPayload = x;
        currentOpacity = x.opacity;

        var basemaps = x.basemaps || ["dark"];
        currentBasemap = basemaps[0];

        var info = BASEMAP_TILES[currentBasemap];
        el.style.background = info ? info.bg : "#000";

        if (getComputedStyle(el).position === "static") {
          el.style.position = "relative";
        }

        var hasFillValue = x.has_fill_value;

        var tooltipFn = x.tooltip
          ? function(info) {
              if (!info || !info.object) return null;
              var obj = info.object;
              var id = obj.pentagon;

              // Clicked cell: always show cell ID
              if (clickedPentagon === id) {
                return { text: id };
              }

              // Hover: show fill value if present, otherwise cell ID
              if (hasFillValue && obj._fill_value != null) {
                var val = obj._fill_value;
                // Format to reasonable precision
                var display = (typeof val === "number")
                  ? (Number.isInteger(val) ? val.toString() : val.toPrecision(4))
                  : String(val);
                return { text: display };
              }

              return { text: id };
            }
          : null;

        var layers = buildLayers(x);

        if (deckgl) {
          deckgl.setProps({ layers: layers, getTooltip: tooltipFn });
        } else {
          deckgl = new deck.DeckGL({
            container: el,
            initialViewState: x.view_state,
            controller: true,
            layers: layers,
            getTooltip: tooltipFn,
            onClick: function(info) {
              // Click empty space → clear clicked state
              if (!info || !info.object) {
                if (clickedPentagon) {
                  clickedPentagon = null;
                  if (lastPayload) {
                    deckgl.setProps({ layers: buildLayers(lastPayload) });
                  }
                }
              }
            }
          });
        }

        buildControls(basemaps, el);
      },

      resize: function(width, height) {}
    };
  }
});
