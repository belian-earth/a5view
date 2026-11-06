// =====================================================================
// a5view A5Layer: deck.gl's A5Layer drawn with the page's a5-js
// =====================================================================
// deck.gl 9.4 bundles a5-js ^0.10 and calls its own cellToBoundary()
// from A5Layer.indexToBounds(). A5 1.0 renumbers every cell, so that
// copy places 1.0 ids in the wrong spot. This subclass overrides only
// indexToBounds(), taking boundaries from window.A5 (the vendored
// a5-js 1.0, lib/a5-js/bridge.js); fill, stroke, extrusion and every
// other prop are deck.gl's own. The body mirrors deck.gl's.
//
// Drop it, and use deck.A5Layer again, once deck.gl ships a release
// built on a5-js >= 1.0. As of 2026-10-14 deck.gl master still pins
// a5-js ^0.10.0 (modules/geo-layers/package.json) and there is no
// issue or PR for 1.0; the last bump was visgl/deck.gl#10620
// (0.7.2 -> 0.10.0).
//
// Callers must wait for window.A5Ready before building the layer.
// =====================================================================
(function () {
  var T = window.A5View = window.A5View || {};
  if (typeof deck === "undefined" || !deck.A5Layer) return;

  class A5Layer extends deck.A5Layer {
    indexToBounds() {
      var A5 = window.A5;
      if (!A5) throw new Error("[a5view] A5Layer built before a5-js loaded");
      var props = this.props;
      var getPentagon = props.getPentagon;
      return {
        data: props.data,
        _normalize: false,
        _windingOrder: "CCW",
        positionFormat: "XY",
        getPolygon: function (x, objectInfo) {
          var p = getPentagon(x, objectInfo);
          var ring = A5.cellToBoundary(typeof p === "string" ? A5.hexToU64(p) : p,
                                       { closedRing: true, segments: "auto" });
          var flat = new Float64Array(ring.length * 2);
          for (var i = 0; i < ring.length; i++) {
            flat[i * 2] = ring[i][0];
            flat[i * 2 + 1] = ring[i][1];
          }
          return flat;
        }
      };
    }
  }
  A5Layer.layerName = "A5ViewA5Layer";

  T.A5Layer = A5Layer;
})();
