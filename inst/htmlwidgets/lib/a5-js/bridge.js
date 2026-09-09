// Bridge: load the vendored a5-js ESM bundle and expose it on window.
//
// htmlwidgets' yaml dependency loader emits plain <script> tags; a5-js
// is ESM-only, so this regular script injects a <script type="module">
// pointing at the sibling a5.js file, then hangs a Promise on
// window.A5Ready that the main widget awaits before any tile work.
// When the sibling file is unreachable (self-contained HTML inlines
// this script, losing its src), fall back to the same version on CDN.
(function () {
  if (window.A5Ready) return;
  var here = document.currentScript;
  var baseUrl = (here && here.src) ? here.src.replace(/[^/]+$/, "") : "";
  var local = baseUrl ? baseUrl + "a5.js" : null;
  var cdn = "https://cdn.jsdelivr.net/npm/a5-js@0.10.0/+esm";
  window.A5Ready = new Promise(function (resolve, reject) {
    var s = document.createElement("script");
    s.type = "module";
    s.textContent =
      "let A5;\n" +
      "try { A5 = await import(" + JSON.stringify(local || cdn) + "); }\n" +
      "catch (e) { A5 = await import(" + JSON.stringify(cdn) + "); }\n" +
      "window.A5 = A5;\n" +
      'window.dispatchEvent(new CustomEvent("a5-js-ready"));';
    s.onerror = function (e) { reject(e); };
    window.addEventListener("a5-js-ready", function () { resolve(window.A5); }, { once: true });
    document.head.appendChild(s);
  });
})();
