// Bridge: load the vendored a5-js ESM bundle and expose it on window.
//
// htmlwidgets' yaml dependency loader emits plain <script> tags; a5-js
// is ESM-only, so this regular script injects a <script type="module">
// pointing at the sibling a5.js file, then hangs a Promise on
// window.A5Ready that the main widget awaits before any tile work.
(function () {
  if (window.A5Ready) return;
  var here = document.currentScript;
  var baseUrl = (here && here.src) ? here.src.replace(/[^/]+$/, "") : "";
  window.A5Ready = new Promise(function (resolve, reject) {
    var s = document.createElement("script");
    s.type = "module";
    s.textContent =
      'import * as A5 from "' + baseUrl + 'a5.js";\n' +
      'window.A5 = A5;\n' +
      'window.dispatchEvent(new CustomEvent("a5-js-ready"));';
    s.onerror = function (e) { reject(e); };
    window.addEventListener("a5-js-ready", function () { resolve(window.A5); }, { once: true });
    document.head.appendChild(s);
  });
})();
