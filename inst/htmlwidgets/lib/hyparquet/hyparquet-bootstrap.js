// Non-module bootstrap that loads hyparquet's ESM bundle and exposes the
// resolved module on window.hyparquet. Other code awaits
// window.A5ViewHyparquetReady before calling parquet APIs.
(function () {
  if (window.A5ViewHyparquetReady) return;
  var scripts = document.getElementsByTagName("script");
  var base = "./";
  for (var i = scripts.length - 1; i >= 0; i--) {
    var src = scripts[i].src || "";
    if (src.indexOf("hyparquet-bootstrap.js") !== -1) {
      base = src.replace(/[^/]*$/, "");
      break;
    }
  }
  // Primary path: vendored sibling file. Used when htmlwidgets emits
  // dependencies as separate <script src=...> tags (the default
  // interactive viewer + non-selfcontained saveWidget).
  // Fallback: jsdelivr CDN bundle (selfcontained HTML where the
  // sibling file is unreachable). Same version as the vendored copy.
  window.A5ViewHyparquetReady = import(base + "hyparquet.esm.js")
    .catch(function () {
      return import("https://cdn.jsdelivr.net/npm/hyparquet@1.25.6/+esm");
    })
    .then(function (mod) {
      window.hyparquet = mod;
      console.log("[a5view] hyparquet ready");
      return mod;
    })
    .catch(function (e) {
      console.error("[a5view] hyparquet failed to load:", e);
      throw e;
    });
})();
