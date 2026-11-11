/* ============================================================================
 * embed.js — the app's side of the contract with a host page
 * ----------------------------------------------------------------------------
 * FUNCTIONAL, NOT THEME. Keep this file when the stylesheet is replaced.
 *
 * The atlas is meant to sit in an <iframe> on recepresearch.org, with the
 * site's own interface around it. This file lets the two talk, through
 * window.postMessage, which works across origins and needs no shared cookies.
 * The full contract is documented in README.md, "Embedding".
 *
 * APP -> HOST   every message: { source: "karbon-atlasi", version: 1, type, ... }
 *   type "ready"   the Shiny session is connected; commands are accepted now
 *   type "resize"  { height } — the document height in CSS pixels, sent
 *                  whenever it changes, so the host can size the iframe
 *   type "state"   { tab, year, fleet_year, facility, facility_name } — sent
 *                  whenever the view changes, so the host can build a
 *                  shareable URL from it
 *
 * HOST -> APP   { target: "karbon-atlasi", tab?, year?, fleet_year?, facility? }
 *   Same fields and same validation as the URL query string. Unknown or out
 *   of range values are ignored on the server, never an error.
 *
 * Messages are accepted only from window.parent and only when addressed to
 * "karbon-atlasi", so other frames, browser extensions and stray messages are
 * ignored. Outgoing messages carry only public view state (a tab name, a year,
 * a public facility id), so they are posted with targetOrigin "*": the app
 * cannot know in advance every origin that will embed it, and nothing in them
 * is private.
 * ========================================================================== */
(function () {
  "use strict";

  var embedded = window.parent !== window;

  // A hook for the host's styling: present only when framed.
  if (embedded) document.documentElement.classList.add("atlas-embedded");

  function post(msg) {
    if (!embedded) return;
    msg.source = "karbon-atlasi";
    msg.version = 1;
    window.parent.postMessage(msg, "*");
  }

  // --- height -----------------------------------------------------------------
  // Measured from the CONTENT, not from the document. AdminLTE stretches the
  // content wrapper to at least the window height, so the document is always
  // as tall as the iframe. Reporting document.scrollHeight therefore fed a
  // loop: the host grew the iframe to the reported height, the page grew to
  // the new window, the next report was taller, and so on without end — found
  // when the host harness hung in testing, 2026-11-11. The header, the content
  // section and the sidebar's own content are not stretched; the lowest of
  // their bottom edges is the height the page actually needs.
  var MEASURED = [".main-header", ".content-wrapper > .content", ".main-sidebar > .sidebar"];

  function contentHeight() {
    var bottom = 0;
    MEASURED.forEach(function (sel) {
      var el = document.querySelector(sel);
      if (el) bottom = Math.max(bottom, el.getBoundingClientRect().bottom + window.scrollY);
    });
    return Math.ceil(bottom);
  }

  var lastHeight = 0;
  function reportHeight() {
    var h = contentHeight();
    if (h > 0 && h !== lastHeight) {
      lastHeight = h;
      post({ type: "resize", height: h });
    }
  }

  function observeHeight() {
    if (!window.ResizeObserver) return;
    var ro = new ResizeObserver(reportHeight);
    MEASURED.forEach(function (sel) {
      var el = document.querySelector(sel);
      if (el) ro.observe(el);
    });
  }
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", observeHeight);
  } else {
    observeHeight();
  }
  window.addEventListener("load", reportHeight);

  // --- state, pushed by the server (server.R, section 8) ------------------------
  Shiny.addCustomMessageHandler("atlas-state", function (s) {
    s.type = "state";
    post(s);
  });

  $(document).on("shiny:connected", function () {
    post({ type: "ready" });
    reportHeight();
  });

  // --- commands from the host -----------------------------------------------------
  window.addEventListener("message", function (e) {
    if (e.source !== window.parent) return;
    var d = e.data;
    if (!d || typeof d !== "object" || d.target !== "karbon-atlasi") return;
    // Plain values only; the server validates every field again.
    var cmd = {};
    ["tab", "year", "fleet_year", "facility"].forEach(function (k) {
      if (d[k] !== undefined && d[k] !== null) cmd[k] = String(d[k]);
    });
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("embed_command", cmd, { priority: "event" });
    }
  });
})();
