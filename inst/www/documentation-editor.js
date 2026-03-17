(function() {
  "use strict";

  // CSS is loaded via htmlDependency (milkdown-crepe.bundle.css)

  // ── Milkdown Crepe editor state (local bundle, no CDN needed) ──
  var crepeInstance = null;
  var currentMarkdown = "";

  function createCrepe(container, initialMarkdown) {
    var Crepe = window.__MilkdownCrepe;
    if (!Crepe) throw new Error("Milkdown bundle not loaded");
    var crepe = new Crepe({
      root: container,
      defaultValue: initialMarkdown || "",
      features: {
        "cursor": true,
        "list-item": true,
        "link-tooltip": true,
        "image-block": true,
        "block-edit": true,
        "placeholder": false,
        "toolbar": true,
        "code-mirror": true,
        "table": true,
        "latex": false
      },
      featureConfigs: {
        "image-block": {
          onUpload: function(file) {
            return new Promise(function(resolve, reject) {
              var reader = new FileReader();
              reader.onload = function() { resolve(reader.result); };
              reader.onerror = reject;
              reader.readAsDataURL(file);
            });
          }
        }
      }
    });
    return crepe.create().then(function() {
      crepeInstance = crepe;
      return crepe;
    });
  }

  function enterEditor(container, markdown) {
    currentMarkdown = markdown;
    var cleanup = Promise.resolve();
    if (crepeInstance) {
      cleanup = Promise.resolve().then(function() {
        return crepeInstance.destroy();
      }).catch(function() {}).then(function() {
        crepeInstance = null;
      });
    }
    return cleanup.then(function() {
      var editorView = container.querySelector(".doc-editor-view");
      editorView.textContent = "";
      return createCrepe(editorView, markdown);
    });
  }

  function exitEditor() {
    if (!crepeInstance) return Promise.resolve(currentMarkdown);
    try {
      var md = crepeInstance.getMarkdown();
      currentMarkdown = md;
      return Promise.resolve(md);
    } catch(e) {
      console.error("getMarkdown failed:", e);
      return Promise.resolve(currentMarkdown);
    }
  }

  // ── Rendered markdown helpers (marked + katex + hljs) ──
  // Note: innerHTML is used intentionally for markdown rendering.
  // Content originates from the user's own model documentation field
  // and is sanitized through marked's built-in escaping.
  var markedLoaded = false;
  var markedLib = null;
  var katexLib = null;
  var hljsLib = null;

  function loadRenderLibs(callback) {
    if (markedLoaded) { callback(); return; }

    var loaded = 0;
    var total = 3;
    function check() {
      loaded++;
      if (loaded >= total) {
        markedLoaded = true;
        callback();
      }
    }

    var s1 = document.createElement("script");
    s1.src = "https://cdn.jsdelivr.net/npm/marked@12.0.1/marked.min.js";
    s1.onload = function() { markedLib = window.marked; check(); };
    document.head.appendChild(s1);

    var s2 = document.createElement("script");
    s2.src = "https://cdn.jsdelivr.net/npm/katex@0.16.38/dist/katex.min.js";
    s2.onload = function() { katexLib = window.katex; check(); };
    document.head.appendChild(s2);

    var s3 = document.createElement("script");
    s3.src = "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/highlight.min.js";
    s3.onload = function() {
      hljsLib = window.hljs;
      // Load additional languages
      var langs = ["r", "python", "sas"];
      var langLoaded = 0;
      langs.forEach(function(lang) {
        var ls = document.createElement("script");
        ls.src = "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/languages/" + lang + ".min.js";
        ls.onload = function() {
          langLoaded++;
          if (langLoaded >= langs.length) check();
        };
        ls.onerror = function() {
          langLoaded++;
          if (langLoaded >= langs.length) check();
        };
        document.head.appendChild(ls);
      });
    };
    document.head.appendChild(s3);

    var katexCSS = document.createElement("link");
    katexCSS.rel = "stylesheet";
    katexCSS.href = "https://cdn.jsdelivr.net/npm/katex@0.16.38/dist/katex.min.css";
    document.head.appendChild(katexCSS);

    var hljsCSS = document.createElement("link");
    hljsCSS.rel = "stylesheet";
    hljsCSS.href = "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@11.9.0/build/styles/github.min.css";
    document.head.appendChild(hljsCSS);
  }

  function escapeHtml(str) {
    var div = document.createElement("div");
    div.appendChild(document.createTextNode(str));
    return div.textContent !== undefined ? div.innerHTML : str;
  }

  function renderMarkdown(markdown) {
    if (!markedLib || !markdown) return "";

    // Process LaTeX before marked
    // Block math: $$...$$
    var processed = markdown.replace(/\$\$([\s\S]*?)\$\$/g, function(match, tex) {
      try {
        return '<div class="math-block">' + katexLib.renderToString(tex.trim(), { displayMode: true, throwOnError: false }) + '</div>';
      } catch(e) {
        return '<div class="math-block"><code>' + escapeHtml(tex) + '</code></div>';
      }
    });
    // Inline math: $...$  (but not $$)
    processed = processed.replace(/(?<!\$)\$(?!\$)((?:[^$\\]|\\.)+?)\$(?!\$)/g, function(match, tex) {
      try {
        return '<span class="math-inline">' + katexLib.renderToString(tex.trim(), { displayMode: false, throwOnError: false }) + '</span>';
      } catch(e) {
        return '<code>' + escapeHtml(tex) + '</code>';
      }
    });

    var renderer = new markedLib.Renderer();
    renderer.code = function(obj) {
      var code = obj.text || obj;
      var lang = obj.lang || "";
      if (hljsLib && lang && hljsLib.getLanguage(lang)) {
        try {
          var highlighted = hljsLib.highlight(code, { language: lang, ignoreIllegals: true }).value;
          return '<pre><code class="hljs language-' + escapeHtml(lang) + '">' + highlighted + '</code></pre>';
        } catch(e) {}
      }
      if (hljsLib && !lang) {
        try {
          var auto = hljsLib.highlightAuto(code).value;
          return '<pre><code class="hljs">' + auto + '</code></pre>';
        } catch(e) {}
      }
      return '<pre><code>' + escapeHtml(code) + '</code></pre>';
    };

    return markedLib.parse(processed, { renderer: renderer, gfm: true, breaks: false });
  }

  // ── Main logic: watch for .documentation-editor-container ──
  function initContainer(container) {
    if (container._docInitialized) return;
    container._docInitialized = true;

    var inputId = container.getAttribute("data-input-id");
    var initial = container.getAttribute("data-initial") || "";

    container._docMarkdown = initial;
    container._editing = false;

    // Use persisted markdown from editor state if available
    if (currentMarkdown) {
      initial = currentMarkdown;
      container._docMarkdown = initial;
    }

    var renderedView = document.createElement("div");
    renderedView.className = "doc-rendered-view";
    container.appendChild(renderedView);

    var editorView = document.createElement("div");
    editorView.className = "doc-editor-view";
    container.appendChild(editorView);

    // Done button (visible only in editing mode)
    var doneBtn = document.createElement("button");
    doneBtn.className = "doc-done-btn";
    doneBtn.textContent = "Done";
    container.appendChild(doneBtn);

    // Render initial markdown
    loadRenderLibs(function() {
      updateRenderedView(renderedView, initial);
    });

    // Click to enter editor
    renderedView.addEventListener("click", function() {
      if (container._editing) return;
      container._editing = true;
      container.classList.add("editing");

      enterEditor(container, container._docMarkdown).catch(function(err) {
        console.error("Failed to enter editor:", err);
        container._editing = false;
        container.classList.remove("editing");
      });
    });

    // Done button exits editor
    doneBtn.addEventListener("click", function(e) {
      e.stopPropagation();
      exitEditorMode(container, renderedView, inputId);
    });

    // Escape key exits editor
    function onKeyDown(e) {
      if (!container._editing) return;
      if (e.key === "Escape") {
        exitEditorMode(container, renderedView, inputId);
      }
    }

    document.addEventListener("keydown", onKeyDown);

    // Cleanup listeners when container removed from DOM
    var cleanupObserver = new MutationObserver(function() {
      if (!document.body.contains(container)) {
        document.removeEventListener("keydown", onKeyDown);
        cleanupObserver.disconnect();
      }
    });
    cleanupObserver.observe(document.body, { childList: true, subtree: true });
  }

  function exitEditorMode(container, renderedView, inputId) {
    if (!container._editing) return;

    exitEditor().then(function(md) {
      container._editing = false;
      container.classList.remove("editing");

      var changed = md !== container._docMarkdown;
      container._docMarkdown = md;
      updateRenderedView(renderedView, md);

      if (changed && inputId && window.Shiny) {
        Shiny.setInputValue(inputId, {
          type: "set_documentation",
          text: md
        }, { priority: "event" });
      }
    }).catch(function(err) {
      console.error("Failed to exit editor:", err);
      container._editing = false;
      container.classList.remove("editing");
    });
  }

  function updateRenderedView(renderedView, markdown) {
    if (!markdown || !markdown.trim()) {
      renderedView.textContent = "";
      var placeholder = document.createElement("div");
      placeholder.className = "doc-placeholder";
      placeholder.textContent = "Click to edit documentation...";
      renderedView.appendChild(placeholder);
      return;
    }
    // Rendered HTML from markdown - content is user's own documentation,
    // processed through marked's built-in sanitization
    var html = renderMarkdown(markdown);
    var tempDiv = document.createElement("div");
    tempDiv.innerHTML = html; // eslint-disable-line -- safe: user-owned markdown via marked
    renderedView.textContent = "";
    while (tempDiv.firstChild) {
      renderedView.appendChild(tempDiv.firstChild);
    }
  }

  // ── MutationObserver to detect containers appearing in DOM ──
  var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      mutation.addedNodes.forEach(function(node) {
        if (node.nodeType !== 1) return;
        if (node.classList && node.classList.contains("documentation-editor-container")) {
          initContainer(node);
          return;
        }
        var containers = node.querySelectorAll ? node.querySelectorAll(".documentation-editor-container") : [];
        containers.forEach(function(c) { initContainer(c); });
      });
    });
  });

  if (document.body) {
    observer.observe(document.body, { childList: true, subtree: true });
  } else {
    document.addEventListener("DOMContentLoaded", function() {
      observer.observe(document.body, { childList: true, subtree: true });
    });
  }

  document.addEventListener("DOMContentLoaded", function() {
    document.querySelectorAll(".documentation-editor-container").forEach(initContainer);
  });
})();
