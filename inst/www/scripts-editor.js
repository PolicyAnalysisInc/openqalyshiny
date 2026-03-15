(function() {
  var editor = null;
  var loading = false;
  var debounceTimer = null;
  var pendingCode = null;

  function initAce() {
    var el = document.getElementById('scripts_ace_editor');
    if (!el || editor) return;
    if (typeof ace === 'undefined') {
      setTimeout(initAce, 100);
      return;
    }
    editor = ace.edit(el);
    editor.setTheme('ace/theme/chrome');
    editor.session.setMode('ace/mode/r');
    editor.setFontSize(12);
    editor.session.setTabSize(2);
    editor.setShowPrintMargin(false);
    editor.renderer.setScrollMargin(4, 4);
    editor.session.on('change', function() {
      if (loading) return;
      clearTimeout(debounceTimer);
      debounceTimer = setTimeout(function() {
        Shiny.setInputValue('scripts_ace_code', editor.getValue(), {priority: 'event'});
      }, 500);
    });
    // Load any pending content
    if (pendingCode !== null) {
      loading = true;
      editor.setValue(pendingCode, -1);
      loading = false;
      pendingCode = null;
    }
    editor.resize();
    // Resize again after layout settles (flex dimensions may not be final yet)
    requestAnimationFrame(function() { editor.resize(); });
  }

  function setup() {
    // Watch for #scripts_ace_editor appearing in the DOM
    var observer = new MutationObserver(function() {
      var el = document.getElementById('scripts_ace_editor');
      if (el && !editor) {
        initAce();
      } else if (el && editor && editor.container !== el) {
        // DOM element was replaced (renderUI re-rendered); preserve content and re-init
        pendingCode = editor.getValue();
        editor.destroy();
        editor = null;
        initAce();
      } else if (!el && editor) {
        pendingCode = editor.getValue();
        editor.destroy();
        editor = null;
      }
    });
    observer.observe(document.body, { childList: true, subtree: true });

    Shiny.addCustomMessageHandler('scripts_load_content', function(data) {
      if (editor) {
        loading = true;
        editor.setValue(data.code || '', -1);
        loading = false;
      } else {
        pendingCode = data.code || '';
      }
    });

    window.addEventListener('resize', function() {
      if (editor) editor.resize();
    });

    // If the element already exists, init immediately
    initAce();
  }

  // Run setup now if Shiny is already connected, otherwise wait
  if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket) {
    setup();
  } else {
    $(document).on('shiny:connected', setup);
  }
})();
