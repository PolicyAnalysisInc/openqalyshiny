// Formula Input - Single-line R code editor for Shiny
// Uses Ace Editor for syntax highlighting with custom term highlighting and autocomplete

(function() {
  "use strict";

  let formulaInstanceCounter = 0;

  // Wait for Shiny, Ace, and our custom modules to be available
  function waitForDependencies() {
    return new Promise((resolve) => {
      const check = () => {
        if (typeof Shiny !== "undefined" &&
            Shiny.inputBindings &&
            typeof ace !== "undefined" &&
            typeof FormulaInputMode !== "undefined" &&
            typeof FormulaInputAutocomplete !== "undefined") {
          resolve();
        } else {
          setTimeout(check, 50);
        }
      };
      check();
    });
  }

  // Validate bracket matching
  function validateBrackets(code) {
    const stack = [];
    const pairs = { "(": ")", "[": "]", "{": "}" };
    const opening = Object.keys(pairs);
    const closing = Object.values(pairs);

    let inString = false;
    let stringChar = null;
    let inComment = false;

    for (let i = 0; i < code.length; i++) {
      const char = code[i];

      if (char === "#" && !inString) {
        inComment = true;
        continue;
      }

      if (inComment) continue;

      if (char === '"' || char === "'") {
        let numBackslashes = 0;
        for (let j = i - 1; j >= 0 && code[j] === '\\'; j--) {
          numBackslashes++;
        }
        const isEscaped = numBackslashes % 2 === 1;

        if (!isEscaped) {
          if (!inString) {
            inString = true;
            stringChar = char;
          } else if (char === stringChar) {
            inString = false;
            stringChar = null;
          }
        }
        continue;
      }

      if (inString) continue;

      if (opening.includes(char)) {
        stack.push(char);
      } else if (closing.includes(char)) {
        const last = stack.pop();
        if (!last || pairs[last] !== char) {
          return false;
        }
      }
    }

    return stack.length === 0;
  }

  function getUpdateOn(el) {
    return el.getAttribute("data-update-on") || "change";
  }

  function getAutocompleteParent(el) {
    return el.getAttribute("data-autocomplete-parent") || "";
  }

  function ensureAutocompleteLayer(editor, el) {
    if (!editor || getAutocompleteParent(el) !== "body") {
      return;
    }

    const completer = editor.completer;
    const popup = completer && completer.popup;
    const container = popup && popup.container;

    if (!container) {
      return;
    }

    container.classList.add("formula-input-autocomplete-popup");
    if (container.parentNode !== document.body) {
      document.body.appendChild(container);
    }

    const tooltip = document.querySelector(".ace_tooltip");
    if (tooltip) {
      tooltip.classList.add("formula-input-autocomplete-tooltip");
      if (tooltip.parentNode !== document.body) {
        document.body.appendChild(tooltip);
      }
    }
  }

  function setFormulaEditorValue(el, editor, value) {
    const stringValue = (value == null) ? "" : String(value);

    el._formulaSuppressChange = true;
    try {
      editor.setValue(stringValue, -1);
    } finally {
      el._formulaDirty = false;
      el._formulaSuppressChange = false;
    }
  }

  // Initialize when dependencies are ready
  waitForDependencies().then(() => {
    // Inject default styles for custom token types
    FormulaInputMode.injectDefaultStyles();

    // Create the Shiny input binding
    const FormulaInputBinding = new Shiny.InputBinding();

    $.extend(FormulaInputBinding, {
      find: function(scope) {
        return $(scope).find(".formula-input");
      },

      initialize: function(el) {
        // Read the raw attribute so numeric-looking formulas like "0.03"
        // are not coerced to numbers by jQuery's data parser.
        const initialValueAttr = el.getAttribute("data-value");
        const initialValue = initialValueAttr != null ? initialValueAttr : "";
        const placeholderText = $(el).data("placeholder") || "";
        const termsData = $(el).data("terms");
        const suggestionsData = $(el).data("suggestions");
        const updateOn = getUpdateOn(el);
        const visibilityNs = ".formulaInputVisibility" + (++formulaInstanceCounter);

        // Parse terms if provided
        let initialTerms = null;
        if (termsData) {
          try {
            // jQuery may have already parsed the JSON
            initialTerms = typeof termsData === "string" ? JSON.parse(termsData) : termsData;
          } catch (e) {
            console.warn("Failed to parse formula input terms:", e);
          }
        }

        // Parse suggestions if provided
        let initialSuggestions = null;
        if (suggestionsData) {
          try {
            initialSuggestions = typeof suggestionsData === "string" ?
              JSON.parse(suggestionsData) : suggestionsData;
          } catch (e) {
            console.warn("Failed to parse formula input suggestions:", e);
          }
        }

        // Load language tools extension for autocomplete
        ace.require("ace/ext/language_tools");

        // Create Ace editor
        const editor = ace.edit(el);
        editor.setTheme("ace/theme/chrome");
        editor.session.setMode("ace/mode/r");

        // Configure for single-line appearance with autocomplete enabled
        editor.setOptions({
          maxLines: 1,
          minLines: 1,
          showGutter: false,
          showPrintMargin: false,
          highlightActiveLine: false,
          showFoldWidgets: false,
          displayIndentGuides: false,
          scrollPastEnd: 0,
          useSoftTabs: true,
          tabSize: 2,
          // Autocomplete settings
          enableBasicAutocompletion: true,
          enableLiveAutocompletion: true,
          enableSnippets: false
        });

        // Set placeholder
        if (placeholderText) {
          editor.setOption("placeholder", placeholderText);
        }

        // Set initial value
        editor.setValue(String(initialValue), -1);

        // Initialize custom highlighter for term highlighting
        const highlighter = new FormulaInputMode.FormulaHighlighter(editor);
        if (initialTerms) {
          highlighter.setTerms(initialTerms);
        }
        el._formulaHighlighter = highlighter;

        // Initialize autocomplete completer
        const completer = new FormulaInputAutocomplete.FormulaCompleter(editor, initialSuggestions);
        el._formulaCompleter = completer;

        // Set our completer as the only completer (disable built-in completers)
        editor.completers = [completer];
        ensureAutocompleteLayer(editor, el);

        // Intercept Enter key - trigger Shiny update instead of newline
        // Do not accept autocomplete on Enter - that's for Tab
        editor.commands.addCommand({
          name: "submitFormula",
          bindKey: { win: "Enter", mac: "Enter" },
          exec: function(editor) {
            // Close autocomplete popup if open, then submit
            if (editor.completer && editor.completer.popup &&
                editor.completer.popup.isOpen) {
              editor.completer.detach();
            }
            if (updateOn === "blur") {
              el._formulaDirty = false;
              $(el).trigger("formula-input:commit");
            } else {
              $(el).trigger("formula-input:enter");
            }
          }
        });

        // Also intercept Shift+Enter just in case
        editor.commands.addCommand({
          name: "submitFormulaShift",
          bindKey: { win: "Shift-Enter", mac: "Shift-Enter" },
          exec: function(editor) {
            if (editor.completer && editor.completer.popup &&
                editor.completer.popup.isOpen) {
              editor.completer.detach();
            }
            if (updateOn === "blur") {
              el._formulaDirty = false;
              $(el).trigger("formula-input:commit");
            } else {
              $(el).trigger("formula-input:enter");
            }
          }
        });

        // Tab accepts autocomplete completion when popup is open
        editor.commands.addCommand({
          name: "acceptCompletion",
          bindKey: { win: "Tab", mac: "Tab" },
          exec: function(editor) {
            if (editor.completer && editor.completer.popup &&
                editor.completer.popup.isOpen) {
              editor.completer.insertMatch();
              return true;
            }
            // Default tab behavior (insert tab/spaces) if no popup
            return false;
          }
        });

        // Handle paste - strip newlines to keep single-line
        // Use DOM-level event because Ace's paste events are unreliable
        editor.container.addEventListener("paste", function(e) {
          e.preventDefault();
          e.stopPropagation();
          var text = (e.clipboardData || window.clipboardData).getData("text");
          text = text.replace(/[\r\n]+/g, " ");
          editor.insert(text);
        }, true);

        // Trigger change event on input
        editor.on("change", function() {
          ensureAutocompleteLayer(editor, el);
          if (el._formulaSuppressChange) {
            return;
          }

          if (updateOn === "blur") {
            el._formulaDirty = true;
            return;
          }

          $(el).trigger("formula-input:change");
        });

        editor.on("blur", function() {
          if (updateOn !== "blur" || el._formulaSuppressChange || !el._formulaDirty) {
            return;
          }

          el._formulaDirty = false;
          $(el).trigger("formula-input:commit");
        });

        editor.commands.on("afterExec", function() {
          ensureAutocompleteLayer(editor, el);
        });

        editor.on("focus", function() {
          ensureAutocompleteLayer(editor, el);
        });

        // Store editor reference
        el._formulaEditor = editor;
        el._formulaDirty = false;
        el._formulaSetValue = function(value) {
          setFormulaEditorValue(el, editor, value);
        };

        // Ace needs an explicit resize when initialized in hidden panels/pages.
        const resizeEditor = function() {
          if (!el._formulaEditor) return;
          requestAnimationFrame(function() {
            if (!el._formulaEditor) return;
            el._formulaEditor.resize(true);
            el._formulaEditor.renderer.updateFull();
            ensureAutocompleteLayer(el._formulaEditor, el);
          });
        };

        el._formulaResizeHandler = resizeEditor;
        el._formulaVisibilityNs = visibilityNs;

        $(document).on("shown" + visibilityNs, function(evt) {
          const target = evt.target;
          if (!target) return;
          if (target === el || $.contains(target, el)) {
            resizeEditor();
          }
        });

        $(window).on("resize" + visibilityNs, resizeEditor);
        resizeEditor();
      },

      getValue: function(el) {
        const editor = el._formulaEditor;
        if (!editor) return { value: "", valid: true };

        const value = editor.getValue();
        const valid = validateBrackets(value);

        return { value: value, valid: valid };
      },

      setValue: function(el, value) {
        const editor = el._formulaEditor;
        if (!editor) return;

        setFormulaEditorValue(el, editor, value);
      },

      subscribe: function(el, callback) {
        const updateOn = getUpdateOn(el);

        if (updateOn === "blur") {
          $(el).on("formula-input:commit.formulaInputBinding", function() {
            callback(false);
          });
          return;
        }

        $(el).on("formula-input:change.formulaInputBinding", function() {
          callback(false);
        });
        $(el).on("formula-input:enter.formulaInputBinding", function() {
          callback(true);
        });
      },

      unsubscribe: function(el) {
        $(el).off(".formulaInputBinding");
        if (el._formulaVisibilityNs) {
          $(document).off(el._formulaVisibilityNs);
          $(window).off(el._formulaVisibilityNs);
          delete el._formulaVisibilityNs;
          delete el._formulaResizeHandler;
        }
        delete el._formulaSetValue;
      },

      receiveMessage: function(el, data) {
        if (data.hasOwnProperty("value")) {
          this.setValue(el, data.value);
        }
        if (data.hasOwnProperty("terms")) {
          const highlighter = el._formulaHighlighter;
          if (highlighter) {
            highlighter.setTerms(data.terms);
          }
        }
        if (data.hasOwnProperty("suggestions")) {
          const completer = el._formulaCompleter;
          if (completer) {
            completer.setSuggestions(data.suggestions);
          }
        }
      },

      getRatePolicy: function() {
        return { policy: "debounce", delay: 250 };
      }
    });

    Shiny.inputBindings.register(FormulaInputBinding, "openqalyshiny.formulaInput");

    // Since we loaded async, Shiny may have already initialized the page.
    // Manually initialize any existing formula-input elements.
    $(".formula-input").each(function() {
      if (!this._formulaEditor) {
        FormulaInputBinding.initialize(this);
      }
    });

    console.log("formulaInput binding registered successfully (Ace Editor)");
  }).catch(function(error) {
    console.error("Failed to initialize formulaInput:", error);
  });
})();
