// Formula Input Mode - Custom ACE mode with dynamic term highlighting
// Injects custom tokenizer rules for term highlighting

(function(global) {
  "use strict";

  // Highlighter class that injects rules into ACE's tokenizer
  function FormulaHighlighter(editor) {
    this.editor = editor;
    this.session = editor.session;
    this.terms = {};
    this.originalRules = null;
    this.updateTimeout = null;

    // Store original rules for restoration
    this._captureOriginalRules();
  }

  FormulaHighlighter.prototype._captureOriginalRules = function() {
    var mode = this.session.$mode;
    if (mode && mode.$highlightRules) {
      // Deep clone the original rules
      var rules = mode.$highlightRules.getRules();
      this.originalRules = {};
      for (var state in rules) {
        if (rules.hasOwnProperty(state)) {
          this.originalRules[state] = rules[state].slice();
        }
      }
    }
  };

  FormulaHighlighter.prototype.setTerms = function(terms) {
    this.terms = terms || {};
    this.update();
  };

  FormulaHighlighter.prototype.update = function() {
    var mode = this.session.$mode;
    if (!mode || !mode.$highlightRules) {
      return;
    }

    var highlightRules = mode.$highlightRules;
    var rules = highlightRules.getRules();

    // Restore original rules first (remove our injected rules)
    if (this.originalRules) {
      for (var state in this.originalRules) {
        if (this.originalRules.hasOwnProperty(state)) {
          rules[state] = this.originalRules[state].slice();
        }
      }
    }

    // Build regex patterns for each token type
    // Process in reverse priority order so higher priority types come first
    var tokenTypes = Object.keys(this.terms);

    for (var i = tokenTypes.length - 1; i >= 0; i--) {
      var tokenType = tokenTypes[i];
      var termList = this.terms[tokenType];

      if (!termList || termList.length === 0) {
        continue;
      }

      // Escape special regex characters and join with |
      var escapedTerms = termList.map(function(term) {
        return term.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
      });

      // Create word-boundary pattern
      var pattern = "\\b(" + escapedTerms.join("|") + ")\\b";

      // Create the rule
      var rule = {
        token: "oq-" + tokenType,
        regex: pattern
      };

      // Inject at the start of the "start" state for highest priority
      if (rules.start) {
        rules.start.unshift(rule);
      }
    }

    // Force tokenizer rebuild
    mode.$tokenizer = null;
    this.session.bgTokenizer.setTokenizer(mode.getTokenizer());
    this.session.bgTokenizer.start(0);
  };

  FormulaHighlighter.prototype.destroy = function() {
    // Restore original rules
    if (this.originalRules) {
      var mode = this.session.$mode;
      if (mode && mode.$highlightRules) {
        var rules = mode.$highlightRules.getRules();
        for (var state in this.originalRules) {
          if (this.originalRules.hasOwnProperty(state)) {
            rules[state] = this.originalRules[state].slice();
          }
        }
        mode.$tokenizer = null;
        this.session.bgTokenizer.setTokenizer(mode.getTokenizer());
        this.session.bgTokenizer.start(0);
      }
    }
  };

  // CSS injection for custom token types
  var injectedStyles = {};

  function injectTokenStyle(tokenType, styles) {
    var styleId = "formula-input-style-" + tokenType;

    // Don't re-inject if already present with same styles
    if (injectedStyles[tokenType] === styles) {
      return;
    }

    // Remove existing style if any
    var existing = document.getElementById(styleId);
    if (existing) {
      existing.parentNode.removeChild(existing);
    }

    // Create and inject new style
    var style = document.createElement("style");
    style.id = styleId;
    style.type = "text/css";
    style.textContent = ".formula-input .ace_oq-" + tokenType + " { " + styles + " }";
    document.head.appendChild(style);

    injectedStyles[tokenType] = styles;
  }

  // Inject default styles for standard token types
  function injectDefaultStyles() {
    // Default color scheme for openqaly token types
    var defaults = {
      keyword: "color: #d73a49; font-weight: bold;",
      variable: "color: #6f42c1;",
      table: "color: #005cc5; font-style: italic;",
      value: "color: #22863a;",
      script_variable: "color: #e36209;"
    };

    for (var tokenType in defaults) {
      if (defaults.hasOwnProperty(tokenType)) {
        injectTokenStyle(tokenType, defaults[tokenType]);
      }
    }
  }

  // Export the module
  global.FormulaInputMode = {
    FormulaHighlighter: FormulaHighlighter,
    injectTokenStyle: injectTokenStyle,
    injectDefaultStyles: injectDefaultStyles
  };

})(typeof window !== "undefined" ? window : this);
