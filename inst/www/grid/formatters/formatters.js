/* OQGrid — Shared Formatters */
(function() {
  "use strict";

  var OQGrid = window.OQGrid = window.OQGrid || {};
  OQGrid.fmt = OQGrid.fmt || {};

  // Em-dash for empty/null/undefined values
  OQGrid.fmt.emdash = function(cell) {
    var val = cell.getValue();
    if (val === null || val === undefined || val === "") return "\u2014";
    return val;
  };

  // Cycle limit: show ∞ for Inf, em-dash for empty
  OQGrid.fmt.cycleLimit = function(cell) {
    var val = cell.getValue();
    if (val === null || val === undefined || val === "") return "\u2014";
    if (String(val) === "Inf") return "\u221E";
    return val;
  };

  // Display map reverse-lookup formatter factory
  // Usage: formatter: OQGrid.fmt.displayMap({"drug_a": "Drug A"})
  OQGrid.fmt.displayMap = function(map) {
    return function(cell) {
      var val = cell.getValue();
      if (!val || val === "") return "\u2014";
      return map[val] || val;
    };
  };

  // Boolean → "Yes"/"No"
  OQGrid.fmt.yesNo = function(cell) {
    var val = cell.getValue();
    return (val === 1 || val === "1" || val === true || val === "Yes" || val === "TRUE") ? "Yes" : "No";
  };

  // Capitalize first letter
  OQGrid.fmt.capitalize = function(cell) {
    var val = cell.getValue();
    if (!val) return "\u2014";
    return val.charAt(0).toUpperCase() + val.slice(1);
  };

  // =========================================================================
  // Formula formatter factory — uses Ace's tokenizer for syntax highlighting
  // =========================================================================

  // Ace token type → CSS class mapping
  var ACE_TOKEN_MAP = {
    "oq-keyword": "tok-keyword",
    "oq-variable": "tok-variable",
    "oq-table": "tok-table",
    "oq-value": "tok-value",
    "oq-script_variable": "tok-script",
    "oq-tree": "tok-tree",
    "constant.numeric": "tok-number",
    "string": "tok-string",
    "support.function": "tok-fn"
  };

  function mapAceTokenClass(aceType) {
    if (!aceType || aceType === "text") return null;
    // Direct match
    if (ACE_TOKEN_MAP[aceType]) return ACE_TOKEN_MAP[aceType];
    // Partial match (Ace types can be compound like "constant.numeric.r")
    var keys = Object.keys(ACE_TOKEN_MAP);
    for (var i = 0; i < keys.length; i++) {
      if (aceType.indexOf(keys[i]) !== -1) return ACE_TOKEN_MAP[keys[i]];
    }
    return null;
  }

  // Shared tokenizer cache keyed by serialized terms
  var _formulaCache = { terms: null, tokenizer: null };

  function getFormulaTokenizer(terms) {
    // Return cached tokenizer if terms haven't changed
    if (_formulaCache.terms === terms && _formulaCache.tokenizer) {
      return _formulaCache.tokenizer;
    }

    // Need Ace to be loaded
    if (typeof ace === "undefined") return null;

    var EditSession = ace.require("ace/edit_session").EditSession;
    var session = new EditSession("", "ace/mode/r");
    var mode = session.$mode;
    if (!mode || !mode.$highlightRules) return null;
    var rules = mode.$highlightRules.getRules();

    // Inject term rules using the same algorithm as FormulaHighlighter
    if (terms) {
      var tokenTypes = Object.keys(terms);
      for (var i = tokenTypes.length - 1; i >= 0; i--) {
        var tokenType = tokenTypes[i];
        var termList = terms[tokenType];
        if (!termList || termList.length === 0) continue;

        var escapedTerms = termList.map(function(term) {
          return term.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
        });
        var pattern = "\\b(" + escapedTerms.join("|") + ")\\b";
        var rule = { token: "oq-" + tokenType, regex: pattern };

        if (rules.start) {
          rules.start.unshift(rule);
        }
      }
      // Rebuild tokenizer
      mode.$tokenizer = null;
    }

    var tokenizer = mode.getTokenizer();
    _formulaCache = { terms: terms, tokenizer: tokenizer };
    return tokenizer;
  }

  // Factory: OQGrid.fmt.formula(terms) returns a Tabulator formatter function
  OQGrid.fmt.formula = function(terms) {
    return function(cell) {
      var val = cell.getValue();
      if (val === null || val === undefined || val === "") return "\u2014";

      var tokenizer = getFormulaTokenizer(terms);
      if (!tokenizer) {
        // Ace not loaded yet — fall back to monospace plain text
        var fallback = document.createElement("span");
        fallback.className = "cell-formula";
        fallback.textContent = val;
        return fallback;
      }

      // Tokenize the line
      var state = "start";
      var result = tokenizer.getLineTokens(String(val), state);
      var tokens = result.tokens;

      var container = document.createElement("span");
      container.className = "cell-formula";

      for (var i = 0; i < tokens.length; i++) {
        var tok = tokens[i];
        var cssClass = mapAceTokenClass(tok.type);

        if (cssClass) {
          var span = document.createElement("span");
          span.className = cssClass;
          span.textContent = tok.value;
          container.appendChild(span);
        } else {
          container.appendChild(document.createTextNode(tok.value));
        }
      }

      return container;
    };
  };

  // Comma-separated tags rendered as pill badges
  OQGrid.fmt.tags = function(cell) {
    var val = cell.getValue();
    if (!val) return "\u2014";
    var container = document.createElement("div");
    container.className = "oq-grid-tags-display";
    val.split(",").forEach(function(v) {
      v = v.trim();
      if (!v) return;
      var tag = document.createElement("span");
      tag.className = "oq-grid-tag";
      tag.textContent = v;
      container.appendChild(tag);
    });
    return container;
  };
})();
