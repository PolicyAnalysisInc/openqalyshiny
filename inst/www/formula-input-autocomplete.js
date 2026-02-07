// Formula Input Autocomplete - ACE completer for formula suggestions
// Provides typeahead suggestions with details panel

(function(global) {
  "use strict";

  /**
   * FormulaCompleter - ACE completer implementation
   * @param {Object} editor - ACE editor instance
   * @param {Object} suggestions - Initial suggestions object
   */
  function FormulaCompleter(editor, suggestions) {
    this.editor = editor;
    this.suggestions = {};
    this.completionList = [];
    if (suggestions) {
      this.setSuggestions(suggestions);
    }
  }

  /**
   * Set suggestions for autocomplete
   * @param {Object} suggestions - Object with category keys and arrays of items
   */
  FormulaCompleter.prototype.setSuggestions = function(suggestions) {
    this.suggestions = suggestions || {};
    this._buildCompletionList();
  };

  /**
   * Build flat completion list from suggestions object
   */
  FormulaCompleter.prototype._buildCompletionList = function() {
    this.completionList = [];
    for (var category in this.suggestions) {
      if (!this.suggestions.hasOwnProperty(category)) continue;

      var items = this.suggestions[category];
      if (!Array.isArray(items)) continue;

      for (var i = 0; i < items.length; i++) {
        var item = items[i];
        if (!item || !item.name) continue;

        this.completionList.push({
          name: item.name,
          value: item.name,
          caption: item.label || item.name,
          meta: category,
          score: 1000,
          _description: item.description || null,
          _signature: item.signature || null,
          _package: item.package || null
        });
      }
    }
  };

  /**
   * ACE completer interface - get completions
   * @param {Object} editor - ACE editor
   * @param {Object} session - Editor session
   * @param {Object} pos - Cursor position
   * @param {string} prefix - Current word prefix
   * @param {Function} callback - Callback for results
   */
  FormulaCompleter.prototype.getCompletions = function(editor, session, pos, prefix, callback) {
    if (!prefix || prefix.length === 0) {
      callback(null, []);
      return;
    }

    var prefixLower = prefix.toLowerCase();
    var matches = this.completionList.filter(function(item) {
      return item.name.toLowerCase().indexOf(prefixLower) === 0;
    });

    // Sort alphabetically
    matches.sort(function(a, b) {
      return a.name.localeCompare(b.name);
    });

    callback(null, matches);
  };

  /**
   * ACE completer interface - get documentation tooltip
   * @param {Object} item - Completion item
   * @returns {Object|undefined} - Object with docHTML property
   */
  FormulaCompleter.prototype.getDocTooltip = function(item) {
    if (!item._description && !item._signature) {
      return undefined;
    }

    var html = '<div class="formula-autocomplete-doc">';

    if (item._signature) {
      html += '<div class="formula-autocomplete-signature">' +
              escapeHtml(item._signature) + '</div>';
    }

    if (item._description) {
      html += '<div class="formula-autocomplete-description">' +
              escapeHtml(item._description) + '</div>';
    }

    if (item._package) {
      html += '<div class="formula-autocomplete-source">Source: ' +
              escapeHtml(item._package) + '</div>';
    }

    html += '</div>';

    return { docHTML: html };
  };

  /**
   * Escape HTML special characters
   * @param {string} text - Text to escape
   * @returns {string} - Escaped text
   */
  function escapeHtml(text) {
    if (!text) return '';
    var div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  }

  // Export to global scope
  global.FormulaInputAutocomplete = {
    FormulaCompleter: FormulaCompleter
  };

})(typeof window !== "undefined" ? window : this);
