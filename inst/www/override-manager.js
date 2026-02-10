// Override Manager - Kanban-style CRUD management for override categories
// Manages all UI interactions in JavaScript; serializes state to R on Apply.

(function() {
  "use strict";

  // =========================================================================
  // Dynamic SortableJS loader
  // =========================================================================
  var SORTABLE_CDN = "https://cdnjs.cloudflare.com/ajax/libs/Sortable/1.15.0/Sortable.min.js";
  var _sortableCallbacks = [];
  var _sortableLoading = false;

  function ensureSortable(callback) {
    if (typeof Sortable !== "undefined") {
      callback();
      return;
    }
    _sortableCallbacks.push(callback);
    if (_sortableLoading) return;
    _sortableLoading = true;

    var script = document.createElement("script");
    script.src = SORTABLE_CDN;
    script.onload = function() {
      console.log("[OverrideManager] SortableJS loaded");
      var cbs = _sortableCallbacks.slice();
      _sortableCallbacks = [];
      for (var i = 0; i < cbs.length; i++) cbs[i]();
    };
    script.onerror = function() {
      console.error("[OverrideManager] Failed to load SortableJS from CDN");
    };
    document.head.appendChild(script);
  }

  // =========================================================================
  // OverrideManager singleton
  // =========================================================================
  var OverrideManager = {
    inputId: null,
    categories: null,
    container: null,
    modelMeta: null,
    _nextId: 1,
    _sortables: [],

    init: function(data) {
      console.log("[OverrideManager] init called");
      this.inputId = data.inputId;
      this.categories = JSON.parse(JSON.stringify(data.categories || []));
      this.modelMeta = data.model_meta || {
        variables: [], settings: [], strategies: {}, groups: {}
      };
      this._nextId = 1;

      for (var i = 0; i < this.categories.length; i++) {
        var cat = this.categories[i];
        if (!cat.overrides) cat.overrides = [];
        for (var j = 0; j < cat.overrides.length; j++) {
          cat.overrides[j]._id = this._nextId++;
        }
      }

      // Modal may not be in DOM yet - wait for it
      var self = this;
      var attempts = 0;
      var findContainer = function() {
        self.container = document.querySelector(".override-manager-body");
        if (self.container) {
          console.log("[OverrideManager] container found");
          self._destroySortables();
          self.render();
          ensureSortable(function() {
            self._initAllSortables();
          });
        } else if (attempts < 20) {
          attempts++;
          setTimeout(findContainer, 50);
        } else {
          console.error("[OverrideManager] container not found after retries");
        }
      };
      findContainer();
    },

    _destroySortables: function() {
      for (var i = 0; i < this._sortables.length; i++) {
        this._sortables[i].destroy();
      }
      this._sortables = [];
    },

    _fullRender: function() {
      this._destroySortables();
      this.render();
      var self = this;
      ensureSortable(function() {
        self._initAllSortables();
      });
    },

    render: function() {
      if (!this.container) return;
      this.container.innerHTML = "";

      for (var i = 0; i < this.categories.length; i++) {
        this.container.appendChild(this._buildColumn(this.categories[i], i));
      }

      // Add Category button
      var addCol = document.createElement("div");
      addCol.className = "override-manager-add-column";
      addCol.innerHTML =
        '<button type="button" class="override-manager-add-column-btn" ' +
        'data-action="add-category">+ Add Category</button>';
      this.container.appendChild(addCol);
    },

    _buildColumn: function(category, index) {
      var col = document.createElement("div");
      col.className = "override-manager-column";
      col.dataset.categoryIndex = index;

      var header = document.createElement("div");
      header.className = "override-manager-column-header";

      var title = document.createElement("span");
      title.className = "override-manager-column-title";
      title.textContent = category.name;
      title.dataset.action = "rename-category";
      title.dataset.categoryIndex = index;

      var actions = document.createElement("div");
      actions.className = "override-manager-column-actions";
      actions.innerHTML =
        '<button type="button" data-action="delete-category" data-category-index="' +
        index + '" title="Delete category">&times;</button>';

      header.appendChild(title);
      header.appendChild(actions);
      col.appendChild(header);

      var cardList = document.createElement("div");
      cardList.className = "override-manager-card-list";
      cardList.dataset.categoryIndex = index;

      var overrides = category.overrides || [];
      for (var j = 0; j < overrides.length; j++) {
        cardList.appendChild(this._buildCard(overrides[j]));
      }
      col.appendChild(cardList);

      var addBtn = document.createElement("button");
      addBtn.type = "button";
      addBtn.className = "override-manager-add-btn";
      addBtn.dataset.action = "add-card";
      addBtn.dataset.categoryIndex = index;
      addBtn.textContent = "+ Add Override";
      col.appendChild(addBtn);

      return col;
    },

    _buildCard: function(override) {
      var card = document.createElement("div");
      card.className = "override-manager-card";
      card.dataset.overrideId = override._id;
      card.dataset.override = JSON.stringify(override);

      var top = document.createElement("div");
      top.className = "override-manager-card-top";

      var info = document.createElement("div");
      info.className = "override-manager-card-info";

      var name = document.createElement("div");
      name.className = "override-manager-card-name";
      name.textContent = override.title || override.display_name || override.name;

      var type = document.createElement("div");
      type.className = "override-manager-card-type";
      type.textContent = override.input_type || "numeric";

      info.appendChild(name);
      if (override.description) {
        var titleEl = document.createElement("div");
        titleEl.className = "override-manager-card-title";
        titleEl.textContent = override.description;
        info.appendChild(titleEl);
      }

      // Type badge and targeting info
      var badges = document.createElement("div");
      badges.className = "override-manager-card-badges";

      var typeBadge = document.createElement("span");
      typeBadge.className = "override-manager-badge override-manager-badge-" +
        (override.type || "variable");
      typeBadge.textContent = (override.type || "variable") === "setting" ? "Setting" : "Variable";
      badges.appendChild(typeBadge);

      if (override.strategy) {
        var stratBadge = document.createElement("span");
        stratBadge.className = "override-manager-badge override-manager-badge-target";
        var stratLabel = this._getStrategyLabel(override.strategy);
        stratBadge.textContent = stratLabel;
        badges.appendChild(stratBadge);
      }

      if (override.group) {
        var grpBadge = document.createElement("span");
        grpBadge.className = "override-manager-badge override-manager-badge-target";
        var grpLabel = this._getGroupLabel(override.group);
        grpBadge.textContent = grpLabel;
        badges.appendChild(grpBadge);
      }

      info.appendChild(badges);
      info.appendChild(type);

      var menuBtn = document.createElement("button");
      menuBtn.type = "button";
      menuBtn.className = "override-manager-card-menu-btn";
      menuBtn.dataset.action = "toggle-menu";
      menuBtn.dataset.overrideId = override._id;
      menuBtn.innerHTML = "&#8230;";

      var menu = document.createElement("div");
      menu.className = "override-manager-card-menu";
      menu.dataset.overrideId = override._id;
      menu.innerHTML =
        '<button type="button" data-action="edit-card" data-override-id="' +
        override._id + '">Edit</button>' +
        '<button type="button" class="danger" data-action="delete-card" data-override-id="' +
        override._id + '">Delete</button>';

      top.appendChild(info);
      top.appendChild(menuBtn);
      card.appendChild(top);
      card.appendChild(menu);

      return card;
    },

    _initAllSortables: function() {
      if (typeof Sortable === "undefined") {
        console.error("[OverrideManager] Sortable not available");
        return;
      }

      var cardLists = this.container.querySelectorAll(".override-manager-card-list");
      console.log("[OverrideManager] init sortable on", cardLists.length, "lists");
      var self = this;

      for (var i = 0; i < cardLists.length; i++) {
        var sortable = Sortable.create(cardLists[i], {
          group: "overrides",
          animation: 150,
          ghostClass: "override-manager-ghost",
          chosenClass: "override-manager-chosen",
          draggable: ".override-manager-card",
          filter: ".editing",
          preventOnFilter: false,
          forceFallback: true,
          fallbackClass: "override-manager-fallback",
          fallbackOnBody: true,
          onEnd: function() {
            self._syncFromDOM();
          }
        });
        this._sortables.push(sortable);
      }
    },

    _syncFromDOM: function() {
      var columns = this.container.querySelectorAll(".override-manager-column");
      var newCategories = [];

      for (var i = 0; i < columns.length; i++) {
        var catIndex = parseInt(columns[i].dataset.categoryIndex, 10);
        var cat = this.categories[catIndex];
        if (!cat) continue;

        var cards = columns[i].querySelectorAll(
          ":scope > .override-manager-card-list > .override-manager-card"
        );
        var overrides = [];
        for (var j = 0; j < cards.length; j++) {
          overrides.push(JSON.parse(cards[j].dataset.override));
        }

        newCategories.push({
          name: cat.name,
          general: cat.general || false,
          overrides: overrides
        });
      }
      this.categories = newCategories;
    },

    // ---- CRUD operations ----

    addCategory: function() {
      // Create a new empty column with inline rename
      this.categories.push({
        name: "New Category",
        general: false,
        overrides: []
      });
      this._fullRender();

      // Trigger rename on the new column title
      var cols = this.container.querySelectorAll(".override-manager-column");
      var lastCol = cols[cols.length - 1];
      if (lastCol) {
        var titleEl = lastCol.querySelector(".override-manager-column-title");
        if (titleEl) this.renameCategory(this.categories.length - 1, titleEl);
      }
    },

    renameCategory: function(categoryIndex, titleEl) {
      var cat = this.categories[categoryIndex];
      if (!cat) return;

      var currentName = cat.name;
      var input = document.createElement("input");
      input.type = "text";
      input.className = "override-manager-column-rename";
      input.value = currentName;

      titleEl.parentNode.replaceChild(input, titleEl);
      input.focus();
      input.select();

      var self = this;
      var committed = false;

      var commit = function() {
        if (committed) return;
        committed = true;
        var newName = input.value.trim();
        if (newName) cat.name = newName;
        self._fullRender();
      };

      input.addEventListener("blur", commit);
      input.addEventListener("keydown", function(e) {
        if (e.key === "Enter") { e.preventDefault(); commit(); }
        else if (e.key === "Escape") { committed = true; self._fullRender(); }
      });
    },

    deleteCategory: function(categoryIndex) {
      var cat = this.categories[categoryIndex];
      if (!cat) return;

      var count = (cat.overrides || []).length;
      var msg = 'Delete category "' + cat.name + '"';
      if (count > 0) msg += " and its " + count + " override(s)";
      msg += "?";
      if (!confirm(msg)) return;

      this.categories.splice(categoryIndex, 1);
      this._fullRender();
    },

    editCard: function(overrideId) {
      var cardEl = this.container.querySelector(
        '.override-manager-card[data-override-id="' + overrideId + '"]'
      );
      if (!cardEl) return;
      var override = JSON.parse(cardEl.dataset.override);
      cardEl.classList.add("editing");
      cardEl.innerHTML = this._buildEditForm(override);
    },

    addCard: function(categoryIndex) {
      var newOverride = {
        _id: this._nextId++,
        name: "", title: "", description: "",
        type: "variable", strategy: "", group: "",
        input_type: "numeric", input_config: {}, default_value: ""
      };
      if (!this.categories[categoryIndex]) return;
      this.categories[categoryIndex].overrides.push(newOverride);
      this._fullRender();

      var cardEl = this.container.querySelector(
        '.override-manager-card[data-override-id="' + newOverride._id + '"]'
      );
      if (cardEl) {
        cardEl.classList.add("editing");
        cardEl.innerHTML = this._buildEditForm(newOverride);
      }
    },

    saveCard: function(triggerEl) {
      var cardEl = triggerEl.closest(".override-manager-card");
      if (!cardEl) return;
      var form = cardEl.querySelector(".override-manager-edit-form");
      if (!form) return;

      var overrideType = form.querySelector('[name="override_type"]').value;
      var nameEl = form.querySelector('[name="name"]');
      var name = nameEl.value.trim();
      if (!name) {
        nameEl.style.borderColor = "#dc3545";
        nameEl.focus();
        return;
      }

      var strategy = "";
      var group = "";
      var stratEl = form.querySelector('[name="strategy"]');
      var grpEl = form.querySelector('[name="group"]');
      if (stratEl) strategy = stratEl.value;
      if (grpEl) group = grpEl.value;

      // Duplicate validation
      var override = JSON.parse(cardEl.dataset.override);
      if (this._isDuplicateOverride(overrideType, name, strategy, group, override._id)) {
        nameEl.style.borderColor = "#dc3545";
        var errEl = form.querySelector(".override-manager-error");
        if (!errEl) {
          errEl = document.createElement("div");
          errEl.className = "override-manager-error";
          nameEl.parentNode.appendChild(errEl);
        }
        errEl.textContent = "This combination already exists.";
        return;
      }

      override.type = overrideType;
      override.name = name;
      override.strategy = strategy;
      override.group = group;
      override.title = form.querySelector('[name="title"]').value.trim();
      override.description = form.querySelector('[name="description"]').value.trim();
      override.input_type = form.querySelector('[name="input_type"]').value;
      override.default_value = form.querySelector('[name="default_value"]').value.trim();
      override.input_config = this._readConfigFields(form, override.input_type);

      cardEl.dataset.override = JSON.stringify(override);
      this._updateOverrideInCategories(override);
      this._fullRender();
    },

    cancelCardEdit: function(triggerEl) {
      var cardEl = triggerEl.closest(".override-manager-card");
      if (!cardEl) return;
      var override = JSON.parse(cardEl.dataset.override);
      if (!override.name) this._removeOverrideFromCategories(override._id);
      this._fullRender();
    },

    deleteCard: function(overrideId) {
      this._removeOverrideFromCategories(overrideId);
      this._fullRender();
    },

    serialize: function() {
      var result = [];
      for (var i = 0; i < this.categories.length; i++) {
        var cat = this.categories[i];
        var overrides = [];
        for (var j = 0; j < (cat.overrides || []).length; j++) {
          var o = cat.overrides[j];
          overrides.push({
            name: o.name,
            title: o.title || null,
            display_name: o.display_name || o.title || null,
            description: o.description || null,
            type: o.type || "variable",
            strategy: o.strategy || "",
            group: o.group || "",
            input_type: o.input_type || "numeric",
            input_config: o.input_config || {},
            default_value: o.default_value || "",
            overridden_expression: o.overridden_expression || null
          });
        }
        result.push({ name: cat.name, general: cat.general || false, overrides: overrides });
      }
      return result;
    },

    apply: function() {
      Shiny.setInputValue(
        this.inputId + "-manager_state",
        JSON.stringify(this.serialize()),
        { priority: "event" }
      );
    },

    cancel: function() {
      Shiny.setInputValue(
        this.inputId + "-manager_cancel",
        Date.now(),
        { priority: "event" }
      );
    },

    // ---- Edit form helpers ----

    _buildEditForm: function(override) {
      var inputType = override.input_type || "numeric";
      var configHtml = this._buildConfigFields(inputType, override.input_config || {});
      var oType = override.type || "variable";
      var meta = this.modelMeta || { variables: [], settings: [], strategies: {}, groups: {} };

      // Build name dropdown using smart filtering
      var availableNames = this._getAvailableNames(oType, override._id);
      // Ensure the current override's own name is included when editing
      if (override.name && availableNames.indexOf(override.name) === -1) {
        availableNames.unshift(override.name);
      }
      var nameOptsHtml = '<option value="">-- Select --</option>';
      for (var i = 0; i < availableNames.length; i++) {
        nameOptsHtml += '<option value="' + this._esc(availableNames[i]) + '"' +
          (availableNames[i] === override.name ? " selected" : "") + ">" +
          this._esc(availableNames[i]) + "</option>";
      }

      // Build strategy/group dropdowns based on selected variable's metadata
      var stratHtml = "";
      var grpHtml = "";
      var showStrategy = false;
      var showGroup = false;

      if (oType === "variable" && override.name) {
        var vm = this._getVarMeta(override.name);
        if (vm) {
          var unclaimed = this._getUnclaimedTargets(override.name, override._id);

          if (vm.strategies && vm.strategies.length > 0) {
            showStrategy = true;
            var strats = unclaimed.strategies;
            // Include current override's own strategy even if claimed
            if (override.strategy && strats.indexOf(override.strategy) === -1) {
              strats = [override.strategy].concat(strats);
            }
            for (var s = 0; s < strats.length; s++) {
              stratHtml += '<option value="' + this._esc(strats[s]) + '"' +
                (strats[s] === (override.strategy || "") ? " selected" : "") + ">" +
                this._esc(meta.strategies[strats[s]] || strats[s]) + "</option>";
            }
          }

          if (vm.groups && vm.groups.length > 0) {
            showGroup = true;
            var grps = unclaimed.groups;
            // Include current override's own group even if claimed
            if (override.group && grps.indexOf(override.group) === -1) {
              grps = [override.group].concat(grps);
            }
            for (var g = 0; g < grps.length; g++) {
              grpHtml += '<option value="' + this._esc(grps[g]) + '"' +
                (grps[g] === (override.group || "") ? " selected" : "") + ">" +
                this._esc(meta.groups[grps[g]] || grps[g]) + "</option>";
            }
          }
        }
      }

      var stratStyle = showStrategy ? "" : ' style="display:none"';
      var grpStyle = showGroup ? "" : ' style="display:none"';

      return (
        '<div class="override-manager-edit-form">' +
          '<div class="override-manager-field-group">' +
            '<label>Override Type</label>' +
            '<select name="override_type" class="override-manager-override-type">' +
              '<option value="variable"' + (oType === "variable" ? " selected" : "") + '>Variable</option>' +
              '<option value="setting"' + (oType === "setting" ? " selected" : "") + '>Setting</option>' +
            "</select>" +
          "</div>" +
          '<div class="override-manager-field-group">' +
            '<label>Name</label>' +
            '<select name="name" class="override-manager-name-select">' + nameOptsHtml + "</select>" +
          "</div>" +
          '<div class="override-manager-field-group override-manager-targeting override-manager-strategy-field"' + stratStyle + '>' +
            '<label>Strategy</label>' +
            '<select name="strategy">' + stratHtml + "</select>" +
          "</div>" +
          '<div class="override-manager-field-group override-manager-targeting override-manager-group-field"' + grpStyle + '>' +
            '<label>Group</label>' +
            '<select name="group">' + grpHtml + "</select>" +
          "</div>" +
          '<div class="override-manager-field-group">' +
            '<label>Title</label>' +
            '<input type="text" name="title" value="' + this._esc(override.title || "") +
            '" placeholder="Display title" />' +
          "</div>" +
          '<div class="override-manager-field-group">' +
            '<label>Description</label>' +
            '<textarea name="description" placeholder="Optional description">' +
            this._esc(override.description || "") + "</textarea>" +
          "</div>" +
          '<div class="override-manager-field-group">' +
            '<label>Input Type</label>' +
            '<select name="input_type" class="override-manager-input-type">' +
              this._opt("numeric", inputType) +
              this._opt("slider", inputType) +
              this._opt("dropdown", inputType) +
              this._opt("formula", inputType) +
              this._opt("timeframe", inputType) +
            "</select>" +
          "</div>" +
          '<div class="override-manager-field-group">' +
            '<label>Default Value</label>' +
            '<input type="text" name="default_value" value="' +
            this._esc(override.default_value != null ? String(override.default_value) : "") +
            '" placeholder="Default value or expression" />' +
          "</div>" +
          '<div class="override-manager-config-fields">' + configHtml + "</div>" +
          '<div class="override-manager-edit-actions">' +
            '<button type="button" class="btn-cancel" data-action="cancel-card-edit">Cancel</button>' +
            '<button type="button" class="btn-save" data-action="save-card">Save</button>' +
          "</div>" +
        "</div>"
      );
    },

    _opt: function(value, selected) {
      var label = value.charAt(0).toUpperCase() + value.slice(1);
      return '<option value="' + value + '"' +
        (value === selected ? " selected" : "") + ">" + label + "</option>";
    },

    _buildConfigFields: function(inputType, config) {
      config = config || {};
      switch (inputType) {
        case "numeric":
          return this._numField("Min", "config_min", config.min) +
                 this._numField("Max", "config_max", config.max);
        case "slider":
          return this._numField("Min", "config_min", config.min != null ? config.min : 0) +
                 this._numField("Max", "config_max", config.max != null ? config.max : 1) +
                 this._numField("Step Size", "config_step_size", config.step_size != null ? config.step_size : 0.01);
        case "dropdown":
          var options = config.options || [];
          if (!Array.isArray(options)) options = Object.values(options);
          var html = '<div class="override-manager-field-group"><label>Options</label>' +
            '<div class="override-manager-options-list">';
          for (var i = 0; i < options.length; i++) {
            html += '<div class="override-manager-option-row">' +
              '<input type="text" name="config_option" value="' + this._esc(String(options[i])) + '" />' +
              '<button type="button" data-action="remove-option" title="Remove">&times;</button></div>';
          }
          html += '</div><button type="button" class="override-manager-add-option-btn" ' +
            'data-action="add-option">+ Add Option</button></div>';
          return html;
        case "timeframe":
          var allUnits = ["day", "week", "month", "year"];
          var sel = config.units || allUnits;
          var html2 = '<div class="override-manager-field-group"><label>Allowed Units</label>' +
            '<div class="override-manager-units-list">';
          for (var u = 0; u < allUnits.length; u++) {
            html2 += '<label><input type="checkbox" name="config_unit" value="' +
              allUnits[u] + '"' + (sel.indexOf(allUnits[u]) >= 0 ? " checked" : "") +
              " /> " + allUnits[u] + "</label>";
          }
          html2 += "</div></div>";
          return html2;
        default:
          return "";
      }
    },

    _numField: function(label, name, value) {
      return '<div class="override-manager-field-group"><label>' + label +
        '</label><input type="number" name="' + name + '" value="' +
        this._esc(value != null ? String(value) : "") + '" step="any" /></div>';
    },

    _updateConfigFields: function(selectEl) {
      var form = selectEl.closest(".override-manager-edit-form");
      if (!form) return;
      var c = form.querySelector(".override-manager-config-fields");
      if (c) c.innerHTML = this._buildConfigFields(selectEl.value, {});
    },

    _readConfigFields: function(form, inputType) {
      var config = {};
      var getNum = function(name) {
        var el = form.querySelector('[name="' + name + '"]');
        return (el && el.value !== "") ? parseFloat(el.value) : null;
      };

      switch (inputType) {
        case "numeric":
          var min = getNum("config_min"); if (min !== null) config.min = min;
          var max = getNum("config_max"); if (max !== null) config.max = max;
          break;
        case "slider":
          var sMin = getNum("config_min"); if (sMin !== null) config.min = sMin;
          var sMax = getNum("config_max"); if (sMax !== null) config.max = sMax;
          var step = getNum("config_step_size"); if (step !== null) config.step_size = step;
          break;
        case "dropdown":
          var opts = [];
          form.querySelectorAll('[name="config_option"]').forEach(function(el) {
            var v = el.value.trim(); if (v) opts.push(v);
          });
          config.options = opts;
          break;
        case "timeframe":
          var units = [];
          form.querySelectorAll('[name="config_unit"]:checked').forEach(function(el) {
            units.push(el.value);
          });
          if (units.length > 0) config.units = units;
          break;
      }
      return config;
    },

    _addOptionRow: function(btn) {
      var list = btn.previousElementSibling;
      if (!list) return;
      var row = document.createElement("div");
      row.className = "override-manager-option-row";
      row.innerHTML =
        '<input type="text" name="config_option" value="" placeholder="Option value" />' +
        '<button type="button" data-action="remove-option" title="Remove">&times;</button>';
      list.appendChild(row);
      row.querySelector("input").focus();
    },

    _removeOptionRow: function(btn) {
      var row = btn.closest(".override-manager-option-row");
      if (row) row.remove();
    },

    // ---- Smart filtering helpers ----

    _getVarMeta: function(varName) {
      var vars = (this.modelMeta || {}).variables || [];
      for (var i = 0; i < vars.length; i++) {
        if (vars[i].name === varName) return vars[i];
      }
      return null;
    },

    _getClaimedOverrides: function(excludeId) {
      var claimed = [];
      for (var i = 0; i < this.categories.length; i++) {
        var ovs = this.categories[i].overrides || [];
        for (var j = 0; j < ovs.length; j++) {
          var o = ovs[j];
          if (o._id === excludeId) continue;
          if (!o.name) continue;
          claimed.push({
            type: o.type || "variable",
            name: o.name,
            strategy: o.strategy || "",
            group: o.group || ""
          });
        }
      }
      return claimed;
    },

    _getAvailableNames: function(type, excludeId) {
      var meta = this.modelMeta || { variables: [], settings: [] };
      var claimed = this._getClaimedOverrides(excludeId);

      if (type === "setting") {
        return meta.settings.filter(function(s) {
          for (var i = 0; i < claimed.length; i++) {
            if (claimed[i].type === "setting" && claimed[i].name === s) return false;
          }
          return true;
        });
      }

      // type === "variable"
      var vars = meta.variables || [];
      var available = [];
      for (var v = 0; v < vars.length; v++) {
        var vm = vars[v];
        var hasStrats = vm.strategies && vm.strategies.length > 0;
        var hasGroups = vm.groups && vm.groups.length > 0;

        if (!hasStrats && !hasGroups) {
          // Non-specific: available if no override with that name exists
          var taken = false;
          for (var c = 0; c < claimed.length; c++) {
            if (claimed[c].type === "variable" && claimed[c].name === vm.name) {
              taken = true; break;
            }
          }
          if (!taken) available.push(vm.name);
        } else {
          // Has targeting: available if at least one combo is unclaimed
          var unclaimed = this._getUnclaimedTargets(vm.name, excludeId);
          if (unclaimed.strategies.length > 0 || unclaimed.groups.length > 0 ||
              (!hasStrats && !hasGroups)) {
            available.push(vm.name);
          }
        }
      }
      return available;
    },

    _getUnclaimedTargets: function(varName, excludeId) {
      var vm = this._getVarMeta(varName);
      if (!vm) return { strategies: [], groups: [] };

      var claimed = this._getClaimedOverrides(excludeId);
      var varClaimed = claimed.filter(function(c) {
        return c.type === "variable" && c.name === varName;
      });

      var allStrats = (vm.strategies || []).slice();
      var allGroups = (vm.groups || []).slice();

      var unclaimedStrats = allStrats.filter(function(s) {
        for (var i = 0; i < varClaimed.length; i++) {
          if (varClaimed[i].strategy === s) return false;
        }
        return true;
      });

      var unclaimedGroups = allGroups.filter(function(g) {
        for (var i = 0; i < varClaimed.length; i++) {
          if (varClaimed[i].group === g) return false;
        }
        return true;
      });

      return { strategies: unclaimedStrats, groups: unclaimedGroups };
    },

    // ---- Data helpers ----

    _updateOverrideInCategories: function(override) {
      for (var i = 0; i < this.categories.length; i++) {
        var ovs = this.categories[i].overrides || [];
        for (var j = 0; j < ovs.length; j++) {
          if (ovs[j]._id === override._id) { ovs[j] = override; return; }
        }
      }
    },

    _removeOverrideFromCategories: function(id) {
      for (var i = 0; i < this.categories.length; i++) {
        var ovs = this.categories[i].overrides || [];
        for (var j = 0; j < ovs.length; j++) {
          if (ovs[j]._id === id) { ovs.splice(j, 1); return; }
        }
      }
    },

    _closeAllMenus: function() {
      if (!this.container) return;
      var menus = this.container.querySelectorAll(".override-manager-card-menu.show");
      for (var i = 0; i < menus.length; i++) menus[i].classList.remove("show");
    },

    _toggleMenu: function(overrideId) {
      var menu = this.container.querySelector(
        '.override-manager-card-menu[data-override-id="' + overrideId + '"]'
      );
      if (!menu) return;
      var wasOpen = menu.classList.contains("show");
      this._closeAllMenus();
      if (!wasOpen) menu.classList.add("show");
    },

    _isDuplicateOverride: function(type, name, strategy, group, excludeId) {
      for (var i = 0; i < this.categories.length; i++) {
        var ovs = this.categories[i].overrides || [];
        for (var j = 0; j < ovs.length; j++) {
          var o = ovs[j];
          if (o._id === excludeId) continue;
          if ((o.type || "variable") === type &&
              o.name === name &&
              (o.strategy || "") === strategy &&
              (o.group || "") === group) {
            return true;
          }
        }
      }
      return false;
    },

    _getStrategyLabel: function(key) {
      var meta = this.modelMeta || { strategies: {} };
      return meta.strategies[key] || key;
    },

    _getGroupLabel: function(key) {
      var meta = this.modelMeta || { groups: {} };
      return meta.groups[key] || key;
    },

    _updateOverrideType: function(selectEl) {
      var form = selectEl.closest(".override-manager-edit-form");
      if (!form) return;
      var newType = selectEl.value;
      var cardEl = form.closest(".override-manager-card");
      var override = cardEl ? JSON.parse(cardEl.dataset.override) : { _id: 0 };

      // Rebuild name dropdown with smart filtering
      var nameEl = form.querySelector('[name="name"]');
      var availableNames = this._getAvailableNames(newType, override._id);
      var html = '<option value="">-- Select --</option>';
      for (var i = 0; i < availableNames.length; i++) {
        html += '<option value="' + this._esc(availableNames[i]) + '">' +
          this._esc(availableNames[i]) + '</option>';
      }
      nameEl.innerHTML = html;

      // Hide strategy and group (will be rebuilt when name is selected)
      var stratField = form.querySelector(".override-manager-strategy-field");
      var grpField = form.querySelector(".override-manager-group-field");
      if (stratField) stratField.style.display = "none";
      if (grpField) grpField.style.display = "none";

      // Reset strategy and group
      var stratEl = form.querySelector('[name="strategy"]');
      var grpEl = form.querySelector('[name="group"]');
      if (stratEl) { stratEl.innerHTML = ""; stratEl.value = ""; }
      if (grpEl) { grpEl.innerHTML = ""; grpEl.value = ""; }
    },

    _onNameChange: function(selectEl) {
      var form = selectEl.closest(".override-manager-edit-form");
      if (!form) return;
      var selectedName = selectEl.value;
      var typeEl = form.querySelector('[name="override_type"]');
      var oType = typeEl ? typeEl.value : "variable";
      var meta = this.modelMeta || { variables: [], settings: [], strategies: {}, groups: {} };
      var cardEl = form.closest(".override-manager-card");
      var override = cardEl ? JSON.parse(cardEl.dataset.override) : { _id: 0 };

      var stratField = form.querySelector(".override-manager-strategy-field");
      var grpField = form.querySelector(".override-manager-group-field");
      var stratEl = form.querySelector('[name="strategy"]');
      var grpEl = form.querySelector('[name="group"]');

      // Settings never have targeting
      if (oType === "setting" || !selectedName) {
        if (stratField) stratField.style.display = "none";
        if (grpField) grpField.style.display = "none";
        if (stratEl) { stratEl.innerHTML = ""; stratEl.value = ""; }
        if (grpEl) { grpEl.innerHTML = ""; grpEl.value = ""; }
        return;
      }

      // Look up variable metadata
      var vm = this._getVarMeta(selectedName);
      if (!vm) {
        if (stratField) stratField.style.display = "none";
        if (grpField) grpField.style.display = "none";
        return;
      }

      var unclaimed = this._getUnclaimedTargets(selectedName, override._id);

      // Strategy dropdown
      if (vm.strategies && vm.strategies.length > 0) {
        if (stratField) stratField.style.display = "";
        var strats = unclaimed.strategies;
        var stratHtml = "";
        for (var s = 0; s < strats.length; s++) {
          stratHtml += '<option value="' + this._esc(strats[s]) + '">' +
            this._esc(meta.strategies[strats[s]] || strats[s]) + "</option>";
        }
        if (stratEl) stratEl.innerHTML = stratHtml;
      } else {
        if (stratField) stratField.style.display = "none";
        if (stratEl) { stratEl.innerHTML = ""; stratEl.value = ""; }
      }

      // Group dropdown
      if (vm.groups && vm.groups.length > 0) {
        if (grpField) grpField.style.display = "";
        var grps = unclaimed.groups;
        var grpHtml = "";
        for (var g = 0; g < grps.length; g++) {
          grpHtml += '<option value="' + this._esc(grps[g]) + '">' +
            this._esc(meta.groups[grps[g]] || grps[g]) + "</option>";
        }
        if (grpEl) grpEl.innerHTML = grpHtml;
      } else {
        if (grpField) grpField.style.display = "none";
        if (grpEl) { grpEl.innerHTML = ""; grpEl.value = ""; }
      }
    },

    _esc: function(str) {
      if (str == null) return "";
      return String(str).replace(/&/g, "&amp;").replace(/"/g, "&quot;")
        .replace(/</g, "&lt;").replace(/>/g, "&gt;");
    }
  };

  // =========================================================================
  // Global event delegation using jQuery (proven to work in this codebase).
  // Uses $(document).on() so it works regardless of when elements are created.
  // =========================================================================
  function setupGlobalHandlers() {
    // Click delegation for all manager actions
    $(document).on("click", "[data-action]", function(e) {
      // Only handle actions inside the manager modal
      if (!$(this).closest(".override-manager-body, .override-manager-footer").length &&
          !$(this).is('[data-action="manager-apply"], [data-action="manager-cancel"]')) {
        return;
      }

      var action = $(this).data("action");
      if (!action) return;

      // Close menus on any click that isn't a menu toggle
      if (action !== "toggle-menu") {
        OverrideManager._closeAllMenus();
      }

      switch (action) {
        case "toggle-menu":
          OverrideManager._toggleMenu($(this).data("override-id"));
          break;
        case "edit-card":
          OverrideManager.editCard(parseInt($(this).data("override-id"), 10));
          break;
        case "delete-card":
          OverrideManager.deleteCard(parseInt($(this).data("override-id"), 10));
          break;
        case "add-card":
          OverrideManager.addCard(parseInt($(this).data("category-index"), 10));
          break;
        case "add-category":
          OverrideManager.addCategory();
          break;
        case "delete-category":
          OverrideManager.deleteCategory(parseInt($(this).data("category-index"), 10));
          break;
        case "rename-category":
          OverrideManager.renameCategory(parseInt($(this).data("category-index"), 10), this);
          break;
        case "save-card":
          OverrideManager.saveCard(this);
          break;
        case "cancel-card-edit":
          OverrideManager.cancelCardEdit(this);
          break;
        case "add-option":
          OverrideManager._addOptionRow(this);
          break;
        case "remove-option":
          OverrideManager._removeOptionRow(this);
          break;
        case "manager-apply":
          OverrideManager.apply();
          break;
        case "manager-cancel":
          OverrideManager.cancel();
          break;
      }
    });

    // Close menus when clicking outside
    $(document).on("click", ".modal", function(e) {
      if (!$(e.target).closest(".override-manager-card-menu, .override-manager-card-menu-btn").length) {
        OverrideManager._closeAllMenus();
      }
    });

    // Input type change
    $(document).on("change", ".override-manager-input-type", function() {
      OverrideManager._updateConfigFields(this);
    });

    // Override type (variable/setting) change
    $(document).on("change", ".override-manager-override-type", function() {
      OverrideManager._updateOverrideType(this);
    });

    // Name dropdown change
    $(document).on("change", ".override-manager-name-select", function() {
      OverrideManager._onNameChange(this);
    });
  }

  // =========================================================================
  // Register handlers
  // =========================================================================
  function waitForShiny() {
    return new Promise(function(resolve) {
      var check = function() {
        if (typeof Shiny !== "undefined" && Shiny.addCustomMessageHandler) {
          resolve();
        } else {
          setTimeout(check, 50);
        }
      };
      check();
    });
  }

  waitForShiny().then(function() {
    // Set up global jQuery handlers once
    setupGlobalHandlers();

    Shiny.addCustomMessageHandler("override-manager-init", function(data) {
      OverrideManager.init(data);
    });
    console.log("[OverrideManager] handler registered");
  });
})();
