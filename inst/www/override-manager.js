// Override Manager - Kanban-style CRUD management for override categories
// Each CRUD operation dispatches to R individually; JS re-renders from R state.

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
    _editingState: null, // track in-progress edit form {categoryIndex, override, isNew}

    init: function(data) {
      console.log("[OverrideManager] init called");
      this.inputId = data.inputId;
      this.categories = JSON.parse(JSON.stringify(data.categories || []));
      this.modelMeta = data.model_meta || {
        variables: [], settings: [], strategies: {}, groups: {}
      };
      this._nextId = 1;
      this._editingState = null;

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

    // Called when R sends updated state (reactive subscription)
    update: function(data) {
      console.log("[OverrideManager] update from R");
      this.categories = JSON.parse(JSON.stringify(data.categories || []));
      this.modelMeta = data.model_meta || this.modelMeta;
      this._nextId = 1;

      for (var i = 0; i < this.categories.length; i++) {
        var cat = this.categories[i];
        if (!cat.overrides) cat.overrides = [];
        for (var j = 0; j < cat.overrides.length; j++) {
          cat.overrides[j]._id = this._nextId++;
        }
      }

      if (this.container) {
        this._fullRender();
      }
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
      this.container.textContent = "";

      for (var i = 0; i < this.categories.length; i++) {
        this.container.appendChild(this._buildColumn(this.categories[i], i));
      }

      // Add Category button
      var addCol = document.createElement("div");
      addCol.className = "override-manager-add-column";
      var addBtn = document.createElement("button");
      addBtn.type = "button";
      addBtn.className = "override-manager-add-column-btn";
      addBtn.dataset.action = "add-category";
      addBtn.textContent = "+ Add Category";
      addCol.appendChild(addBtn);
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
      var delBtn = document.createElement("button");
      delBtn.type = "button";
      delBtn.dataset.action = "delete-category";
      delBtn.dataset.categoryIndex = index;
      delBtn.title = "Delete category";
      delBtn.textContent = "\u00D7";
      actions.appendChild(delBtn);

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
      menuBtn.textContent = "\u2026";

      var menu = document.createElement("div");
      menu.className = "override-manager-card-menu";
      menu.dataset.overrideId = override._id;

      var editBtn = document.createElement("button");
      editBtn.type = "button";
      editBtn.dataset.action = "edit-card";
      editBtn.dataset.overrideId = override._id;
      editBtn.textContent = "Edit";

      var deleteBtn = document.createElement("button");
      deleteBtn.type = "button";
      deleteBtn.className = "danger";
      deleteBtn.dataset.action = "delete-card";
      deleteBtn.dataset.overrideId = override._id;
      deleteBtn.textContent = "Delete";

      menu.appendChild(editBtn);
      menu.appendChild(deleteBtn);

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

      // Send reorder to R
      Shiny.setInputValue(
        this.inputId + "-reorder_overrides",
        JSON.stringify(this.serialize()),
        { priority: "event" }
      );
    },

    // ---- Form validation ----

    _clearFieldErrors: function(form) {
      var errors = form.querySelectorAll(".override-manager-error");
      for (var i = 0; i < errors.length; i++) errors[i].remove();
      var inputs = form.querySelectorAll("input, select, textarea");
      for (var j = 0; j < inputs.length; j++) inputs[j].style.borderColor = "";
    },

    _showFieldError: function(el, message) {
      if (!el) return;
      // Set red border on input/select elements
      if (el.tagName === "INPUT" || el.tagName === "SELECT" || el.tagName === "TEXTAREA") {
        el.style.borderColor = "#dc3545";
      }
      var parent = el.closest(".override-manager-field-group");
      if (!parent) parent = el.parentNode;
      var errDiv = document.createElement("div");
      errDiv.className = "override-manager-error";
      errDiv.textContent = message;
      parent.appendChild(errDiv);
    },

    _validateCard: function(form, inputType) {
      var valid = true;

      // Title required
      var titleEl = form.querySelector('[name="title"]');
      if (titleEl && !titleEl.value.trim()) {
        this._showFieldError(titleEl, "Title is required.");
        valid = false;
      }

      // Numeric: default_value must parse as number if provided
      if (inputType === "numeric") {
        var defEl = form.querySelector('[name="default_value"]');
        if (defEl && defEl.value.trim() !== "" && isNaN(parseFloat(defEl.value))) {
          this._showFieldError(defEl, "Must be a valid number.");
          valid = false;
        }
        var minEl = form.querySelector('[name="config_min"]');
        var maxEl = form.querySelector('[name="config_max"]');
        if (minEl && maxEl && minEl.value.trim() !== "" && maxEl.value.trim() !== "") {
          if (parseFloat(minEl.value) >= parseFloat(maxEl.value)) {
            this._showFieldError(maxEl, "Max must be greater than min.");
            valid = false;
          }
        }
      }

      // Slider: min, max, step_size, default_value all required and valid
      if (inputType === "slider") {
        var sMinEl = form.querySelector('[name="config_min"]');
        var sMaxEl = form.querySelector('[name="config_max"]');
        var stepEl = form.querySelector('[name="config_step_size"]');
        var sDefEl = form.querySelector('[name="default_value"]');

        if (!sMinEl || sMinEl.value.trim() === "" || isNaN(parseFloat(sMinEl.value))) {
          this._showFieldError(sMinEl, "Min is required and must be a number.");
          valid = false;
        }
        if (!sMaxEl || sMaxEl.value.trim() === "" || isNaN(parseFloat(sMaxEl.value))) {
          this._showFieldError(sMaxEl, "Max is required and must be a number.");
          valid = false;
        }
        if (sMinEl && sMaxEl && sMinEl.value.trim() !== "" && sMaxEl.value.trim() !== "" &&
            !isNaN(parseFloat(sMinEl.value)) && !isNaN(parseFloat(sMaxEl.value))) {
          if (parseFloat(sMinEl.value) >= parseFloat(sMaxEl.value)) {
            this._showFieldError(sMaxEl, "Max must be greater than min.");
            valid = false;
          }
        }
        if (!stepEl || stepEl.value.trim() === "" || isNaN(parseFloat(stepEl.value)) ||
            parseFloat(stepEl.value) <= 0) {
          this._showFieldError(stepEl, "Step size must be a positive number.");
          valid = false;
        }
        if (!sDefEl || sDefEl.value.trim() === "" || isNaN(parseFloat(sDefEl.value))) {
          this._showFieldError(sDefEl, "Default value is required and must be a number.");
          valid = false;
        } else if (sMinEl && sMaxEl &&
                   !isNaN(parseFloat(sMinEl.value)) && !isNaN(parseFloat(sMaxEl.value))) {
          var defVal = parseFloat(sDefEl.value);
          if (defVal < parseFloat(sMinEl.value) || defVal > parseFloat(sMaxEl.value)) {
            this._showFieldError(sDefEl, "Default must be between min and max.");
            valid = false;
          }
        }
      }

      // Dropdown: at least 1 non-empty option
      if (inputType === "dropdown") {
        var optInputs = form.querySelectorAll('[name="config_option"]');
        var hasOption = false;
        for (var i = 0; i < optInputs.length; i++) {
          if (optInputs[i].value.trim() !== "") { hasOption = true; break; }
        }
        if (!hasOption) {
          var optContainer = form.querySelector(".override-manager-options-list");
          if (optContainer) {
            this._showFieldError(optContainer, "At least one option is required.");
          }
          valid = false;
        }
      }

      // Timeframe: at least 1 checked unit
      if (inputType === "timeframe") {
        var checked = form.querySelectorAll('[name="config_unit"]:checked');
        if (checked.length === 0) {
          var unitContainer = form.querySelector(".override-manager-units-list");
          if (unitContainer) {
            this._showFieldError(unitContainer, "At least one unit must be selected.");
          }
          valid = false;
        }
      }

      return valid;
    },

    // ---- CRUD operations ----

    addCategory: function() {
      // Send to R -- do NOT update local state
      Shiny.setInputValue(
        this.inputId + "-add_category",
        { name: "New Category", general: false },
        { priority: "event" }
      );
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
        if (newName && newName !== currentName) {
          // Send rename to R
          Shiny.setInputValue(
            self.inputId + "-edit_category",
            { name: currentName, new_name: newName },
            { priority: "event" }
          );
        } else {
          // No change -- just re-render
          self._fullRender();
        }
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

      // Send to R -- do NOT splice locally
      Shiny.setInputValue(
        this.inputId + "-remove_category",
        { name: cat.name },
        { priority: "event" }
      );
    },

    editCard: function(overrideId) {
      var cardEl = this.container.querySelector(
        '.override-manager-card[data-override-id="' + overrideId + '"]'
      );
      if (!cardEl) return;
      var override = JSON.parse(cardEl.dataset.override);

      // Find which category this override belongs to
      var catIndex = this._findCategoryIndex(overrideId);

      this._editingState = {
        categoryIndex: catIndex,
        override: override,
        isNew: false
      };

      cardEl.classList.add("editing");
      this._renderEditForm(cardEl, override);
    },

    addCard: function(categoryIndex) {
      // Show blank edit form inline -- local UI only, do NOT push to this.categories
      var newOverride = {
        _id: this._nextId++,
        name: "", title: "", description: "",
        type: "variable", strategy: "", group: "",
        input_type: "numeric", input_config: {}, default_value: ""
      };

      this._editingState = {
        categoryIndex: categoryIndex,
        override: newOverride,
        isNew: true
      };

      // Temporarily add to categories for rendering
      if (!this.categories[categoryIndex]) return;
      this.categories[categoryIndex].overrides.push(newOverride);
      this._fullRender();

      var cardEl = this.container.querySelector(
        '.override-manager-card[data-override-id="' + newOverride._id + '"]'
      );
      if (cardEl) {
        cardEl.classList.add("editing");
        this._renderEditForm(cardEl, newOverride);
      }
    },

    _renderEditForm: function(cardEl, override) {
      // Build the edit form using DOM methods
      var form = this._buildEditFormDOM(override);
      cardEl.textContent = "";
      cardEl.appendChild(form);
    },

    _buildEditFormDOM: function(override) {
      var inputType = override.input_type || "numeric";
      var oType = override.type || "variable";
      var meta = this.modelMeta || { variables: [], settings: [], strategies: {}, groups: {} };
      var self = this;

      var form = document.createElement("div");
      form.className = "override-manager-edit-form";

      // Override Type field
      var typeGroup = this._createFieldGroup("Override Type");
      var typeSelect = document.createElement("select");
      typeSelect.name = "override_type";
      typeSelect.className = "override-manager-override-type";
      this._addOption(typeSelect, "variable", "Variable", oType === "variable");
      this._addOption(typeSelect, "setting", "Setting", oType === "setting");
      typeGroup.appendChild(typeSelect);
      form.appendChild(typeGroup);

      // Name field
      var nameGroup = this._createFieldGroup("Name");
      var nameSelect = document.createElement("select");
      nameSelect.name = "name";
      nameSelect.className = "override-manager-name-select";
      var availableNames = this._getAvailableNames(oType, override._id);
      if (override.name && availableNames.indexOf(override.name) === -1) {
        availableNames.unshift(override.name);
      }
      this._addOption(nameSelect, "", "-- Select --", !override.name);
      for (var i = 0; i < availableNames.length; i++) {
        this._addOption(nameSelect, availableNames[i], availableNames[i], availableNames[i] === override.name);
      }
      nameGroup.appendChild(nameSelect);
      form.appendChild(nameGroup);

      // Base formula display (hidden by default)
      var formulaGroup = document.createElement("div");
      formulaGroup.className = "override-manager-field-group override-manager-base-formula";
      formulaGroup.style.display = "none";
      var formulaLabel = document.createElement("label");
      formulaLabel.textContent = "Base Case";
      var formulaCode = document.createElement("code");
      formulaCode.className = "override-manager-formula-display";
      formulaGroup.appendChild(formulaLabel);
      formulaGroup.appendChild(formulaCode);
      form.appendChild(formulaGroup);

      // Strategy field
      var stratGroup = document.createElement("div");
      stratGroup.className = "override-manager-field-group override-manager-targeting override-manager-strategy-field";
      var stratLabel = document.createElement("label");
      stratLabel.textContent = "Strategy";
      var stratSelect = document.createElement("select");
      stratSelect.name = "strategy";
      stratGroup.appendChild(stratLabel);
      stratGroup.appendChild(stratSelect);

      // Group field
      var grpGroup = document.createElement("div");
      grpGroup.className = "override-manager-field-group override-manager-targeting override-manager-group-field";
      var grpLabel = document.createElement("label");
      grpLabel.textContent = "Group";
      var grpSelect = document.createElement("select");
      grpSelect.name = "group";
      grpGroup.appendChild(grpLabel);
      grpGroup.appendChild(grpSelect);

      // Populate strategy/group if applicable
      var showStrategy = false;
      var showGroup = false;
      if (oType === "variable" && override.name) {
        var vm = this._getVarMeta(override.name);
        if (vm) {
          var unclaimed = this._getUnclaimedTargets(override.name, override._id);
          if (vm.strategies && vm.strategies.length > 0) {
            showStrategy = true;
            var strats = unclaimed.strategies;
            if (override.strategy && strats.indexOf(override.strategy) === -1) {
              strats = [override.strategy].concat(strats);
            }
            for (var s = 0; s < strats.length; s++) {
              this._addOption(stratSelect, strats[s], meta.strategies[strats[s]] || strats[s], strats[s] === (override.strategy || ""));
            }
          }
          if (vm.groups && vm.groups.length > 0) {
            showGroup = true;
            var grps = unclaimed.groups;
            if (override.group && grps.indexOf(override.group) === -1) {
              grps = [override.group].concat(grps);
            }
            for (var g = 0; g < grps.length; g++) {
              this._addOption(grpSelect, grps[g], meta.groups[grps[g]] || grps[g], grps[g] === (override.group || ""));
            }
          }
        }
      }
      stratGroup.style.display = showStrategy ? "" : "none";
      grpGroup.style.display = showGroup ? "" : "none";

      form.appendChild(stratGroup);
      form.appendChild(grpGroup);

      // Title field
      var titleGroup = this._createFieldGroup("Title");
      var titleInput = document.createElement("input");
      titleInput.type = "text";
      titleInput.name = "title";
      titleInput.value = override.title || "";
      titleInput.placeholder = "Display title";
      titleGroup.appendChild(titleInput);
      form.appendChild(titleGroup);

      // Description field
      var descGroup = this._createFieldGroup("Description");
      var descArea = document.createElement("textarea");
      descArea.name = "description";
      descArea.placeholder = "Optional description";
      descArea.textContent = override.description || "";
      descGroup.appendChild(descArea);
      form.appendChild(descGroup);

      // Input Type field
      var itGroup = this._createFieldGroup("Input Type");
      var itSelect = document.createElement("select");
      itSelect.name = "input_type";
      itSelect.className = "override-manager-input-type";
      var inputTypes = ["numeric", "slider", "dropdown", "formula", "timeframe"];
      for (var t = 0; t < inputTypes.length; t++) {
        this._addOption(itSelect, inputTypes[t], inputTypes[t].charAt(0).toUpperCase() + inputTypes[t].slice(1), inputTypes[t] === inputType);
      }
      itGroup.appendChild(itSelect);
      form.appendChild(itGroup);

      // Default Value field
      var dvGroup = this._createFieldGroup("Default Value");
      var dvInput = document.createElement("input");
      dvInput.type = "text";
      dvInput.name = "default_value";
      dvInput.value = override.default_value != null ? String(override.default_value) : "";
      dvInput.placeholder = "Default value or expression";
      dvGroup.appendChild(dvInput);
      form.appendChild(dvGroup);

      // Config fields container
      var configDiv = document.createElement("div");
      configDiv.className = "override-manager-config-fields";
      this._buildConfigFieldsDOM(configDiv, inputType, override.input_config || {});
      form.appendChild(configDiv);

      // Action buttons
      var actionsDiv = document.createElement("div");
      actionsDiv.className = "override-manager-edit-actions";
      var cancelBtn = document.createElement("button");
      cancelBtn.type = "button";
      cancelBtn.className = "btn-cancel";
      cancelBtn.dataset.action = "cancel-card-edit";
      cancelBtn.textContent = "Cancel";
      var saveBtn = document.createElement("button");
      saveBtn.type = "button";
      saveBtn.className = "btn-save";
      saveBtn.dataset.action = "save-card";
      saveBtn.textContent = "Save";
      actionsDiv.appendChild(cancelBtn);
      actionsDiv.appendChild(saveBtn);
      form.appendChild(actionsDiv);

      // Update base formula after building the form
      setTimeout(function() { self._updateBaseFormula(form); }, 0);

      return form;
    },

    _createFieldGroup: function(labelText) {
      var group = document.createElement("div");
      group.className = "override-manager-field-group";
      var label = document.createElement("label");
      label.textContent = labelText;
      group.appendChild(label);
      return group;
    },

    _addOption: function(select, value, text, selected) {
      var opt = document.createElement("option");
      opt.value = value;
      opt.textContent = text;
      if (selected) opt.selected = true;
      select.appendChild(opt);
    },

    _buildConfigFieldsDOM: function(container, inputType, config) {
      config = config || {};
      container.textContent = "";

      switch (inputType) {
        case "numeric":
          container.appendChild(this._createNumFieldDOM("Min", "config_min", config.min));
          container.appendChild(this._createNumFieldDOM("Max", "config_max", config.max));
          break;
        case "slider":
          container.appendChild(this._createNumFieldDOM("Min", "config_min", config.min != null ? config.min : 0));
          container.appendChild(this._createNumFieldDOM("Max", "config_max", config.max != null ? config.max : 1));
          container.appendChild(this._createNumFieldDOM("Step Size", "config_step_size", config.step_size != null ? config.step_size : 0.01));
          break;
        case "dropdown":
          var options = config.options || [];
          if (!Array.isArray(options)) options = Object.values(options);
          var optGroup = this._createFieldGroup("Options");
          var optList = document.createElement("div");
          optList.className = "override-manager-options-list";
          for (var i = 0; i < options.length; i++) {
            var optVal = typeof options[i] === "object" ? (options[i].value || options[i].label || "") : String(options[i]);
            optList.appendChild(this._createOptionRowDOM(optVal));
          }
          optGroup.appendChild(optList);
          var addOptBtn = document.createElement("button");
          addOptBtn.type = "button";
          addOptBtn.className = "override-manager-add-option-btn";
          addOptBtn.dataset.action = "add-option";
          addOptBtn.textContent = "+ Add Option";
          optGroup.appendChild(addOptBtn);
          container.appendChild(optGroup);
          break;
        case "timeframe":
          var allUnits = ["day", "week", "month", "year"];
          var sel = config.units || allUnits;
          var unitGroup = this._createFieldGroup("Allowed Units");
          var unitList = document.createElement("div");
          unitList.className = "override-manager-units-list";
          for (var u = 0; u < allUnits.length; u++) {
            var unitLabel = document.createElement("label");
            var cb = document.createElement("input");
            cb.type = "checkbox";
            cb.name = "config_unit";
            cb.value = allUnits[u];
            if (sel.indexOf(allUnits[u]) >= 0) cb.checked = true;
            unitLabel.appendChild(cb);
            unitLabel.appendChild(document.createTextNode(" " + allUnits[u]));
            unitList.appendChild(unitLabel);
          }
          unitGroup.appendChild(unitList);
          container.appendChild(unitGroup);
          break;
      }
    },

    _createNumFieldDOM: function(labelText, name, value) {
      var group = this._createFieldGroup(labelText);
      var input = document.createElement("input");
      input.type = "number";
      input.name = name;
      input.value = value != null ? String(value) : "";
      input.step = "any";
      group.appendChild(input);
      return group;
    },

    _createOptionRowDOM: function(value) {
      var row = document.createElement("div");
      row.className = "override-manager-option-row";
      var input = document.createElement("input");
      input.type = "text";
      input.name = "config_option";
      input.value = value;
      var removeBtn = document.createElement("button");
      removeBtn.type = "button";
      removeBtn.dataset.action = "remove-option";
      removeBtn.title = "Remove";
      removeBtn.textContent = "\u00D7";
      row.appendChild(input);
      row.appendChild(removeBtn);
      return row;
    },

    _updateConfigFields: function(selectEl) {
      var form = selectEl.closest(".override-manager-edit-form");
      if (!form) return;
      var c = form.querySelector(".override-manager-config-fields");
      if (c) this._buildConfigFieldsDOM(c, selectEl.value, {});
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
      var row = this._createOptionRowDOM("");
      list.appendChild(row);
      row.querySelector("input").focus();
    },

    _removeOptionRow: function(btn) {
      var row = btn.closest(".override-manager-option-row");
      if (row) row.remove();
    },

    // ---- Base formula display ----

    _updateBaseFormula: function(form) {
      var formulaDiv = form.querySelector(".override-manager-base-formula");
      var codeEl = form.querySelector(".override-manager-formula-display");
      if (!formulaDiv || !codeEl) return;

      var typeEl = form.querySelector('[name="override_type"]');
      var nameEl = form.querySelector('[name="name"]');
      var oType = typeEl ? typeEl.value : "variable";
      var selectedName = nameEl ? nameEl.value : "";

      if (!selectedName) {
        formulaDiv.style.display = "none";
        return;
      }

      var meta = this.modelMeta || {};

      if (oType === "variable") {
        var vm = this._getVarMeta(selectedName);
        if (vm && vm.formulas) {
          var stratEl = form.querySelector('[name="strategy"]');
          var grpEl = form.querySelector('[name="group"]');
          var strat = stratEl ? stratEl.value : "";
          var grp = grpEl ? grpEl.value : "";
          var key = (strat || "") + "|" + (grp || "");
          var formula = vm.formulas[key];
          // Try fallback keys
          if (!formula) formula = vm.formulas["|"];
          if (!formula) {
            // Try first available formula
            var keys = Object.keys(vm.formulas);
            if (keys.length > 0) formula = vm.formulas[keys[0]];
          }
          if (formula) {
            codeEl.textContent = formula;
            formulaDiv.style.display = "";
            return;
          }
        }
      } else if (oType === "setting") {
        var settingValues = meta.setting_values || [];
        for (var i = 0; i < settingValues.length; i++) {
          if (settingValues[i].name === selectedName) {
            codeEl.textContent = settingValues[i].value;
            formulaDiv.style.display = "";
            return;
          }
        }
      }

      formulaDiv.style.display = "none";
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
          var taken = false;
          for (var c = 0; c < claimed.length; c++) {
            if (claimed[c].type === "variable" && claimed[c].name === vm.name) {
              taken = true; break;
            }
          }
          if (!taken) available.push(vm.name);
        } else {
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

    _findCategoryIndex: function(overrideId) {
      for (var i = 0; i < this.categories.length; i++) {
        var ovs = this.categories[i].overrides || [];
        for (var j = 0; j < ovs.length; j++) {
          if (ovs[j]._id === overrideId) return i;
        }
      }
      return -1;
    },

    _findOverrideInfo: function(overrideId) {
      for (var i = 0; i < this.categories.length; i++) {
        var ovs = this.categories[i].overrides || [];
        for (var j = 0; j < ovs.length; j++) {
          if (ovs[j]._id === overrideId) {
            return {
              categoryName: this.categories[i].name,
              categoryIndex: i,
              override: ovs[j],
              overrideIndex: j
            };
          }
        }
      }
      return null;
    },

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
      nameEl.textContent = "";
      this._addOption(nameEl, "", "-- Select --", true);
      for (var i = 0; i < availableNames.length; i++) {
        this._addOption(nameEl, availableNames[i], availableNames[i], false);
      }

      // Hide strategy and group (will be rebuilt when name is selected)
      var stratField = form.querySelector(".override-manager-strategy-field");
      var grpField = form.querySelector(".override-manager-group-field");
      if (stratField) stratField.style.display = "none";
      if (grpField) grpField.style.display = "none";

      // Reset strategy and group
      var stratEl = form.querySelector('[name="strategy"]');
      var grpEl = form.querySelector('[name="group"]');
      if (stratEl) { stratEl.textContent = ""; stratEl.value = ""; }
      if (grpEl) { grpEl.textContent = ""; grpEl.value = ""; }

      // Update base formula
      this._updateBaseFormula(form);
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
        if (stratEl) { stratEl.textContent = ""; stratEl.value = ""; }
        if (grpEl) { grpEl.textContent = ""; grpEl.value = ""; }
        this._updateBaseFormula(form);
        return;
      }

      // Look up variable metadata
      var vm = this._getVarMeta(selectedName);
      if (!vm) {
        if (stratField) stratField.style.display = "none";
        if (grpField) grpField.style.display = "none";
        this._updateBaseFormula(form);
        return;
      }

      var unclaimed = this._getUnclaimedTargets(selectedName, override._id);

      // Strategy dropdown
      if (vm.strategies && vm.strategies.length > 0) {
        if (stratField) stratField.style.display = "";
        var strats = unclaimed.strategies;
        if (stratEl) {
          stratEl.textContent = "";
          for (var s = 0; s < strats.length; s++) {
            this._addOption(stratEl, strats[s], meta.strategies[strats[s]] || strats[s], s === 0);
          }
        }
      } else {
        if (stratField) stratField.style.display = "none";
        if (stratEl) { stratEl.textContent = ""; stratEl.value = ""; }
      }

      // Group dropdown
      if (vm.groups && vm.groups.length > 0) {
        if (grpField) grpField.style.display = "";
        var grps = unclaimed.groups;
        if (grpEl) {
          grpEl.textContent = "";
          for (var g = 0; g < grps.length; g++) {
            this._addOption(grpEl, grps[g], meta.groups[grps[g]] || grps[g], g === 0);
          }
        }
      } else {
        if (grpField) grpField.style.display = "none";
        if (grpEl) { grpEl.textContent = ""; grpEl.value = ""; }
      }

      // Update base formula
      this._updateBaseFormula(form);
    },

    saveCard: function(triggerEl) {
      var cardEl = triggerEl.closest(".override-manager-card");
      if (!cardEl) return;
      var form = cardEl.querySelector(".override-manager-edit-form");
      if (!form) return;

      // Clear previous errors first
      this._clearFieldErrors(form);

      var overrideType = form.querySelector('[name="override_type"]').value;
      var nameEl = form.querySelector('[name="name"]');
      var name = nameEl.value.trim();
      if (!name) {
        this._showFieldError(nameEl, "Name is required.");
        return;
      }

      var strategy = "";
      var group = "";
      var stratEl = form.querySelector('[name="strategy"]');
      var grpEl = form.querySelector('[name="group"]');
      if (stratEl) strategy = stratEl.value;
      if (grpEl) group = grpEl.value;

      // Duplicate validation (local check)
      var override = JSON.parse(cardEl.dataset.override);
      if (this._isDuplicateOverride(overrideType, name, strategy, group, override._id)) {
        this._showFieldError(nameEl, "This combination already exists.");
        return;
      }

      // Input-type-specific validation
      var inputType = form.querySelector('[name="input_type"]').value;
      if (!this._validateCard(form, inputType)) {
        return;
      }

      var title = form.querySelector('[name="title"]').value.trim();
      var description = form.querySelector('[name="description"]').value.trim();
      var defaultValue = form.querySelector('[name="default_value"]').value.trim();
      var config = this._readConfigFields(form, inputType);

      // Determine category name
      var editing = this._editingState;
      var categoryName = editing ? this.categories[editing.categoryIndex].name : null;
      if (!categoryName) {
        var colEl = cardEl.closest(".override-manager-column");
        if (colEl) {
          var ci = parseInt(colEl.dataset.categoryIndex, 10);
          categoryName = this.categories[ci] ? this.categories[ci].name : null;
        }
      }

      if (editing && editing.isNew) {
        // Remove the temporary local override before sending to R
        this._removeOverrideFromCategories(override._id);
        this._editingState = null;

        // ADD: send add_override to R
        var addData = {
          category: categoryName,
          title: title,
          name: name,
          override_type: overrideType,
          input_type: inputType,
          expression: defaultValue || "0",
          description: description || null,
          strategy: strategy,
          group: group
        };
        if (config.min != null) addData.min = config.min;
        if (config.max != null) addData.max = config.max;
        if (config.step_size != null) addData.step_size = config.step_size;
        if (config.options) addData.options = config.options;

        Shiny.setInputValue(
          this.inputId + "-add_override",
          addData,
          { priority: "event" }
        );
      } else {
        // EDIT: send edit_override to R
        this._editingState = null;

        var editData = {
          category: categoryName,
          override_type: override.type || "variable",
          name: override.name,
          strategy: override.strategy || "",
          group: override.group || ""
        };

        // Only include changed fields
        if (overrideType !== (override.type || "variable")) editData.new_type = overrideType;
        if (name !== override.name) editData.new_name = name;
        if (strategy !== (override.strategy || "")) editData.new_strategy = strategy;
        if (group !== (override.group || "")) editData.new_group = group;
        editData.title = title;
        editData.description = description || null;
        editData.expression = defaultValue || "0";
        editData.input_type = inputType;
        if (config.min != null) editData.min = config.min;
        if (config.max != null) editData.max = config.max;
        if (config.step_size != null) editData.step_size = config.step_size;
        if (config.options) editData.options = config.options;

        Shiny.setInputValue(
          this.inputId + "-edit_override",
          editData,
          { priority: "event" }
        );
      }
    },

    cancelCardEdit: function(triggerEl) {
      var cardEl = triggerEl.closest(".override-manager-card");
      if (!cardEl) return;
      var override = JSON.parse(cardEl.dataset.override);
      var editing = this._editingState;

      if (editing && editing.isNew) {
        // Remove the temporary override we added locally
        this._removeOverrideFromCategories(override._id);
      }

      this._editingState = null;
      this._fullRender();
    },

    deleteCard: function(overrideId) {
      // Find the override data and its category
      var info = this._findOverrideInfo(overrideId);
      if (!info) return;

      // Send to R -- do NOT splice locally
      Shiny.setInputValue(
        this.inputId + "-remove_override",
        {
          category: info.categoryName,
          override_type: info.override.type || "variable",
          name: info.override.name,
          strategy: info.override.strategy || "",
          group: info.override.group || ""
        },
        { priority: "event" }
      );
    },

    close: function() {
      Shiny.setInputValue(
        this.inputId + "-manager_close",
        Date.now(),
        { priority: "event" }
      );
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

    _esc: function(str) {
      if (str == null) return "";
      return String(str).replace(/&/g, "&amp;").replace(/"/g, "&quot;")
        .replace(/</g, "&lt;").replace(/>/g, "&gt;");
    },

    showError: function(message) {
      if (!this.container) return;
      // Remove existing error banner
      var existing = this.container.querySelector(".override-manager-error-banner");
      if (existing) existing.remove();

      var banner = document.createElement("div");
      banner.className = "override-manager-error-banner";
      banner.textContent = message;
      this.container.insertBefore(banner, this.container.firstChild);

      // Auto-dismiss after 6 seconds
      setTimeout(function() {
        if (banner.parentNode) banner.remove();
      }, 6000);
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
          !$(this).is('[data-action="manager-close"]')) {
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
        case "manager-close":
          OverrideManager.close();
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

    // Strategy/group change -- update base formula
    $(document).on("change", ".override-manager-strategy-field select, .override-manager-group-field select", function() {
      var form = $(this).closest(".override-manager-edit-form")[0];
      if (form) OverrideManager._updateBaseFormula(form);
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

    Shiny.addCustomMessageHandler("override-manager-update", function(data) {
      OverrideManager.update(data);
    });

    Shiny.addCustomMessageHandler("override-manager-error", function(data) {
      OverrideManager.showError(data.message);
    });

    console.log("[OverrideManager] handlers registered");
  });
})();
