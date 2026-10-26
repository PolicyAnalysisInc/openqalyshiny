/* OQGrid Spec — Values Table */
(function() {
  "use strict";

  OQGrid.registerSpec("values", function() {
    // Module-level table reference for typeahead's dynamic data access
    var _table = null;

    return {
      name: "values",
      nonClearableFields: ["name", "state", "destination", "type", "formula", "display_name"],
      containerSelector: ".values-table-container",
      dispatchMode: "event",

      parseData: function(el) {
        return {
          initial: OQGrid.utils.parseDataAttr(el, "initial", []),
          modelType: el.dataset.modelType || "markov",
          stateNames: OQGrid.utils.parseDataAttr(el, "stateNames", []),
          terms: OQGrid.utils.parseDataAttr(el, "terms", null),
          suggestions: OQGrid.utils.parseDataAttr(el, "suggestions", null),
          overrideTerms: OQGrid.utils.parseDataAttr(el, "overrideTerms", null),
          overrideSuggestions: OQGrid.utils.parseDataAttr(el, "overrideSuggestions", null)
        };
      },

      getColumnDefs: function(data, controller) {
        _table = controller.table;
        var fEditor = OQGrid.editors.formula(data.terms, data.suggestions);
        var overrideEditor = OQGrid.editors.formula(data.overrideTerms, data.overrideSuggestions);

        // Typeahead for name — suggests existing value names from table data
        var nameEditor = OQGrid.editors.typeahead(function() {
          if (!_table) return [];
          var tableData = _table.getData();
          var seen = {}, names = [];
          for (var i = 0; i < tableData.length; i++) {
            var n = tableData[i].name;
            if (n && !seen[n]) { seen[n] = true; names.push(n); }
          }
          return names;
        });

        // State dropdown
        var stateDropdownValues = [
          { label: "All", value: "All" },
          { label: "All Other", value: "All Other" }
        ].concat(data.stateNames.map(function(s) { return { label: s, value: s }; }));

        // Destination dropdown (markov only)
        var destDropdownValues = [
          { label: "\u2014 (None)", value: "" }
        ].concat(data.stateNames.map(function(s) { return { label: s, value: s }; }));

        var typeValues = [
          { label: "Outcome", value: "outcome" },
          { label: "Cost", value: "cost" }
        ];

        // Per-value half-cycle method (blank = model setting)
        var halfCycleValues = [
          { label: "\u2014 (Model setting)", value: "" },
          { label: "Start", value: "start" },
          { label: "End", value: "end" },
          { label: "Life-Table", value: "life-table" }
        ];
        var halfCycleLabels = { "start": "Start", "end": "End", "life-table": "Life-Table" };

        var cols = [
          { title: "Name", field: "name", widthGrow: 1, minWidth: 120,
            editor: nameEditor, formatter: OQGrid.fmt.emdash },
          { title: "Display Name", field: "display_name", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Description", field: "description", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Type", field: "type", minWidth: 100,
            titleFormatter: OQGrid.utils.infoTitle("Outcome values contribute to health outcomes (e.g. QALYs). Cost values contribute to total costs."),
            editor: "list", editorParams: { values: typeValues },
            formatter: OQGrid.fmt.capitalize }
        ];

        // State column (not for decision_tree)
        if (data.modelType !== "decision_tree") {
          cols.push({
            title: "State", field: "state", minWidth: 140,
            titleFormatter: OQGrid.utils.infoTitle("'All' applies to every state. 'All Other' applies to states without a specific value defined."),
            editor: "list", editorParams: { values: stateDropdownValues },
            formatter: OQGrid.fmt.emdash
          });
        }

        // Destination column (markov and psm)
        if (data.modelType === "markov") {
          cols.push({
            title: "Destination", field: "destination", minWidth: 140,
            titleFormatter: OQGrid.utils.infoTitle("Specifies the destination state for transition values. Combined with State, creates a from/to transition pair. Leave empty for residency values."),
            editor: "list", editorParams: { values: destDropdownValues },
            formatter: OQGrid.fmt.emdash
          });
        } else if (data.modelType === "psm") {
          // A PSM only has two transitions: state 1 -> 2 (PFS events) and 2 -> 3 (deaths)
          var s1 = data.stateNames[0] || "", s2 = data.stateNames[1] || "", s3 = data.stateNames[2] || "";
          var psmDestinations = function(cell) {
            var from = cell.getRow().getData().state;
            var opts = [{ label: "\u2014 (None)", value: "" }];
            if (from === s1) opts.push({ label: s2, value: s2 });
            if (from === s2) opts.push({ label: s3, value: s3 });
            return { values: opts };
          };
          cols.push({
            title: "Destination", field: "destination", minWidth: 140,
            titleFormatter: OQGrid.utils.infoTitle("For transition values: " + s1 + " \u2192 " + s2 + " is charged on PFS events and " + s2 + " \u2192 " + s3 + " on deaths. Leave empty for residency values."),
            editor: "list", editorParams: psmDestinations,
            formatter: OQGrid.fmt.emdash
          });
        }

        cols.push({ title: "Formula", field: "formula", widthGrow: 2, minWidth: 450,
          titleFormatter: OQGrid.utils.infoTitle("Evaluated once per cycle: cycle_lag, year_lag, ... are the start of the cycle and cycle, year, ... its end. Cycle k's value applies only to cycle k, to the people counted by the half-cycle method."),
          editor: fEditor, formatter: OQGrid.fmt.formula(data.terms) });
        // Half-cycle method column (not for decision_tree)
        if (data.modelType !== "decision_tree") {
          cols.push({
            title: "Half-Cycle Method", field: "half_cycle_method", minWidth: 130,
            titleFormatter: OQGrid.utils.infoTitle("Overrides the model's half-cycle method for this value, including when it is discounted. Leave empty to use the model setting. For a one-off charged to everyone in the state at the start of a cycle, such as ifelse(cycle == 1, cost, 0), use Start. For an entry cost written with state_cycle == 1, use End. On transition values this only changes when the value is discounted. All rows of a value share one setting."),
            editor: "list", editorParams: { values: halfCycleValues },
            formatter: function(cell) {
              var v = cell.getValue();
              return halfCycleLabels[v] || "\u2014";
            }
          });
        }

        cols.push({ title: "Discounting Override", field: "discounting_override",
          titleFormatter: OQGrid.utils.infoTitle("Formula replacing this value's discount factors. Return one number to multiply the value's undiscounted cycle totals, or one discount factor per time point, from the start of the model to the end of the last cycle (e.g. 1.015^-discount_time). Can reference discount_rate, and per time point discount_factors, discount_time and trace('state'), the person-time in a state. For example, sum(discount_factors * trace('progressed')) / sum(trace('progressed')) is the average discount factor of time in progressed."),
          widthGrow: 1, minWidth: 160,
          editor: overrideEditor, formatter: OQGrid.fmt.formula(data.overrideTerms) });

        return cols;
      },

      addRow: {
        buttonText: "+ Add Value",
        emptyRow: function(data) {
          var row = {
            name: "", formula: "0", type: "cost",
            display_name: "", description: "", discounting_override: "",
            half_cycle_method: ""
          };
          if (data.modelType !== "decision_tree") row.state = "All";
          if (data.modelType === "markov" || data.modelType === "psm") row.destination = "";
          return row;
        },
        generateDefaults: function(row, tableData) {
          var existing = {};
          for (var i = 0; i < tableData.length; i++) existing[tableData[i].name] = true;
          var n = 1;
          while (existing["new_value_" + n]) n++;
          row.name = "new_value_" + n;
        }
      },

      // Register server-push message handler for values-table-update
      messageHandler: function(controller) {
        _table = controller.table;
        var inputId = controller.inputId;
        OQGrid.shiny.registerMessageHandler("values-table-update", function(msg) {
          if (msg.inputId !== inputId) return;
          if (!_table) return;

          // Save scroll position
          var holder = _table.element.querySelector(".tabulator-tableholder");
          var scrollLeft = holder ? holder.scrollLeft : 0;
          var scrollTop = holder ? holder.scrollTop : 0;

          // Replace data
          _table.replaceData(msg.data);

          // Restore scroll position
          if (holder) {
            requestAnimationFrame(function() {
              holder.scrollLeft = scrollLeft;
              holder.scrollTop = scrollTop;
            });
          }
        });
      },

      actions: {
        add: function(row) {
          return {
            type: "add_value",
            name: (row.name || "").trim(),
            formula: (row.formula || "").trim(),
            state: (row.state || "").trim(),
            destination: (row.destination || "").trim(),
            value_type: (row.type || "cost").trim(),
            display_name: (row.display_name || "").trim(),
            description: (row.description || "").trim(),
            discounting_override: (row.discounting_override || "").trim(),
            half_cycle_method: (row.half_cycle_method || "").trim()
          };
        },
        remove: function(row) {
          return {
            type: "remove_value",
            name: row.name,
            state: row.state || "",
            destination: row.destination || ""
          };
        },
        edit: function(row, field, value, oldValue) {
          // Composite key: name + state + destination
          var name = row.name;
          var state = row.state || "";
          var destination = row.destination || "";
          if (field === "name") name = oldValue || "";
          if (field === "state") state = oldValue || "";
          if (field === "destination") destination = oldValue || "";

          var payload = {
            type: "edit_value",
            name: name, state: state, destination: destination,
            field: field, value: value
          };

          if (field === "name") {
            payload.error_on_name_sharing = true;
            payload.error_on_field_changes = true;
          }

          return payload;
        }
      }
    };
  });
})();
