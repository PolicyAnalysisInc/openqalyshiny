/* OQGrid Spec — Values Table */
(function() {
  "use strict";

  OQGrid.registerSpec("values", function() {
    // Module-level table reference for typeahead's dynamic data access
    var _table = null;

    return {
      name: "values",
      containerSelector: ".values-table-container",
      dispatchMode: "event",

      parseData: function(el) {
        return {
          initial: OQGrid.utils.parseDataAttr(el, "initial", []),
          modelType: el.dataset.modelType || "markov",
          stateNames: OQGrid.utils.parseDataAttr(el, "stateNames", []),
          terms: OQGrid.utils.parseDataAttr(el, "terms", null),
          suggestions: OQGrid.utils.parseDataAttr(el, "suggestions", null)
        };
      },

      getColumnDefs: function(data, controller) {
        _table = controller.table;
        var fEditor = OQGrid.editors.formula(data.terms, data.suggestions);

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

        var cols = [
          { title: "Name", field: "name", widthGrow: 1, minWidth: 120,
            editor: nameEditor, formatter: OQGrid.fmt.emdash },
          { title: "Display Name", field: "display_name", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Description", field: "description", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Type", field: "type", minWidth: 100,
            editor: "list", editorParams: { values: typeValues },
            formatter: OQGrid.fmt.capitalize }
        ];

        // State column (not for decision_tree)
        if (data.modelType !== "decision_tree") {
          cols.push({
            title: "State", field: "state", minWidth: 140,
            editor: "list", editorParams: { values: stateDropdownValues },
            formatter: OQGrid.fmt.emdash
          });
        }

        // Destination column (markov only)
        if (data.modelType === "markov") {
          cols.push({
            title: "Destination", field: "destination", minWidth: 140,
            editor: "list", editorParams: { values: destDropdownValues },
            formatter: OQGrid.fmt.emdash
          });
        }

        cols.push({ title: "Formula", field: "formula", widthGrow: 2, minWidth: 450,
          editor: fEditor, formatter: OQGrid.fmt.emdash });
        cols.push({ title: "Discounting Override", field: "discounting_override",
          widthGrow: 1, minWidth: 120,
          editor: "input", formatter: OQGrid.fmt.emdash });

        return cols;
      },

      addRow: {
        buttonText: "+ Add Value",
        requireConfirm: true,
        firstEditField: "name",
        emptyRow: function(data) {
          var row = {
            _isNew: true, name: "", formula: "", type: "cost",
            display_name: "", description: "", discounting_override: ""
          };
          if (data.modelType !== "decision_tree") row.state = "";
          if (data.modelType === "markov") row.destination = "";
          return row;
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
            discounting_override: (row.discounting_override || "").trim()
          };
        },
        addValidate: function(row) {
          if (!(row.name || "").trim() || !(row.formula || "").trim())
            return "Name and formula are required.";
          return null;
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
