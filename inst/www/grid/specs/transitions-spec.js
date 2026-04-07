/* OQGrid Spec — Transitions Table */
(function() {
  "use strict";

  OQGrid.registerSpec("transitions", function() {
    return {
      name: "transitions",
      containerSelector: ".transitions-table-container",
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

      getColumnDefs: function(data) {
        var cols = [];
        var fEditor = OQGrid.editors.formula(data.terms, data.suggestions);
        var stateValues = data.stateNames.map(function(s) {
          return { label: s, value: s };
        });
        var timeUnitValues = [
          { label: "Cycles", value: "cycles" },
          { label: "Days", value: "days" },
          { label: "Weeks", value: "weeks" },
          { label: "Months", value: "months" },
          { label: "Years", value: "years" }
        ];
        var timeUnitMap = { cycles: "Cycles", days: "Days", weeks: "Weeks", months: "Months", years: "Years" };

        if (data.modelType === "markov") {
          cols.push({ title: "From State", field: "from_state", minWidth: 140,
            editor: "list", editorParams: { values: stateValues },
            formatter: OQGrid.fmt.emdash });
          cols.push({ title: "To State", field: "to_state", minWidth: 140,
            editor: "list", editorParams: { values: stateValues },
            formatter: OQGrid.fmt.emdash });
          cols.push({ title: "Formula", field: "formula", widthGrow: 2, minWidth: 450,
            editor: fEditor, formatter: OQGrid.fmt.formula(data.terms) });
        } else if (data.modelType === "psm") {
          cols.push({ title: "Endpoint", field: "endpoint", widthGrow: 1, minWidth: 140,
            editor: "input", formatter: OQGrid.fmt.emdash });
          cols.push({ title: "Time Unit", field: "time_unit", minWidth: 120,
            editor: "list", editorParams: { values: timeUnitValues },
            formatter: OQGrid.fmt.displayMap(timeUnitMap) });
          cols.push({ title: "Formula", field: "formula", widthGrow: 2, minWidth: 450,
            editor: fEditor, formatter: OQGrid.fmt.formula(data.terms) });
        } else if (data.modelType === "custom_psm") {
          cols.push({ title: "State", field: "state", minWidth: 140,
            editor: "list", editorParams: { values: stateValues },
            formatter: OQGrid.fmt.emdash });
          cols.push({ title: "Formula", field: "formula", widthGrow: 2, minWidth: 450,
            editor: fEditor, formatter: OQGrid.fmt.formula(data.terms) });
        }

        return cols;
      },

      addRow: {
        buttonText: "+ Add Transition",
        emptyRow: function(data) {
          var row = { formula: "0" };
          if (data.modelType === "markov") {
            row.from_state = "";
            row.to_state = "";
          } else if (data.modelType === "psm") {
            row.endpoint = "";
            row.time_unit = "cycles";
          } else if (data.modelType === "custom_psm") {
            row.state = "";
          }
          return row;
        },
        generateDefaults: function(row, tableData, data) {
          if (data.modelType === "markov") {
            var states = data.stateNames;
            var existing = {};
            for (var i = 0; i < tableData.length; i++) {
              existing[tableData[i].from_state + "\u2192" + tableData[i].to_state] = true;
            }
            for (var f = 0; f < states.length; f++) {
              for (var t = 0; t < states.length; t++) {
                if (!existing[states[f] + "\u2192" + states[t]]) {
                  row.from_state = states[f];
                  row.to_state = states[t];
                  return;
                }
              }
            }
            row.from_state = states[0] || "";
            row.to_state = states[0] || "";
          } else if (data.modelType === "psm") {
            var n = tableData.length + 1;
            var names = {};
            for (var i = 0; i < tableData.length; i++) names[tableData[i].endpoint] = true;
            while (names["endpoint_" + n]) n++;
            row.endpoint = "endpoint_" + n;
          } else if (data.modelType === "custom_psm") {
            var states = data.stateNames;
            var used = {};
            for (var i = 0; i < tableData.length; i++) used[tableData[i].state] = true;
            for (var s = 0; s < states.length; s++) {
              if (!used[states[s]]) { row.state = states[s]; return; }
            }
            row.state = states[0] || "";
          }
        }
      },

      actions: {
        add: function(row) {
          var payload = { type: "add_transition", model_type: row._modelType || "markov" };
          if (row.from_state !== undefined) {
            payload.model_type = "markov";
            payload.from_state = (row.from_state || "").trim();
            payload.to_state = (row.to_state || "").trim();
            payload.formula = (row.formula || "").trim();
          } else if (row.endpoint !== undefined) {
            payload.model_type = "psm";
            payload.endpoint = (row.endpoint || "").trim();
            payload.time_unit = (row.time_unit || "cycles").trim();
            payload.formula = (row.formula || "").trim();
          } else if (row.state !== undefined) {
            payload.model_type = "custom_psm";
            payload.state = (row.state || "").trim();
            payload.formula = (row.formula || "").trim();
          }
          return payload;
        },
        remove: function(row) {
          var payload = { type: "remove_transition" };
          if (row.from_state !== undefined) {
            payload.model_type = "markov";
            payload.from_state = row.from_state;
            payload.to_state = row.to_state;
          } else if (row.endpoint !== undefined) {
            payload.model_type = "psm";
            payload.endpoint = row.endpoint;
          } else if (row.state !== undefined) {
            payload.model_type = "custom_psm";
            payload.state = row.state;
          }
          return payload;
        },
        edit: function(row, field, value, oldValue) {
          var payload = { type: "edit_transition", field: field, value: value };
          if (row.from_state !== undefined || field === "from_state" || field === "to_state") {
            payload.model_type = "markov";
            payload.from_state = row.from_state;
            payload.to_state = row.to_state;
            if (field === "from_state") payload.from_state = oldValue || "";
            if (field === "to_state") payload.to_state = oldValue || "";
          } else if (row.endpoint !== undefined || field === "endpoint") {
            payload.model_type = "psm";
            payload.endpoint = row.endpoint;
            if (field === "endpoint") payload.endpoint = oldValue || "";
          } else if (row.state !== undefined || field === "state") {
            payload.model_type = "custom_psm";
            payload.state = row.state;
            if (field === "state") payload.state = oldValue || "";
          }
          return payload;
        }
      }
    };
  });
})();
