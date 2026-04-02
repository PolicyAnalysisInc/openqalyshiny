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
            editor: fEditor, formatter: OQGrid.fmt.emdash });
        } else if (data.modelType === "psm") {
          cols.push({ title: "Endpoint", field: "endpoint", widthGrow: 1, minWidth: 140,
            editor: "input", formatter: OQGrid.fmt.emdash });
          cols.push({ title: "Time Unit", field: "time_unit", minWidth: 120,
            editor: "list", editorParams: { values: timeUnitValues },
            formatter: OQGrid.fmt.displayMap(timeUnitMap) });
          cols.push({ title: "Formula", field: "formula", widthGrow: 2, minWidth: 450,
            editor: fEditor, formatter: OQGrid.fmt.emdash });
        } else if (data.modelType === "custom_psm") {
          cols.push({ title: "State", field: "state", minWidth: 140,
            editor: "list", editorParams: { values: stateValues },
            formatter: OQGrid.fmt.emdash });
          cols.push({ title: "Formula", field: "formula", widthGrow: 2, minWidth: 450,
            editor: fEditor, formatter: OQGrid.fmt.emdash });
        }

        return cols;
      },

      addRow: {
        buttonText: "+ Add Transition",
        requireConfirm: true,
        firstEditField: null, // set dynamically below
        emptyRow: function(data) {
          var row = { _isNew: true, formula: "" };
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
        }
      },

      // Set firstEditField dynamically after init
      onInit: function(controller) {
        var mt = controller.data.modelType;
        controller.spec.addRow.firstEditField =
          mt === "markov" ? "from_state" :
          mt === "psm" ? "endpoint" : "state";
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
        addValidate: function(row) {
          if (row.from_state !== undefined) {
            if (!(row.from_state || "").trim() || !(row.to_state || "").trim() || !(row.formula || "").trim())
              return "From State, To State, and Formula are required.";
          } else if (row.endpoint !== undefined) {
            if (!(row.endpoint || "").trim() || !(row.formula || "").trim())
              return "Endpoint and Formula are required.";
          } else if (row.state !== undefined) {
            if (!(row.state || "").trim() || !(row.formula || "").trim())
              return "State and Formula are required.";
          }
          return null;
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
