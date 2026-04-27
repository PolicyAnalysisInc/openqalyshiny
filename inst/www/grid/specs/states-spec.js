/* OQGrid Spec — States Table */
(function() {
  "use strict";

  OQGrid.registerSpec("states", function() {
    return {
      name: "states",
      nonClearableFields: ["name", "share_state_time", "state_cycle_limit_unit", "initial_probability", "display_name"],
      containerSelector: ".states-table-container",
      dispatchMode: "event",

      parseData: function(el) {
        return {
          initial: OQGrid.utils.parseDataAttr(el, "initial", []),
          modelType: el.dataset.modelType || "markov",
          terms: OQGrid.utils.parseDataAttr(el, "terms", null),
          suggestions: OQGrid.utils.parseDataAttr(el, "suggestions", null)
        };
      },

      getColumnDefs: function(data, controller) {
        var cols = [
          { title: "Name", field: "name", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Display Name", field: "display_name", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Description", field: "description", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash }
        ];

        if (data.modelType === "markov" || data.modelType === "custom_psm") {
          // Enable row removal for model types that allow user-defined states
          controller.spec.actions.remove = function(row) {
            return { type: "remove_state", name: row.name };
          };
        }

        if (data.modelType === "markov") {
          cols.push({
            title: "Initial Probability", field: "initial_probability",
            titleFormatter: OQGrid.utils.infoTitle("Starting probability for each state. Values across all states must sum to 1. Use 'C' for the complement."),
            widthGrow: 2, minWidth: 200,
            editor: OQGrid.editors.formula(data.terms, data.suggestions),
            formatter: OQGrid.fmt.formula(data.terms)
          });
          cols.push({
            title: "State Group", field: "state_group", minWidth: 120,
            titleFormatter: OQGrid.utils.infoTitle("Groups related states together. Used to enforce consistent time limits when Share State Time is enabled."),
            editor: "input", formatter: OQGrid.fmt.emdash
          });
          cols.push({
            title: "Share State Time", field: "share_state_time", minWidth: 120,
            titleFormatter: OQGrid.utils.infoTitle("When Yes, all states in the same State Group share a combined time limit and must have identical Cycle Limit values."),
            editor: "list",
            editorParams: {
              values: [{ label: "Yes", value: "Yes" }, { label: "No", value: "No" }]
            },
            formatter: OQGrid.fmt.yesNo
          });
          cols.push({
            title: "Cycle Limit", field: "state_cycle_limit", minWidth: 100,
            titleFormatter: OQGrid.utils.infoTitle("Maximum time an individual can remain in this state before being forced to transition."),
            editor: OQGrid.editors.numeric(), formatter: OQGrid.fmt.cycleLimit
          });
          cols.push({
            title: "Cycle Limit Unit", field: "state_cycle_limit_unit", minWidth: 120,
            titleFormatter: OQGrid.utils.infoTitle("Time unit for the cycle limit. Converted to cycles internally."),
            editor: "list",
            editorParams: {
              values: [
                { label: "Cycles", value: "cycles" },
                { label: "Days", value: "days" },
                { label: "Weeks", value: "weeks" },
                { label: "Months", value: "months" },
                { label: "Years", value: "years" }
              ]
            },
            formatter: OQGrid.fmt.displayMap({
              cycles: "Cycles", days: "Days", weeks: "Weeks",
              months: "Months", years: "Years"
            })
          });
        }

        return cols;
      },

      // Add/delete only for markov/custom_psm — addRow set dynamically in onInit
      addRow: null,

      actions: {
        add: function(row) {
          var payload = {
            type: "add_state",
            name: (row.name || "").trim(),
            display_name: (row.display_name || "").trim(),
            description: (row.description || "").trim()
          };
          // Include markov-specific fields
          if (row.initial_probability !== undefined) {
            payload.initial_probability = (row.initial_probability || "").trim();
            payload.state_group = (row.state_group || "").trim();
            payload.share_state_time = row.share_state_time || "No";
            payload.state_cycle_limit = (row.state_cycle_limit || "").toString().trim();
            payload.state_cycle_limit_unit = row.state_cycle_limit_unit || "cycles";
          }
          return payload;
        },
        edit: function(row, field, value, oldValue) {
          var name = row.name;
          if (field === "name") name = oldValue || "";
          return {
            type: "edit_state",
            name: name, field: field, value: value
          };
        }
      },

      // Dynamic setup based on model type.
      // Markov and custom_psm both allow users to define their own states;
      // PSM does not (its 3 canonical states are seeded at model creation).
      onInit: function(controller) {
        var data = controller.data;
        if (data.modelType === "markov" || data.modelType === "custom_psm") {
          var emptyRow = data.modelType === "markov"
            ? {
                name: "", display_name: "", description: "",
                initial_probability: "0", state_group: "", share_state_time: "No",
                state_cycle_limit: "", state_cycle_limit_unit: "cycles"
              }
            : {
                name: "", display_name: "", description: ""
              };

          controller.spec.addRow = {
            buttonText: "+ Add State",
            emptyRow: emptyRow,
            generateDefaults: function(row, tableData) {
              var existing = {};
              for (var i = 0; i < tableData.length; i++) existing[tableData[i].name] = true;
              var n = 1;
              while (existing["new_state_" + n]) n++;
              row.name = "new_state_" + n;
            }
          };
          controller._setupAddButton();
        }
      }
    };
  });
})();
