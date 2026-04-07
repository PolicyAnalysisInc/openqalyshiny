/* OQGrid Spec — States Table */
(function() {
  "use strict";

  OQGrid.registerSpec("states", function() {
    return {
      name: "states",
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

      getColumnDefs: function(data) {
        var cols = [
          { title: "Name", field: "name", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Display Name", field: "display_name", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Description", field: "description", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash }
        ];

        if (data.modelType === "markov") {
          cols.push({
            title: "Initial Probability", field: "initial_probability",
            widthGrow: 2, minWidth: 200,
            editor: OQGrid.editors.formula(data.terms, data.suggestions),
            formatter: OQGrid.fmt.formula(data.terms)
          });
          cols.push({
            title: "State Group", field: "state_group", minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash
          });
          cols.push({
            title: "Share State Time", field: "share_state_time", minWidth: 120,
            editor: "list",
            editorParams: {
              values: [{ label: "Yes", value: "Yes" }, { label: "No", value: "No" }]
            },
            formatter: OQGrid.fmt.yesNo
          });
          cols.push({
            title: "Cycle Limit", field: "state_cycle_limit", minWidth: 100,
            editor: OQGrid.editors.numeric(), formatter: OQGrid.fmt.cycleLimit
          });
          cols.push({
            title: "Cycle Limit Unit", field: "state_cycle_limit_unit", minWidth: 120,
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

      // Add/delete only for markov — null addRow and no remove action for other types
      addRow: null, // set dynamically below

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
        remove: null, // set dynamically below
        edit: function(row, field, value, oldValue) {
          var name = row.name;
          if (field === "name") name = oldValue || "";
          return {
            type: "edit_state",
            name: name, field: field, value: value
          };
        }
      },

      // Dynamic setup based on model type
      onInit: function(controller) {
        var data = controller.data;
        if (data.modelType === "markov") {
          // Enable add row for markov
          controller.spec.addRow = {
            buttonText: "+ Add State",
            emptyRow: {
              name: "", display_name: "", description: "",
              initial_probability: "0", state_group: "", share_state_time: "No",
              state_cycle_limit: "", state_cycle_limit_unit: "cycles"
            },
            generateDefaults: function(row, tableData) {
              var existing = {};
              for (var i = 0; i < tableData.length; i++) existing[tableData[i].name] = true;
              var n = 1;
              while (existing["new_state_" + n]) n++;
              row.name = "new_state_" + n;
            }
          };
          controller._setupAddButton();

          // Enable remove for markov
          controller.spec.actions.remove = function(row) {
            return { type: "remove_state", name: row.name };
          };
        }
      }
    };
  });
})();
