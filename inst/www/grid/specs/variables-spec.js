/* OQGrid Spec — Variables Table */
(function() {
  "use strict";

  OQGrid.registerSpec("variables", function() {
    return {
      name: "variables",
      nonClearableFields: ["name", "formula", "display_name"],
      containerSelector: ".variables-table-container",
      dispatchMode: "event",

      parseData: function(el) {
        return {
          initial: OQGrid.utils.parseDataAttr(el, "initial", []),
          strategies: OQGrid.utils.parseDataAttr(el, "strategies", {}),
          groups: OQGrid.utils.parseDataAttr(el, "groups", {}),
          terms: OQGrid.utils.parseDataAttr(el, "terms", null),
          suggestions: OQGrid.utils.parseDataAttr(el, "suggestions", null)
        };
      },

      getColumnDefs: function(data) {
        var stratDisplay = OQGrid.utils.buildDisplayMap(data.strategies);
        var groupDisplay = OQGrid.utils.buildDisplayMap(data.groups);

        return [
          { title: "Name", field: "name", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },

          { title: "Strategy", field: "strategy", minWidth: 120,
            titleFormatter: OQGrid.utils.infoTitle("Scope this variable to a specific strategy. Leave empty for a global variable."),
            editor: "list",
            editorParams: { values: OQGrid.utils.toListValues(data.strategies, { emptyLabel: "\u2014 (None)" }) },
            formatter: OQGrid.fmt.displayMap(stratDisplay) },

          { title: "Group", field: "group", minWidth: 120,
            titleFormatter: OQGrid.utils.infoTitle("Scope this variable to a specific group. Leave empty for a global variable."),
            editor: "list",
            editorParams: { values: OQGrid.utils.toListValues(data.groups, { emptyLabel: "\u2014 (None)" }) },
            formatter: OQGrid.fmt.displayMap(groupDisplay) },

          { title: "Display Name", field: "display_name", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },

          { title: "Description", field: "description", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },

          { title: "Formula", field: "formula", widthGrow: 2, minWidth: 450,
            titleFormatter: OQGrid.utils.infoTitle("Expression using numbers, operators, other variables, time variables (cycle, day, month, year and state_ variants), and functions like trace()."),
            editor: OQGrid.editors.formula(data.terms, data.suggestions),
            formatter: OQGrid.fmt.formula(data.terms) }
        ];
      },

      addRow: {
        buttonText: "+ Add Variable",
        emptyRow: {
          name: "", formula: "0",
          display_name: "", description: "",
          strategy: "", group: ""
        },
        generateDefaults: function(row, tableData) {
          var existing = {};
          for (var i = 0; i < tableData.length; i++) existing[tableData[i].name] = true;
          var n = 1;
          while (existing["new_variable_" + n]) n++;
          row.name = "new_variable_" + n;
        }
      },

      actions: {
        add: function(row) {
          return {
            type: "add_variable",
            name: (row.name || "").trim(),
            formula: (row.formula || "").trim(),
            display_name: (row.display_name || "").trim(),
            description: (row.description || "").trim()
          };
        },
        remove: function(row) {
          return {
            type: "remove_variable",
            name: row.name,
            strategy: row.strategy || "",
            group: row.group || ""
          };
        },
        edit: function(row, field, value, oldValue) {
          var name = row.name, strategy = row.strategy || "", group = row.group || "";
          if (field === "name") name = oldValue || "";
          if (field === "strategy") strategy = oldValue || "";
          if (field === "group") group = oldValue || "";
          return {
            type: "edit_variable",
            name: name, strategy: strategy, group: group,
            field: field, value: value
          };
        }
      }
    };
  });
})();
