/* OQGrid Spec — Groups Table */
(function() {
  "use strict";

  OQGrid.registerSpec("groups", function() {
    return {
      name: "groups",
      containerSelector: ".groups-table-container",
      dispatchMode: "event",

      parseData: function(el) {
        return {
          initial: OQGrid.utils.parseDataAttr(el, "initial", []),
          varColumns: OQGrid.utils.parseDataAttr(el, "varColumns", []),
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
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Weight", field: "weight", minWidth: 80,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Enabled", field: "enabled", minWidth: 120,
            editor: "list",
            editorParams: { values: [{ label: "Yes", value: 1 }, { label: "No", value: 0 }] },
            formatter: OQGrid.fmt.yesNo }
        ];

        // Dynamic var__ columns for group-scoped variables
        if (data.varColumns && data.varColumns.length > 0) {
          var fEditor = OQGrid.editors.formula(data.terms, data.suggestions);
          for (var i = 0; i < data.varColumns.length; i++) {
            cols.push({
              title: data.varColumns[i].display_name || data.varColumns[i].name,
              field: "var__" + data.varColumns[i].name,
              widthGrow: 1, minWidth: 450,
              editor: fEditor,
              formatter: OQGrid.fmt.formula(data.terms)
            });
          }
        }

        return cols;
      },

      addRow: {
        buttonText: "+ Add Group",
        useModal: true,
        modalAction: "show_add_group_modal"
      },

      actions: {
        remove: function(row) {
          return { type: "remove_group", name: row.name };
        },
        edit: function(row, field, value, oldValue) {
          var name = row.name;
          if (field === "name") name = oldValue || "";
          return {
            type: "edit_group",
            name: name, field: field, value: value
          };
        },
        editVariable: function(row, field, value, oldValue) {
          var varName = field.substring(5);
          var groupName = row.name;
          if (!oldValue) {
            return {
              type: "add_variable",
              name: varName, group: groupName, formula: value
            };
          }
          return {
            type: "edit_variable",
            name: varName, group: groupName,
            field: "formula", value: value
          };
        }
      }
    };
  });
})();
