/* OQGrid Spec — Summaries Table */
(function() {
  "use strict";

  OQGrid.registerSpec("summaries", function() {
    return {
      name: "summaries",
      containerSelector: ".summaries-table-container",
      dispatchMode: "event",

      parseData: function(el) {
        return {
          initial: OQGrid.utils.parseDataAttr(el, "initial", []),
          outcomeValues: OQGrid.utils.parseDataAttr(el, "outcomeValues", []),
          costValues: OQGrid.utils.parseDataAttr(el, "costValues", [])
        };
      },

      getColumnDefs: function(data) {
        return [
          { title: "Name", field: "name", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Display Name", field: "display_name", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Description", field: "description", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Type", field: "type", minWidth: 100,
            editor: "list",
            editorParams: {
              values: [
                { label: "Outcome", value: "outcome" },
                { label: "Cost", value: "cost" }
              ]
            },
            formatter: OQGrid.fmt.capitalize },
          { title: "WTP", field: "wtp", minWidth: 80,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Values", field: "values", widthGrow: 2, minWidth: 300,
            editor: OQGrid.editors.multiTag(function(rowData) {
              var rowType = rowData.type || "outcome";
              return rowType === "cost" ? data.costValues : data.outcomeValues;
            }),
            formatter: OQGrid.fmt.tags }
        ];
      },

      addRow: {
        buttonText: "+ Add Summary",
        requireConfirm: true,
        firstEditField: "name",
        emptyRow: {
          _isNew: true, name: "", values: "", type: "outcome",
          display_name: "", description: "", wtp: ""
        }
      },

      actions: {
        add: function(row) {
          return {
            type: "add_summary",
            name: (row.name || "").trim(),
            values: (row.values || "").trim(),
            display_name: (row.display_name || "").trim(),
            description: (row.description || "").trim(),
            summary_type: row.type || "outcome",
            wtp: (row.wtp || "").trim()
          };
        },
        addValidate: function(row) {
          if (!(row.name || "").trim() || !(row.values || "").trim())
            return "Name and values are required.";
          return null;
        },
        remove: function(row) {
          return { type: "remove_summary", name: row.name };
        },
        edit: function(row, field, value, oldValue) {
          var name = row.name;
          if (field === "name") name = oldValue || "";
          return {
            type: "edit_summary",
            name: name, field: field, value: value
          };
        }
      }
    };
  });
})();
