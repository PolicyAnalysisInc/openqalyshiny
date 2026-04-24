/* OQGrid Spec — Summaries Table */
(function() {
  "use strict";

  OQGrid.registerSpec("summaries", function() {
    return {
      name: "summaries",
      nonClearableFields: ["name", "type", "display_name"],
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
            titleFormatter: OQGrid.utils.infoTitle("Outcome summaries aggregate health outcomes. Cost summaries aggregate costs."),
            editor: "list",
            editorParams: {
              values: [
                { label: "Outcome", value: "outcome" },
                { label: "Cost", value: "cost" }
              ]
            },
            formatter: OQGrid.fmt.capitalize },
          { title: "WTP", field: "wtp", minWidth: 80,
            titleFormatter: OQGrid.utils.infoTitle("Willingness-to-pay threshold (per unit of outcome) used for net monetary benefit calculations."),
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Values", field: "values", widthGrow: 2, minWidth: 300,
            titleFormatter: OQGrid.utils.infoTitle("Select which outcome or cost values to include in this summary."),
            editor: OQGrid.editors.multiTag(function(rowData) {
              var rowType = rowData.type || "outcome";
              return rowType === "cost" ? data.costValues : data.outcomeValues;
            }),
            formatter: OQGrid.fmt.tags }
        ];
      },

      addRow: {
        buttonText: "+ Add Summary",
        emptyRow: {
          name: "", values: "", type: "outcome",
          display_name: "", description: "", wtp: ""
        },
        canAdd: function(tableData, data) {
          return (data.outcomeValues && data.outcomeValues.length > 0) ||
                 (data.costValues && data.costValues.length > 0);
        },
        generateDefaults: function(row, tableData, data) {
          var existing = {};
          for (var i = 0; i < tableData.length; i++) existing[tableData[i].name] = true;
          var n = 1;
          while (existing["new_summary_" + n]) n++;
          row.name = "new_summary_" + n;
          var availableVals = (row.type === "cost") ? data.costValues : data.outcomeValues;
          if (availableVals && availableVals.length > 0) {
            row.values = availableVals.join(",");
          }
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
