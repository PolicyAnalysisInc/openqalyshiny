/* OQGrid Spec — Trees Table */
(function() {
  "use strict";

  OQGrid.registerSpec("trees", function() {
    var _table = null;

    return {
      name: "trees",
      nonClearableFields: ["node", "formula"],
      containerSelector: ".trees-table-container",
      dispatchMode: "event",

      parseData: function(el) {
        return {
          initial: OQGrid.utils.parseDataAttr(el, "initial", []),
          treeName: el.dataset.treeName || "",
          terms: OQGrid.utils.parseDataAttr(el, "terms", null),
          suggestions: OQGrid.utils.parseDataAttr(el, "suggestions", null)
        };
      },

      getColumnDefs: function(data, controller) {
        _table = controller.table;

        var getTableData = function() {
          return _table ? _table.getData() : data.initial;
        };

        // Available tags from current table data
        var getAvailableTags = function() {
          var tableData = getTableData();
          var seen = {}, tags = [];
          for (var i = 0; i < tableData.length; i++) {
            var t = tableData[i].tags;
            if (!t) continue;
            t.split(",").forEach(function(v) {
              v = v.trim();
              if (v && !seen[v]) { seen[v] = true; tags.push(v); }
            });
          }
          return tags;
        };

        return [
          { title: "Node", field: "node", widthGrow: 1, minWidth: 120,
            editor: "input", formatter: OQGrid.fmt.emdash },
          { title: "Parent", field: "parent", widthGrow: 1, minWidth: 120,
            titleFormatter: OQGrid.utils.infoTitle("Parent node in the decision tree hierarchy. Leave empty for the root node."),
            editor: "list",
            editorParams: {
              valuesLookup: function() {
                var tableData = getTableData();
                var seen = {}, nodes = [];
                nodes.push({ label: "\u2014 (None)", value: "" });
                for (var i = 0; i < tableData.length; i++) {
                  var n = tableData[i].node;
                  if (n && !seen[n]) { seen[n] = true; nodes.push({ label: n, value: n }); }
                }
                return nodes;
              }
            },
            formatter: OQGrid.fmt.emdash },
          { title: "Tags", field: "tags", widthGrow: 1, minWidth: 120,
            titleFormatter: OQGrid.utils.infoTitle("Labels for filtering or grouping tree branches (e.g. by treatment arm)."),
            editor: OQGrid.editors.multiTag(getAvailableTags, { allowFreeText: true }),
            formatter: OQGrid.fmt.tags },
          { title: "Formula", field: "formula", widthGrow: 2, minWidth: 200,
            titleFormatter: OQGrid.utils.infoTitle("Probability expression for chance nodes. Use 'C' for the complement. Terminal node probabilities are referenced via p() in value formulas."),
            editor: OQGrid.editors.formula(data.terms, data.suggestions),
            formatter: OQGrid.fmt.formula(data.terms) }
        ];
      },

      addRow: {
        buttonText: "+ Add Node",
        emptyRow: { node: "", parent: "", formula: "0", tags: "" },
        generateDefaults: function(row, tableData) {
          var existing = {};
          for (var i = 0; i < tableData.length; i++) existing[tableData[i].node] = true;
          var n = 1;
          while (existing["new_node_" + n]) n++;
          row.node = "new_node_" + n;
        },
        // Custom button placement in trees toolbar
        buttonContainer: function(containerDiv) {
          var treesEditor = containerDiv.closest(".trees-editor");
          return treesEditor ? treesEditor.querySelector(".trees-toolbar-left") : null;
        }
      },

      actions: {
        add: function(row) {
          return {
            type: "add_tree_node",
            tree_name: row._treeName || "",
            node: (row.node || "").trim(),
            parent: (row.parent || "").trim(),
            formula: (row.formula || "").trim(),
            tags: (row.tags || "").trim()
          };
        },
        remove: function(row) {
          return {
            type: "remove_tree_node",
            tree_name: row._treeName || "",
            node: row.node
          };
        },
        edit: function(row, field, value, oldValue) {
          var node = row.node;
          if (field === "node") node = oldValue || "";
          return {
            type: "edit_tree_node",
            tree_name: row._treeName || "",
            node: node, field: field, value: value
          };
        }
      },

      // Inject treeName into row data for add/remove/edit payloads
      onInit: function(controller) {
        var treeName = controller.data.treeName;
        var origAdd = controller.spec.actions.add;
        controller.spec.actions.add = function(row) {
          row._treeName = treeName;
          return origAdd(row);
        };
        var origRemove = controller.spec.actions.remove;
        controller.spec.actions.remove = function(row) {
          row._treeName = treeName;
          return origRemove(row);
        };
        var origEdit = controller.spec.actions.edit;
        controller.spec.actions.edit = function(row, field, value, oldValue) {
          row._treeName = treeName;
          return origEdit(row, field, value, oldValue);
        };
      }
    };
  });
})();
